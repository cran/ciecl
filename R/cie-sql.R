#' Obtener directorio de cache CIE-10
#'
#' @description
#' Retorna el path al directorio de cache. Permite override via env var
#' CIECL_CACHE_DIR para tests unitarios aislados.
#'
#' @returns String path al directorio
#' @keywords internal
#' @noRd
get_cache_dir <- function() {
  # Una env var seteada pero vacia ("") cuenta como no definida:
  # file.path("", "cie10.db") escribiria el cache en el directorio de
  # trabajo, fuera del directorio designado del usuario (CRAN policy)
  env <- Sys.getenv("CIECL_CACHE_DIR", unset = NA_character_)
  if (is.na(env) || !nzchar(env)) {
    return(tools::R_user_dir("ciecl", "data"))
  }
  env
}

#' Obtener conexion SQLite pooled CIE-10
#'
#' @description
#' Retorna conexion reutilizable a base SQLite en cache usuario.
#' Si no existe cache, lo construye atomicamente. Si la version no coincide,
#' reconstruye automaticamente.
#' Ubicacion: get_cache_dir()/cie10.db
#'
#' @returns Conexion DBI SQLite activa (pooled)
#' @keywords internal
#' @importFrom DBI dbConnect dbExistsTable dbWriteTable dbDisconnect dbIsValid
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom utils data packageVersion
#' @noRd
get_cie10_db <- function() {
  cache_dir <- get_cache_dir()
  db_path <- file.path(cache_dir, "cie10.db")

  # Pooling: reutilizar conexion existente si es valida y apunta al mismo path

  if (!is.null(.ciecl_env$con) &&
    inherits(.ciecl_env$con, "SQLiteConnection") &&
    DBI::dbIsValid(.ciecl_env$con) &&
    identical(.ciecl_env$db_path, db_path)) {
    # Failsafe: verificar que FTS5 existe (fix cache parcial)
    if (!DBI::dbExistsTable(.ciecl_env$con, "cie10_fts")) {
      build_fts(.ciecl_env$con)
    }

    # Verificar version del cache
    if (!cache_is_current(.ciecl_env$con)) {
      DBI::dbDisconnect(.ciecl_env$con)
      .ciecl_env$con <- NULL
      .ciecl_env$db_path <- NULL
      build_cache_atomic(cache_dir, db_path)
    } else {
      return(.ciecl_env$con)
    }
  }

  # Cerrar conexion anterior si existe pero es invalida

  if (!is.null(.ciecl_env$con)) {
    if (DBI::dbIsValid(.ciecl_env$con)) {
      suppressWarnings(DBI::dbDisconnect(.ciecl_env$con))
    }
    .ciecl_env$con <- NULL
    .ciecl_env$db_path <- NULL
  }

  # Construir cache si no existe
  if (!file.exists(db_path)) {
    build_cache_atomic(cache_dir, db_path)
  }

  # Conectar
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, loadable.extensions = FALSE)

  # Verificar integridad: tabla principal debe existir

  if (!DBI::dbExistsTable(con, "cie10")) {
    DBI::dbDisconnect(con)
    build_cache_atomic(cache_dir, db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path, loadable.extensions = FALSE)
  }

  # Failsafe FTS5
  if (!DBI::dbExistsTable(con, "cie10_fts")) {
    build_fts(con)
  }

  # Verificar version
  if (!cache_is_current(con)) {
    DBI::dbDisconnect(con)
    build_cache_atomic(cache_dir, db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path, loadable.extensions = FALSE)
  }

  # Guardar en pool
  .ciecl_env$con <- con
  .ciecl_env$db_path <- db_path

  return(con)
}

#' Construir cache SQLite atomicamente
#'
#' Construye en archivo temporal y renombra al final.
#' Si falla en cualquier punto, no queda cache parcial.
#'
#' @param cache_dir Directorio del cache
#' @param db_path Path final del archivo .db
#' @keywords internal
#' @noRd
build_cache_atomic <- function(cache_dir, db_path) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  tmp_path <- paste0(db_path, ".tmp")

  # Cleanup de .tmp residuales
  if (file.exists(tmp_path)) {
    file.remove(tmp_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_path)
  on.exit(
    {
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    },
    add = TRUE
  )

  # Progress steps solo en sesion interactiva para no contaminar tests/CI.
  # Usamos un id propio para gestionar el ciclo de vida manualmente y poder
  # cerrar el progreso en caso de error.
  show_progress <- rlang::is_interactive()
  progress_id <- NULL

  tryCatch(
    {
      # Etapa 1: Cargar dataset cie10_cl
      if (show_progress) {
        progress_id <- cli::cli_progress_step(
          "Cargando dataset {.field cie10_cl}",
          msg_done = "Dataset {.field cie10_cl} cargado"
        )
      }
      utils::data(cie10_cl, package = "ciecl", envir = environment())

      # Etapa 2: Escribir tabla cie10
      if (show_progress) {
        cli::cli_progress_step(
          "Escribiendo tabla {.field cie10}",
          msg_done = "Tabla {.field cie10} escrita"
        )
      }
      DBI::dbWriteTable(con, "cie10", cie10_cl, overwrite = TRUE)

      # Etapa 3: Construir indices
      if (show_progress) {
        cli::cli_progress_step(
          "Construyendo indices",
          msg_done = "Indices construidos"
        )
      }
      DBI::dbExecute(con, "CREATE INDEX idx_codigo ON cie10(codigo)")
      DBI::dbExecute(con, "CREATE INDEX idx_desc ON cie10(descripcion)")

      # Etapa 4: Crear FTS5
      if (show_progress) {
        cli::cli_progress_step(
          "Creando tabla {.field FTS5}",
          msg_done = "Tabla {.field FTS5} creada"
        )
      }
      build_fts(con, .progress = FALSE)

      # Metadata con version (sin step propio, parte del cierre)
      DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS cie10_meta (
        key TEXT PRIMARY KEY,
        value TEXT
      )
    ")
      pkg_version <- as.character(utils::packageVersion("ciecl"))
      DBI::dbExecute(
        con,
        "INSERT OR REPLACE INTO cie10_meta
       (key, value) VALUES ('cache_version', ?)",
        params = list(pkg_version)
      )

      # Cerrar antes de renombrar
      DBI::dbDisconnect(con)

      # Atomico: renombrar .tmp -> .db
      # En Windows el rename/remove puede fallar si otro proceso tiene
      # un lock sobre el .db: no silenciar el fallo
      if (file.exists(db_path) && !file.remove(db_path)) {
        cli::cli_warn("No se pudo eliminar el cache previo: {.path {db_path}}")
      }
      if (!file.rename(tmp_path, db_path)) {
        cli::cli_warn("No se pudo renombrar el cache temporal a {.path {db_path}}.")
      }

      if (show_progress) {
        cli::cli_progress_done(id = progress_id)
        cli::cli_inform(c("v" = "Cache SQLite creado: {.path {db_path}}"))
      }
    },
    error = function(e) {
      # Cleanup en caso de error
      if (show_progress && !is.null(progress_id)) {
        cli::cli_progress_done(id = progress_id, result = "failed")
      }
      if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
      if (file.exists(tmp_path)) file.remove(tmp_path)
      cli::cli_abort(
        "Error construyendo cache SQLite: {conditionMessage(e)}",
        class = "ciecl_cache_error",
        parent = e
      )
    }
  )
}

#' Construir tabla FTS5 sobre conexion existente
#'
#' @param con Conexion DBI activa
#' @keywords internal
#' @noRd
build_fts <- function(con, .progress = TRUE) {
  # Progress step solo en sesion interactiva. En tests/CI (no interactivo)
  # debe permanecer en silencio: hay tests con expect_silent(build_fts(con)).
  # `.progress = FALSE` permite que el caller (build_cache_atomic) gestione
  # el progreso global y evita anidar cli_progress_step.
  show_progress <- isTRUE(.progress) && rlang::is_interactive()
  if (show_progress) {
    cli::cli_progress_step(
      "Creando tabla FTS5",
      msg_done = "Tabla FTS5 creada/reconstruida"
    )
  }
  DBI::dbExecute(con, "
    CREATE VIRTUAL TABLE IF NOT EXISTS cie10_fts USING fts5(
      codigo, descripcion, inclusion, exclusion,
      content='cie10', content_rowid='rowid'
    )
  ")
  DBI::dbExecute(con, "INSERT INTO cie10_fts(cie10_fts) VALUES('rebuild')")
  if (show_progress) cli::cli_progress_done()
}

#' Verificar si el cache corresponde a la version actual del paquete
#'
#' @param con Conexion DBI activa
#' @returns Logical TRUE si version coincide
#' @keywords internal
#' @noRd
cache_is_current <- function(con) {
  if (!DBI::dbExistsTable(con, "cie10_meta")) {
    return(FALSE)
  }

  tryCatch(
    {
      cached_version <- DBI::dbGetQuery(
        con,
        "SELECT value FROM cie10_meta WHERE key = 'cache_version'"
      )
      if (nrow(cached_version) == 0) {
        return(FALSE)
      }

      pkg_version <- as.character(utils::packageVersion("ciecl"))
      return(identical(cached_version$value[1], pkg_version))
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

#' Ejecutar consultas SQL sobre CIE-10 Chile
#'
#' @description
#' Permite ejecutar sentencias SQL de solo lectura sobre la tabla `cie10`,
#' el mismo dataset que entrega [cie10_cl]. Útil para consultas que no
#' están cubiertas por [cie_search()]/[cie_lookup()] (agregaciones,
#' conteos por capítulo, joins con datos propios cargados en la misma
#' conexión, etc.). Para aprender SQL desde cero puede revisar
#' <https://www.w3schools.com/sql/> o la documentación de SQLite
#' (<https://www.sqlite.org/lang_select.html>).
#'
#' @param query String SQL válido SQLite. Soporta `SELECT`, `WHERE`,
#'   `JOIN`, `FROM`, `ORDER BY`, `GROUP BY` y `HAVING`. Por seguridad
#'   solo se permiten sentencias `SELECT` (sin escritura ni múltiples
#'   sentencias).
#' @param close `r lifecycle::badge("deprecated")` Ignorado - la conexión
#'   es pooled y se gestiona automáticamente. Será eliminado en una
#'   versión futura.
#' @returns tibble con el resultado de la consulta
#' @family sql_backend
#' @seealso [cie10_cl], [cie10_clear_cache()], [cie10_disconnect()],
#'   [cie_search()], [cie_guide()]
#' @export
#' @examples
#' # Buscar diabetes
#' cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE codigo LIKE 'E11%'")
#'
#' @examplesIf rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Contar por capitulo
#' cie10_sql("SELECT capitulo, COUNT(*) n FROM cie10 GROUP BY capitulo")
cie10_sql <- function(query, close = lifecycle::deprecated()) {
  if (lifecycle::is_present(close)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie10_sql(close = )",
      details = "La conexi\u00f3n es pooled y se gestiona autom\u00e1ticamente."
    )
  }

  check_required_es(missing(query), "query")
  if (!rlang::is_string(query)) {
    cli::cli_abort(
      "{.arg query} debe ser un string character no-NA de longitud 1, no {.obj_type_friendly {query}}.",
      class = "ciecl_invalid_input"
    )
  }

  # Normalizar query: eliminar espacios y saltos de línea al inicio
  query_norm <- stringr::str_trim(query)

  # Remover strings, comentarios de línea (--) y comentarios de bloque
  # (/* */) ANTES de validar y escanear: el chequeo de SELECT, el
  # blocklist y el chequeo de ";" deben operar sobre el SQL ejecutable,
  # no sobre literales legítimos (ej. LIKE '%drop%') ni comentarios
  query_sin_strings <- query_norm
  query_sin_strings <- stringr::str_remove_all(query_sin_strings, "'[^']*'")
  query_sin_strings <- stringr::str_remove_all(query_sin_strings, "--[^\n]*")
  query_sin_strings <- stringr::str_remove_all(
    query_sin_strings, "(?s)/\\*.*?\\*/"
  )
  query_sin_strings <- stringr::str_trim(query_sin_strings)

  # Validación de seguridad: solo SELECT permitido (tras el strip, así
  # una query que comienza con comentario sigue siendo válida)
  if (!stringr::str_detect(query_sin_strings, "(?i)^SELECT")) {
    cli::cli_abort("Solo consultas {.code SELECT} permitidas (seguridad).", class = "ciecl_unsafe_query")
  }

  # Bloquear keywords peligrosos (case-insensitive) sobre el SQL ya
  # limpio de literales y comentarios
  keywords_peligrosos <- c(
    "\\bDROP\\b", "\\bDELETE\\b", "\\bUPDATE\\b", "\\bINSERT\\b",
    "\\bALTER\\b", "\\bCREATE\\b", "\\bTRUNCATE\\b", "\\bEXEC\\b",
    "\\bATTACH\\b", "\\bDETACH\\b", "\\bPRAGMA\\b", "\\bWITH\\b",
    "\\bVACUUM\\b", "\\bREINDEX\\b", "\\bload_extension\\b"
  )

  for (keyword in keywords_peligrosos) {
    keyword_found <- stringr::str_detect(
      query_sin_strings, stringr::regex(keyword, ignore_case = TRUE)
    )
    if (keyword_found) {
      # Mostrar la palabra clave detectada (sin los anclajes \b del regex)
      keyword_limpio <- stringr::str_remove_all(keyword, "\\\\b")
      cli::cli_abort(
        "La consulta contiene una palabra clave no permitida: {.val {keyword_limpio}} (seguridad).",
        class = "ciecl_unsafe_query"
      )
    }
  }

  # Bloquear multiples statements (;)
  if (stringr::str_detect(query_sin_strings, ";")) {
    cli::cli_abort("M\u00faltiples sentencias SQL no permitidas (seguridad).", class = "ciecl_unsafe_query")
  }

  con <- get_cie10_db()

  resultado <- tryCatch(
    DBI::dbGetQuery(con, query) |> tibble::as_tibble(),
    error = function(e) {
      cli::cli_abort(
        "Error al ejecutar la consulta SQL: {conditionMessage(e)}",
        class = "ciecl_sql_error",
        parent = e
      )
    }
  )

  return(resultado)
}

#' Limpiar caché SQLite local (forzar rebuild)
#'
#' @description
#' `ciecl` construye, en el primer uso, un archivo SQLite (`cie10.db`)
#' a partir del dataset [cie10_cl] y lo guarda en una carpeta de datos
#' del usuario (ver `tools::R_user_dir("ciecl", "data")`). Esa "caché"
#' evita reconstruir la base en cada sesión. Esta función la elimina y
#' fuerza que la próxima consulta ([cie_search()], [cie_lookup()],
#' [cie10_sql()], etc.) la reconstruya desde cero.
#'
#' No es necesario llamarla tras actualizar el paquete: la caché guarda
#' la versión del paquete con que se construyó (tabla `cie10_meta`) y,
#' si la versión cambió, se reconstruye automáticamente en el primer
#' uso. Los casos en que conviene forzar el rebuild manual son: (1) se
#' sospecha que el archivo `.db` está corrupto (errores de lectura SQL
#' inesperados), o (2) se quiere liberar el espacio en disco que ocupa
#' la caché.
#'
#' @returns Sin valor de retorno, se llama por sus efectos secundarios
#'   (elimina la caché SQLite).
#' @family sql_backend
#' @seealso [cie10_sql()], [cie10_disconnect()]
#' @export
#' @examples
#' # Ver ubicación de la caché
#' tools::R_user_dir("ciecl", "data")
#'
#' @examplesIf rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' cie10_clear_cache() # Elimina cie10.db local
cie10_clear_cache <- function() {
  # Cerrar conexion pooled antes de borrar
  if (!is.null(.ciecl_env$con)) {
    if (DBI::dbIsValid(.ciecl_env$con)) {
      suppressWarnings(DBI::dbDisconnect(.ciecl_env$con))
    }
    .ciecl_env$con <- NULL
    .ciecl_env$db_path <- NULL
  }

  cache_dir <- get_cache_dir()
  db_path <- file.path(cache_dir, "cie10.db")
  tmp_path <- paste0(db_path, ".tmp")

  # file.remove() devuelve logical: no silenciar el fallo (típico en
  # Windows si otro proceso mantiene un lock sobre el archivo)
  habia_cache <- file.exists(db_path) || file.exists(tmp_path)
  eliminados <- FALSE

  if (file.exists(db_path)) {
    if (file.remove(db_path)) {
      eliminados <- TRUE
    } else {
      cli::cli_warn("No se pudo eliminar {.path {db_path}} (posible lock de otro proceso).")
    }
  }

  # Limpiar .tmp residuales
  if (file.exists(tmp_path)) {
    if (file.remove(tmp_path)) {
      eliminados <- TRUE
    } else {
      cli::cli_warn("No se pudo eliminar {.path {tmp_path}}.")
    }
  }

  if (eliminados) {
    cli::cli_inform(c("v" = "Cache SQLite eliminado: {.path {db_path}}"))
  } else if (!habia_cache) {
    cli::cli_inform(c("i" = "Cache no existe"))
  }

  invisible(NULL)
}

#' Cerrar conexión pooled SQLite
#'
#' @description
#' `ciecl` mantiene una única conexión SQLite reutilizable ("pooled")
#' abierta al archivo `cie10.db` durante la sesión, en lugar de abrir y
#' cerrar una conexión por cada consulta. Mientras esa conexión está
#' abierta, SQLite mantiene un "lock" (bloqueo) sobre el archivo `.db`:
#' es la forma en que SQLite evita lecturas/escrituras concurrentes
#' inconsistentes sobre el mismo archivo. Esta función cierra esa
#' conexión y libera el lock.
#'
#' Solo hace falta llamarla manualmente en dos casos: cuando se va a
#' eliminar o reemplazar el archivo `cie10.db` por fuera del paquete
#' (por ejemplo, con herramientas del sistema operativo), o al finalizar
#' un proceso batch largo para no dejar el archivo bloqueado. Para usar
#' [cie10_clear_cache()] no es necesario llamarla antes: esa función ya
#' cierra la conexión pooled internamente. Si no se libera el lock, el
#' archivo `.db` puede seguir abierto hasta que termine la sesión de R;
#' en la práctica esto rara vez es un problema porque cada sesión de R
#' tiene su propia conexión, pero impide que otro proceso externo (no R)
#' edite el archivo mientras la conexión esté abierta.
#'
#' @returns Sin valor de retorno, se llama por sus efectos secundarios
#'   (cierra la conexión SQLite pooled).
#' @family sql_backend
#' @seealso [cie10_sql()], [cie10_clear_cache()]
#' @export
#' @examples
#' # No hay un ejemplo no interactivo: el objeto de conexion vive en un
#' # entorno interno del paquete (.ciecl_env) y no es parte de la API
#' # publica, por lo que no hay nada que inspeccionar desde afuera.
#'
#' @examplesIf rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' cie10_disconnect()
cie10_disconnect <- function() {
  if (!is.null(.ciecl_env$con)) {
    if (DBI::dbIsValid(.ciecl_env$con)) {
      suppressWarnings(DBI::dbDisconnect(.ciecl_env$con))
    }
    .ciecl_env$con <- NULL
    .ciecl_env$db_path <- NULL
  }
  invisible(NULL)
}
