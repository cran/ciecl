#' Obtener conexion SQLite pooled CIE-10
#'
#' @description
#' Retorna conexion reutilizable a base SQLite en cache usuario.
#' Si no existe cache, lo construye atomicamente. Si la version no coincide,
#' reconstruye automaticamente.
#' Ubicacion: tools::R_user_dir("ciecl", "data")/cie10.db
#'
#' @return Conexion DBI SQLite activa (pooled)
#' @keywords internal
#' @importFrom DBI dbConnect dbExistsTable dbWriteTable dbDisconnect dbIsValid
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom utils data packageVersion
#' @importFrom dplyr %>%
#' @noRd
get_cie10_db <- function() {
  cache_dir <- tools::R_user_dir("ciecl", "data")
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
    try(DBI::dbDisconnect(.ciecl_env$con), silent = TRUE)
    .ciecl_env$con <- NULL
    .ciecl_env$db_path <- NULL
  }

  # Construir cache si no existe
  if (!file.exists(db_path)) {
    build_cache_atomic(cache_dir, db_path)
  }

  # Conectar
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Verificar integridad: tabla principal debe existir

  if (!DBI::dbExistsTable(con, "cie10")) {
    DBI::dbDisconnect(con)
    build_cache_atomic(cache_dir, db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  }

  # Failsafe FTS5
  if (!DBI::dbExistsTable(con, "cie10_fts")) {
    build_fts(con)
  }

  # Verificar version
  if (!cache_is_current(con)) {
    DBI::dbDisconnect(con)
    build_cache_atomic(cache_dir, db_path)
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
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
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  }, add = TRUE)

  tryCatch({
    # Cargar datos
    data(cie10_cl, envir = environment())
    DBI::dbWriteTable(con, "cie10", cie10_cl, overwrite = TRUE)

    # Indices
    DBI::dbExecute(con, "CREATE INDEX idx_codigo ON cie10(codigo)")
    DBI::dbExecute(con, "CREATE INDEX idx_desc ON cie10(descripcion)")

    # FTS5
    build_fts(con)

    # Metadata con version
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
    if (file.exists(db_path)) {
      file.remove(db_path)
    }
    file.rename(tmp_path, db_path)

    if (interactive()) message("Cache SQLite creado: ", db_path)
  }, error = function(e) {
    # Cleanup en caso de error
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
    if (file.exists(tmp_path)) file.remove(tmp_path)
    stop(
      "Error construyendo cache SQLite: ",
      conditionMessage(e), call. = FALSE
    )
  })
}

#' Construir tabla FTS5 sobre conexion existente
#'
#' @param con Conexion DBI activa
#' @keywords internal
#' @noRd
build_fts <- function(con) {
  DBI::dbExecute(con, "
    CREATE VIRTUAL TABLE IF NOT EXISTS cie10_fts USING fts5(
      codigo, descripcion, inclusion, exclusion,
      content='cie10', content_rowid='rowid'
    )
  ")
  DBI::dbExecute(con, "INSERT INTO cie10_fts(cie10_fts) VALUES('rebuild')")
  if (interactive()) message("Tabla FTS5 creada/reconstruida")
}

#' Verificar si el cache corresponde a la version actual del paquete
#'
#' @param con Conexion DBI activa
#' @return Logical TRUE si version coincide
#' @keywords internal
#' @noRd
cache_is_current <- function(con) {
  if (!DBI::dbExistsTable(con, "cie10_meta")) {
    return(FALSE)
  }

  tryCatch({
    cached_version <- DBI::dbGetQuery(
      con,
      "SELECT value FROM cie10_meta WHERE key = 'cache_version'"
    )
    if (nrow(cached_version) == 0) return(FALSE)

    pkg_version <- as.character(utils::packageVersion("ciecl"))
    return(identical(cached_version$value[1], pkg_version))
  }, error = function(e) {
    return(FALSE)
  })
}

#' Ejecutar consultas SQL sobre CIE-10 Chile
#'
#' @param query String SQL valido SQLite (SELECT/WHERE/JOIN)
#' @param close Deprecated. Ignorado — la conexion es pooled y se
#'   gestiona automaticamente. Sera eliminado en una version futura.
#' @return tibble resultado query
#' @family sql
#' @seealso \code{\link{cie10_clear_cache}}, \code{\link{cie10_disconnect}},
#'   \code{\link{cie_search}}
#' @export
#' @examples
#' # Buscar diabetes
#' cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE codigo LIKE 'E11%'")
#'
#' \donttest{
#' # Contar por capitulo
#' cie10_sql("SELECT capitulo, COUNT(*) n FROM cie10 GROUP BY capitulo")
#' }
cie10_sql <- function(query, close = TRUE) {
  if (!missing(close)) {
    .Deprecated(msg = paste0(
      "El argumento 'close' de cie10_sql() esta deprecado ",
      "y sera eliminado en una version futura.\n",
      "La conexion es pooled y se gestiona automaticamente."
    ))
  }
  # Normalizar query: eliminar espacios y saltos de linea al inicio
  query_norm <- stringr::str_trim(query)

  # Validacion de seguridad: solo SELECT permitido
  if (!stringr::str_detect(query_norm, "(?i)^SELECT")) {
    stop("Solo queries SELECT permitidas (seguridad)")
  }

 # Bloquear keywords peligrosos (case-insensitive)
  keywords_peligrosos <- c(
    "\\bDROP\\b", "\\bDELETE\\b", "\\bUPDATE\\b", "\\bINSERT\\b",
    "\\bALTER\\b", "\\bCREATE\\b", "\\bTRUNCATE\\b", "\\bEXEC\\b",
    "\\bATTACH\\b", "\\bDETACH\\b", "\\bPRAGMA\\b"
  )

  for (keyword in keywords_peligrosos) {
    keyword_found <- stringr::str_detect(
      query_norm, stringr::regex(keyword, ignore_case = TRUE)
    )
    if (keyword_found) {
      stop("Query contiene keyword no permitido (seguridad)")
    }
  }

  # Bloquear multiples statements (;)
  # Remover strings, comentarios de linea (--) y comentarios de bloque (/* */)
  query_sin_strings <- query_norm
  query_sin_strings <- stringr::str_remove_all(query_sin_strings, "'[^']*'")
  query_sin_strings <- stringr::str_remove_all(query_sin_strings, "--[^\n]*")
  query_sin_strings <- stringr::str_remove_all(
    query_sin_strings, "(?s)/\\*.*?\\*/"
  )
  if (stringr::str_detect(query_sin_strings, ";")) {
    stop("Multiples statements SQL no permitidos (seguridad)")
  }

  con <- get_cie10_db()

  resultado <- DBI::dbGetQuery(con, query) %>% tibble::as_tibble()

  return(resultado)
}

#' Limpiar cache SQLite (forzar rebuild)
#'
#' @return No return value, called for side effects (deletes SQLite cache).
#' @family sql
#' @seealso \code{\link{cie10_sql}}, \code{\link{cie10_disconnect}}
#' @export
#' @examples
#' # Ver ubicacion del cache
#' tools::R_user_dir("ciecl", "data")
#'
#' \donttest{
#' cie10_clear_cache()  # Elimina cie10.db local
#' }
cie10_clear_cache <- function() {
  # Cerrar conexion pooled antes de borrar
  if (!is.null(.ciecl_env$con)) {
    try(DBI::dbDisconnect(.ciecl_env$con), silent = TRUE)
    .ciecl_env$con <- NULL
    .ciecl_env$db_path <- NULL
  }

  cache_dir <- tools::R_user_dir("ciecl", "data")
  db_path <- file.path(cache_dir, "cie10.db")
  tmp_path <- paste0(db_path, ".tmp")

  eliminados <- FALSE

  if (file.exists(db_path)) {
    file.remove(db_path)
    eliminados <- TRUE
  }

  # Limpiar .tmp residuales
  if (file.exists(tmp_path)) {
    file.remove(tmp_path)
    eliminados <- TRUE
  }

  if (eliminados) {
    message("Cache SQLite eliminado: ", db_path)
  } else {
    message("i Cache no existe")
  }

  invisible(NULL)
}

#' Cerrar conexion pooled SQLite
#'
#' Cierra la conexion reutilizable al archivo SQLite.
#' Util para liberar el lock del archivo .db.
#'
#' @return No return value, called for side effects.
#' @family sql
#' @seealso \code{\link{cie10_sql}}, \code{\link{cie10_clear_cache}}
#' @export
#' @examples
#' # Verificar si hay conexion activa
#' is.null(ciecl:::.ciecl_env$con)
#'
#' \donttest{
#' cie10_disconnect()
#' }
cie10_disconnect <- function() {
  if (!is.null(.ciecl_env$con)) {
    try(DBI::dbDisconnect(.ciecl_env$con), silent = TRUE)
    .ciecl_env$con <- NULL
    .ciecl_env$db_path <- NULL
  }
  invisible(NULL)
}
