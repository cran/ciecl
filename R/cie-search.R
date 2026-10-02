#' @importFrom stringr str_trim str_split fixed
#' @importFrom tibble as_tibble
#' @importFrom stringdist stringsim
#' @importFrom DBI dbGetQuery
NULL

#' Normalizar texto removiendo tildes y caracteres especiales
#'
#' @param text Character vector a normalizar
#' @returns Character vector sin tildes ni caracteres especiales
#' @keywords internal
#' @noRd
normalizar_tildes <- function(text) {
  # Manejar vector vacio o NA
  if (length(text) == 0) {
    return(character(0))
  }

  # Usar chartr() que es mas rapido para sustituciones multiples
  # Caracteres con tilde -> sin tilde
  chartr(
    paste0(
      "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1",
      "\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1"
    ),
    "aeiouunAEIOUUN",
    text
  )
}

#' Búsqueda difusa (fuzzy) de términos médicos CIE-10
#'
#' Busca en descripciones CIE-10 usando múltiples estrategias:
#' 1. Expansión de siglas médicas (IAM, TBC, DM, etc.)
#' 2. Búsqueda exacta por subcadena (más rápida)
#' 3. Búsqueda fuzzy con Jaro-Winkler (tolera typos)
#'
#' La búsqueda es tolerante a tildes: "neumonia" (sin tilde) encuentra
#' "neumonía" (con tilde) en el catálogo.
#' Soporta siglas médicas comunes: "IAM" busca "infarto agudo miocardio".
#'
#' @param text String término médico en español o sigla
#'   (ej. "diabetes", "IAM", "TBC")
#' @param threshold Numeric entre 0 y 1, umbral similitud
#'   Jaro-Winkler (default 0.70)
#' @param max_results Integer, máximo resultados a retornar (default 50)
#' @param field Character, campo búsqueda ("descripcion" o "inclusion")
#' @param only_fuzzy Logical, usar solo búsqueda fuzzy
#'   sin búsqueda exacta (default FALSE)
#' @param verbose Logical, mostrar mensajes informativos
#'   (default TRUE). Usar FALSE en scripts.
#' @param include_uso_cl Logical, incluir columna `uso_cl` en el output
#'   (default FALSE). El default difiere de [cie_lookup()] (TRUE) para
#'   preservar el contrato histórico de cada función. Valores posibles:
#'   `"principal"`, `"legado"`, `"causa_externa"`, `"etiologico"`,
#'   `"causa_externa | principal"`.
#' @param only_uso_cl Logical, filtrar a códigos vigentes de uso clínico
#'   en Chile (default FALSE). Cuando es TRUE, excluye los códigos con
#'   `uso_cl == "legado"`.
#' @param texto `r lifecycle::badge("deprecated")` Use `text`.
#' @param campo `r lifecycle::badge("deprecated")` Use `field`.
#' @param solo_fuzzy `r lifecycle::badge("deprecated")` Use `only_fuzzy`.
#' @returns tibble ordenado por score descendente (1.0 = coincidencia exacta).
#'   Si el text corresponde a una sigla médica, se expande
#'   automáticamente antes de buscar.
#' @family search
#' @seealso [cie_lookup()], [cie_short()], [cie10_sql()]
#' @export
#' @examples
#' # Búsqueda básica
#' cie_search("diabetes")
#'
#' @examplesIf rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' cie_search("neumonia")
#'
#' # Búsqueda por siglas médicas
#' cie_search("IAM")
#' cie_search("DM2")
#'
#' # Tolerante a tildes y typos
#' cie_search("diabetis")
#'
#' # Buscar en inclusiones
#' cie_search("bacteriana", field = "inclusion")
#' # Filtrar a códigos vigentes Chile (excluye 'legado')
#' cie_search("diabetes", only_uso_cl = TRUE)
#' # Mostrar la columna uso_cl en el output
#' cie_search("diabetes", include_uso_cl = TRUE)
cie_search <- function(text, threshold = 0.70, max_results = 50,
                       field = c("descripcion", "inclusion"),
                       only_fuzzy = FALSE, verbose = TRUE,
                       include_uso_cl = FALSE, only_uso_cl = FALSE,
                       texto = lifecycle::deprecated(),
                       campo = lifecycle::deprecated(),
                       solo_fuzzy = lifecycle::deprecated()) {
  # Deprecation: argumentos en espanol -> ingles
  if (lifecycle::is_present(texto)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_search(texto = )",
      "cie_search(text = )"
    )
    text <- texto
  }
  if (lifecycle::is_present(campo)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_search(campo = )",
      "cie_search(field = )"
    )
    field <- campo
  }
  if (lifecycle::is_present(solo_fuzzy)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_search(solo_fuzzy = )",
      "cie_search(only_fuzzy = )"
    )
    only_fuzzy <- solo_fuzzy
  }

  check_required_es(missing(text), "text")
  field <- rlang::arg_match(field)

  # Validacion de parametros
  if (!rlang::is_string(text)) {
    cli::cli_abort(
      "{.arg text} debe ser un string character no-NA de longitud 1, no {.obj_type_friendly {text}}.",
      class = "ciecl_invalid_input"
    )
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || threshold < 0 || threshold > 1) {
    cli::cli_abort("{.arg threshold} debe estar entre 0 y 1.", class = "ciecl_invalid_input")
  }
  if (!is.numeric(max_results) || length(max_results) != 1L ||
      is.na(max_results) || max_results < 1) {
    cli::cli_abort("{.arg max_results} debe ser >= 1.", class = "ciecl_invalid_input")
  }

  texto_limpio <- stringr::str_trim(text)

  # Permitir siglas de 2 caracteres (DM, TB, FA, etc.)
  if (nchar(texto_limpio) < 2) {
    cli::cli_abort("Texto m\u00ednimo 2 caracteres.", class = "ciecl_invalid_input")
  }

  # Verificar si es una sigla medica y expandirla
  sigla_expandida <- expandir_sigla(texto_limpio)
  texto_busqueda <- if (!is.null(sigla_expandida)) {
    if (verbose) {
      cli::cli_inform(c(
        "i" = "Sigla detectada: {.val {toupper(texto_limpio)}} -> {.val {sigla_expandida$termino}}"
      ))
      if (isTRUE(sigla_expandida$ambiguo) && !is.null(sigla_expandida$aviso)) {
        cli::cli_warn(c("!" = sigla_expandida$aviso))
      }
    }
    sigla_expandida$termino
  } else {
    texto_limpio
  }

  # Conexion pooled (no cerrar, gestionada por .ciecl_env)
  con <- get_cie10_db()

  # Normalizar texto de busqueda (minusculas + sin tildes)
  texto_norm <- tolower(texto_busqueda)
  texto_sin_tildes <- normalizar_tildes(texto_norm)

  # Dividir en palabras para FTS5
  palabras <- unlist(stringr::str_split(texto_sin_tildes, "\\s+"))
  palabras <- palabras[nchar(palabras) >= 2]

  # Pre-filtrar usando FTS5 para velocidad
  query_params <- NULL
  if (length(palabras) > 0) {
    # Sanitizar palabras para FTS5 (prevenir SQL injection)
    # Solo permitir alfanumericos y acentos normalizados
    palabras_fts <- gsub("[^a-z0-9]", "", palabras)
    palabras_fts <- palabras_fts[nchar(palabras_fts) >= 2]

    if (length(palabras_fts) > 0) {
      # Construir query FTS5: palabra1* OR palabra2*
      texto_fts <- paste0(palabras_fts, "*", collapse = " OR ")
      query_params <- list(texto_fts)

      if (field == "descripcion") {
        query_sql <- "
          SELECT c.codigo, c.descripcion, c.categoria, c.uso_cl
          FROM cie10 c
          WHERE c.rowid IN (SELECT rowid FROM cie10_fts WHERE cie10_fts MATCH ?)
        "
      } else {
        field_quoted <- DBI::dbQuoteIdentifier(con, field)
        query_sql <- sprintf("
          SELECT c.codigo, c.descripcion, c.categoria, c.uso_cl, c.%s
          FROM cie10 c
          WHERE c.rowid IN (SELECT rowid FROM cie10_fts WHERE cie10_fts MATCH ?)
        ", field_quoted)
      }
    } else {
      # Sin palabras validas tras sanitizar, cargar todo
      if (field == "descripcion") {
        query_sql <- "SELECT codigo, descripcion, categoria, uso_cl FROM cie10"
      } else {
        field_quoted <- DBI::dbQuoteIdentifier(con, field)
        query_sql <- sprintf(
          "SELECT codigo, descripcion, categoria, uso_cl, %s FROM cie10",
          field_quoted
        )
      }
    }
  } else {
    # Sin palabras validas, cargar todo (fallback)
    # Se incluye uso_cl para un esquema de salida estable en todos los caminos
    if (field == "descripcion") {
      query_sql <- "SELECT codigo, descripcion, categoria, uso_cl FROM cie10"
    } else {
      field_quoted <- DBI::dbQuoteIdentifier(con, field)
      query_sql <- sprintf(
        "SELECT codigo, descripcion, categoria, uso_cl, %s FROM cie10",
        field_quoted
      )
    }
  }

  # Helper local: aplicar flags uso_cl al output final
  # (el filtro only_uso_cl ya se aplicó sobre `base`, antes del límite;
  # aquí solo queda omitir la columna si include_uso_cl = FALSE)
  apply_uso_cl_flags <- function(df) {
    if (!include_uso_cl) {
      df <- dplyr::select(df, -dplyr::any_of("uso_cl"))
    }
    df
  }

  base <- DBI::dbGetQuery(con, query_sql, params = query_params) |>
    tibble::as_tibble()

  # Si FTS5 no retorno resultados, intentar carga completa para fuzzy
  # Se incluye uso_cl para un esquema de salida estable en todos los caminos
  if (nrow(base) == 0 && length(palabras) > 0) {
    if (field == "descripcion") {
      query_sql <- "SELECT codigo, descripcion, categoria, uso_cl FROM cie10"
    } else {
      field_quoted <- DBI::dbQuoteIdentifier(con, field)
      query_sql <- sprintf(
        "SELECT codigo, descripcion, categoria, uso_cl, %s FROM cie10",
        field_quoted
      )
    }
    base <- DBI::dbGetQuery(con, query_sql) |>
      tibble::as_tibble()
  }

  # Filtrar códigos legado ANTES de truncar con slice_head(max_results):
  # el límite debe aplicarse sobre el conjunto solicitado por el usuario
  if (only_uso_cl) {
    base <- dplyr::filter(base, .data$uso_cl != "legado")
  }

  # Normalizar texto de la base (minusculas + sin tildes)
  base_texto <- tolower(stringr::str_trim(base[[field]]))
  base_texto[is.na(base_texto)] <- ""
  base_texto_sin_tildes <- normalizar_tildes(base_texto)

  # ESTRATEGIA 1: Busqueda exacta por subcadena (mas rapida y precisa)
  if (!only_fuzzy) {
    # Buscar coincidencias exactas (subcadena)
    matches_exactos <- stringr::str_detect(
      base_texto_sin_tildes,
      stringr::fixed(texto_sin_tildes)
    )

    if (any(matches_exactos)) {
      resultado_exacto <- base[matches_exactos, ] |>
        dplyr::mutate(score = 1.0) |>
        dplyr::slice_head(n = max_results) |>
        dplyr::select(codigo, descripcion, score, dplyr::everything())

      return(apply_uso_cl_flags(resultado_exacto))
    }
  }

  # ESTRATEGIA 2: Busqueda fuzzy palabra por palabra
  # palabras ya fue definido arriba para FTS5
  palabras_fuzzy <- palabras[nchar(palabras) >= 3]

  if (length(palabras_fuzzy) > 0) {
    # Calcular score basado en cuantas palabras coinciden
    scores_palabras <- vapply(seq_along(base_texto_sin_tildes), function(i) {
      texto_base <- base_texto_sin_tildes[i]
      # Contar palabras que aparecen en la descripcion
      matches <- vapply(palabras_fuzzy, function(p) {
        stringr::str_detect(texto_base, stringr::fixed(p))
      }, logical(1))
      sum(matches) / length(palabras_fuzzy)
    }, numeric(1))

    # Si hay coincidencias parciales de palabras
    if (any(scores_palabras > 0)) {
      resultado <- base |>
        dplyr::mutate(score = scores_palabras) |>
        dplyr::filter(score > 0) |>
        dplyr::arrange(dplyr::desc(score)) |>
        dplyr::slice_head(n = max_results) |>
        dplyr::select(codigo, descripcion, score, dplyr::everything())

      if (nrow(resultado) > 0) {
        return(apply_uso_cl_flags(resultado))
      }
    }
  }

  # ESTRATEGIA 3: Fuzzy matching con Jaro-Winkler (para typos)
  # Calcular similitud de cada palabra del texto con palabras de la descripcion

  # Sin candidatos para fuzzy (todas las palabras tienen < 3 chars, p. ej.
  # un texto de solo simbolos como "!!"): iterar daria mean(numeric(0)) ->
  # NaN en los scores. Early return con tibble vacio y el esquema estable.
  if (length(palabras_fuzzy) == 0) {
    resultado <- base[0, ] |>
      dplyr::mutate(score = numeric(0)) |>
      dplyr::select(codigo, descripcion, score, dplyr::everything())

    if (verbose) {
      cli::cli_inform(c("x" = "Sin coincidencias >= threshold {.val {threshold}}"))
    }

    return(apply_uso_cl_flags(resultado))
  }

  scores_fuzzy <- vapply(seq_along(base_texto_sin_tildes), function(i) {
    texto_base <- base_texto_sin_tildes[i]
    palabras_base <- unlist(stringr::str_split(texto_base, "\\s+"))
    palabras_base <- palabras_base[nchar(palabras_base) >= 3]

    if (length(palabras_base) == 0) {
      return(0)
    }

    # Para cada palabra del texto buscar la mejor coincidencia en la descripcion
    best_scores <- vapply(palabras_fuzzy, function(p) {
      if (length(palabras_base) == 0) {
        return(0)
      }
      max(stringdist::stringsim(p, palabras_base, method = "jw"))
    }, numeric(1))

    mean(best_scores)
  }, numeric(1))

  # Filtrar + ordenar resultados fuzzy
  resultado <- base |>
    dplyr::mutate(score = scores_fuzzy) |>
    dplyr::filter(score >= threshold) |>
    dplyr::arrange(dplyr::desc(score)) |>
    dplyr::slice_head(n = max_results) |>
    dplyr::select(codigo, descripcion, score, dplyr::everything())

  if (nrow(resultado) == 0 && verbose) {
    cli::cli_inform(c("x" = "Sin coincidencias >= threshold {.val {threshold}}"))
  }

  return(apply_uso_cl_flags(resultado))
}
