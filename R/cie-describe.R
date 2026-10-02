#' Obtener descripción de códigos CIE-10 (vector)
#'
#' @description
#' Devuelve un vector character con la descripción de cada código,
#' pensado para usar dentro de `dplyr::mutate()` sin necesidad de
#' un `left_join` contra `cie10_cl`.
#'
#' @param codes Character vector de códigos CIE-10 (ej. "E11.0",
#'   c("E11.0", "I10")).
#' @param normalize Logical, intentar normalizar los códigos antes
#'   de buscar la descripción? (default FALSE). Usar TRUE para
#'   limpiar formatos (ej. "E110" -> "E11.0"); usar FALSE para
#'   auditar la calidad original del registro.
#' @param default Valor devuelto cuando un código no se encuentra
#'   en el catálogo. Default `NA_character_`.
#' @param codigos `r lifecycle::badge("deprecated")` Use `codes`.
#' @returns Character vector del mismo largo que `codes` con la
#'   descripción oficial MINSAL/DEIS. `NA_character_` (o `default`)
#'   para códigos sin match.
#' @family search
#' @seealso [cie_lookup()] para resultado como tibble con todas
#'   las columnas; [cie_norm()] para normalización.
#' @importFrom stats setNames
#' @export
#' @examples
#' # Auditoría: buscar tal cual (E110 no existe sin punto)
#' cie_describe("E110", normalize = FALSE)
#'
#' # Rescate: normalizar antes de buscar
#' cie_describe("E110", normalize = TRUE)
#'
#' @examplesIf rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Uso típico en auditoría VIU (contar fallos de origen)
#' diags <- c("E11.0", "E110", "I10X", "INVALIDO")
#' descripciones <- cie_describe(diags, normalize = FALSE)
#' sum(is.na(descripciones)) # Detecta 3 errores de registro
cie_describe <- function(codes, normalize = FALSE, default = NA_character_,
                        codigos = lifecycle::deprecated()) {
  if (lifecycle::is_present(codigos)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_describe(codigos = )",
      "cie_describe(codes = )"
    )
    codes <- codigos
  }

  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    cli::cli_abort("{.arg normalize} debe ser {.code TRUE} o {.code FALSE}.", class = "ciecl_invalid_input")
  }

  default_ok <- (is.character(default) && length(default) == 1L) ||
    (length(default) == 1L && is.na(default))
  if (!default_ok) {
    cli::cli_abort(
      "{.arg default} debe ser un string character de longitud 1 o {.code NA}, no {.obj_type_friendly {default}}.",
      class = "ciecl_invalid_input"
    )
  }

  # Coerción a character: absorbe NA_real_/NaN/list(NA) para que los
  # retornos tempranos sean siempre character (contrato @returns).
  default <- as.character(default)

  if (length(codes) == 0) {
    return(character(0))
  }

  # Flujo separado: normalizar solo si se pide (Rescate vs Auditoria)
  codes <- if (normalize) {
    cie_norm(codes, search_db = FALSE)
  } else {
    as.character(codes)
  }

  resultado <- rep(default, length(codes))

  no_na <- !is.na(codes)
  if (!any(no_na)) {
    return(resultado)
  }

  con <- get_cie10_db()
  codes_unicos <- unique(codes[no_na])
  placeholders <- paste(rep("?", length(codes_unicos)), collapse = ",")
  query <- sprintf(
    "SELECT codigo, descripcion FROM cie10 WHERE codigo IN (%s)",
    placeholders
  )
  hits <- DBI::dbGetQuery(con, query, params = as.list(codes_unicos))

  if (nrow(hits) == 0) {
    return(resultado)
  }

  lookup <- setNames(hits$descripcion, hits$codigo)
  idx <- codes %in% names(lookup)
  resultado[idx] <- unname(lookup[codes[idx]])
  resultado
}
