# Declarar variables NSE para evitar NOTEs en R CMD check
#' @importFrom rlang is_string arg_match check_installed is_installed .data
#' @importFrom lifecycle is_present deprecated
#' @importFrom cli cli_abort cli_warn cli_inform
utils::globalVariables(c(
  # Variables dplyr NSE
  "codigo", "descripcion", "score", "categoria", "inclusion", "exclusion",
  "patron", "title", "capitulo", "sigla", "termino_busqueda",
  # Dataset
  "cie10_cl",
  # Dot placeholder
  "."
))

#' Validar que un parametro obligatorio fue suministrado
#'
#' Reemplaza a `rlang::check_required()` para mantener el mensaje de
#' error en espanol, consistente con el resto del paquete (rlang/R base
#' generan el mensaje en ingles "argument is missing, with no default" /
#' "is absent but must be supplied").
#'
#' @param falta Logical, resultado de `missing(x)` evaluado en la
#'   funcion llamante.
#' @param arg Character, nombre del parametro para el mensaje de error.
#' @keywords internal
#' @noRd
check_required_es <- function(falta, arg) {
  if (falta) {
    cli::cli_abort(
      "{.arg {arg}} es obligatorio.",
      class = "ciecl_invalid_input"
    )
  }
}

#' Tibble vacio con estructura CIE-10
#' @keywords internal
#' @noRd
cie10_empty_tibble <- function(add_descripcion_completa = FALSE) {
  resultado <- tibble::tibble(
    codigo = character(0),
    descripcion = character(0),
    categoria = character(0),
    seccion = character(0),
    capitulo_nombre = character(0),
    inclusion = character(0),
    exclusion = character(0),
    capitulo = character(0),
    es_daga = logical(0),
    es_cruz = logical(0),
    uso_cl = character(0)
  )

  if (add_descripcion_completa) {
    resultado$descripcion_completa <- character(0)
  }

  return(resultado)
}
