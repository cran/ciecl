#' @importFrom tibble as_tibble
NULL

#' Guía de funciones de búsqueda CIE-10
#'
#' Muestra tabla comparativa de cuándo usar cada función de búsqueda.
#'
#' @returns tibble con guia comparativa de funciones de busqueda
#' @family search
#' @seealso [cie_search()], [cie_lookup()], [cie_short()]
#' @export
#' @examples
#' cie_guide()
cie_guide <- function() {
  guia <- data.frame(
    `Tengo...` = c(
      "Codigo exacto (E11.0)",
      "Codigo sin punto (e110)",
      "Codigo con espacios (E 11.0)",
      "Codigo con prefijos/sufijos",
      "Codigo categoria (E11)",
      "Descripcion (diabetes)",
      "Sigla medica (IAM, DM2)",
      "No se que tengo"
    ),
    `Usar funcion` = c(
      "cie_lookup()",
      "cie_lookup()",
      "cie_lookup()",
      "cie_lookup(extract = TRUE)",
      "cie_lookup() o cie_lookup(expand = TRUE)",
      "cie_search()",
      "cie_lookup(check_siglas = TRUE)",
      "cie_search() con threshold bajo"
    ),
    `Ejemplo` = c(
      'cie_lookup("E11.0")',
      'cie_lookup("e110")',
      'cie_lookup("E 11.0")',
      'cie_lookup("CIE:E11.0", extract = TRUE)',
      'cie_lookup("E11")',
      'cie_search("diabetes")',
      'cie_lookup("IAM", check_siglas = TRUE)',
      'cie_search("dolor", threshold = 0.5)'
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  return(tibble::as_tibble(guia))
}

#' Guía de funciones de búsqueda (deprecated)
#'
#' `r lifecycle::badge("deprecated")` Use [cie_guide()].
#'
#' @returns tibble con guía comparativa
#' @family search
#' @keywords internal
#' @export
#' @examples
#' # Deprecated: usar cie_guide()
#' cie_guia_busqueda()
cie_guia_busqueda <- function() {
  lifecycle::deprecate_warn(
    "0.9.8",
    "cie_guia_busqueda()",
    "cie_guide()"
  )
  cie_guide()
}
