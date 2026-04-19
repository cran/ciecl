#' Generar tabla HTML interactiva GT de codigo CIE-10
#'
#' @param codigo String codigo (ej. "E11" muestra jerarquia)
#' @return Objeto de clase \code{gt_tbl} (tabla HTML interactiva)
#' @family visualizacion
#' @seealso \code{\link{cie_search}}, \code{\link{cie_lookup}}
#' @export
#' @importFrom dplyr select everything
#' @examples
#' # Requiere paquete gt
#' requireNamespace("gt", quietly = TRUE)
#'
#' \donttest{
#' cie_table("E11")  # Diabetes tipo 2 completo
#' }
cie_table <- function(codigo) {
  # Verificar que gt este instalado
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("El paquete 'gt' es necesario para esta funci\u00f3n.\n",
         "Inst\u00e1lalo con: install.packages('gt')")
  }
  
  datos <- cie_lookup(codigo, expandir = TRUE)
  
  if (nrow(datos) == 0) {
    stop("Codigo no encontrado: ", codigo)
  }
  
  tabla <- datos %>%
    dplyr::select(codigo, descripcion, inclusion, exclusion) %>%
    gt::gt() %>%
    gt::tab_header(
      title = sprintf("CIE-10 Chile: %s", codigo),
      subtitle = "Fuente: MINSAL/DEIS v2018"
    ) %>%
    gt::cols_label(
      codigo = "Codigo",
      descripcion = "Diagnostico",
      inclusion = "Incluye",
      exclusion = "Excluye"
    ) %>%
    gt::fmt_markdown(columns = dplyr::everything()) %>%
    gt::tab_options(
      table.font.size = 12,
      heading.background.color = "#1f77b4"
    )
  
  return(tabla)
}
