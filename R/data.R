#' Dataset CIE-10 Chile oficial MINSAL/DEIS v2018
#'
#' Catálogo completo de diagnósticos CIE-10 en versión chilena (MINSAL/DEIS 2018),
#' incluyendo categorías, subcategorías y metadatos jerárquicos. Contiene 39,877
#' filas que abarcan la totalidad de códigos del sistema de clasificación.
#'
#' @format Un tibble con 39,877 filas y 11 columnas:
#' \describe{
#'   \item{codigo}{character. Código CIE-10 alfanumérico (p. ej. \code{"E11.0"},
#'     \code{"A00"}, \code{"Z99.9"}). Incluye tanto categorías de 3 caracteres
#'     como subcategorías con decimal.}
#'   \item{descripcion}{character. Denominación del diagnóstico en español
#'     según terminología oficial MINSAL/DEIS.}
#'   \item{categoria}{character. Código de categoría de 3 caracteres al que
#'     pertenece la fila (p. ej. \code{"E11"} para el código \code{"E11.0"}).
#'     Igual a \code{codigo} cuando la propia fila es una categoría.}
#'   \item{seccion}{character. Rango de categorías que define la sección dentro
#'     del capítulo (p. ej. \code{"E10-E14"}).}
#'   \item{capitulo_nombre}{character. Nombre descriptivo del capítulo CIE-10
#'     (p. ej. \code{"Enfermedades endocrinas, nutricionales y metabólicas"}).}
#'   \item{inclusion}{character. Términos de inclusión asociados al código,
#'     tal como figuran en el catálogo oficial. \code{NA} cuando no aplica.}
#'   \item{exclusion}{character. Términos de exclusión (notas "Excl.") asociados
#'     al código. \code{NA} cuando no aplica.}
#'   \item{capitulo}{character. Letra o rango que identifica el capítulo
#'     CIE-10 (valores \code{"A"} a \code{"Z"}).}
#'   \item{es_daga}{logical. \code{TRUE} si el código es daga (\code{†}), es
#'     decir, etiología en un par daga/asterisco. \code{FALSE} en caso contrario.}
#'   \item{es_cruz}{logical. \code{TRUE} si el código es asterisco (\code{*}),
#'     es decir, manifestación en un par daga/asterisco. \code{FALSE} en caso contrario.}
#'   \item{uso_cl}{character. Categoría de uso clínico del código según el
#'     catálogo MINSAL/DEIS: \code{"principal"}, \code{"legado"},
#'     \code{"causa_externa"}, \code{"etiologico"}, o la combinación
#'     \code{"causa_externa | principal"}. El valor \code{"legado"} identifica
#'     códigos sin uso clínico vigente en Chile.}
#' }
#' @family datasets
#' @seealso [cie_lookup()], [cie_search()], [cie10_sql()]
#' @source \url{https://deis.minsal.cl/centrofic/}
#' @examples
#' data(cie10_cl)
#' head(cie10_cl)
#'
#' # Códigos vigentes de uso clínico en Chile (excluye categoría "legado")
#' subset(cie10_cl, uso_cl != "legado") |> nrow()
#'
#' # Capítulos disponibles
#' unique(cie10_cl$capitulo)
"cie10_cl"
