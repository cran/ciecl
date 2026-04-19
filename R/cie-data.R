#' @importFrom stringr str_trim str_replace_all str_detect str_extract
#' @importFrom dplyr select mutate filter all_of any_of %>%
#' @importFrom tibble as_tibble
NULL

#' Parsear datos CIE-10 MINSAL/DEIS desde XLS
#' 
#' @description
#' Funcion interna para procesar Lista-Tabular-CIE-10-1-1.xls oficial DEIS.
#' Estructura columnas: codigo, descripcion, categoria, inclusion, exclusion.
#' Detecta columnas automaticamente para mayor robustez.
#' 
#' @param xls_path Ruta al archivo XLS descargado DEIS
#' @return tibble con 39,877 codigos CIE-10 Chile limpios
#' @keywords internal
#' @noRd
parsear_cie10_minsal <- function(xls_path) {
  # Verificar que readxl este instalado
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("El paquete 'readxl' es necesario para generar el dataset.\n",
         "Inst\u00e1lalo con: install.packages('readxl')")
  }
  
  if (!file.exists(xls_path)) {
    stop("Archivo XLS no encontrado: ", xls_path)
  }
  
  # Leer XLS con deteccion automatica columnas
  raw <- readxl::read_excel(
    xls_path,
    sheet = 1,
    col_names = TRUE,
    skip = 0,
    .name_repair = "minimal"
  )
  
  # Normalizar nombres
  names(raw) <- tolower(stringr::str_trim(names(raw)))
  names(raw) <- stringr::str_replace_all(names(raw), "\\s+", "_")
  
  # Detectar columnas dinamicamente (soporta tildes: codigo/descripcion)
  col_codigo <- names(raw)[stringr::str_detect(names(raw), "c[o\u00f3]d|clave")]
  col_desc <- names(raw)[stringr::str_detect(names(raw), "desc|t[i\u00ed]tulo")]
  
  if (length(col_codigo) == 0 || length(col_desc) == 0) {
    stop("No se detectaron columnas codigo/descripcion. Columnas: ", 
         paste(names(raw), collapse = ", "))
  }
  
  # Construir dataset
  cie10_limpio <- raw %>%
    dplyr::select(
      codigo = dplyr::all_of(col_codigo[1]),
      descripcion = dplyr::all_of(col_desc[1]),
      categoria = dplyr::any_of(
        names(raw)[stringr::str_detect(
          names(raw), "cat|tipo|categor[i\u00ed]a"
        )]
      ),
      inclusion = dplyr::any_of(
        names(raw)[stringr::str_detect(names(raw), "incl")]
      ),
      exclusion = dplyr::any_of(
        names(raw)[stringr::str_detect(names(raw), "excl")]
      )
    ) %>%
    dplyr::mutate(
      codigo = stringr::str_trim(as.character(codigo)),
      descripcion = stringr::str_trim(as.character(descripcion)),
      capitulo = stringr::str_extract(codigo, "^[A-Z]\\d{1,2}"),
      es_daga = stringr::str_detect(codigo, "\u2020"),
      es_cruz = stringr::str_detect(codigo, "\\*")
    ) %>%
    dplyr::filter(!is.na(codigo), !is.na(descripcion), nchar(codigo) >= 3) %>%
    tibble::as_tibble()
  
  return(cie10_limpio)
}

#' Generar dataset cie10_cl.rda
#'
#' @description
#' EJECUTAR UNA VEZ para crear data/cie10_cl.rda desde archivo DEIS.
#' Deteccion automatica por prioridad: XLSX completo (39K+ codigos)
#' vs XLS legado (~8K).
#'
#' @param archivo_path Ruta al archivo XLSX/XLS DEIS
#'   (opcional, deteccion automatica).
#'   Debe ser una ruta de confianza; no usar con input de usuarios finales.
#' @return Invisible tibble with generated 'ICD-10' data.
#' @examples
#' \dontrun{
#' # Desde ciecl/ ejecutar:
#' generar_cie10_cl()
#' }
#' @keywords internal
#' @noRd
generar_cie10_cl <- function(archivo_path = NULL) {
  # Verificar que usethis este instalado
  if (!requireNamespace("usethis", quietly = TRUE)) {
    stop("El paquete 'usethis' es necesario para generar el dataset.\n",
         "Inst\u00e1lalo con: install.packages('usethis')")
  }

  # Deteccion automatica si no se proporciona ruta (prioridad: XLSX > XLS)
  if (is.null(archivo_path)) {
    candidatos <- c(
      normalizePath("../CIE-10 (1).xlsx", mustWork = FALSE),
      normalizePath("CIE-10 (1).xlsx", mustWork = FALSE),
      normalizePath("../Lista-Tabular-CIE-10-1-1.xls", mustWork = FALSE),
      normalizePath("Lista-Tabular-CIE-10-1-1.xls", mustWork = FALSE)
    )
    existentes <- candidatos[file.exists(candidatos)]
    if (length(existentes) == 0) {
      stop("Archivo no encontrado. Proporcionar ruta con archivo_path = '...'")
    }
    archivo_path <- existentes[1]
  }
  xls_path <- archivo_path
  
  message("Parseando: ", xls_path)
  cie10_cl <- parsear_cie10_minsal(xls_path)
  
  # Guardar en data/
  usethis::use_data(cie10_cl, overwrite = TRUE, compress = "xz")
  
  message("Generado data/cie10_cl.rda con ", nrow(cie10_cl), " codigos")
  invisible(cie10_cl)
}

#' Dataset CIE-10 Chile oficial MINSAL/DEIS v2018
#'
#' @format tibble con 39,877 filas (categorias y subcategorias):
#' \describe{
#'   \item{codigo}{Codigo CIE-10 (ej. "E11.0")}
#'   \item{descripcion}{Diagnostico en espanol chileno}
#'   \item{categoria}{Categoria jerarquica}
#'   \item{seccion}{Seccion dentro del capitulo}
#'   \item{capitulo_nombre}{Nombre descriptivo del capitulo}
#'   \item{inclusion}{Terminos incluidos}
#'   \item{exclusion}{Terminos excluidos}
#'   \item{capitulo}{Capitulo CIE-10 (A-Z)}
#'   \item{es_daga}{Logical, codigo daga (+)}
#'   \item{es_cruz}{Logical, codigo asterisco (*)}
#' }
#' @source \url{https://deis.minsal.cl/centrofic/}
#' @examples
#' data(cie10_cl)
#' head(cie10_cl)
"cie10_cl"
