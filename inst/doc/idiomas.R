## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(ciecl)
# 
# # Ver estructura del dataset
# head(cie10_cl)
# #> # A tibble: 6 x 8
# #>   codigo descripcion                    categoria inclusion exclusion capitulo
# #>   <chr>  <chr>                          <chr>     <chr>     <chr>     <chr>
# #> 1 A00    Colera                         A00       ...       ...       01
# #> 2 A00.0  Colera debido a Vibrio...      A00       ...       ...       01

## -----------------------------------------------------------------------------
# # Todas estas busquedas encuentran "neumonia"
# cie_search("neumonia")
# cie_search("neumonía")
# cie_search("NEUMONIA")
# 
# # Funciona con n
# cie_search("rinon")     # Encuentra "rinon"
# cie_search("espanol")   # Encuentra terminos con "n"

## -----------------------------------------------------------------------------
# # Ver todas las siglas disponibles
# cie_siglas()
# 
# # Filtrar por categoria
# cie_siglas("cardiovascular")
# cie_siglas("respiratoria")
# cie_siglas("oncologica")
# 
# # Buscar usando siglas
# cie_search("IAM")   # Infarto Agudo del Miocardio
# cie_search("EPOC")  # Enfermedad Pulmonar Obstructiva Cronica
# cie_search("DM2")   # Diabetes Mellitus tipo 2
# cie_search("HTA")   # Hipertension Arterial
# cie_search("TBC")   # Tuberculosis

## -----------------------------------------------------------------------------
# # Busqueda en espanol (default)
# cie11_search("diabetes mellitus", lang = "es")
# 
# # Busqueda en ingles
# cie11_search("diabetes mellitus", lang = "en")
# 
# # Comparar resultados
# es <- cie11_search("infarto", lang = "es")
# en <- cie11_search("infarction", lang = "en")

## -----------------------------------------------------------------------------
# # Configurar idioma por defecto en sesion
# options(ciecl.lang = "es")
# 
# # Ahora todas las busquedas CIE-11 usaran espanol
# cie11_search("neumonia")

## -----------------------------------------------------------------------------
# # Verificar encoding del dataset
# Encoding(cie10_cl$descripcion[1])
# #> [1] "UTF-8"

