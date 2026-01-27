## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# install.packages("ciecl")

## -----------------------------------------------------------------------------
# # Opcion 1: pak (recomendado)
# install.packages("pak")
# pak::pak("RodoTasso/ciecl")
# 
# # Opcion 2: devtools
# install.packages("devtools")
# devtools::install_github("RodoTasso/ciecl")
# 
# # Opcion 3: remotes
# install.packages("remotes")
# remotes::install_github("RodoTasso/ciecl")

## -----------------------------------------------------------------------------
# # Instalacion completa con todos los paquetes opcionales
# pak::pak("RodoTasso/ciecl", dependencies = TRUE)

## -----------------------------------------------------------------------------
# # Abrir archivo .Renviron para editar
# usethis::edit_r_environ()

## -----------------------------------------------------------------------------
# Sys.setenv(ICD_API_KEY = "tu_client_id:tu_client_secret")

## -----------------------------------------------------------------------------
# # Verificar que la API key esta configurada
# Sys.getenv("ICD_API_KEY")
# 
# # Probar busqueda CIE-11
# library(ciecl)
# cie11_search("diabetes")

## -----------------------------------------------------------------------------
# # Ver ubicacion del cache
# tools::R_user_dir("ciecl", "data")

## -----------------------------------------------------------------------------
# library(ciecl)
# cie10_clear_cache()

## -----------------------------------------------------------------------------
# library(ciecl)
# 
# # Verificar que el paquete carga correctamente
# packageVersion("ciecl")
# 
# # Verificar acceso al dataset
# nrow(cie10_cl)  # Debe retornar 39873
# 
# # Probar busqueda basica
# cie_lookup("E11.0")
# 
# # Probar busqueda fuzzy
# cie_search("diabetes")

## -----------------------------------------------------------------------------
# pak::pak("RodoTasso/ciecl")

## -----------------------------------------------------------------------------
# ciecl::cie10_clear_cache()

