## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("ciecl")

## ----eval=FALSE---------------------------------------------------------------
# install.packages("pak")
# pak::pak("ropensci/ciecl")

## ----eval=FALSE---------------------------------------------------------------
# pak::pak("ropensci/ciecl", dependencies = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # Una sola vez: guarda "client_id:client_secret" en el keychain
# keyring::key_set("ciecl_icd11")
# 
# # En cada sesión en la que uses la API
# Sys.setenv(ICD_API_KEY = keyring::key_get("ciecl_icd11"))

## ----eval=FALSE---------------------------------------------------------------
# Sys.setenv(ICD_API_KEY = "tu_client_id:tu_client_secret")

## ----eval=FALSE---------------------------------------------------------------
# # Verificar que la variable de entorno esta definida
# Sys.getenv("ICD_API_KEY")
# 
# # Probar una busqueda CIE-11
# library(ciecl)
# cie11_search("diabetes")

## ----eval=FALSE---------------------------------------------------------------
# # Ver la ubicacion del cache
# tools::R_user_dir("ciecl", "data")

## ----eval=FALSE---------------------------------------------------------------
# library(ciecl)
# cie10_clear_cache()

## ----eval=FALSE---------------------------------------------------------------
# library(ciecl)
# 
# # Verificar que el paquete carga correctamente
# packageVersion("ciecl")
# 
# # Verificar acceso al catálogo
# nrow(cie10_cl)
# 
# # Probar búsqueda básica
# cie_lookup("E11.0")
# 
# # Probar búsqueda fuzzy
# cie_search("diabetes")

## ----eval=FALSE---------------------------------------------------------------
# install.packages("ciecl")

## ----eval=FALSE---------------------------------------------------------------
# ciecl::cie10_clear_cache()

