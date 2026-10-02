## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ciecl)

head(cie10_cl[, c("codigo", "descripcion", "capitulo")])

## -----------------------------------------------------------------------------
# Con o sin tilde: mismo resultado
cie_search("neumonia")
cie_search("neumonía")
cie_search("NEUMONIA")

## -----------------------------------------------------------------------------
cie_search("rinon")

## -----------------------------------------------------------------------------
# Listar todas las siglas disponibles
head(cie_short())

# Filtrar por categoría
cie_short(category = "cardiovascular")

# Usar la sigla directamente en búsqueda
cie_search("IAM")   # Infarto Agudo del Miocardio
cie_search("EPOC")  # Enfermedad Pulmonar Obstructiva Crónica
cie_search("DM2")   # Diabetes Mellitus tipo 2

## ----eval=FALSE---------------------------------------------------------------
# # Búsqueda en español (por defecto)
# # Requiere API Key OMS
# cie11_search("diabetes mellitus", lang = "es")

## -----------------------------------------------------------------------------
Encoding(cie10_cl$descripcion[1])

