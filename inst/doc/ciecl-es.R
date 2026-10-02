## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(ciecl)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("ciecl")

## ----eval=FALSE---------------------------------------------------------------
# # Requiere el paquete pak para una gestión eficiente de dependencias
# pak::pak("ropensci/ciecl")

## -----------------------------------------------------------------------------
cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 5")

## -----------------------------------------------------------------------------
# Recuperar un código único
cie_lookup("E11.0")

## -----------------------------------------------------------------------------
# Múltiples códigos de distintos capítulos en una sola llamada
cie_lookup(c("E11.0", "I10", "Z00", "J44.0"))

## -----------------------------------------------------------------------------
cie_lookup("E11", expand = TRUE)

## -----------------------------------------------------------------------------
cie_describe(c("E11.0", "I10"))

## -----------------------------------------------------------------------------
library(dplyr)

egresos <- data.frame(
  id = 1:4,
  codigo_diag = c("E11.0", "I10", "J44.0", "E11.0")
)

egresos |>
  mutate(descripcion = cie_describe(codigo_diag))

## -----------------------------------------------------------------------------
# Búsqueda tolerante: "diabetis" en lugar de "diabetes"
cie_search("diabetis con coma", threshold = 0.75)

## ----eval=rlang::is_installed("comorbidity")----------------------------------
# Requiere el paquete externo 'comorbidity'
df_pacientes <- data.frame(
  id_pac     = c(1, 1, 2, 2, 3),
  diagnostico = c("E11.0", "I50.9", "C50.9", "N18.5", "J44.0")
)

cie_comorbid(df_pacientes, id = "id_pac", code = "diagnostico", map = "charlson")

## ----eval=rlang::is_installed("gt")-------------------------------------------
# Requiere el paquete 'gt' instalado
cie_table("E11")

