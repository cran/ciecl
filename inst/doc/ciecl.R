## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(ciecl)

## ----eval=FALSE---------------------------------------------------------------
# # Desde GitHub (version beta)
# pak::pak("RodoTasso/ciecl")
# 
# # Alternativa con devtools
# devtools::install_github("RodoTasso/ciecl")

## ----eval=FALSE---------------------------------------------------------------
# # Todos los codigos diabetes tipo 2
# cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 5")

## ----eval=FALSE---------------------------------------------------------------
# # Busqueda de un solo codigo
# cie_lookup("E11.0")
# 
# # Busqueda vectorizada - multiples codigos a la vez
# codigos <- c("E11.0", "I10", "Z00", "J44.0")
# resultados <- cie_lookup(codigos)
# 
# # Expansion jerarquica
# cie_lookup("E11", expandir = TRUE)  # Retorna todos los E11.x

## ----eval=FALSE---------------------------------------------------------------
# # Encuentra aunque este mal escrito
# cie_search("diabetis con coma", threshold = 0.75)

## ----eval=FALSE---------------------------------------------------------------
# # Requiere: install.packages("comorbidity")
# df_pacientes <- data.frame(
#   id_pac = c(1, 1, 2, 2, 3),
#   diagnostico = c("E11.0", "I50.9", "C50.9", "N18.5", "J44.0")
# )
# 
# cie_comorbid(df_pacientes, id = "id_pac", code = "diagnostico", map = "charlson")

## ----eval=FALSE---------------------------------------------------------------
# # Requiere: install.packages("gt")
# cie_table("E11")  # Visualizacion GT completa

