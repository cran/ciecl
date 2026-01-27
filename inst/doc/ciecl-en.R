## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(ciecl)

## ----eval=FALSE---------------------------------------------------------------
# # From GitHub (beta version)
# pak::pak("RodoTasso/ciecl")
# 
# # Alternative with devtools
# devtools::install_github("RodoTasso/ciecl")

## ----eval=FALSE---------------------------------------------------------------
# # All type 2 diabetes codes
# cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 5")

## ----eval=FALSE---------------------------------------------------------------
# # Single code search
# cie_lookup("E11.0")
# 
# # Vectorized search - multiple codes at once
# codes <- c("E11.0", "I10", "Z00", "J44.0")
# results <- cie_lookup(codes)
# 
# # Hierarchical expansion
# cie_lookup("E11", expandir = TRUE)  # Returns all E11.x

## ----eval=FALSE---------------------------------------------------------------
# # Finds even if misspelled
# cie_search("diabetis with coma", threshold = 0.75)

## ----eval=FALSE---------------------------------------------------------------
# # Requires: install.packages("comorbidity")
# patient_df <- data.frame(
#   patient_id = c(1, 1, 2, 2, 3),
#   diagnosis = c("E11.0", "I50.9", "C50.9", "N18.5", "J44.0")
# )
# 
# cie_comorbid(patient_df, id = "patient_id", code = "diagnosis", map = "charlson")

## ----eval=FALSE---------------------------------------------------------------
# # Requires: install.packages("gt")
# cie_table("E11")  # Full GT visualization

