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
# # Requires the pak package
# pak::pak("ropensci/ciecl")

## -----------------------------------------------------------------------------
cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 5")

## -----------------------------------------------------------------------------
# Single code
cie_lookup("E11.0")

## -----------------------------------------------------------------------------
# Multiple codes from different chapters
cie_lookup(c("E11.0", "I10", "Z00", "J44.0"))

## -----------------------------------------------------------------------------
cie_lookup("E11", expand = TRUE)

## -----------------------------------------------------------------------------
cie_describe(c("E11.0", "I10"))

## -----------------------------------------------------------------------------
library(dplyr)

discharges <- data.frame(
  id          = 1:4,
  diag_code   = c("E11.0", "I10", "J44.0", "E11.0")
)

discharges |>
  mutate(description = cie_describe(diag_code))

## -----------------------------------------------------------------------------
# "diabetis" instead of "diabetes" — the typo does not prevent finding the code
cie_search("diabetis with coma", threshold = 0.75)

## ----eval=rlang::is_installed("comorbidity")----------------------------------
# Requires: install.packages("comorbidity")
patient_df <- data.frame(
  patient_id  = c(1, 1, 2, 2, 3),
  diagnosis   = c("E11.0", "I50.9", "C50.9", "N18.5", "J44.0")
)

cie_comorbid(patient_df, id = "patient_id", code = "diagnosis", map = "charlson")

## ----eval=rlang::is_installed("gt")-------------------------------------------
# Requires: install.packages("gt")
cie_table("E11")

