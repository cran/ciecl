## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ciecl)
library(dplyr)

## ----datos--------------------------------------------------------------------
set.seed(42)

# Simulation of 200 records with typical DEIS Chile formats
discharges <- data.frame(
  DISCHARGE_ID = 1:200,
  PATIENT_ID   = sample(1:50, 200, replace = TRUE),
  YEAR         = sample(2018:2022, 200, replace = TRUE),
  DIAG1        = sample(
    c(
      "J189", "O800", "Z380", "K359", "N390",
      "I10X", "J449", "E119", "O829", "J069",
      "K922", "N185", "I509", "C509", "A099",
      "N40X", "K800", "I259", "J180", "E149"
    ),
    size    = 200,
    replace = TRUE
  ),
  stringsAsFactors = FALSE
)

head(discharges)

## ----normalizacion------------------------------------------------------------
# Cleaning and standardization of diagnoses in the workflow
discharges <- discharges |>
  mutate(
    DIAG1_NORM = cie_norm(codes = DIAG1)
  )

# Comparison between original and normalized formats
discharges |>
  select(DIAG1, DIAG1_NORM) |>
  distinct() |>
  head(5)

## ----describe-----------------------------------------------------------------
# Direct integration of descriptions into the main dataframe
discharges_full <- discharges |>
  mutate(
    description = cie_describe(DIAG1_NORM)
  )

head(discharges_full |> select(DISCHARGE_ID, DIAG1, description))

## ----lookup-------------------------------------------------------------------
# Extracting full metadata via lookup + join
metadata <- cie_lookup(
  code = unique(discharges$DIAG1_NORM),
  full_description = TRUE
)

discharges_metadata <- discharges |>
  left_join(metadata, by = c("DIAG1_NORM" = "codigo"))

## ----busqueda-----------------------------------------------------------------
# Tolerant search: "diabetis" instead of "diabetes"
# (by default the 50 most similar results are shown;
#  we raise the limit because the catalog has many diabetes codes)
search_results <- cie_search(text = "diabetis", threshold = 0.7, max_results = 100)

search_results

## ----cruce--------------------------------------------------------------------
# Which diabetes codes are actually in my data?
diabetes_codes <- intersect(
  search_results$codigo,
  unique(discharges$DIAG1_NORM)
)

diabetes_codes

## ----reporte-diabetes---------------------------------------------------------
# Final report: diabetes discharges, summarized by type
discharges_full |>
  filter(DIAG1_NORM %in% diabetes_codes) |>
  count(description, sort = TRUE)

## ----sin-resultados-lookup----------------------------------------------------
cie_lookup("XYZ123")

## ----sin-resultados-search----------------------------------------------------
cie_search("zzzqwerty", threshold = 0.95)

## ----validacion---------------------------------------------------------------
cie_validate_vector(c("E11.0", "XYZ123", "I10X"))

## ----comorbilidad, eval=rlang::is_installed("comorbidity")--------------------
# Requires the 'comorbidity' package to be installed
# Calculation of the Charlson Index consolidated by patient
comorbidities <- cie_comorbid(
  data = discharges,
  id   = "PATIENT_ID",
  code = "DIAG1",
  map  = "charlson"
)

head(comorbidities, 10)

