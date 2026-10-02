## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ciecl)

head(cie10_cl[, c("codigo", "descripcion", "capitulo")])

## -----------------------------------------------------------------------------
# With or without accent: same result
cie_search("neumonia")
cie_search("neumonía")
cie_search("NEUMONIA")

## -----------------------------------------------------------------------------
cie_search("rinon")

## -----------------------------------------------------------------------------
# List all available abbreviations
head(cie_short())

# Filter by category
cie_short(category = "cardiovascular")

# Use the abbreviation directly in a search
cie_search("IAM")   # Acute Myocardial Infarction
cie_search("EPOC")  # Chronic Obstructive Pulmonary Disease
cie_search("DM2")   # Type 2 Diabetes Mellitus

## ----eval=FALSE---------------------------------------------------------------
# # Search in Spanish (default)
# # Requires a WHO API Key
# cie11_search("diabetes mellitus", lang = "es")

## -----------------------------------------------------------------------------
Encoding(cie10_cl$descripcion[1])

