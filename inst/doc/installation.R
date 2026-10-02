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
# # Once: store "client_id:client_secret" in the keychain
# keyring::key_set("ciecl_icd11")
# 
# # In each session where you use the API
# Sys.setenv(ICD_API_KEY = keyring::key_get("ciecl_icd11"))

## ----eval=FALSE---------------------------------------------------------------
# Sys.setenv(ICD_API_KEY = "your_client_id:your_client_secret")

## ----eval=FALSE---------------------------------------------------------------
# # Check that the environment variable is set
# Sys.getenv("ICD_API_KEY")
# 
# # Test an ICD-11 search
# library(ciecl)
# cie11_search("diabetes")

## ----eval=FALSE---------------------------------------------------------------
# # Show the cache location
# tools::R_user_dir("ciecl", "data")

## ----eval=FALSE---------------------------------------------------------------
# library(ciecl)
# cie10_clear_cache()

## ----eval=FALSE---------------------------------------------------------------
# library(ciecl)
# 
# # Check that the package loads correctly
# packageVersion("ciecl")
# 
# # Verify catalogue access
# nrow(cie10_cl)
# 
# # Test a basic lookup
# cie_lookup("E11.0")
# 
# # Test fuzzy search
# cie_search("diabetes")

## ----eval=FALSE---------------------------------------------------------------
# install.packages("ciecl")

## ----eval=FALSE---------------------------------------------------------------
# ciecl::cie10_clear_cache()

