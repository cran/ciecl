# setup-vcr.R - Configuracion de vcr para tests (convencion oficial vcr)
# vcr esta en Suggests: el guard evita fallos donde no este instalado
if (requireNamespace("vcr", quietly = TRUE)) {
  # Nunca grabar la llave real en los cassettes: se reemplaza por un placeholder
  icd_key <- Sys.getenv("ICD_API_KEY", unset = "")
  filtros <- if (nzchar(icd_key)) list("<ICD_API_KEY>" = icd_key) else list()

  invisible(vcr::vcr_configure(
    dir = vcr::vcr_test_path("fixtures"),
    filter_sensitive_data = filtros
  ))
}
