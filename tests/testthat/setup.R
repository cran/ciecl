# setup.R - Configuracion para tests
# Solo establecer NOT_CRAN=true si no viene de R CMD check --as-cran
# --as-cran establece NOT_CRAN="" o "false", respetamos eso
if (identical(Sys.getenv("NOT_CRAN"), "")) {
  # Verificar si es un check de CRAN real mirando otras variables
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")) {
    # No es R CMD check, establecer NOT_CRAN=true para tests locales
    Sys.setenv(NOT_CRAN = "true")
  }
}

# Buscar .Renviron en multiples ubicaciones posibles
renviron_paths <- c(
  file.path(getwd(), ".Renviron"),
  normalizePath(file.path("..", "..", ".Renviron"), mustWork = FALSE),
  normalizePath(file.path("..", ".Renviron"), mustWork = FALSE),
  normalizePath("D:/MAGISTER/01_Paquete_R/ciecl/.Renviron", mustWork = FALSE)
)

for (p in renviron_paths) {
  if (file.exists(p)) {
    readRenviron(p)
    break
  }
}
