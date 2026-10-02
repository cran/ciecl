# setup.R - Configuracion para tests
# Solo establecer NOT_CRAN=true si no viene de R CMD check --as-cran
# --as-cran establece NOT_CRAN="" o "false", respetamos eso
if (identical(Sys.getenv("NOT_CRAN"), "")) {
  # Verificar si es un check de CRAN real mirando otras variables
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "")) {
    # No es R CMD check, establecer NOT_CRAN=true para tests locales
    # Usamos withr para asegurar que se restaure al finalizar la suite
    withr::local_envvar(NOT_CRAN = "true", .local_envir = teardown_env())
  }
}

# Aislar cache SQLite en directorio temporal para no contaminar el del usuario
# Esto tambien permite paralelizar tests en el futuro
tmp_cache <- withr::local_tempdir(.local_envir = teardown_env())
withr::local_envvar(CIECL_CACHE_DIR = tmp_cache, .local_envir = teardown_env())

# Asegurar que la conexion pooled se cierre al finalizar la suite
# para permitir la limpieza del directorio temporal en Windows
withr::defer(ciecl::cie10_disconnect(), envir = teardown_env())

# Buscar .Renviron en multiples ubicaciones posibles
renviron_paths <- c(
  file.path(getwd(), ".Renviron"),
  normalizePath(file.path("..", "..", ".Renviron"), mustWork = FALSE),
  normalizePath(file.path("..", ".Renviron"), mustWork = FALSE)
)

for (p in renviron_paths) {
  if (file.exists(p)) {
    readRenviron(p)
    break
  }
}

# -----------------------------------------------------------------------------
# Politica de skip_on_cran() y canario CRAN
# -----------------------------------------------------------------------------
# Llevan skip_on_cran() los tests que NO pueden correr en CRAN porque:
#   - requieren red (API CIE-11 de la OMS: credenciales y conexion),
#   - dependen de paquetes opcionales de Suggests (comorbidity, gt),
#   - son de escala/rendimiento (umbrales de tiempo inestables en hardware
#     heterogeneo) o inspeccionan el cache SQLite local en detalle.
# "Canario CRAN": test SIN skip_on_cran() que corre tambien en CRAN como
# detector temprano de roturas del flujo base (p. ej. normalizar -> validar
# -> lookup sobre la DB de datos del paquete, en test-integration.R).
# Requisitos de todo canario: ligero, sin red, sin Suggests opcionales y
# sin escribir fuera de tempdir (el cache ya se aísla arriba via
# CIECL_CACHE_DIR). La mayoria de los tests corren localmente con
# NOT_CRAN=true (arriba) y en CI; CRAN solo ejecuta los canarios.
