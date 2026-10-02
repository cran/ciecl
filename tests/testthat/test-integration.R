# test-integration.R
# Pruebas de integracion que verifican flujos de trabajo completos

# ============================================================
# FLUJOS DE TRABAJO TIPICOS DE USUARIO
# ============================================================

test_that("flujo: buscar termino -> obtener codigos -> validar", {
  skip_on_cran()

  # 1. Usuario busca termino medico
  resultados_busqueda <- cie_search("diabetes mellitus", threshold = 0.70, max_results = 10)
  expect_gt(nrow(resultados_busqueda), 0)

  # 2. Obtiene codigos de los resultados
  codigos_encontrados <- resultados_busqueda$codigo

  # 3. Valida que los codigos son correctos
  validacion <- cie_validate_vector(codigos_encontrados)
  expect_true(all(validacion), info = "Todos los codigos de busqueda deben ser validos")

  # 4. Obtiene detalles de cada codigo
  detalles <- cie_lookup(codigos_encontrados)
  expect_length(unique(codigos_encontrados), nrow(detalles))
})

test_that("flujo: buscar categoria -> expandir -> calcular comorbilidad", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  # 1. Buscar categoria general de diabetes
  resultado_e11 <- cie_lookup("E11")
  expect_equal(nrow(resultado_e11), 1)

  # 2. Expandir para obtener todos los subcigodos
  hijos_e11 <- cie_expand("E11")
  expect_gt(length(hijos_e11), 5)

  # 3. Crear datos de paciente simulados
  set.seed(123)
  datos_pacientes <- data.frame(
    id_paciente = rep(1:5, each = 2),
    codigo_cie = sample(hijos_e11, 10, replace = TRUE)
  )

  # 4. Calcular comorbilidades
  resultado_comorbid <- cie_comorbid(
    datos_pacientes,
    id = "id_paciente",
    code = "codigo_cie",
    map = "charlson"
  )

  expect_s3_class(resultado_comorbid, "tbl_df")
  expect_true("score_charlson" %in% names(resultado_comorbid))
})

test_that("flujo: SQL personalizado -> procesamiento -> validacion", {
  skip_on_cran()

  # 1. Query SQL personalizada para obtener codigos por capitulo
  codigos_cap4 <- cie10_sql("
    SELECT codigo, descripcion
    FROM cie10
    WHERE codigo LIKE 'E%'
    LIMIT 50
  ")
  expect_s3_class(codigos_cap4, "tbl_df")
  expect_gt(nrow(codigos_cap4), 0)

  # 2. Validar codigos obtenidos
  validacion <- cie_validate_vector(codigos_cap4$codigo)
  expect_true(all(validacion))

  # 3. Obtener detalles completos
  detalles <- cie_lookup(codigos_cap4$codigo, full_description = TRUE)
  expect_true("descripcion_completa" %in% names(detalles))
})

# ============================================================
# PRUEBAS DE CONSISTENCIA ENTRE FUNCIONES
# ============================================================

test_that("cie_lookup y cie10_sql retornan mismos datos", {
  skip_on_cran()

  # Obtener codigo via cie_lookup
  resultado_lookup <- cie_lookup("E11.0")

  # Obtener mismo codigo via SQL directo
  resultado_sql <- cie10_sql("SELECT * FROM cie10 WHERE codigo = 'E11.0'")

  # Deben ser equivalentes
  expect_equal(resultado_lookup$codigo, resultado_sql$codigo)
  expect_equal(resultado_lookup$descripcion, resultado_sql$descripcion)
})

test_that("cie_expand y cie_lookup expandir dan mismos resultados", {
  skip_on_cran()

  # Via cie_expand
  hijos_expand <- cie_expand("E11")

  # Via cie_lookup con expand = TRUE
  hijos_lookup <- cie_lookup("E11", expand = TRUE)$codigo

  # Deben ser iguales
  expect_setequal(hijos_expand, hijos_lookup)
})

# ============================================================
# PRUEBAS DE ESCENARIOS REALES
# ============================================================

test_that("escenario: limpieza de datos con codigos sucios", {
  skip_on_cran()

  # Datos con codigos en diferentes formatos (escenario real)
  codigos_sucios <- c(
    "E110",     # Sin punto - valido
    "e11.0",    # Minusculas - valido
    " E11.0 ",  # Espacios - valido (se normaliza con trim)
    "E11",      # Categoria - valido
    "DIABETES", # Texto (invalido)
    NA,         # NA - invalido
    "",         # Vacio - invalido
    "E11.0"     # Correcto - valido
  )

  # 1. Validar formato (normaliza internamente: trim, punto, sufijo X)
  formato_ok <- cie_validate_vector(codigos_sucios)
  expect_equal(formato_ok, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))

  # 2. Filtrar solo validos
  codigos_validos <- codigos_sucios[formato_ok]

  # 3. Normalizar
  codigos_norm <- cie_norm(codigos_validos, search_db = FALSE)

  # 4. Buscar en base
  suppressMessages({
    resultado <- cie_lookup(codigos_norm)
  })
  expect_gt(nrow(resultado), 0)
})

# ============================================================
# PRUEBAS DE RENDIMIENTO BASICAS
# ============================================================

test_that("busquedas multiples son razonablemente rapidas", {
  skip_on_cran()
  # Flaky en CI: el umbral de tiempo depende del hardware del runner
  skip_on_ci()

  # Medir tiempo de 100 busquedas simples
  tiempo_inicio <- Sys.time()

  for (i in 1:100) {
    resultado <- cie_lookup("E11.0")
  }

  tiempo_fin <- Sys.time()
  duracion <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))

  # Debe completar en menos de 30 segundos (muy generoso)
  expect_lt(duracion, 30)
})

test_that("validacion de vector grande es rapida", {
  skip_on_cran()
  # Flaky en CI: el umbral de tiempo depende del hardware del runner
  skip_on_ci()

  # Vector de 10000 codigos
  codigos <- rep(c("E11.0", "Z00", "INVALIDO"), 3333)
  codigos <- c(codigos, "E11.0")  # Para llegar a 10000

  tiempo_inicio <- Sys.time()
  resultado <- cie_validate_vector(codigos)
  tiempo_fin <- Sys.time()

  duracion <- as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs"))

  # Debe completar en menos de 5 segundos
  expect_lt(duracion, 5)
  expect_length(resultado, 10000)
})

# ============================================================
# PRUEBAS DE INTEROPERABILIDAD CON dplyr
# ============================================================

test_that("resultados son compatibles con verbos dplyr", {
  skip_on_cran()

  # filter sobre resultados de busqueda
  filtrado <- cie_search("diabetes", threshold = 0.70, max_results = 20) |>
    dplyr::filter(score > 0.80)
  expect_s3_class(filtrado, "tbl_df")

  # mutate sobre resultados de lookup
  mutado <- cie_lookup(c("E11.0", "E11.1", "E11.2")) |>
    dplyr::mutate(codigo_corto = substr(codigo, 1, 3))
  expect_s3_class(mutado, "tbl_df")
  expect_true("codigo_corto" %in% names(mutado))

  # group_by + summarise sobre resultados SQL
  resumen <- cie10_sql(
    "SELECT codigo, capitulo FROM cie10 WHERE codigo LIKE 'E1%' LIMIT 100"
  ) |>
    dplyr::group_by(capitulo) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  expect_s3_class(resumen, "tbl_df")
  expect_true("n" %in% names(resumen))
})

# ============================================================
# PRUEBAS DE API CIE-11 (SOLO SI HAY CREDENCIALES)
# ============================================================

test_that("cie11_search falla gracefully sin credenciales", {
  skip_on_cran()
  skip_if_not_installed("httr2")

  # Limpiar credenciales temporalmente
  withr::local_envvar(ICD_API_KEY = "")

  # Sin credenciales, debe dar error informativo
  expect_error(
    cie11_search("diabetes"),
    regexp = "API key|requerida|OMS",
    ignore.case = TRUE
  )
})

# ============================================================
# CANARIO CRAN: flujo E2E minimo (ver politica en setup.R)
# ============================================================

test_that("canario CRAN: normalizar -> validar -> lookup sobre DB del paquete", {
  # Sin skip_on_cran(): corre tambien en CRAN. Sin red, sin paquetes
  # opcionales; el cache SQLite escribe solo en tempdir (setup.R).
  codigos <- cie_norm(c("e11.0", " I50.9 "), search_db = FALSE)
  expect_true(all(cie_validate_vector(codigos)))

  suppressMessages({
    resultado <- cie_lookup(codigos)
  })
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 2L)
})
