# test-integration.R
# Pruebas de integracion que verifican flujos de trabajo completos

# ==============================================================================
# FLUJOS DE TRABAJO TIPICOS DE USUARIO
# ==============================================================================

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
  expect_equal(nrow(detalles), length(unique(codigos_encontrados)))
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

test_that("flujo: normalizar codigos -> buscar -> mapear comorbilidad", {
  skip_on_cran()

  # 1. Codigos en formato mixto (como vendrian de datos reales)
  codigos_raw <- c("E110", "I509", "C509", "e11.0", " Z00 ")

  # 2. Normalizar codigos
  codigos_norm <- cie_normalizar(codigos_raw, buscar_db = FALSE)
  expect_equal(length(codigos_norm), length(codigos_raw))

  # 3. Buscar detalles
  suppressMessages({
    detalles <- cie_lookup(codigos_norm)
  })
  expect_gt(nrow(detalles), 0)

  # 4. Mapear a categorias de comorbilidad
  mapa <- cie_map_comorbid(codigos_norm)
  expect_equal(nrow(mapa), length(codigos_norm))
  expect_true("Diabetes" %in% mapa$categoria)
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
  detalles <- cie_lookup(codigos_cap4$codigo, descripcion_completa = TRUE)
  expect_true("descripcion_completa" %in% names(detalles))
})

# ==============================================================================
# PRUEBAS DE CONSISTENCIA ENTRE FUNCIONES
# ==============================================================================

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

  # Via cie_lookup con expandir=TRUE
  hijos_lookup <- cie_lookup("E11", expandir = TRUE)$codigo

  # Deben ser iguales
  expect_setequal(hijos_expand, hijos_lookup)
})

test_that("cie_normalizar y cie_lookup son coherentes", {
  skip_on_cran()

  # Codigo sin punto
  codigo_raw <- "E110"

  # Normalizar
  codigo_norm <- cie_normalizar(codigo_raw, buscar_db = FALSE)
  expect_equal(codigo_norm, "E11.0")

  # Buscar con codigo raw y normalizado
  resultado_raw <- cie_lookup(codigo_raw)
  resultado_norm <- cie_lookup(codigo_norm)

  # Deben dar mismo resultado
  expect_equal(resultado_raw$codigo, resultado_norm$codigo)
})

test_that("cie_validate_vector y cie_lookup son coherentes", {
  skip_on_cran()

  codigos <- c("E11.0", "INVALIDO", "Z00")

  # Validar
  validacion <- cie_validate_vector(codigos)
  expect_equal(validacion, c(TRUE, FALSE, TRUE))

  # Buscar (solo los validos deben retornar)
  suppressMessages({
    resultado <- cie_lookup(codigos)
  })

  # Solo codigos validos en resultado
  expect_true(all(resultado$codigo %in% codigos[validacion]))
})

# ==============================================================================
# PRUEBAS DE ESCENARIOS REALES
# ==============================================================================

test_that("escenario: analisis de egreso hospitalario", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  # Simular datos de egresos hospitalarios
  set.seed(42)
  egresos <- data.frame(
    id_egreso = 1:20,
    codigo_principal = sample(c("E11.0", "I50.9", "C50.9", "J18.9", "K70.3"), 20, replace = TRUE),
    codigo_secundario = sample(c("E11.9", "I10", "Z86.7", "N18.9", "F32.9"), 20, replace = TRUE)
  )

  # 1. Validar codigos principales
  val_principal <- cie_validate_vector(egresos$codigo_principal)
  expect_true(all(val_principal))

  # 2. Validar codigos secundarios
  val_secundario <- cie_validate_vector(egresos$codigo_secundario)
  expect_true(all(val_secundario))

  # 3. Preparar datos para comorbilidad (formato largo)
  datos_largo <- data.frame(
    id = rep(egresos$id_egreso, 2),
    codigo = c(egresos$codigo_principal, egresos$codigo_secundario)
  )

  # 4. Calcular comorbilidades
  comorbilidades <- cie_comorbid(datos_largo, id = "id", code = "codigo", map = "charlson")
  expect_s3_class(comorbilidades, "tbl_df")
  expect_equal(nrow(comorbilidades), 20)
})

test_that("escenario: busqueda de codigos para estudio", {
  skip_on_cran()

  # Investigador busca codigos relacionados con su estudio
  # 1. Buscar por termino
  resultados_diabetes <- cie_search("diabetes mellitus tipo 2", threshold = 0.65)

  # 2. Buscar categoria general
  categoria_e11 <- cie_lookup("E11", expandir = TRUE)

  # 3. Combinar resultados
  codigos_estudio <- unique(c(resultados_diabetes$codigo, categoria_e11$codigo))

  # 4. Validar todos
  validacion <- cie_validate_vector(codigos_estudio)
  expect_true(all(validacion))

  # 5. Obtener descripciones completas
  descripciones <- cie_lookup(codigos_estudio, descripcion_completa = TRUE)
  expect_true("descripcion_completa" %in% names(descripciones))
})

test_that("escenario: limpieza de datos con codigos sucios", {
  skip_on_cran()

  # Datos con codigos en diferentes formatos (escenario real)
  codigos_sucios <- c(
    "E110",     # Sin punto - valido
    "e11.0",    # Minusculas - valido
    " E11.0 ",  # Espacios - invalido (debe trim antes)
    "E11",      # Categoria - valido
    "DIABETES", # Texto (invalido)
    NA,         # NA - invalido
    "",         # Vacio - invalido
    "E11.0"     # Correcto - valido
  )

  # 1. Validar formato (nota: espacios hacen invalido el codigo)
  formato_ok <- cie_validate_vector(codigos_sucios)
  expect_equal(formato_ok, c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))

  # 2. Filtrar solo validos
  codigos_validos <- codigos_sucios[formato_ok]

  # 3. Normalizar
  codigos_norm <- cie_normalizar(codigos_validos, buscar_db = FALSE)

  # 4. Buscar en base
  suppressMessages({
    resultado <- cie_lookup(codigos_norm)
  })
  expect_gt(nrow(resultado), 0)
})

# ==============================================================================
# PRUEBAS DE RENDIMIENTO BASICAS
# ==============================================================================

test_that("busquedas multiples son razonablemente rapidas", {
  skip_on_cran()

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

# ==============================================================================
# PRUEBAS DE INTEROPERABILIDAD CON dplyr
# ==============================================================================

test_that("resultados funcionan con dplyr::filter", {
  skip_on_cran()

  resultado <- cie_search("diabetes", threshold = 0.70, max_results = 20)

  # Filtrar con dplyr
  filtrado <- resultado %>%
    dplyr::filter(score > 0.80)

  expect_s3_class(filtrado, "tbl_df")
})

test_that("resultados funcionan con dplyr::mutate", {
  skip_on_cran()

  resultado <- cie_lookup(c("E11.0", "E11.1", "E11.2"))

  # Mutar con dplyr
  mutado <- resultado %>%
    dplyr::mutate(codigo_corto = substr(codigo, 1, 3))

  expect_s3_class(mutado, "tbl_df")
  expect_true("codigo_corto" %in% names(mutado))
})

test_that("resultados funcionan con dplyr::group_by y summarise", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT codigo, capitulo FROM cie10 WHERE codigo LIKE 'E1%' LIMIT 100")

  # Agrupar y resumir
  resumen <- resultado %>%
    dplyr::group_by(capitulo) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  expect_s3_class(resumen, "tbl_df")
  expect_true("n" %in% names(resumen))
})

# ==============================================================================
# PRUEBAS DE API CIE-11 (SOLO SI HAY CREDENCIALES)
# ==============================================================================

test_that("cie11_search falla gracefully sin credenciales", {
  skip_on_cran()
  skip_if_not_installed("httr2")

  # Limpiar credenciales temporalmente
  old_key <- Sys.getenv("ICD_API_KEY")
  Sys.unsetenv("ICD_API_KEY")
  on.exit(if (nchar(old_key) > 0) Sys.setenv(ICD_API_KEY = old_key))

  # Sin credenciales, debe dar error informativo
  expect_error(
    cie11_search("diabetes"),
    regexp = "API key|requerida|OMS",
    ignore.case = TRUE
  )
})
