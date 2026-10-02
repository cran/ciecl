# test-utils-internal.R
# Pruebas de funciones internas (no exportadas)

# ============================================================
# PRUEBAS normalizar_tildes()
# ============================================================

test_that("normalizar_tildes remueve tildes correctamente", {
  # Acceder a funcion interna
  normalizar_tildes <- normalizar_tildes

  # Vocales minusculas con tilde
  expect_equal(normalizar_tildes("café"), "cafe")
  expect_equal(normalizar_tildes("árbol"), "arbol")
  expect_equal(normalizar_tildes("riñón"), "rinon")
  expect_equal(normalizar_tildes("maís"), "mais")
  expect_equal(normalizar_tildes("bahía"), "bahia")

  # Vocales mayusculas con tilde
  expect_equal(normalizar_tildes("ÁRBOL"), "ARBOL")
  expect_equal(normalizar_tildes("ESPAÑA"), "ESPANA")

  # Dieresis
  expect_equal(normalizar_tildes("pingüino"), "pinguino")
  expect_equal(normalizar_tildes("PINGÜINO"), "PINGUINO")
})

test_that("normalizar_tildes maneja vector vacio", {
  normalizar_tildes <- normalizar_tildes

  resultado <- normalizar_tildes(character(0))
  expect_length(resultado, 0)
  expect_type(resultado, "character")
})

test_that("normalizar_tildes maneja texto sin tildes", {
  normalizar_tildes <- normalizar_tildes

  # Texto ya normalizado
  expect_equal(normalizar_tildes("diabetes"), "diabetes")
  expect_equal(normalizar_tildes("INFARTO"), "INFARTO")
})

test_that("normalizar_tildes es vectorizado", {
  normalizar_tildes <- normalizar_tildes

  entrada <- c("café", "riñón", "normal")
  esperado <- c("cafe", "rinon", "normal")

  expect_equal(normalizar_tildes(entrada), esperado)
})

# ============================================================
# PRUEBAS get_siglas_medicas()
# ============================================================

test_that("get_siglas_medicas retorna lista completa", {
  get_siglas_medicas <- get_siglas_medicas

  siglas <- get_siglas_medicas()

  expect_type(siglas, "list")
  expect_gt(length(siglas), 50)

  # Verificar estructura de cada entrada
  for (sigla in names(siglas)) {
    expect_true("termino" %in% names(siglas[[sigla]]))
    expect_true("categoria" %in% names(siglas[[sigla]]))
  }
})

test_that("get_siglas_medicas contiene siglas comunes", {
  get_siglas_medicas <- get_siglas_medicas

  siglas <- get_siglas_medicas()

  # Siglas que deben existir
  esperadas <- c("iam", "dm", "dm2", "hta", "epoc", "tbc", "vih", "icc", "fa")

  for (s in esperadas) {
    expect_true(s %in% names(siglas),
                info = paste("Sigla faltante:", s))
  }
})

test_that("get_siglas_medicas tiene categorias validas", {
  get_siglas_medicas <- get_siglas_medicas

  siglas <- get_siglas_medicas()
  categorias <- unique(vapply(siglas, function(x) x$categoria, character(1)))

  # Categorias esperadas
  esperadas <- c("cardiovascular", "respiratoria", "metabolica",
                 "gastrointestinal", "infecciosa", "oncologica")

  for (cat in esperadas) {
    expect_true(cat %in% categorias,
                info = paste("Categoria faltante:", cat))
  }
})

# ============================================================
# PRUEBAS expandir_sigla()
# ============================================================

test_that("expandir_sigla expande siglas conocidas", {
  expandir_sigla <- expandir_sigla

  # Siglas comunes
  expect_equal(expandir_sigla("iam")$termino, "infarto agudo miocardio")
  expect_equal(expandir_sigla("IAM")$termino, "infarto agudo miocardio")
  expect_equal(expandir_sigla("dm")$termino, "diabetes mellitus")
  expect_equal(expandir_sigla("hta")$termino, "hipertension arterial")
  expect_equal(expandir_sigla("epoc")$termino, "enfermedad pulmonar obstructiva cronica")

  # Siglas comunes no son ambiguas
  expect_false(expandir_sigla("iam")$ambiguo)

  # IRA es ambigua y trae aviso
  ira <- expandir_sigla("ira")
  expect_equal(ira$termino, "infeccion respiratoria aguda")
  expect_true(ira$ambiguo)
  expect_type(ira$aviso, "character")

  # Aliases explicitos no son ambiguos
  expect_false(expandir_sigla("ira_resp")$ambiguo)
  expect_false(expandir_sigla("ira_renal")$ambiguo)
  expect_equal(expandir_sigla("ira_resp")$termino, "infeccion respiratoria aguda")
  expect_equal(expandir_sigla("ira_renal")$termino, "insuficiencia renal aguda")
})

test_that("expandir_sigla retorna NULL para no-siglas", {
  expandir_sigla <- expandir_sigla

  expect_null(expandir_sigla("diabetes"))
  expect_null(expandir_sigla("xyz123"))
  expect_null(expandir_sigla("NOTASIGLA"))
})

test_that("expandir_sigla maneja espacios", {
  expandir_sigla <- expandir_sigla

  expect_equal(expandir_sigla("  iam  ")$termino, "infarto agudo miocardio")
  expect_equal(expandir_sigla(" DM ")$termino, "diabetes mellitus")
})

# ============================================================
# PRUEBAS extract_cie_from_text()
# ============================================================

test_that("extract_cie_from_text extrae codigo con prefijos", {
  extract_cie <- extract_cie_from_text

  # Prefijos comunes
  expect_equal(extract_cie("CIE:E11.0"), "E11.0")
  expect_equal(extract_cie("cie-E11.0"), "E11.0")
  expect_equal(extract_cie("DX:I10"), "I10")
})

test_that("extract_cie_from_text extrae codigo con sufijos", {
  extract_cie <- extract_cie_from_text

  # Sufijos comunes
  expect_equal(extract_cie("E11.0-confirmado"), "E11.0")
  expect_equal(extract_cie("I10_principal"), "I10")
  expect_equal(extract_cie("Z00.0#1"), "Z00.0")
})

test_that("extract_cie_from_text maneja codigo limpio", {
  extract_cie <- extract_cie_from_text

  # Codigo sin ruido
  expect_equal(extract_cie("E11.0"), "E11.0")
  expect_equal(extract_cie("I10"), "I10")
  expect_equal(extract_cie("Z00"), "Z00")
})

test_that("extract_cie_from_text maneja minusculas", {
  extract_cie <- extract_cie_from_text

  # Convierte a mayusculas
  expect_equal(extract_cie("e11.0"), "E11.0")
  expect_equal(extract_cie("cie:e11.0"), "E11.0")
})

test_that("extract_cie_from_text maneja texto sin codigo valido", {
  extract_cie <- extract_cie_from_text

  # Retorna original si no encuentra patron
  expect_equal(extract_cie("TEXTO SIN CODIGO"), "TEXTO SIN CODIGO")
  expect_equal(extract_cie("123456"), "123456")
})

# ============================================================
# PRUEBAS cie10_empty_tibble()
# ============================================================

test_that("cie10_empty_tibble retorna tibble vacio con estructura correcta", {
  cie10_empty_tibble <- cie10_empty_tibble

  resultado <- cie10_empty_tibble()

  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$codigo, 0)

  # Columnas esperadas
  columnas <- c("codigo", "descripcion", "categoria", "seccion",
                "capitulo_nombre", "inclusion", "exclusion", "capitulo",
                "es_daga", "es_cruz", "uso_cl")

  expect_true(all(columnas %in% names(resultado)))
})

test_that("cie10_empty_tibble con descripcion_completa agrega columna", {
  cie10_empty_tibble <- cie10_empty_tibble

  resultado <- cie10_empty_tibble(add_descripcion_completa = TRUE)

  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$codigo, 0)
  expect_true("descripcion_completa" %in% names(resultado))
  expect_equal(ncol(resultado), 12)
})

test_that("cie10_empty_tibble tiene tipos correctos", {
  cie10_empty_tibble <- cie10_empty_tibble

  resultado <- cie10_empty_tibble()

  expect_type(resultado$codigo, "character")
  expect_type(resultado$descripcion, "character")
  expect_type(resultado$es_daga, "logical")
  expect_type(resultado$es_cruz, "logical")
})

# ============================================================
# PRUEBAS sigla_to_codigo()
# ============================================================

test_that("sigla_to_codigo convierte siglas a codigos CIE-10", {
  skip_on_cran()  # Requiere DB

  sigla_to_codigo <- sigla_to_codigo

  # IAM debe retornar codigo I2x; si retorna NULL el test falla
  codigo_iam <- sigla_to_codigo("iam")
  expect_false(is.null(codigo_iam))
  expect_match(codigo_iam, "^I2[0-5]",
               info = paste("IAM deberia dar I2x, dio:", codigo_iam))
})

test_that("sigla_to_codigo retorna NULL para texto normal", {
  skip_on_cran()

  sigla_to_codigo <- sigla_to_codigo

  expect_null(sigla_to_codigo("diabetes"))
  expect_null(sigla_to_codigo("cualquier texto"))
})

test_that("sigla_to_codigo retorna NULL cuando la busqueda fuzzy no tiene match", {
  # Fija el comportamiento actual del camino sin resultados FTS
  # (cie-siglas.R): NULL silencioso, no tibble vacio
  sigla_to_codigo <- sigla_to_codigo

  local_mocked_bindings(
    cie_search = function(...) tibble::tibble(codigo = character(0))
  )
  expect_null(sigla_to_codigo("iam"))
})

# ============================================================
# PRUEBAS cie_lookup_single()
# ============================================================

test_that("cie_lookup_single funciona con codigo valido", {
  skip_on_cran()

  cie_lookup_single <- cie_lookup_single

  resultado <- cie_lookup_single("E11.0")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$codigo, "E11.0")
})

test_that("cie_lookup_single retorna vacio para codigo invalido", {
  skip_on_cran()

  cie_lookup_single <- cie_lookup_single

  suppressMessages({
    resultado <- cie_lookup_single("XXXXX")
  })

  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$codigo, 0)
})

test_that("cie_lookup_single maneja NA", {
  skip_on_cran()

  cie_lookup_single <- cie_lookup_single

  resultado <- cie_lookup_single(NA_character_)

  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$codigo, 0)
})

test_that("cie_lookup_single maneja cadena vacia", {
  skip_on_cran()

  cie_lookup_single <- cie_lookup_single

  resultado <- cie_lookup_single("")

  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$codigo, 0)
})

test_that("cie_lookup_single rechaza caracteres invalidos", {
  skip_on_cran()

  cie_lookup_single <- cie_lookup_single

  # SQL injection attempt
  suppressMessages({
    resultado <- cie_lookup_single("E11'; DROP TABLE cie10;--")
  })

  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$codigo, 0)
})

test_that("cie_lookup_single expande con patron LIKE", {
  skip_on_cran()

  cie_lookup_single <- cie_lookup_single

  resultado <- cie_lookup_single("E11", expand = TRUE)

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 5)
  expect_true(all(grepl("^E11", resultado$codigo)))
})

test_that("cie_lookup_single maneja rangos", {
  skip_on_cran()

  cie_lookup_single <- cie_lookup_single

  resultado <- cie_lookup_single("E10-E11")

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)
})

test_that("cie_lookup_single error con vector", {
  cie_lookup_single <- cie_lookup_single

  expect_error(cie_lookup_single(c("E11.0", "I10")),
               "solo acepta un c\u00f3digo")
})
