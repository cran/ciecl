# test-utils-internal.R
# Pruebas de funciones internas (no exportadas)

# ============================================================
# PRUEBAS normalizar_tildes()
# ============================================================

test_that("normalizar_tildes remueve tildes correctamente", {
  # Acceder a funcion interna
  normalizar_tildes <- ciecl:::normalizar_tildes

  # Vocales minusculas con tilde
  expect_equal(normalizar_tildes("caf\u00e9"), "cafe")
  expect_equal(normalizar_tildes("\u00e1rbol"), "arbol")
  expect_equal(normalizar_tildes("ri\u00f1\u00f3n"), "rinon")
  expect_equal(normalizar_tildes("ma\u00eds"), "mais")
  expect_equal(normalizar_tildes("bah\u00eda"), "bahia")

  # Vocales mayusculas con tilde
  expect_equal(normalizar_tildes("\u00c1RBOL"), "ARBOL")
  expect_equal(normalizar_tildes("ESPA\u00d1A"), "ESPANA")

  # Dieresis
  expect_equal(normalizar_tildes("ping\u00fcino"), "pinguino")
  expect_equal(normalizar_tildes("PING\u00dcINO"), "PINGUINO")
})

test_that("normalizar_tildes maneja vector vacio", {
  normalizar_tildes <- ciecl:::normalizar_tildes

  resultado <- normalizar_tildes(character(0))
  expect_length(resultado, 0)
  expect_type(resultado, "character")
})

test_that("normalizar_tildes maneja texto sin tildes", {
  normalizar_tildes <- ciecl:::normalizar_tildes

  # Texto ya normalizado
  expect_equal(normalizar_tildes("diabetes"), "diabetes")
  expect_equal(normalizar_tildes("INFARTO"), "INFARTO")
})

test_that("normalizar_tildes es vectorizado", {
  normalizar_tildes <- ciecl:::normalizar_tildes

  entrada <- c("caf\u00e9", "ri\u00f1\u00f3n", "normal")
  esperado <- c("cafe", "rinon", "normal")

  expect_equal(normalizar_tildes(entrada), esperado)
})

# ============================================================
# PRUEBAS get_siglas_medicas()
# ============================================================

test_that("get_siglas_medicas retorna lista completa", {
  get_siglas_medicas <- ciecl:::get_siglas_medicas

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
  get_siglas_medicas <- ciecl:::get_siglas_medicas

  siglas <- get_siglas_medicas()

  # Siglas que deben existir
  esperadas <- c("iam", "dm", "dm2", "hta", "epoc", "tbc", "vih", "icc", "fa")

  for (s in esperadas) {
    expect_true(s %in% names(siglas),
                info = paste("Sigla faltante:", s))
  }
})

test_that("get_siglas_medicas tiene categorias validas", {
  get_siglas_medicas <- ciecl:::get_siglas_medicas

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
  expandir_sigla <- ciecl:::expandir_sigla

  # Siglas comunes
  expect_equal(expandir_sigla("iam"), "infarto agudo miocardio")
  expect_equal(expandir_sigla("IAM"), "infarto agudo miocardio")
  expect_equal(expandir_sigla("dm"), "diabetes mellitus")
  expect_equal(expandir_sigla("hta"), "hipertension arterial")
  expect_equal(expandir_sigla("epoc"), "enfermedad pulmonar obstructiva cronica")
})

test_that("expandir_sigla retorna NULL para no-siglas", {
  expandir_sigla <- ciecl:::expandir_sigla

  expect_null(expandir_sigla("diabetes"))
  expect_null(expandir_sigla("xyz123"))
  expect_null(expandir_sigla("NOTASIGLA"))
})

test_that("expandir_sigla maneja espacios", {
  expandir_sigla <- ciecl:::expandir_sigla

  expect_equal(expandir_sigla("  iam  "), "infarto agudo miocardio")
  expect_equal(expandir_sigla(" DM "), "diabetes mellitus")
})

# ============================================================
# PRUEBAS extract_cie_from_text()
# ============================================================

test_that("extract_cie_from_text extrae codigo con prefijos", {
  extract_cie <- ciecl:::extract_cie_from_text

  # Prefijos comunes
  expect_equal(extract_cie("CIE:E11.0"), "E11.0")
  expect_equal(extract_cie("cie-E11.0"), "E11.0")
  expect_equal(extract_cie("DX:I10"), "I10")
})

test_that("extract_cie_from_text extrae codigo con sufijos", {
  extract_cie <- ciecl:::extract_cie_from_text

  # Sufijos comunes
  expect_equal(extract_cie("E11.0-confirmado"), "E11.0")
  expect_equal(extract_cie("I10_principal"), "I10")
  expect_equal(extract_cie("Z00.0#1"), "Z00.0")
})

test_that("extract_cie_from_text maneja codigo limpio", {
  extract_cie <- ciecl:::extract_cie_from_text

  # Codigo sin ruido
  expect_equal(extract_cie("E11.0"), "E11.0")
  expect_equal(extract_cie("I10"), "I10")
  expect_equal(extract_cie("Z00"), "Z00")
})

test_that("extract_cie_from_text maneja minusculas", {
  extract_cie <- ciecl:::extract_cie_from_text

  # Convierte a mayusculas
  expect_equal(extract_cie("e11.0"), "E11.0")
  expect_equal(extract_cie("cie:e11.0"), "E11.0")
})

test_that("extract_cie_from_text maneja texto sin codigo valido", {
  extract_cie <- ciecl:::extract_cie_from_text

  # Retorna original si no encuentra patron
  expect_equal(extract_cie("TEXTO SIN CODIGO"), "TEXTO SIN CODIGO")
  expect_equal(extract_cie("123456"), "123456")
})

# ============================================================
# PRUEBAS cie10_empty_tibble()
# ============================================================

test_that("cie10_empty_tibble retorna tibble vacio con estructura correcta", {
  cie10_empty_tibble <- ciecl:::cie10_empty_tibble

  resultado <- cie10_empty_tibble()

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)

  # Columnas esperadas
  columnas <- c("codigo", "descripcion", "categoria", "seccion",
                "capitulo_nombre", "inclusion", "exclusion", "capitulo",
                "es_daga", "es_cruz")

  expect_true(all(columnas %in% names(resultado)))
})

test_that("cie10_empty_tibble con descripcion_completa agrega columna", {
  cie10_empty_tibble <- ciecl:::cie10_empty_tibble

  resultado <- cie10_empty_tibble(add_descripcion_completa = TRUE)

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
  expect_true("descripcion_completa" %in% names(resultado))
  expect_equal(ncol(resultado), 11)
})

test_that("cie10_empty_tibble tiene tipos correctos", {
  cie10_empty_tibble <- ciecl:::cie10_empty_tibble

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

  sigla_to_codigo <- ciecl:::sigla_to_codigo

  # IAM debe retornar codigo I21.x
  codigo_iam <- sigla_to_codigo("iam")
  if (!is.null(codigo_iam)) {
    expect_true(grepl("^I2[0-5]", codigo_iam),
                info = paste("IAM deberia dar I2x, dio:", codigo_iam))
  }
})

test_that("sigla_to_codigo retorna NULL para texto normal", {
  skip_on_cran()

  sigla_to_codigo <- ciecl:::sigla_to_codigo

  expect_null(sigla_to_codigo("diabetes"))
  expect_null(sigla_to_codigo("cualquier texto"))
})

# ============================================================
# PRUEBAS cie_lookup_single()
# ============================================================

test_that("cie_lookup_single funciona con codigo valido", {
  skip_on_cran()

  cie_lookup_single <- ciecl:::cie_lookup_single

  resultado <- cie_lookup_single("E11.0")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$codigo, "E11.0")
})

test_that("cie_lookup_single retorna vacio para codigo invalido", {
  skip_on_cran()

  cie_lookup_single <- ciecl:::cie_lookup_single

  suppressMessages({
    resultado <- cie_lookup_single("XXXXX")
  })

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup_single maneja NA", {
  skip_on_cran()

  cie_lookup_single <- ciecl:::cie_lookup_single

  resultado <- cie_lookup_single(NA_character_)

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup_single maneja cadena vacia", {
  skip_on_cran()

  cie_lookup_single <- ciecl:::cie_lookup_single

  resultado <- cie_lookup_single("")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup_single rechaza caracteres invalidos", {
  skip_on_cran()

  cie_lookup_single <- ciecl:::cie_lookup_single

  # SQL injection attempt
  suppressMessages({
    resultado <- cie_lookup_single("E11'; DROP TABLE cie10;--")
  })

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup_single expande con patron LIKE", {
  skip_on_cran()

  cie_lookup_single <- ciecl:::cie_lookup_single

  resultado <- cie_lookup_single("E11", expandir = TRUE)

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 5)
  expect_true(all(grepl("^E11", resultado$codigo)))
})

test_that("cie_lookup_single maneja rangos", {
  skip_on_cran()

  cie_lookup_single <- ciecl:::cie_lookup_single

  resultado <- cie_lookup_single("E10-E11")

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)
})

test_that("cie_lookup_single error con vector", {
  cie_lookup_single <- ciecl:::cie_lookup_single

  expect_error(cie_lookup_single(c("E11.0", "I10")),
               "solo acepta un codigo")
})
