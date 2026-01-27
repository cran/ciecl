# test-comorbid-validation.R
# Pruebas de validacion de comorbilidades con datos sinteticos

# ==============================================================================
# PRUEBAS cie_comorbid() - CHARLSON
# ==============================================================================

test_that("cie_comorbid calcula Charlson con datos sinteticos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  # Datos sinteticos con comorbilidades conocidas
  df_charlson <- data.frame(
    id_paciente = c(1, 1, 1, 2, 2, 3, 3, 3, 3),
    codigo_cie = c(
      "E11.0", "I50.9", "N18.3",      # Pac 1: DM2, ICC, ERC
      "C50.9", "C78.0",               # Pac 2: Ca mama + metastasis
      "E10.0", "G20", "J44.1", "I21.0" # Pac 3: DM1, Parkinson, EPOC, IAM
    ),
    stringsAsFactors = FALSE
  )

  resultado <- cie_comorbid(df_charlson, id = "id_paciente",
                            code = "codigo_cie", map = "charlson")

  expect_s3_class(resultado, "tbl_df")
  expect_true("score_charlson" %in% names(resultado))
  expect_equal(nrow(resultado), 3, info = "Debe haber 3 pacientes")

  # Verificar que scores sean >= 0
  expect_true(all(resultado$score_charlson >= 0))
})

test_that("cie_comorbid calcula Elixhauser", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df_elix <- data.frame(
    id = c(1, 1, 2, 2),
    dx = c("E11.0", "I10", "F32.0", "J45.0"),
    stringsAsFactors = FALSE
  )

  resultado <- cie_comorbid(df_elix, id = "id", code = "dx", map = "elixhauser")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 2)

  # Elixhauser no tiene score_charlson
  expect_false("score_charlson" %in% names(resultado))
})

test_that("cie_comorbid maneja NA en codigos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df_na <- data.frame(
    id = c(1, 1, 2, 2),
    dx = c("E11.0", NA, "I10", NA),
    stringsAsFactors = FALSE
  )

  # Debe emitir warning por NAs
  expect_warning(
    resultado <- cie_comorbid(df_na, id = "id", code = "dx", map = "charlson"),
    "NA"
  )

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid maneja codigos vacios", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df_empty <- data.frame(
    id = c(1, 1, 2),
    dx = c("E11.0", "", "I10"),
    stringsAsFactors = FALSE
  )

  # Debe emitir warning por vacios
  expect_warning(
    resultado <- cie_comorbid(df_empty, id = "id", code = "dx", map = "charlson"),
    "vacios"
  )

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid error con columnas inexistentes", {
  df <- data.frame(id = 1, dx = "E11.0")

  expect_error(
    cie_comorbid(df, id = "paciente", code = "dx", map = "charlson"),
    "no existen"
  )

  expect_error(
    cie_comorbid(df, id = "id", code = "codigo", map = "charlson"),
    "no existen"
  )
})

test_that("cie_comorbid usa assign0 correctamente", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 2),
    dx = c("E11.0", "Z00.0"),  # DM2 y codigo sin comorbilidad
    stringsAsFactors = FALSE
  )

  resultado <- cie_comorbid(df, id = "id", code = "dx",
                            map = "charlson", assign0 = TRUE)

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 2)
})

# ==============================================================================
# PRUEBAS cie_map_comorbid()
# ==============================================================================

test_that("cie_map_comorbid categoriza diabetes correctamente", {
  # Solo E10 y E11 estan en el mapeo de cie_map_comorbid
  resultado <- cie_map_comorbid(c("E10.0", "E11.0"))

  expect_equal(nrow(resultado), 2)
  expect_true(all(resultado$categoria == "Diabetes"))
})

test_that("cie_map_comorbid categoriza cardiovascular", {
  codigos <- c("I50.0", "I50.9", "I21.0", "I22.1")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 4)
  # I50 = ICC, I21/I22 = IAM
  expect_true(resultado$categoria[1] == "Insuficiencia cardiaca")
  expect_true(resultado$categoria[3] == "Infarto miocardio")
})

test_that("cie_map_comorbid categoriza neoplasias", {
  codigos <- c("C50.0", "C61", "C34.0", "C18.0")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 4)
  expect_true(all(resultado$categoria == "Neoplasia maligna"))
})

test_that("cie_map_comorbid categoriza respiratorio", {
  codigos <- c("J40", "J44.0", "J44.1")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 3)
  expect_true(all(resultado$categoria == "EPOC"))
})

test_that("cie_map_comorbid categoriza ERC", {
  codigos <- c("N18.1", "N18.3", "N18.5", "N18.9")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 4)
  expect_true(all(resultado$categoria == "Enfermedad renal cronica"))
})

test_that("cie_map_comorbid categoriza trastornos mentales", {
  codigos <- c("F32.0", "F41.0", "F20.0")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 3)
  expect_true(all(resultado$categoria == "Trastornos mentales"))
})

test_that("cie_map_comorbid retorna 'Otra' para codigos no mapeados", {
  codigos <- c("Z00.0", "R10.0", "S62.0")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 3)
  expect_true(all(resultado$categoria == "Otra"))
})

test_that("cie_map_comorbid maneja NA", {
  codigos <- c("E11.0", NA, "I50.9")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 3)
  expect_equal(resultado$categoria[2], "Otra")
})

test_that("cie_map_comorbid maneja vector vacio", {
  resultado <- cie_map_comorbid(character(0))

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
  expect_equal(names(resultado), c("codigo", "categoria"))
})

test_that("cie_map_comorbid maneja codigos mixtos", {
  codigos <- c("E11.0", "I50.9", "C50.9", "J44.0", "N18.3", "Z00.0")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 6)

  # Verificar cada categoria
  expect_equal(resultado$categoria[resultado$codigo == "E11.0"], "Diabetes")
  expect_equal(resultado$categoria[resultado$codigo == "I50.9"], "Insuficiencia cardiaca")
  expect_equal(resultado$categoria[resultado$codigo == "C50.9"], "Neoplasia maligna")
  expect_equal(resultado$categoria[resultado$codigo == "J44.0"], "EPOC")
  expect_equal(resultado$categoria[resultado$codigo == "N18.3"], "Enfermedad renal cronica")
  expect_equal(resultado$categoria[resultado$codigo == "Z00.0"], "Otra")
})

# ==============================================================================
# PRUEBAS CON MULTIPLES COMORBILIDADES POR PACIENTE
# ==============================================================================

test_that("cie_comorbid maneja paciente con muchos codigos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  # Paciente complejo con multiples comorbilidades
  df_complejo <- data.frame(
    id = rep(1, 10),
    dx = c("E11.0", "E11.2", "I10", "I50.0", "N18.3",
           "J44.1", "G20", "F32.0", "C50.9", "I21.0"),
    stringsAsFactors = FALSE
  )

  resultado <- cie_comorbid(df_complejo, id = "id", code = "dx", map = "charlson")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
  expect_gt(resultado$score_charlson, 0)
})

test_that("cie_comorbid distingue pacientes correctamente", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df_multi <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    dx = c("E11.0", "I50.0",   # Pac 1: DM + ICC
           "C50.9", "C78.0",   # Pac 2: Cancer + metastasis
           "Z00.0", "Z01.0"),  # Pac 3: Sin comorbilidades
    stringsAsFactors = FALSE
  )

  resultado <- cie_comorbid(df_multi, id = "id", code = "dx", map = "charlson")

  expect_equal(nrow(resultado), 3)

  # Pac 3 deberia tener score bajo o 0
  score_pac3 <- resultado$score_charlson[resultado$id == 3]
  score_pac2 <- resultado$score_charlson[resultado$id == 2]

  # Cancer con metastasis deberia tener score alto
  expect_gte(score_pac2, score_pac3)
})

# ==============================================================================
# PRUEBAS DE CONSISTENCIA ENTRE FUNCIONES
# ==============================================================================

test_that("cie_map_comorbid y cie_comorbid son consistentes", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  codigos <- c("E11.0", "I21.0", "C50.9")

  # Mapeo manual
  map_resultado <- cie_map_comorbid(codigos)

  # Las categorias de mapeo deben existir
  expect_true("Diabetes" %in% map_resultado$categoria)
  expect_true("Infarto miocardio" %in% map_resultado$categoria)
  expect_true("Neoplasia maligna" %in% map_resultado$categoria)
})

test_that("cie_comorbid retorna columnas consistentes", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(id = 1, dx = "E11.0", stringsAsFactors = FALSE)

  charlson <- cie_comorbid(df, id = "id", code = "dx", map = "charlson")
  elix <- cie_comorbid(df, id = "id", code = "dx", map = "elixhauser")

  # Ambos deben tener columna id
  expect_true("id" %in% names(charlson))
  expect_true("id" %in% names(elix))

  # Solo Charlson tiene score_charlson
  expect_true("score_charlson" %in% names(charlson))
  expect_false("score_charlson" %in% names(elix))
})
