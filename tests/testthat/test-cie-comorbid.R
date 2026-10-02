# ============================================================
# PRUEBAS cie_comorbid()
# ============================================================

test_that("cie_comorbid valida data, id y code al inicio", {
  # Canario CRAN: la validacion precede a check_installed("comorbidity"),
  # por lo que no requiere el paquete opcional
  df <- data.frame(id = 1, diag = "E11.0")

  expect_error(
    cie_comorbid(df, id = c("id", "diag"), code = "diag"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_comorbid(df, id = "id", code = NA_character_),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_comorbid(df, id = 1, code = "diag"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_comorbid("no_es_df", id = "id", code = "diag"),
    class = "ciecl_invalid_input"
  )
})

# ------------------------------------------------------------------------------
# Pruebas basicas de Charlson
# ------------------------------------------------------------------------------

test_that("cie_comorbid calcula Charlson", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 2),
    diag = c("E11.0", "I50.9", "C50.9")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_true("score_charlson" %in% names(resultado))
})

test_that("cie_comorbid Charlson con diabetes tipo 1 y 2", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 2),
    diag = c("E11.0", "E10.9")  # Diabetes mellitus tipo 2 y tipo 1
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_true("score_charlson" %in% names(resultado))
  # Diabetes sin complicaciones = 1 punto en Charlson (valores
  # verificados con comorbidity 1.1.0, mapa charlson_icd10_quan).
  # as.numeric: comorbidity::score() adjunta atributos map/weights
  expect_equal(resultado$diab, c(1, 1))
  expect_equal(as.numeric(resultado$score_charlson), c(1, 1))
})

test_that("cie_comorbid Charlson con infarto miocardio", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "I21.0"  # Infarto agudo miocardio
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_true("score_charlson" %in% names(resultado))
  # IAM = 1 punto en Charlson (verificado con comorbidity 1.1.0)
  expect_equal(resultado$mi[1], 1)
  expect_equal(as.numeric(resultado$score_charlson[1]), 1)
})

test_that("cie_comorbid Charlson con cancer", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 2),
    diag = c("C50.9", "C34.9")  # Cancer mama, cancer pulmon
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$id, 2)
  # Cancer sin metastasis = 2 puntos c/u (verificado comorbidity 1.1.0)
  expect_equal(resultado$canc, c(1, 1))
  expect_equal(as.numeric(resultado$score_charlson), c(2, 2))
})

test_that("cie_comorbid Charlson con insuficiencia cardiaca", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "I50.9"  # Insuficiencia cardiaca
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  # ICC = 1 punto en Charlson (verificado con comorbidity 1.1.0)
  expect_equal(resultado$chf[1], 1)
  expect_equal(as.numeric(resultado$score_charlson[1]), 1)
})

# ------------------------------------------------------------------------------
# Pruebas basicas de Elixhauser
# ------------------------------------------------------------------------------

test_that("cie_comorbid calcula Elixhauser", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 2),
    diag = c("E11.0", "I50.9", "J44.9")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "elixhauser")
  expect_s3_class(resultado, "tbl_df")
  expect_length(resultado$id, 2)
})

test_that("cie_comorbid Elixhauser marca binarias esperadas", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  # Un paciente por comorbilidad (valores verificados con
  # comorbidity 1.1.0, mapa elixhauser_icd10_quan)
  df <- data.frame(
    id = 1:4,
    diag = c("I10", "J44.9", "E66.9", "F32.9")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "elixhauser")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(resultado$hypunc, c(1, 0, 0, 0))  # hipertension
  expect_equal(resultado$cpd,    c(0, 1, 0, 0))  # EPOC
  expect_equal(resultado$obes,   c(0, 0, 1, 0))  # obesidad
  expect_equal(resultado$depre,  c(0, 0, 0, 1))  # depresion
})

# ------------------------------------------------------------------------------
# Pruebas de manejo de NA y vectores vacios
# ------------------------------------------------------------------------------

test_that("cie_comorbid maneja NA en codigos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 2),
    diag = c("E11.0", NA, "I50.9")
  )

  expect_warning(
    resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson"),
    "NA"
  )
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid maneja strings vacios", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 2),
    diag = c("E11.0", "", "I50.9")
  )

  expect_warning(
    resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson"),
    "vac.os"
  )
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid maneja multiples NA", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 1, 2),
    diag = c("E11.0", NA, NA, "I50.9")
  )

  expect_warning(
    resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson"),
    "2 valores NA"
  )
  expect_s3_class(resultado, "tbl_df")
})

# ------------------------------------------------------------------------------
# Pruebas de codigos invalidos y no-CIE
# ------------------------------------------------------------------------------

test_that("cie_comorbid con codigos no reconocidos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1),
    diag = c("E11.0", "XXXXX")  # Codigo invalido
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid con mezcla validos e invalidos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 1, 2, 2),
    diag = c("E11.0", "INVALIDO", "I50.9", "C50.9", "NOVALIDO")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 2)
})

test_that("cie_comorbid con solo codigos invalidos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 2),
    diag = c("INVALIDO1", "INVALIDO2")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  # Score deberia ser 0 para codigos no reconocidos
  expect_true(all(resultado$score_charlson == 0))
})

test_that("cie_comorbid con numeros como codigos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 2),
    diag = c("12345", "99999")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
})

# ------------------------------------------------------------------------------
# Pruebas de edge cases
# ------------------------------------------------------------------------------

test_that("cie_comorbid con un solo codigo", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "E11.0"
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
})

test_that("cie_comorbid con un solo paciente multiples diagnosticos", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = rep(1, 10),
    diag = c("E11.0", "I50.9", "C50.9", "J44.9", "I10",
             "N18.9", "F32.9", "E66.9", "I21.0", "K70.3")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
  # 10 diagnosticos combinados = 9 puntos exactos
  # (verificado con comorbidity 1.1.0)
  expect_equal(as.numeric(resultado$score_charlson[1]), 9)
})

test_that("cie_comorbid con muchos pacientes", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  # Crear dataset con 100 pacientes
  n_pacientes <- 100
  df <- data.frame(
    id = rep(1:n_pacientes, each = 3),
    diag = rep(c("E11.0", "I50.9", "C50.9"), n_pacientes)
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), n_pacientes)
})

test_that("cie_comorbid con codigos duplicados mismo paciente", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 1),
    diag = c("E11.0", "E11.0", "E11.0")  # Mismo codigo repetido
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
})

# ------------------------------------------------------------------------------
# Pruebas de scores conocidos
# ------------------------------------------------------------------------------

test_that("cie_comorbid score sin comorbilidades es 0", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "Z00.0"  # Examen general (no es comorbilidad)
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_equal(resultado$score_charlson[1], 0)
})

test_that("cie_comorbid con SIDA tiene score alto", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "B24"  # VIH/SIDA
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  # SIDA = 6 puntos en Charlson (verificado con comorbidity 1.1.0)
  expect_equal(resultado$aids[1], 1)
  expect_equal(as.numeric(resultado$score_charlson[1]), 6)
})

test_that("cie_comorbid pacientes con diferentes cargas comorbidas", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 2, 2, 2, 3, 3, 3, 3, 3),
    diag = c(
      "Z00.0",                    # Paciente 1: sin comorbilidades
      "E11.0", "I50.9", "J44.9",  # Paciente 2: 3 comorbilidades
      "E11.0", "I50.9", "C50.9", "N18.9", "B24"  # Paciente 3: 5 comorbilidades
    )
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_equal(nrow(resultado), 3)

  # Valores exactos verificados con comorbidity 1.1.0:
  # paciente sin comorbilidades = 0, paciente 2 = 3, paciente 3 = 12
  expect_equal(as.numeric(resultado$score_charlson), c(0, 3, 12))
})

test_that("cie_comorbid suma correctamente comorbilidades multiples", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 1, 1),
    diag = c("E11.0", "I50.9", "C50.9", "J44.9")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  # 4 comorbilidades (DM2 + ICC + cancer + EPOC) = 5 puntos exactos
  # (verificado con comorbidity 1.1.0)
  expect_equal(as.numeric(resultado$score_charlson[1]), 5)
})

# ------------------------------------------------------------------------------
# Pruebas de validacion de parametros
# ------------------------------------------------------------------------------

test_that("cie_comorbid error si comorbidity no instalado", {
  skip_on_cran()
  skip_if(requireNamespace("comorbidity", quietly = TRUE),
          "comorbidity esta instalado")

  df <- data.frame(
    id = 1,
    diag = "E11.0"
  )

  expect_error(
    cie_comorbid(df, id = "id", code = "diag", map = "charlson"),
    "comorbidity"
  )
})

test_that("cie_comorbid acepta nombres de columna personalizados", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    paciente_id = c(1, 1, 2),
    codigo_cie = c("E11.0", "I50.9", "C50.9")
  )

  resultado <- cie_comorbid(df, id = "paciente_id", code = "codigo_cie", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid assign0 = FALSE", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "E11.0"
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson", assign0 = FALSE)
  expect_s3_class(resultado, "tbl_df")

  # Con assign0 = TRUE se retiene al paciente sin comorbilidad (Z00.0)
  df_sin_comorb <- data.frame(
    id = c(1, 2),
    diag = c("E11.0", "Z00.0")
  )

  resultado_true <- cie_comorbid(df_sin_comorb, id = "id", code = "diag",
                                 map = "charlson", assign0 = TRUE)
  expect_s3_class(resultado_true, "tbl_df")
  expect_equal(nrow(resultado_true), 2)
})

test_that("cie_comorbid map default es charlson", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "E11.0"
  )

  # Sin especificar map, debe usar charlson
  resultado <- cie_comorbid(df, id = "id", code = "diag")
  expect_true("score_charlson" %in% names(resultado))
})
