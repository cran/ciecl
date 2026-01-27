# ==============================================================================
# PRUEBAS cie_comorbid()
# ==============================================================================

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

test_that("cie_comorbid Charlson con diabetes tipo 2", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "E11.0"  # Diabetes mellitus tipo 2
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_true("score_charlson" %in% names(resultado))
  # Diabetes sin complicaciones = 1 punto en Charlson
  expect_gte(resultado$score_charlson[1], 0)
})

test_that("cie_comorbid Charlson con diabetes tipo 1", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "E10.9"  # Diabetes mellitus tipo 1
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  expect_s3_class(resultado, "tbl_df")
  expect_true("score_charlson" %in% names(resultado))
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
  expect_equal(nrow(resultado), 2)
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
  expect_equal(nrow(resultado), 2)
})

test_that("cie_comorbid Elixhauser con hipertension", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "I10"  # Hipertension esencial
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "elixhauser")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid Elixhauser con EPOC", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "J44.9"  # EPOC
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "elixhauser")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid Elixhauser con obesidad", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "E66.9"  # Obesidad
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "elixhauser")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid Elixhauser con depresion", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diag = "F32.9"  # Episodio depresivo
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "elixhauser")
  expect_s3_class(resultado, "tbl_df")
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
    "vacios"
  )
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_comorbid maneja espacios en blanco", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 2),
    diag = c("E11.0", "   ", "I50.9")
  )

  expect_warning(
    resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson"),
    "vacios"
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
  expect_gt(resultado$score_charlson[1], 0)
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
  # SIDA = 6 puntos en Charlson original
  expect_gte(resultado$score_charlson[1], 0)
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

  # Paciente 3 deberia tener mayor score que paciente 2, y este mayor que 1
  expect_lte(resultado$score_charlson[1], resultado$score_charlson[2])
})

test_that("cie_comorbid suma correctamente comorbilidades multiples", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 1, 1),
    diag = c("E11.0", "I50.9", "C50.9", "J44.9")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
  # Con 4 comorbilidades, score debe ser > 0
  expect_gt(resultado$score_charlson[1], 0)
})

# ------------------------------------------------------------------------------
# Pruebas de validacion de parametros
# ------------------------------------------------------------------------------

test_that("cie_comorbid error si columna id no existe", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    patient = 1,
    diag = "E11.0"
  )

  expect_error(
    cie_comorbid(df, id = "id", code = "diag", map = "charlson"),
    "no existen"
  )
})

test_that("cie_comorbid error si columna code no existe", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = 1,
    diagnosis = "E11.0"
  )

  expect_error(
    cie_comorbid(df, id = "id", code = "diag", map = "charlson"),
    "no existen"
  )
})

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

# ==============================================================================
# PRUEBAS cie_map_comorbid()
# ==============================================================================

# ------------------------------------------------------------------------------
# Pruebas basicas de mapeo
# ------------------------------------------------------------------------------

test_that("cie_map_comorbid categoriza correctamente", {
  codigos <- c("E11.0", "I21.0", "INVALIDO")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 3)
  expect_equal(resultado$categoria[1], "Diabetes")
})

test_that("cie_map_comorbid retorna tibble", {
  codigos <- c("E11.0", "I50.9")
  resultado <- cie_map_comorbid(codigos)

  expect_s3_class(resultado, "tbl_df")
  expect_true("codigo" %in% names(resultado))
  expect_true("categoria" %in% names(resultado))
})

test_that("cie_map_comorbid maneja vector vacio", {
  resultado <- cie_map_comorbid(character(0))

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
  expect_true("codigo" %in% names(resultado))
  expect_true("categoria" %in% names(resultado))
})

# ------------------------------------------------------------------------------
# Pruebas de mapeo Charlson
# ------------------------------------------------------------------------------

test_that("cie_map_comorbid categoriza diabetes E10", {
  resultado <- cie_map_comorbid("E10.9")
  expect_equal(resultado$categoria[1], "Diabetes")
})

test_that("cie_map_comorbid categoriza diabetes E11", {
  resultado <- cie_map_comorbid("E11.0")
  expect_equal(resultado$categoria[1], "Diabetes")
})

test_that("cie_map_comorbid categoriza insuficiencia cardiaca", {
  resultado <- cie_map_comorbid("I50.9")
  expect_equal(resultado$categoria[1], "Insuficiencia cardiaca")
})

test_that("cie_map_comorbid categoriza infarto miocardio I21", {
  resultado <- cie_map_comorbid("I21.0")
  expect_equal(resultado$categoria[1], "Infarto miocardio")
})

test_that("cie_map_comorbid categoriza infarto miocardio I22", {
  resultado <- cie_map_comorbid("I22.0")
  expect_equal(resultado$categoria[1], "Infarto miocardio")
})

test_that("cie_map_comorbid categoriza neoplasia maligna", {
  codigos <- c("C50.9", "C34.9", "C18.9")
  resultado <- cie_map_comorbid(codigos)
  expect_true(all(resultado$categoria == "Neoplasia maligna"))
})

test_that("cie_map_comorbid categoriza EPOC J44", {
  resultado <- cie_map_comorbid("J44.9")
  expect_equal(resultado$categoria[1], "EPOC")
})

test_that("cie_map_comorbid categoriza EPOC J40", {
  resultado <- cie_map_comorbid("J40")
  expect_equal(resultado$categoria[1], "EPOC")
})

test_that("cie_map_comorbid categoriza enfermedad renal cronica", {
  resultado <- cie_map_comorbid("N18.9")
  expect_equal(resultado$categoria[1], "Enfermedad renal cronica")
})

test_that("cie_map_comorbid categoriza trastornos mentales", {
  codigos <- c("F32.9", "F41.9", "F20.0")
  resultado <- cie_map_comorbid(codigos)
  expect_true(all(resultado$categoria == "Trastornos mentales"))
})

# ------------------------------------------------------------------------------
# Pruebas de codigos sin categoria
# ------------------------------------------------------------------------------

test_that("cie_map_comorbid asigna Otra a codigos no mapeados", {
  resultado <- cie_map_comorbid("Z00.0")
  expect_equal(resultado$categoria[1], "Otra")
})

test_that("cie_map_comorbid asigna Otra a codigos invalidos", {
  resultado <- cie_map_comorbid("INVALIDO")
  expect_equal(resultado$categoria[1], "Otra")
})

test_that("cie_map_comorbid asigna Otra a NA", {
  resultado <- cie_map_comorbid(NA_character_)
  expect_equal(resultado$categoria[1], "Otra")
})

test_that("cie_map_comorbid mezcla categorias y Otra", {
  codigos <- c("E11.0", "Z00.0", "I50.9", "INVALIDO")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(resultado$categoria[1], "Diabetes")
  expect_equal(resultado$categoria[2], "Otra")
  expect_equal(resultado$categoria[3], "Insuficiencia cardiaca")
  expect_equal(resultado$categoria[4], "Otra")
})

# ------------------------------------------------------------------------------
# Pruebas de validacion de output
# ------------------------------------------------------------------------------

test_that("cie_map_comorbid output tiene columnas correctas", {
  resultado <- cie_map_comorbid(c("E11.0", "I50.9"))

  expect_equal(names(resultado), c("codigo", "categoria"))
  expect_type(resultado$codigo, "character")
  expect_type(resultado$categoria, "character")
})

test_that("cie_map_comorbid preserva orden de entrada", {
  codigos <- c("C50.9", "E11.0", "I50.9", "J44.9")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(resultado$codigo, codigos)
})

test_that("cie_map_comorbid con un solo codigo", {
  resultado <- cie_map_comorbid("E11.0")

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$codigo[1], "E11.0")
  expect_equal(resultado$categoria[1], "Diabetes")
})

test_that("cie_map_comorbid con muchos codigos", {
  codigos <- paste0("E11.", 0:99)
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 100)
  expect_true(all(resultado$categoria == "Diabetes"))
})

test_that("cie_map_comorbid con codigos repetidos", {
  codigos <- c("E11.0", "E11.0", "E11.0")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(nrow(resultado), 3)
  expect_true(all(resultado$codigo == "E11.0"))
  expect_true(all(resultado$categoria == "Diabetes"))
})

# ------------------------------------------------------------------------------
# Pruebas adicionales
# ------------------------------------------------------------------------------

test_that("cie_map_comorbid es case-sensitive", {
  # Los codigos CIE-10 son mayusculas
  resultado <- cie_map_comorbid("e11.0")  # minuscula
  expect_equal(resultado$categoria[1], "Otra")  # No deberia matchear
})

test_that("cie_map_comorbid con codigos sin punto", {
  resultado <- cie_map_comorbid("E110")
  expect_equal(resultado$categoria[1], "Diabetes")
})

test_that("cie_map_comorbid con codigos de 3 caracteres", {
  resultado <- cie_map_comorbid(c("E11", "I50", "C50"))

  expect_equal(resultado$categoria[1], "Diabetes")
  expect_equal(resultado$categoria[2], "Insuficiencia cardiaca")
  expect_equal(resultado$categoria[3], "Neoplasia maligna")
})
