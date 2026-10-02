# ============================================================
# PRUEBAS cie_map_comorbid()
# ============================================================

# ------------------------------------------------------------------------------
# Pruebas basicas de mapeo
# ------------------------------------------------------------------------------

test_that("cie_map_comorbid categoriza correctamente", {
  codigos <- c("E11.0", "I21.0", "INVALIDO")
  expect_warning(resultado <- cie_map_comorbid(codigos), "formato CIE-10")

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

test_that("cie_map_comorbid categoriza diabetes E10 y E11", {
  resultado <- cie_map_comorbid(c("E10.9", "E11.0"))
  expect_equal(resultado$categoria, c("Diabetes", "Diabetes"))
})

test_that("cie_map_comorbid categoriza infarto miocardio I21 e I22", {
  resultado <- cie_map_comorbid(c("I21.0", "I22.0"))
  expect_true(all(resultado$categoria == "Infarto miocardio"))
})

test_that("cie_map_comorbid categoriza neoplasia maligna", {
  codigos <- c("C50.9", "C34.9", "C18.9")
  resultado <- cie_map_comorbid(codigos)
  expect_true(all(resultado$categoria == "Neoplasia maligna"))
})

test_that("cie_map_comorbid categoriza todo espectro EPOC J40-J44", {
  codigos <- c("J40", "J41.0", "J42", "J43.9", "J44.1", "J44.9")
  resultado <- cie_map_comorbid(codigos)
  expect_true(all(resultado$categoria == "EPOC"))
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

test_that("cie_map_comorbid asigna Otra a codigos invalidos y advierte", {
  expect_warning(
    resultado <- cie_map_comorbid("INVALIDO"),
    "formato CIE-10"
  )
  expect_equal(resultado$categoria[1], "Otra")
})

test_that("cie_map_comorbid asigna Otra a NA sin advertir", {
  resultado <- expect_silent(cie_map_comorbid(NA_character_))
  expect_equal(resultado$categoria[1], "Otra")
})

test_that("cie_map_comorbid mezcla categorias y Otra, advierte solo por INVALIDO", {
  codigos <- c("E11.0", "Z00.0", "I50.9", "INVALIDO")
  expect_warning(
    resultado <- cie_map_comorbid(codigos),
    "formato CIE-10"
  )

  expect_equal(resultado$categoria[1], "Diabetes")
  expect_equal(resultado$categoria[2], "Otra")
  expect_equal(resultado$categoria[3], "Insuficiencia cardiaca")
  expect_equal(resultado$categoria[4], "Otra")
})

test_that("cie_map_comorbid no advierte para codigos CIE-10 validos no mapeados", {
  expect_silent(cie_map_comorbid(c("Z00.0", "R10.0", "S62.0")))
  resultado <- cie_map_comorbid(c("Z00.0", "R10.0", "S62.0"))

  # Codigos validos no mapeados se clasifican como "Otra"
  expect_equal(resultado$categoria[2], "Otra")
  expect_equal(resultado$categoria[3], "Otra")
})

test_that("cie_map_comorbid advierte con multiples codigos invalidos usando conector 'y'", {
  expect_warning(
    cie_map_comorbid(c("hola", "E11.0", "35")),
    "hola.*y.*35|35.*y.*hola"
  )
})

# ------------------------------------------------------------------------------
# Pruebas de validacion de output
# ------------------------------------------------------------------------------

test_that("cie_map_comorbid output tiene columnas correctas", {
  resultado <- cie_map_comorbid(c("E11.0", "I50.9"))

  expect_named(resultado, c("codigo", "categoria"))
  expect_type(resultado$codigo, "character")
  expect_type(resultado$categoria, "character")
})

test_that("cie_map_comorbid preserva orden de entrada", {
  codigos <- c("C50.9", "E11.0", "I50.9", "J44.9")
  resultado <- cie_map_comorbid(codigos)

  expect_equal(resultado$codigo, codigos)
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
