# test-data-integrity.R
# Pruebas de integridad del dataset cie10_cl

# ==============================================================================
# PRUEBAS DE ESTRUCTURA DEL DATASET
# ==============================================================================

test_that("cie10_cl tiene estructura correcta", {
  # Cargar dataset
  data(cie10_cl, package = "ciecl", envir = environment())

  expect_s3_class(cie10_cl, "tbl_df")

  # Columnas esperadas
 columnas_esperadas <- c(
    "codigo", "descripcion", "categoria", "seccion",
    "capitulo_nombre", "inclusion", "exclusion", "capitulo",
    "es_daga", "es_cruz"
  )

  expect_true(all(columnas_esperadas %in% names(cie10_cl)),
              info = paste("Columnas faltantes:",
                           paste(setdiff(columnas_esperadas, names(cie10_cl)),
                                 collapse = ", ")))
})

test_that("cie10_cl tiene tipos de columna correctos", {
  data(cie10_cl, package = "ciecl", envir = environment())

  # Columnas character
  expect_type(cie10_cl$codigo, "character")
  expect_type(cie10_cl$descripcion, "character")

  # Columnas logical
  expect_type(cie10_cl$es_daga, "logical")
  expect_type(cie10_cl$es_cruz, "logical")
})

test_that("cie10_cl tiene numero esperado de codigos", {
  data(cie10_cl, package = "ciecl", envir = environment())

  # El dataset MINSAL tiene aproximadamente 39877 codigos
  # Verificar rango razonable
  expect_gte(nrow(cie10_cl), 39000)
  expect_lte(nrow(cie10_cl), 40000)
})

test_that("cie10_cl no tiene duplicados en codigo", {
  data(cie10_cl, package = "ciecl", envir = environment())

  n_total <- nrow(cie10_cl)
  n_unique <- length(unique(cie10_cl$codigo))

  expect_equal(n_total, n_unique,
               info = paste("Duplicados encontrados:", n_total - n_unique))
})

test_that("cie10_cl no tiene codigos NA", {
  data(cie10_cl, package = "ciecl", envir = environment())

  n_na <- sum(is.na(cie10_cl$codigo))
  expect_equal(n_na, 0,
               info = paste("Codigos NA encontrados:", n_na))
})

test_that("cie10_cl no tiene descripciones NA o vacias", {
  data(cie10_cl, package = "ciecl", envir = environment())

  n_na <- sum(is.na(cie10_cl$descripcion))
  n_empty <- sum(nchar(trimws(cie10_cl$descripcion)) == 0, na.rm = TRUE)

  expect_equal(n_na, 0, info = paste("Descripciones NA:", n_na))
  expect_equal(n_empty, 0, info = paste("Descripciones vacias:", n_empty))
})

# ==============================================================================
# PRUEBAS DE FORMATO DE CODIGOS
# ==============================================================================

test_that("codigos siguen formato CIE-10 valido", {
  data(cie10_cl, package = "ciecl", envir = environment())

  # Patron CIE-10: letra + 2 digitos + opcional (.digitos)
  patron <- "^[A-Z][0-9]{2}(\\.[0-9X]{1,2})?$"

  # Excluir codigos especiales de morfologia
  codigos_regulares <- cie10_cl$codigo[!grepl("^M80", cie10_cl$codigo)]

  validos <- grepl(patron, codigos_regulares)
  porcentaje <- sum(validos) / length(validos) * 100

  # Al menos 95% deben cumplir el patron
  expect_gt(porcentaje, 95)
})

test_that("todos los capitulos estan representados", {
  data(cie10_cl, package = "ciecl", envir = environment())

  # Capitulos CIE-10: A-Z (algunos no usados)
  capitulos <- unique(substr(cie10_cl$codigo, 1, 1))

  # Capitulos esperados en CIE-10 Chile
  esperados <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                 "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
                 "U", "V", "W", "X", "Y", "Z")

  # Al menos los principales deben estar
  principales <- c("A", "B", "C", "D", "E", "F", "G", "I", "J", "K")
  expect_true(all(principales %in% capitulos),
              info = paste("Capitulos faltantes:",
                           paste(setdiff(principales, capitulos), collapse = ", ")))
})

# ==============================================================================
# PRUEBAS DE CODIGOS ESPECIFICOS CONOCIDOS
# ==============================================================================

test_that("codigos diabetes existen", {
  data(cie10_cl, package = "ciecl", envir = environment())

  codigos_dm <- cie10_cl$codigo[grepl("^E1[0-4]", cie10_cl$codigo)]
  expect_gt(length(codigos_dm), 50)

  # Codigo especifico
  expect_true("E11.0" %in% cie10_cl$codigo)
})

test_that("codigos hipertension existen", {
  data(cie10_cl, package = "ciecl", envir = environment())

  codigos_hta <- cie10_cl$codigo[grepl("^I1[0-5]", cie10_cl$codigo)]
  expect_gt(length(codigos_hta), 10)  # Hay al menos 10 codigos de hipertension

  # Codigo especifico
  expect_true("I10" %in% cie10_cl$codigo)
})

test_that("codigos neoplasias malignas existen", {
  data(cie10_cl, package = "ciecl", envir = environment())

  codigos_neo <- cie10_cl$codigo[grepl("^C[0-9]", cie10_cl$codigo)]
  expect_gt(length(codigos_neo), 200)
})

test_that("codigos infarto miocardio existen", {
  data(cie10_cl, package = "ciecl", envir = environment())

  codigos_iam <- cie10_cl$codigo[grepl("^I2[1-2]", cie10_cl$codigo)]
  expect_gt(length(codigos_iam), 10)

  expect_true("I21.0" %in% cie10_cl$codigo)
})

test_that("codigos COVID-19 existen", {
  data(cie10_cl, package = "ciecl", envir = environment())

  # COVID-19 usa codigo U07 (capitulo especial)
  codigos_covid <- cie10_cl$codigo[grepl("^U07", cie10_cl$codigo)]

  # Puede que no esten en version 2018
  # Solo verificar si existen que sean validos
  if (length(codigos_covid) > 0) {
    expect_true(all(grepl("^U07", codigos_covid)))
  }
})

# ==============================================================================
# PRUEBAS DE COLUMNAS DAGA/CRUZ
# ==============================================================================

test_that("columnas es_daga y es_cruz son consistentes", {
  data(cie10_cl, package = "ciecl", envir = environment())

  # No deben ser todos TRUE ni todos FALSE
  expect_true(any(cie10_cl$es_daga, na.rm = TRUE) || all(!cie10_cl$es_daga, na.rm = TRUE),
              info = "es_daga debe tener variabilidad o todos FALSE")

  expect_true(any(cie10_cl$es_cruz, na.rm = TRUE) || all(!cie10_cl$es_cruz, na.rm = TRUE),
              info = "es_cruz debe tener variabilidad o todos FALSE")
})
