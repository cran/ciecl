# test-cie-utils.R
# Pruebas para funciones de utilidad y normalizacion

test_that("cie_norm convierte formatos correctamente", {
  # Caso base
  expect_equal(cie_norm("E11.0", search_db = FALSE), "E11.0")

  # Sin punto
  expect_equal(cie_norm("E110", search_db = FALSE), "E11.0")

  # Sufijo X (Chile)
  expect_equal(cie_norm("I10X", search_db = FALSE), "I10")
  expect_equal(cie_norm("J00X", search_db = FALSE), "J00")
  expect_equal(cie_norm("B54X", search_db = FALSE), "B54")
  expect_equal(cie_norm("i10x", search_db = FALSE), "I10")

  # Codigos con 7+ chars (sufijos de encuentro ICD-10-PCS) se preservan sin cambio
  expect_equal(cie_norm("S72X01A", search_db = FALSE), "S72X01A")
  expect_equal(cie_norm("T84X0XA", search_db = FALSE), "T84X0XA")
})

test_that("cie_norm no deja punto colgado al remover X final (E11.X)", {
  # Regresion F11: la X de "no especificada" puede venir precedida de
  # punto; remover solo la X dejaba "E11." malformado
  expect_equal(cie_norm("E11.X", search_db = FALSE), "E11")
  expect_equal(cie_norm("I10.X", search_db = FALSE), "I10")
  # Sin punto previo el comportamiento historico se mantiene
  expect_equal(cie_norm("E11X", search_db = FALSE), "E11")
})

test_that("cie_norm es vectorizado", {
  codigos <- c("E110", "I10X", "Z00")
  expect_equal(cie_norm(codigos, search_db = FALSE), c("E11.0", "I10", "Z00"))

  codigos_x <- c("A15X", "B20X", "C50X")
  expect_equal(cie_norm(codigos_x, search_db = FALSE), c("A15", "B20", "C50"))
})

test_that("cie_norm maneja NAs y vacios", {
  expect_true(is.na(cie_norm(NA_character_, search_db = FALSE)))
  expect_equal(cie_norm("", search_db = FALSE), "")
})

test_that("cie_norm con search_db=TRUE valida contra catalogo", {
  skip_on_cran()

  # Codigo que existe despues de normalizar
  expect_equal(cie_norm("E110", search_db = TRUE), "E11.0")

  # Codigo de 3 digitos sin punto: fallback agrega "0" si Z000 existe en DB
  result_3dig <- cie_norm("E11", search_db = TRUE)
  expect_true(result_3dig %in% c("E11", "E11.0"))
})

test_that("cie_validate_vector funciona", {
  codigos <- c("E11.0", "INVALIDO", "I10", NA)
  resultado <- cie_validate_vector(codigos)

  expect_equal(resultado, c(TRUE, FALSE, TRUE, FALSE))
})

test_that("cie_validate_vector con strict=TRUE", {
  skip_on_cran()

  # Ambos codigos existen en el catalogo MINSAL
  codigos <- c("E11.0", "I10")
  resultado <- cie_validate_vector(codigos, strict = TRUE)

  expect_equal(resultado, c(TRUE, TRUE))
})

test_that("cie_expand funciona", {
  skip_on_cran()

  # E11 (Diabetes tipo 2) tiene subcategorias .0 a .9
  hijos <- cie_expand("E11")

  expect_gt(length(hijos), 5)
  expect_true("E11.0" %in% hijos)
  expect_true(all(grepl("^E11", hijos)))
})

# canario CRAN: expansion de codigo hoja (lookup ligero); corre sin skip.
test_that("cie_expand maneja codigos sin hijos", {
  # Un codigo de 4 digitos final no deberia tener mas hijos
  hijos <- cie_expand("E11.0")
  expect_equal(hijos, "E11.0")
})

test_that("cie_norm preserva casos especiales", {
  # Codigos con daga/asterisco
  expect_equal(cie_norm("A17.0+", search_db = FALSE), "A17.0")
  expect_equal(cie_norm("G01*", search_db = FALSE), "G01")
})


test_that("cie_expand aborta con input no escalar", {
  # Regresion: el || sobre un vector era error duro en R >= 4.3;
  # validacion previa a la DB: corre sin skip (canario CRAN).
  # El caso NA se cubre en test-edge-cases.R ("cie_expand maneja NA")
  expect_error(
    cie_expand(c("E11", "I10")),
    class = "ciecl_invalid_input"
  )
})
