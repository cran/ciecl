# ============================================================
# PRUEBAS PARA cie_describe()
# ============================================================

# canario CRAN: lookup ligero sobre la DB cacheada; corre sin skip.
test_that("cie_describe retorna character vector del mismo largo", {
  resultado <- cie_describe(c("E11.0", "I10", "Z00"))

  expect_type(resultado, "character")
  expect_length(resultado, 3)
})

test_that("cie_describe retorna descripciones conocidas para codigos validos", {
  skip_on_cran()

  resultado <- cie_describe(c("E11.0", "I10"))

  expect_match(resultado[1], "[Dd]iabetes mellitus")
  expect_match(resultado[2], "[Hh]ipertensi[oó]n")
  expect_false(anyNA(resultado))
})

test_that("cie_describe retorna NA para codigos no encontrados (normalize=FALSE)", {
  skip_on_cran()

  # E110 sin punto NO existe en el catalogo
  resultado <- cie_describe("E110", normalize = FALSE)

  expect_length(resultado, 1)
  expect_true(is.na(resultado))
})

test_that("cie_describe rescata codigos sin punto cuando normalize=TRUE", {
  skip_on_cran()

  resultado <- cie_describe("E110", normalize = TRUE)

  expect_length(resultado, 1)
  expect_false(is.na(resultado))
  expect_match(resultado, "[Dd]iabetes mellitus")
})

test_that("cie_describe respeta argumento default", {
  skip_on_cran()

  resultado <- cie_describe(
    c("E11.0", "INVALIDO"),
    normalize = FALSE,
    default = "no encontrado"
  )

  expect_length(resultado, 2)
  expect_false(is.na(resultado[1]))
  expect_equal(resultado[2], "no encontrado")
})

test_that("cie_describe maneja vector vacio", {
  expect_length(cie_describe(character(0)), 0)
  expect_type(cie_describe(character(0)), "character")
})

test_that("cie_describe propaga NA en input", {
  skip_on_cran()

  resultado <- cie_describe(c("E11.0", NA_character_))

  expect_length(resultado, 2)
  expect_false(is.na(resultado[1]))
  expect_true(is.na(resultado[2]))
})

test_that("cie_describe conserva orden y largo del input con duplicados", {
  skip_on_cran()

  diags <- c("E11.0", "E11.0", "I10", "E11.0")
  resultado <- cie_describe(diags)

  expect_length(resultado, 4)
  expect_equal(resultado[1], resultado[2])
  expect_equal(resultado[1], resultado[4])
  expect_match(resultado[3], "[Hh]ipertensi[oó]n")
})

test_that("cie_describe es vectorizado (sirve dentro de mutate)", {
  skip_on_cran()

  df <- data.frame(diag = c("E11.0", "I10", "Z00"))
  df$desc <- cie_describe(df$diag)

  expect_equal(nrow(df), 3)
  expect_type(df$desc, "character")
  expect_false(anyNA(df$desc))
})

test_that("cie_describe normalize valida tipo logico", {
  expect_error(
    cie_describe("E11.0", normalize = "TRUE"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_describe("E11.0", normalize = c(TRUE, FALSE)),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_describe("E11.0", normalize = NA),
    class = "ciecl_invalid_input"
  )
})

test_that("cie_describe acepta argumento espanol deprecado con warning", {
  skip_on_cran()

  expect_warning(
    res <- cie_describe(codigos = c("E11.0", "I10")),
    class = "lifecycle_warning_deprecated"
  )
  expect_length(res, 2)
  expect_match(res[1], "[Dd]iabetes")
})

test_that("cie_describe rescata diagnostico ambiguo en flujo de auditoria", {
  skip_on_cran()

  diags <- c("E11.0", "E110", "I10X", "INVALIDO")

  auditoria <- cie_describe(diags, normalize = FALSE)
  rescate <- cie_describe(diags, normalize = TRUE)

  # Auditoria: E110 e I10X fallan, INVALIDO falla
  expect_equal(sum(is.na(auditoria)), 3)
  # Rescate: solo INVALIDO falla
  expect_equal(sum(is.na(rescate)), 1)
  expect_true(is.na(rescate[4]))
})

test_that("cie_describe valida default (F14)", {
  expect_error(cie_describe("E11.0", default = c("a", "b")), class = "ciecl_invalid_input")
  expect_error(cie_describe("E11.0", default = 5), class = "ciecl_invalid_input")
  # default valido sigue funcionando
  expect_equal(cie_describe("INVALIDO", default = "sin datos"), "sin datos")
})

test_that("cie_describe coerce default NA_real_/NaN a character", {
  # ZZZ no existe en el catalogo (no es categoria CIE-10 valida)
  # NA_real_ queda NA_character_ tras la coercion
  resultado <- cie_describe("ZZZ", default = NA_real_)
  expect_type(resultado, "character")
  expect_true(is.na(resultado))

  # NaN queda "NaN" (comportamiento de as.character() base)
  resultado_nan <- cie_describe("ZZZ", default = NaN)
  expect_type(resultado_nan, "character")
  expect_equal(resultado_nan, "NaN")
})
