# ============================================================
# COBERTURA: validaciones de input en cie11_search()
# Cubre todos los cli::cli_abort previos a la llamada a la API.
# No requiere conexion ni credenciales OMS.
# ============================================================

test_that("cie11_search aborta con text no-string", {
  expect_error(
    cie11_search(text = 123, api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = c("a", "b"), api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = NA_character_, api_key = "fake"),
    class = "ciecl_invalid_input"
  )
})

test_that("cie11_search pinea mensaje de error de tipo invalido", {
  testthat::local_reproducible_output()

  # Pineo del texto exacto del error de validacion de tipo
  expect_snapshot(cie11_search(123), error = TRUE)
})

test_that("cie11_search aborta con text vacio (solo espacios)", {
  expect_error(
    cie11_search(text = "", api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "   ", api_key = "fake"),
    class = "ciecl_invalid_input"
  )
})

test_that("cie11_search aborta con max_results invalido", {
  expect_error(
    cie11_search(text = "diabetes", max_results = 0, api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", max_results = -1, api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", max_results = 1.5, api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", max_results = "10", api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", max_results = c(1, 2), api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", max_results = NA, api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", max_results = NA_real_, api_key = "fake"),
    class = "ciecl_invalid_input"
  )
})

test_that("cie11_search aborta con release fuera de formato YYYY-MM", {
  expect_error(
    cie11_search(text = "diabetes", release = "2024", api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", release = "2024-1", api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", release = "abcd-ef", api_key = "fake"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie11_search(text = "diabetes", release = c("2024-01", "2024-02"),
                 api_key = "fake"),
    class = "ciecl_invalid_input"
  )
})

test_that("cie11_search valida lang via rlang::arg_match", {
  expect_error(
    cie11_search(text = "diabetes", lang = "fr", api_key = "fake")
  )
})
