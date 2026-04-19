# Tests para cie11_search (API CIE-11 OMS)

test_that("cie11_search requiere API key", {
  # Limpiar variable de entorno temporalmente
  old_key <- Sys.getenv("ICD_API_KEY")
  Sys.unsetenv("ICD_API_KEY")
  on.exit(Sys.setenv(ICD_API_KEY = old_key))

  expect_error(cie11_search("diabetes"), "API key OMS requerida")
})

test_that("cie11_search valida formato de API key", {
  skip_if_not_installed("httr2")

  # API key sin separador ":"
 expect_error(
    cie11_search("diabetes", api_key = "invalid_key_format"),
    "client_id:client_secret"
  )
})

test_that("cie11_search requiere httr2", {
  skip_if(requireNamespace("httr2", quietly = TRUE))

  expect_error(
    cie11_search("diabetes", api_key = "test:test"),
    "httr2"
  )
})

# ============================================================
# PRUEBAS CON API REAL (skip_on_cran, requiere ICD_API_KEY)
# ============================================================

test_that("cie11_search con API real retorna resultados", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  # Verificar que existe API key en environment
  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  # Buscar termino comun que debe existir
  resultado <- cie11_search("diabetes", max_results = 2)

  expect_s3_class(resultado, "tbl_df")
  expect_true("codigo" %in% names(resultado))
  expect_true("titulo" %in% names(resultado))
  expect_true("capitulo" %in% names(resultado))
  expect_lte(nrow(resultado), 2)
  if (nrow(resultado) > 0) {
    expect_false(any(grepl("<em", resultado$titulo, fixed = TRUE)))
    expect_false(any(grepl("</em>", resultado$titulo, fixed = TRUE)))
  }
})


test_that("cie11_search con API real respeta max_results", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado_5 <- cie11_search("cancer", max_results = 5)
  resultado_10 <- cie11_search("cancer", max_results = 10)

  expect_lte(nrow(resultado_5), 5)
  expect_lte(nrow(resultado_10), 10)
})


test_that("cie11_search con API real soporta idioma espanol", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado <- cie11_search("hipertension", lang = "es")

  expect_s3_class(resultado, "tbl_df")
  # Si hay resultados, deberian tener titulos en espanol
  if (nrow(resultado) > 0) {
    # Titulos deberian ser texto en espanol
    expect_type(resultado$titulo, "character")
  }
})


test_that("cie11_search con API real soporta idioma ingles", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado <- cie11_search("hypertension", lang = "en")

  expect_s3_class(resultado, "tbl_df")
})


test_that("cie11_search con API real maneja busqueda sin resultados", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  # Buscar termino que probablemente no existe
  expect_message(
    resultado <- cie11_search("xyznonexistent12345"),
    "Sin resultados"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})


test_that("cie11_search con API real limpia tags HTML", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado <- cie11_search("diabetes")

  if (nrow(resultado) > 0) {
    # No deberia haber tags <em> en los titulos
    expect_false(any(grepl("<em", resultado$titulo, fixed = TRUE)))
    expect_false(any(grepl("</em>", resultado$titulo, fixed = TRUE)))
  }
})


# ============================================================
# PRUEBAS DE ESTRUCTURA DE RESPUESTA DETALLADA
# ============================================================

test_that("cie11_search retorna tibble con tipos correctos incluso en error", {
  skip_if_not_installed("httr2")

  suppressWarnings({
    resultado <- cie11_search("test", api_key = "fake:key")
  })

  # Columnas
  expect_true("codigo" %in% names(resultado))
  expect_true("titulo" %in% names(resultado))
  expect_true("capitulo" %in% names(resultado))

  # Tipos (aunque vacio)
  expect_type(resultado$codigo, "character")
  expect_type(resultado$titulo, "character")
  expect_type(resultado$capitulo, "character")
})

test_that("cie11_search maneja texto de busqueda con caracteres especiales", {
  skip_if_not_installed("httr2")

  # No debe crashear con caracteres especiales
  expect_warning(
    resultado <- cie11_search("diabetes & mellitus", api_key = "test:test"),
    "Error API"
  )
  expect_s3_class(resultado, "tbl_df")

  expect_warning(
    resultado2 <- cie11_search("c\u00e1ncer", api_key = "test:test"),
    "Error API"
  )
  expect_s3_class(resultado2, "tbl_df")
})

test_that("cie11_search rechaza texto vacio", {
  skip_if_not_installed("httr2")

  # Texto vacio debe dar error con validacion de input
  expect_error(
    cie11_search("", api_key = "test:test"),
    "vacio"
  )
})

test_that("cie11_search valida tipos de input", {
  skip_if_not_installed("httr2")

  expect_error(cie11_search(123), "string")
  expect_error(cie11_search(c("a", "b")), "string")
  expect_error(cie11_search(NA_character_), "string")
  expect_error(
    cie11_search("test", lang = "fr", api_key = "a:b"), "lang"
  )
  expect_error(
    cie11_search("test", max_results = -1, api_key = "a:b"),
    "entero positivo"
  )
  expect_error(
    cie11_search("test", release = "invalid", api_key = "a:b"),
    "YYYY-MM"
  )
  expect_error(
    cie11_search("test", release = 2024, api_key = "a:b"),
    "YYYY-MM"
  )
})

# ============================================================
# PRUEBAS ADICIONALES PARA COBERTURA
# ============================================================

test_that("cie11_search con API real busca hipertension", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado <- cie11_search("hipertension arterial", max_results = 3)

  expect_s3_class(resultado, "tbl_df")
  expect_lte(nrow(resultado), 3)
  if (nrow(resultado) > 0) {
    expect_true(all(c("codigo", "titulo", "capitulo") %in% names(resultado)))
  }
})


test_that("cie11_search con API real busca cancer", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado <- cie11_search("neoplasia maligna", lang = "es", max_results = 5)

  expect_s3_class(resultado, "tbl_df")
  expect_type(resultado$codigo, "character")
  expect_type(resultado$capitulo, "character")
})


test_that("cie11_search con API real verifica columna capitulo", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado <- cie11_search("neumonia", max_results = 5)

  expect_true("capitulo" %in% names(resultado))
  if (nrow(resultado) > 0) {
    # Capitulos deben ser strings no vacios o NA
    expect_type(resultado$capitulo, "character")
  }
})


test_that("cie11_search con API real acepta max_results=1", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_offline()

  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  skip_if(is.na(api_key) || api_key == "",
          "ICD_API_KEY no configurada para tests reales")

  resultado <- cie11_search("asma", max_results = 1)

  expect_s3_class(resultado, "tbl_df")
  expect_lte(nrow(resultado), 1)
})

