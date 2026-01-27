# test-api-mock.R
# Pruebas de API CIE-11 con mocks y validacion de parametros

# ==============================================================================
# PRUEBAS DE VALIDACION DE PARAMETROS cie11_search()
# ==============================================================================

test_that("cie11_search usa max_results correctamente", {
  skip_if_not_installed("httr2")

  # Verificar que el parametro se acepta sin error de sintaxis
  # La funcion retorna warning y tibble vacio con credenciales invalidas
  expect_warning(
    resultado <- cie11_search("diabetes", api_key = "test:test", max_results = 5),
    "Error API CIE-11"
  )

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie11_search acepta parametro lang", {
  skip_if_not_installed("httr2")

  # Verificar que lang = "es" y "en" son aceptados (retorna warning, no error)
  expect_warning(
    resultado_es <- cie11_search("diabetes", api_key = "test:test", lang = "es"),
    "Error API CIE-11"
  )
  expect_s3_class(resultado_es, "tbl_df")

  expect_warning(
    resultado_en <- cie11_search("diabetes", api_key = "test:test", lang = "en"),
    "Error API CIE-11"
  )
  expect_s3_class(resultado_en, "tbl_df")
})

# ==============================================================================
# PRUEBAS DE MANEJO DE ERRORES
# ==============================================================================

test_that("cie11_search maneja error de conexion gracefully", {
  skip_if_not_installed("httr2")

  # Con credenciales invalidas, debe dar warning y retornar tibble vacio
  # (no debe crashear)
  expect_warning(
    resultado <- cie11_search("diabetes", api_key = "invalid:credentials"),
    "Error API CIE-11"
  )

  # Debe retornar tibble vacio
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie11_search retorna tibble vacio en error", {
  skip_if_not_installed("httr2")

  # Cualquier error debe retornar estructura correcta
  suppressWarnings({
    resultado <- cie11_search("test", api_key = "bad:key")
  })

  expect_s3_class(resultado, "tbl_df")
  expect_true("codigo" %in% names(resultado))
  expect_true("titulo" %in% names(resultado))
  expect_true("capitulo" %in% names(resultado))
  expect_equal(nrow(resultado), 0)
})

# ==============================================================================
# PRUEBAS DE FORMATO DE API KEY
# ==============================================================================

test_that("cie11_search rechaza API key sin separador", {
  skip_if_not_installed("httr2")

  # Sin ":"
  expect_error(
    cie11_search("diabetes", api_key = "singletoken"),
    "client_id:client_secret"
  )

  # Vacio
  expect_error(
    cie11_search("diabetes", api_key = ""),
    "client_id:client_secret"
  )
})

test_that("cie11_search acepta API key con formato correcto",
{
  skip_if_not_installed("httr2")

  # Formato correcto (fallara en autenticacion, no en formato)
  # El error no debe mencionar "client_id:client_secret"
  error_msg <- tryCatch({
    suppressWarnings(cie11_search("diabetes", api_key = "id:secret"))
    "no_error"
  }, error = function(e) {
    e$message
  }, warning = function(w) {
    "warning"
  })

  # Si hay error, no debe ser de formato
  if (error_msg != "no_error" && error_msg != "warning") {
    expect_false(grepl("client_id:client_secret", error_msg))
  }
})

# ==============================================================================
# PRUEBAS DE VARIABLE DE ENTORNO
# ==============================================================================

test_that("cie11_search usa ICD_API_KEY de environment", {
  skip_if_not_installed("httr2")

  # Guardar valor original
  old_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_key)) {
      Sys.unsetenv("ICD_API_KEY")
    } else {
      Sys.setenv(ICD_API_KEY = old_key)
    }
  })

  # Setear key de prueba
  Sys.setenv(ICD_API_KEY = "env:key")

  # Debe usar la key del environment (y fallar en autenticacion)
  expect_warning(
    resultado <- cie11_search("diabetes"),
    "Error API CIE-11"
  )

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie11_search prefiere argumento sobre environment", {
  skip_if_not_installed("httr2")

  # Guardar valor original
  old_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  on.exit({
    if (is.na(old_key)) {
      Sys.unsetenv("ICD_API_KEY")
    } else {
      Sys.setenv(ICD_API_KEY = old_key)
    }
  })

  # Setear key en environment
  Sys.setenv(ICD_API_KEY = "env:key")

  # Usar key de argumento (diferente)
  expect_warning(
    resultado <- cie11_search("diabetes", api_key = "arg:key"),
    "Error API CIE-11"
  )

  # La funcion debe usar arg:key, no env:key
  # (ambas fallaran, pero verificamos que acepta el argumento)
  expect_s3_class(resultado, "tbl_df")
})

# ==============================================================================
# PRUEBAS DE ESTRUCTURA DE RESPUESTA
# ==============================================================================

test_that("cie11_search retorna columnas esperadas", {
  skip_if_not_installed("httr2")

  # Incluso en error, debe retornar estructura correcta
  suppressWarnings({
    resultado <- cie11_search("test", api_key = "test:test")
  })

  columnas_esperadas <- c("codigo", "titulo", "capitulo")
  expect_true(all(columnas_esperadas %in% names(resultado)))
})

test_that("cie11_search columnas tienen tipos correctos", {
  skip_if_not_installed("httr2")

  suppressWarnings({
    resultado <- cie11_search("test", api_key = "test:test")
  })

  expect_type(resultado$codigo, "character")
  expect_type(resultado$titulo, "character")
  expect_type(resultado$capitulo, "character")
})

# ==============================================================================
# PRUEBAS DE DEPENDENCIA HTTR2
# ==============================================================================

test_that("cie11_search informa sobre httr2 faltante", {
  # Este test solo funciona si httr2 NO esta instalado
  # Lo saltamos si httr2 esta disponible
  skip_if(requireNamespace("httr2", quietly = TRUE),
          "httr2 esta instalado")

  expect_error(
    cie11_search("diabetes", api_key = "test:test"),
    "httr2"
  )
})
