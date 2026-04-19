# test-api-mock.R
# Pruebas de API CIE-11 con mocks y validacion de parametros

# ============================================================
# PRUEBAS DE VALIDACION DE PARAMETROS cie11_search()
# ============================================================

# ============================================================
# PRUEBAS DE MANEJO DE ERRORES
# ============================================================

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

# ============================================================
# PRUEBAS DE FORMATO DE API KEY
# ============================================================

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

# ============================================================
# PRUEBAS DE VARIABLE DE ENTORNO
# ============================================================

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

# ============================================================
# PRUEBAS DE DEPENDENCIA HTTR2
# ============================================================

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

# ============================================================
# PRUEBAS CON HTTP MOCKING (local_mocked_bindings)
# Cubren lineas 41-95 de cie-api.R: OAuth token + search + parsing
# ============================================================

test_that("cie11_search retorna resultados con mock HTTP exitoso", {
  skip_if_not_installed("httr2")

  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      structure(list(call_id = call_count), class = "httr2_response")
    },
    resp_body_json = function(resp, ...) {
      if (resp$call_id == 1L) {
        # Token response
        list(access_token = "mock_token_abc123")
      } else {
        # Search response con resultados
        list(destinationEntities = data.frame(
          theCode = c("5A00", "5A01", "5A02"),
          title = c(
            "<em class='found'>Diabetes</em> mellitus tipo 1",
            "<em class='found'>Diabetes</em> mellitus tipo 2",
            "Otra <em class='found'>diabetes</em> mellitus"
          ),
          chapter = c("05", "05", "05"),
          stringsAsFactors = FALSE
        ))
      }
    },
    .package = "httr2"
  )

  resultado <- cie11_search("diabetes", api_key = "test_id:test_secret")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 3)
  expect_equal(resultado$codigo, c("5A00", "5A01", "5A02"))
  # Verificar que HTML tags fueron limpiados

  expect_false(grepl("<em", resultado$titulo[1]))
  expect_true(grepl("Diabetes mellitus tipo 1", resultado$titulo[1]))
  expect_equal(resultado$capitulo, c("05", "05", "05"))
})

test_that("cie11_search respeta max_results con mock", {
  skip_if_not_installed("httr2")

  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      structure(list(call_id = call_count), class = "httr2_response")
    },
    resp_body_json = function(resp, ...) {
      if (resp$call_id == 1L) {
        list(access_token = "mock_token")
      } else {
        list(destinationEntities = data.frame(
          theCode = c("5A00", "5A01", "5A02", "5A03", "5A04"),
          title = paste("Resultado", 1:5),
          chapter = rep("05", 5),
          stringsAsFactors = FALSE
        ))
      }
    },
    .package = "httr2"
  )

  resultado <- cie11_search("diabetes", api_key = "id:secret", max_results = 2)

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 2)
})

test_that("cie11_search retorna tibble vacio sin destinationEntities", {
  skip_if_not_installed("httr2")

  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      structure(list(call_id = call_count), class = "httr2_response")
    },
    resp_body_json = function(resp, ...) {
      if (resp$call_id == 1L) {
        list(access_token = "mock_token")
      } else {
        # Sin destinationEntities
        list(error = FALSE, errorMessage = "")
      }
    },
    .package = "httr2"
  )

  expect_message(
    resultado <- cie11_search("xyznonexistent", api_key = "id:secret"),
    "Sin resultados"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
  expect_true(all(c("codigo", "titulo", "capitulo") %in% names(resultado)))
})

test_that("cie11_search limpia HTML tags correctamente con mock", {
  skip_if_not_installed("httr2")

  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      structure(list(call_id = call_count), class = "httr2_response")
    },
    resp_body_json = function(resp, ...) {
      if (resp$call_id == 1L) {
        list(access_token = "mock_token")
      } else {
        list(destinationEntities = data.frame(
          theCode = "BA00",
          title = paste0(
            "<em class='found'>Hipertensi\u00f3n</em> ",
            "<em class='found'>arterial</em> esencial"),
          chapter = "11",
          stringsAsFactors = FALSE
        ))
      }
    },
    .package = "httr2"
  )

  resultado <- cie11_search("hipertension", api_key = "id:secret")

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$titulo[1], "Hipertensi\u00f3n arterial esencial")
  expect_false(grepl("<em", resultado$titulo[1]))
})

test_that("cie11_search maneja JSON inesperado sin entities", {
  skip_if_not_installed("httr2")

  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      structure(list(call_id = call_count), class = "httr2_response")
    },
    resp_body_json = function(resp, ...) {
      if (resp$call_id == 1L) {
        list(access_token = "mock_token")
      } else {
        # JSON sin destinationEntities ni error — estructura inesperada
        list(status = "ok", data = list())
      }
    },
    .package = "httr2"
  )

  expect_message(
    resultado <- cie11_search("unexpected", api_key = "id:secret"),
    "Sin resultados"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie11_search retorna tibble vacio con destinationEntities vacio", {
  skip_if_not_installed("httr2")

  call_count <- 0L

  local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      structure(list(call_id = call_count), class = "httr2_response")
    },
    resp_body_json = function(resp, ...) {
      if (resp$call_id == 1L) {
        list(access_token = "mock_token")
      } else {
        # destinationEntities presente pero vacio
        list(destinationEntities = list())
      }
    },
    .package = "httr2"
  )

  expect_message(
    resultado <- cie11_search("nada", api_key = "id:secret"),
    "Sin resultados"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})
