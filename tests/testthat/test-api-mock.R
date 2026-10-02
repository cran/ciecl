# test-api-mock.R
# Pruebas de API CIE-11 con mocks y validacion de parametros

# ============================================================
# PRUEBAS DE VALIDACION DE PARAMETROS cie11_search()
# ============================================================

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

# ============================================================
# PRUEBAS DE DEPENDENCIA HTTR2
# ============================================================

test_that("cie11_search informa sobre httr2 faltante", {
  # Este test solo funciona si httr2 NO esta instalado
  # Lo saltamos si httr2 esta disponible
  skip_if(
    requireNamespace("httr2", quietly = TRUE),
    "httr2 esta instalado"
  )

  expect_error(
    cie11_search("diabetes", api_key = "test:test"),
    "httr2"
  )
})

# ============================================================
# HELPER de mock canonico httr2 (local_mocked_responses)
# Distingue por URL el endpoint OAuth de token del endpoint de
# busqueda OMS para que el flow real de httr2 corra hasta el
# final (incluido el callback req_error(body = who_error_body)).
# ============================================================

mock_who_api <- function(search_body = list(destinationEntities = list()),
                         search_status = 200L) {
  function(req) {
    if (grepl("/connect/token", req$url, fixed = TRUE)) {
      httr2::response_json(body = list(
        access_token = "fake_token",
        token_type = "Bearer",
        expires_in = 3600L
      ))
    } else {
      httr2::response_json(status_code = search_status, body = search_body)
    }
  }
}

# ============================================================
# PRUEBAS CON HTTP MOCKING (httr2::local_mocked_responses)
# El flow real de httr2 corre hasta el final: OAuth token +
# search + parsing JSON + handle_resp.
# ============================================================

test_that("cie11_search retorna resultados con mock HTTP exitoso", {
  skip_if_not_installed("httr2")

  httr2::local_mocked_responses(mock_who_api(
    search_body = list(destinationEntities = data.frame(
      theCode = c("5A00", "5A01", "5A02"),
      title = c(
        "<em class='found'>Diabetes</em> mellitus tipo 1",
        "<em class='found'>Diabetes</em> mellitus tipo 2",
        "Otra <em class='found'>diabetes</em> mellitus"
      ),
      chapter = c("05", "05", "05"),
      stringsAsFactors = FALSE
    ))
  ))

  resultado <- cie11_search("diabetes", api_key = "test_id:test_secret")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 3)
  expect_equal(resultado$codigo, c("5A00", "5A01", "5A02"))
  expect_no_match(resultado$titulo[1], "<em", fixed = TRUE)
  expect_match(resultado$titulo[1], "Diabetes mellitus tipo 1")
  expect_equal(resultado$capitulo, c("05", "05", "05"))
})

test_that("cie11_search respeta max_results con mock", {
  skip_if_not_installed("httr2")

  httr2::local_mocked_responses(mock_who_api(
    search_body = list(destinationEntities = data.frame(
      theCode = c("5A00", "5A01", "5A02", "5A03", "5A04"),
      title = paste("Resultado", 1:5),
      chapter = rep("05", 5),
      stringsAsFactors = FALSE
    ))
  ))

  resultado <- cie11_search("diabetes", api_key = "id:secret", max_results = 2)

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 2)
})

test_that("cie11_search retorna tibble vacio sin destinationEntities", {
  skip_if_not_installed("httr2")

  httr2::local_mocked_responses(mock_who_api(
    search_body = list(error = FALSE, errorMessage = "")
  ))

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

  httr2::local_mocked_responses(mock_who_api(
    search_body = list(destinationEntities = data.frame(
      theCode = "BA00",
      title = paste0(
        "<em class='found'>Hipertensión</em> ",
        "<em class='found'>arterial</em> esencial"
      ),
      chapter = "11",
      stringsAsFactors = FALSE
    ))
  ))

  resultado <- cie11_search("hipertension", api_key = "id:secret")

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$titulo[1], "Hipertensión arterial esencial")
  expect_no_match(resultado$titulo[1], "<em", fixed = TRUE)
})

test_that("cie11_search maneja JSON inesperado sin entities", {
  skip_if_not_installed("httr2")

  httr2::local_mocked_responses(mock_who_api(
    search_body = list(status = "ok", data = list())
  ))

  expect_message(
    resultado <- cie11_search("unexpected", api_key = "id:secret"),
    "Sin resultados"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie11_search retorna tibble vacio con destinationEntities vacio", {
  skip_if_not_installed("httr2")

  httr2::local_mocked_responses(mock_who_api(
    search_body = list(destinationEntities = list())
  ))

  expect_message(
    resultado <- cie11_search("nada", api_key = "id:secret"),
    "Sin resultados"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie11_search advierte y retorna tibble vacio en error HTTP (mock)", {
  skip_if_not_installed("httr2")

  httr2::local_mocked_responses(mock_who_api(
    search_status = 401L,
    search_body = list(error = "invalid_client")
  ))

  # El warning externo siempre lleva el prefijo "Error API CIE-11"
  expect_warning(
    resultado <- cie11_search("diabetes", api_key = "invalid:credentials"),
    "Error API CIE-11"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_true(all(c("codigo", "titulo", "capitulo") %in% names(resultado)))
  expect_length(resultado$codigo, 0)
})

test_that("cie11_search usa ICD_API_KEY de environment (mock)", {
  skip_if_not_installed("httr2")

  withr::local_envvar(ICD_API_KEY = "env:key")
  httr2::local_mocked_responses(mock_who_api(
    search_body = list(destinationEntities = data.frame(
      theCode = "5A00",
      title = "Diabetes mellitus",
      chapter = "05",
      stringsAsFactors = FALSE
    ))
  ))

  # Sin argumento api_key: debe tomar la key del environment
  resultado <- cie11_search("diabetes")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
})

test_that("cie11_search prefiere argumento api_key sobre environment (mock)", {
  skip_if_not_installed("httr2")

  # Si la key del environment tuviera precedencia, abortaria por
  # formato invalido (no contiene ":"); el mock solo responde si
  # el request se construyo con la key del argumento.
  withr::local_envvar(ICD_API_KEY = "singletoken")
  httr2::local_mocked_responses(mock_who_api(
    search_body = list(destinationEntities = data.frame(
      theCode = "5A00",
      title = "Diabetes mellitus",
      chapter = "05",
      stringsAsFactors = FALSE
    ))
  ))

  resultado <- cie11_search("diabetes", api_key = "arg:key")

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
})

# ============================================================
# PRUEBAS UNITARIAS DEL EXTRACTOR who_error_body
# Construimos respuestas reales con httr2::response_json() /
# httr2::response() para no acoplarnos a la estructura interna
# del objeto httr2_response.
# ============================================================

test_that("who_error_body prefiere error_description sobre error y message", {
  skip_if_not_installed("httr2")

  resp <- httr2::response_json(body = list(
    error = "invalid_client",
    error_description = "Bad credentials",
    message = "Should not win"
  ))

  expect_equal(who_error_body(resp), "Bad credentials")
})

test_that("who_error_body cae a error cuando no hay error_description", {
  skip_if_not_installed("httr2")

  resp <- httr2::response_json(body = list(error = "invalid_client"))

  expect_equal(who_error_body(resp), "invalid_client")
})

test_that("who_error_body cae a message como ultimo recurso", {
  skip_if_not_installed("httr2")

  resp <- httr2::response_json(body = list(message = "Rate limit exceeded"))

  expect_equal(who_error_body(resp), "Rate limit exceeded")
})

test_that("who_error_body retorna NULL si body no es JSON parseable", {
  skip_if_not_installed("httr2")

  # response() con body crudo no parseable como JSON
  resp <- httr2::response(body = charToRaw("not valid json {{{"))

  expect_null(who_error_body(resp))
})

test_that("who_error_body retorna NULL si body no tiene fields relevantes", {
  skip_if_not_installed("httr2")

  resp <- httr2::response_json(body = list(status = "ok", data = list()))

  expect_null(who_error_body(resp))
})

test_that("who_error_body ignora fields vacios (nzchar = FALSE)", {
  skip_if_not_installed("httr2")

  resp <- httr2::response_json(body = list(
    error_description = "",
    error = "",
    message = "Fallback usable"
  ))

  expect_equal(who_error_body(resp), "Fallback usable")
})

# ============================================================
# PRUEBA END-TO-END DEL PATH 4xx
# local_mocked_responses devuelve un response_json(401, ...);
# el flow real de httr2 dispara handle_resp -> resp_failure_cnd
# -> error_body(req, resp) -> who_error_body (via req_error).
# Eso enriquece la condicion httr2_http_401 con "Bad credentials"
# antes de que el tryCatch externo de cie11_search la transforme
# en warning amigable.
# ============================================================

test_that("cie11_search propaga detalle de error 4xx via who_error_body", {
  skip_if_not_installed("httr2")

  httr2::local_mocked_responses(mock_who_api(
    search_status = 401L,
    search_body = list(
      error = "invalid_client",
      error_description = "Bad credentials"
    )
  ))

  expect_warning(
    resultado <- cie11_search("diabetes", api_key = "id:secret"),
    "Bad credentials"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})
