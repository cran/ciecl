# test-cie-search.R
# Pruebas para funciones de busqueda y exploracion del catálogo CIE-10

test_that("cie_search encuentra diabetes con fuzzy", {
  skip_on_cran()

  # Buscar diabetes
  resultado <- cie_search("diabetes mellitus", threshold = 0.70)
  expect_gt(nrow(resultado), 0)
  expect_true(any(stringr::str_detect(resultado$codigo, "^E1[0-4]")))
})

test_that("cie_lookup codigo exacto funciona", {
  skip_on_cran()

  resultado <- cie_lookup("E11.0")
  expect_shape(resultado, nrow = 1)
  expect_equal(resultado$codigo, "E11.0")
})

test_that("cie_lookup vectorizado elimina duplicados", {
  skip_on_cran()

  # Vector con codigos duplicados
  codigos <- c("E11.0", "E11.0", "Z00")
  resultado <- cie_lookup(codigos)

  # No debe haber duplicados en resultado
  expect_length(unique(resultado$codigo), nrow(resultado))
})

test_that("cie_lookup puede generar descripcion_completa", {
  skip_on_cran()

  # Con descripcion_completa
  resultado_completo <- cie_lookup("E11.0", full_description = TRUE)
  expect_true("descripcion_completa" %in% names(resultado_completo))
  expect_match(resultado_completo$descripcion_completa, "E11\\.0 - ")
})

test_that("cie_lookup con descripcion_completa mantiene columna en dataframe vacio", {
  skip_on_cran()

  # Bug fix: Cuando todos los codigos son invalidos, debe mantener la columna descripcion_completa
  suppressMessages({
    resultado_vacio <- cie_lookup(code = c("XXXX", "YYYY", "ZZZZ"), full_description = TRUE)
  })

  # El dataframe debe estar vacio
  expect_length(resultado_vacio$codigo, 0)
  expect_true("descripcion_completa" %in% names(resultado_vacio))
})

# ============================================================
# PRUEBAS PARA cie_short() (antes cie_siglas)
# ============================================================

test_that("cie_short retorna todas las siglas", {
  resultado <- cie_short()

  expect_s3_class(resultado, "tbl_df")
  expect_true("sigla" %in% names(resultado))
  expect_gt(nrow(resultado), 50)
})

test_that("cie_short no tiene duplicados", {
  resultado <- cie_short()

  # No debe haber siglas duplicadas
  expect_length(unique(resultado$sigla), nrow(resultado))
})

test_that("cie_short filtra por categoria", {
  testthat::local_reproducible_output()
  # Filtrar cardiovasculares
  cardio <- cie_short("cardiovascular")
  expect_gt(nrow(cardio), 5)
  expect_true(all(cardio$categoria == "cardiovascular"))

  # Categoria invalida debe dar warning y retornar vacio
  expect_snapshot(invalida <- cie_short("inexistente"))
  expect_length(invalida$sigla, 0)
})

test_that("cie_short valida category escalar", {
  # Vector o numerico: error tipado, no error duro de tolower()/%in%
  expect_error(
    cie_short(c("cardiovascular", "oncologica")),
    class = "ciecl_invalid_input"
  )
  expect_error(cie_short(1), class = "ciecl_invalid_input")
  expect_error(cie_short(NA_character_), class = "ciecl_invalid_input")
})

# ============================================================
# PRUEBAS PARA cie_search() (validaciones)
# ============================================================

test_that("cie_search valida inputs", {
  testthat::local_reproducible_output()
  expect_snapshot(cie_search(123), error = TRUE)
  expect_snapshot(cie_search("a"), error = TRUE)
  expect_snapshot(cie_search("diabetes", threshold = 1.1), error = TRUE)
  expect_snapshot(cie_search("diabetes", max_results = 0), error = TRUE)
})

test_that("cie_search rechaza threshold y max_results fuera de rango con error tipado", {
  expect_error(
    cie_search("diabetes", threshold = 1.1),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_search("diabetes", max_results = 0),
    class = "ciecl_invalid_input"
  )
})

# ============================================================
# PRUEBAS PARA cie_guide() (antes cie_guia_busqueda)
# ============================================================

test_that("cie_guide retorna data.frame", {
  resultado <- cie_guide()

  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 5)
  expect_true("Tengo..." %in% names(resultado))
})

test_that("cie_guide referencia funciones publicas reales del paquete", {
  resultado <- cie_guide()

  texto <- paste(unlist(resultado), collapse = " ")

  # Al menos las 3 funciones core deben aparecer en la guia
  expect_match(texto, "cie_lookup")
  expect_match(texto, "cie_search")
})

test_that("cie_guia_busqueda emite warning de deprecacion", {
  expect_warning(
    cie_guia_busqueda(),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("cie_guia_busqueda retorna mismo resultado que cie_guide", {
  expect_warning(
    legacy <- cie_guia_busqueda(),
    class = "lifecycle_warning_deprecated"
  )
  actual <- cie_guide()

  expect_equal(legacy, actual)
})

# ============================================================
# PRUEBAS COBERTURA - mensaje "Sin coincidencias"
# ============================================================

test_that("cie_search muestra mensaje Sin coincidencias con verbose=TRUE", {
  skip_on_cran()

  expect_message(
    resultado <- cie_search("xyznonexistent12345abc", threshold = 0.95, verbose = TRUE),
    "Sin coincidencias"
  )

  expect_length(resultado$codigo, 0)
})

test_that("cie_lookup con normalizar procesa codigo sin punto", {
  skip_on_cran()

  # E110 -> E11.0
  resultado <- cie_lookup("E110", normalize = TRUE)

  expect_shape(resultado, nrow = 1)
  expect_equal(resultado$codigo, "E11.0")
})

test_that("cie_search sin text da error en espanol", {
  expect_error(cie_search(), class = "ciecl_invalid_input")
  expect_error(cie_search(), "es obligatorio")
})

# ============================================================
# REGRESIONES FASE A (auditoria 2026-09-13)
# ============================================================

test_that("cie_lookup con rango incluye subcategorias del limite superior", {
  skip_on_cran()

  # Regresion: 'E14.9' BETWEEN 'E10' AND 'E14' es falso con collation
  # BINARY de SQLite; el rango debe cubrir las subcategorias del limite
  resultado <- cie_lookup("E10-E14")

  expect_true("E14.0" %in% resultado$codigo)
  expect_true("E14.9" %in% resultado$codigo)
})

test_that("cie_lookup aborta si extract = TRUE con input vectorial", {
  # Validacion previa a la DB: corre sin skip (canario CRAN)
  expect_error(
    cie_lookup(c("CIE:E11.0", "CIE:I10"), extract = TRUE),
    class = "ciecl_invalid_input"
  )
})

test_that("cie_lookup vectorial informa codigos invalidos y no encontrados", {
  skip_on_cran()

  # Un solo mensaje agregado por tipo, no uno por codigo
  expect_message(
    cie_lookup(c("E11.0", "BAD!")),
    "inv.lidos"
  )
  expect_message(
    cie_lookup(c("E11.0", "B99.9")),
    "no encontrados"
  )
})

test_that("cie_search aborta con threshold o max_results NA o no numericos", {
  # Validacion previa a la DB: corre sin skip (canario CRAN)
  expect_error(
    cie_search("diabetes", threshold = NA),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_search("diabetes", threshold = "0.5"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_search("diabetes", max_results = NA),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_search("diabetes", max_results = "10"),
    class = "ciecl_invalid_input"
  )
  expect_error(
    cie_search("diabetes", max_results = c(1, 2)),
    class = "ciecl_invalid_input"
  )
})

test_that("cie_search incluye uso_cl en todos los caminos internos", {
  skip_on_cran()

  # Camino FTS (control)
  res_fts <- cie_search("diabetes", include_uso_cl = TRUE, verbose = FALSE)
  expect_true("uso_cl" %in% names(res_fts))

  # Camino fallback "sin palabras validas" (todas < 2 caracteres)
  res_sin_palabras <- cie_search("a e", include_uso_cl = TRUE, verbose = FALSE)
  expect_true("uso_cl" %in% names(res_sin_palabras))

  # Camino fallback "FTS5 sin resultados" (typo que no matchea el indice)
  res_sin_fts <- cie_search("diabetis", include_uso_cl = TRUE, verbose = FALSE)
  expect_true("uso_cl" %in% names(res_sin_fts))

  # Caso borde: resultado vacio tambien omite la columna si include_uso_cl = FALSE
  res_vacio <- cie_search("zzzzzz", include_uso_cl = FALSE, verbose = FALSE)
  expect_false("uso_cl" %in% names(res_vacio))
})

test_that("cie_search aplica only_uso_cl antes del limite max_results", {
  skip_on_cran()

  # Regresion: los codigos legado no deben consumir cupo del limite.
  # "hipertension" tiene 20 coincidencias exactas, 3 de ellas legado
  # (verificado contra la base MINSAL vigente): con el orden antiguo
  # (slice_head y luego filtrar) max_results = 10 devolvia 9 filas
  res <- cie_search(
    "hipertension",
    include_uso_cl = TRUE, only_uso_cl = TRUE,
    max_results = 10, verbose = FALSE
  )

  expect_equal(nrow(res), 10)
  expect_false(any(res$uso_cl == "legado"))
})

test_that("cie_search con texto de solo simbolos devuelve vacio sin warning", {
  skip_on_cran()

  # Regresion: "!!" no deja palabras candidatas para fuzzy; el camino
  # antiguo calculaba mean(numeric(0)) -> NaN silencioso en los scores
  # expect_no_warning() devuelve el valor evaluado: la asignacion va fuera
  res <- expect_no_warning(cie_search("!!", verbose = FALSE))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_named(res, c("codigo", "descripcion", "score", "categoria"))

  # Con verbose se informa la ausencia de coincidencias
  expect_message(cie_search("!!", verbose = TRUE), "Sin coincidencias")
})
