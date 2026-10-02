# ============================================================
# COBERTURA: paths secundarios poco usados
# Cierra ramas no cubiertas en cie-search.R y cie-lookup.R que
# corresponden a parametros no-default (field="inclusion",
# extract=TRUE, check_siglas=TRUE) y procesamiento de rangos
# en vectores mixtos.
# ============================================================

# --- cie_search con field = "inclusion" ------------------------------------

test_that("cie_search retorna data.frame con field='inclusion'", {
  skip_on_cran()

  res <- cie_search("diabetes", field = "inclusion", verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true("inclusion" %in% names(res))
})

test_that("cie_search con field='inclusion' y palabras cortas no rompe", {
  skip_on_cran()

  # palabras de 1-2 chars se filtran del FTS -> path "sin palabras validas"
  res <- cie_search("ab", field = "inclusion", verbose = FALSE)

  expect_s3_class(res, "data.frame")
  # El resultado puede estar vacio o no, lo importante es que no rompa
})

test_that("cie_search con field='inclusion' y termino raro retorna 0", {
  skip_on_cran()

  # Termino que no matchea nada en inclusion -> fallback a carga completa
  res <- cie_search("xyzabc123notfound", field = "inclusion", verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("cie_search: rama 'sin palabras validas tras sanitizar' carga datos reales", {
  skip_on_cran()

  # ".#a .#a": cada palabra sobrevive el split (nchar >= 2) pero al
  # sanitizar para FTS5 (solo alfanumerico) queda en 1 caracter (<2),
  # por lo que palabras_fts queda vacio -> dispara la rama de
  # R/cie-search.R que hace SELECT sin WHERE (carga la tabla completa).
  res <- cie_search(".#a .#a", threshold = 0, verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_true(all(c("codigo", "descripcion", "score", "categoria") %in% names(res)))
  # Los codigos deben existir realmente en el catalogo (la query cargo
  # la tabla cie10 real, no datos vacios/inventados)
  expect_true(all(res$codigo %in% cie10_cl$codigo))
})

test_that("cie_search: rama 'sin palabras validas, fallback' carga datos reales", {
  skip_on_cran()

  # "a b": ambas palabras tienen nchar 1 (< 2) y se descartan en el
  # split inicial -> length(palabras) == 0 -> dispara la rama de
  # R/cie-search.R que carga la tabla completa sin pasar por FTS5.
  res <- cie_search("a b", verbose = FALSE)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_true(all(c("codigo", "descripcion", "score", "categoria") %in% names(res)))
  expect_true(all(res$score == 1)) # match exacto de subcadena "a b"
  expect_true(all(res$codigo %in% cie10_cl$codigo))
})

# --- cie_lookup con extract = TRUE ----------------------------------------

test_that("cie_lookup con extract=TRUE extrae codigo de texto con ruido", {
  skip_on_cran()

  # Texto con diagnostico narrativo y codigo embebido (con espacios)
  # extract_cie_from_text rescata el codigo (puede ser categoria 3-char)
  res <- cie_lookup("Diagnostico: E11.0 diabetes", extract = TRUE)

  expect_s3_class(res, "data.frame")
  expect_gte(nrow(res), 1)
  # Debe retornar al menos un codigo de la familia E11
  expect_true(any(startsWith(res$codigo, "E11")))
})

# --- cie_lookup con check_siglas = TRUE -----------------------------------

test_that("cie_lookup con check_siglas=TRUE expande sigla a codigo", {
  skip_on_cran()

  res <- cie_lookup("IAM", check_siglas = TRUE)

  expect_s3_class(res, "data.frame")
  # IAM se expande a infarto agudo miocardio -> debe encontrar al menos 1 codigo
  expect_gt(nrow(res), 0)
})

test_that("cie_lookup con check_siglas=TRUE deja pasar codigos no-sigla", {
  skip_on_cran()

  res <- cie_lookup(c("E11.0", "I10"), check_siglas = TRUE)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
})

# --- cie_lookup con vector que mezcla codigos y rangos --------------------

test_that("cie_lookup procesa vector con rango y codigo individual", {
  skip_on_cran()

  # Rango E10.0-E10.2 + codigo individual E11.0 en mismo vector
  # Cubre la rama codigos_rango (lineas 226-233)
  res <- cie_lookup(c("E10.0-E10.2", "E11.0"))

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 1)
  expect_true("E11.0" %in% res$codigo)
})

test_that("cie_lookup con multiples rangos en vector", {
  skip_on_cran()

  res <- cie_lookup(c("E10.0-E10.2", "E11.0-E11.2"))

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
})

# --- cie_lookup edge cases ------------------------------------------------

# canario CRAN: lookup de vector solo-NA (camino mas barato del archivo);
# corre sin skip.
test_that("cie_lookup con vector solo de NAs retorna tibble vacio", {
  res <- cie_lookup(c(NA_character_, NA_character_))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

# --- cie_search edge cases en fuzzy ---------------------------------------

test_that("cie_search no rompe con palabras de 1-2 chars en fuzzy", {
  skip_on_cran()

  # Palabras muy cortas se filtran en el path fuzzy
  res <- cie_search("ab cd", verbose = FALSE)

  expect_s3_class(res, "data.frame")
})

test_that("cie_search emite cli_warn al detectar sigla ambigua con verbose", {
  skip_on_cran()

  # IRA es ambigua: cubre el cli_warn dentro del bloque verbose
  expect_message(
    expect_warning(
      cie_search("IRA", verbose = TRUE),
      regexp = "ambigua"
    ),
    regexp = "Sigla detectada"
  )
})
