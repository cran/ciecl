# ============================================================
# COBERTURA: paths de deprecacion de argumentos espanol -> ingles
# Cierra los paths `lifecycle::deprecate_warn()` que estaban
# sin cobertura en cie-utils.R, cie-search.R, cie-lookup.R.
#
# Usa lifecycle::expect_deprecated() (helper canonico r-lib) en
# lugar de expect_warning(class = "lifecycle_warning_deprecated").
# expect_deprecated setea lifecycle_verbosity = "warning" antes
# del test, evitando la deduplicacion "once per session" de
# lifecycle::deprecate_warn que produciria falsos negativos si
# tests se reordenan o duplican.
# ============================================================

# --- cie_norm ---------------------------------------------------------------

test_that("cie_norm acepta arg deprecado 'codigos' con warning", {
  lifecycle::expect_deprecated(res <- cie_norm(codigos = "E110"))
  expect_equal(res, cie_norm("E110"))
})

test_that("cie_norm acepta arg deprecado 'buscar_db' con warning", {
  lifecycle::expect_deprecated(
    res <- cie_norm("E11.0", buscar_db = FALSE)
  )
  expect_equal(res, cie_norm("E11.0", search_db = FALSE))
})

test_that("cie_norm convierte factor a character", {
  fac <- factor("E11.0")
  res <- cie_norm(fac, search_db = FALSE)
  expect_type(res, "character")
  expect_equal(res, "E11.0")
})

# --- cie_normalizar (deprecada completa) ------------------------------------

test_that("cie_normalizar emite warning de deprecacion completa", {
  lifecycle::expect_deprecated(res <- cie_normalizar("E110"))
  expect_equal(res, cie_norm("E110"))
})

test_that("cie_normalizar pasa buscar_db al backend", {
  lifecycle::expect_deprecated(
    res <- cie_normalizar("E11.0", buscar_db = FALSE)
  )
  expect_equal(res, "E11.0")
})

# --- cie_normalize (deprecada completa, alias EN) ---------------------------

test_that("cie_normalize emite warning de deprecacion completa", {
  lifecycle::expect_deprecated(res <- cie_normalize("E110"))
  expect_equal(res, cie_norm("E110"))
})

test_that("cie_normalize acepta arg deprecado 'codigos'", {
  # Doble warning: uno por cie_normalize completa + uno por arg codigos
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      res <- cie_normalize(codigos = "E110")
    )
  )
  expect_equal(res, cie_norm("E110"))
})

test_that("cie_normalize acepta arg deprecado 'buscar_db'", {
  lifecycle::expect_deprecated(
    lifecycle::expect_deprecated(
      res <- cie_normalize("E11.0", buscar_db = FALSE)
    )
  )
  expect_equal(res, "E11.0")
})

# --- cie_validate_vector ---------------------------------------------------

test_that("cie_validate_vector acepta arg deprecado 'codigos'", {
  lifecycle::expect_deprecated(
    res <- cie_validate_vector(codigos = c("E11.0", "INVALIDO"))
  )
  expect_length(res, 2)
  expect_true(res[1])
  expect_false(res[2])
})

# --- cie_expand ------------------------------------------------------------

test_that("cie_expand acepta arg deprecado 'codigo'", {
  lifecycle::expect_deprecated(res <- cie_expand(codigo = "E11"))
  expect_type(res, "character")
  expect_gt(length(res), 0)
})

# --- cie_search ------------------------------------------------------------

test_that("cie_search acepta arg deprecado 'texto'", {
  lifecycle::expect_deprecated(
    res <- cie_search(texto = "diabetes", verbose = FALSE)
  )
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
})

test_that("cie_search acepta arg deprecado 'campo'", {
  lifecycle::expect_deprecated(
    res <- cie_search("diabetes", campo = "descripcion", verbose = FALSE)
  )
  expect_s3_class(res, "data.frame")
})

test_that("cie_search acepta arg deprecado 'solo_fuzzy'", {
  lifecycle::expect_deprecated(
    res <- cie_search("diabetes", solo_fuzzy = TRUE, verbose = FALSE)
  )
  expect_s3_class(res, "data.frame")
})

# --- cie11_search (API) ----------------------------------------------------

test_that("cie11_search acepta arg deprecado 'texto' (sin invocar API)", {
  # El path deprecation corre antes de la validacion de api_key;
  # "fake" sin ":" aborta con ciecl_invalid_input (R/cie-api.R:100-104)
  # antes de cualquier llamada de red. Si la clase cambia, este test
  # debe fallar (no tragar el error).
  lifecycle::expect_deprecated(
    expect_error(
      cie11_search(texto = "diabetes", api_key = "fake"),
      class = "ciecl_invalid_input"
    )
  )
})

# --- cie_table -------------------------------------------------------------

test_that("cie_table acepta arg deprecado 'codigo'", {
  skip_if_not_installed("gt")

  lifecycle::expect_deprecated(res <- cie_table(codigo = "E11"))
  expect_s3_class(res, "gt_tbl")
})

# --- cie_map_comorbid ------------------------------------------------------

test_that("cie_map_comorbid acepta arg deprecado 'codigos'", {
  lifecycle::expect_deprecated(
    res <- cie_map_comorbid(codigos = c("E11.0", "I50.9"))
  )
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
})

# --- cie_lookup ------------------------------------------------------------

test_that("cie_lookup acepta arg deprecado 'codigo'", {
  lifecycle::expect_deprecated(res <- cie_lookup(codigo = "E11.0"))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
})

test_that("cie_lookup acepta arg deprecado 'expandir'", {
  lifecycle::expect_deprecated(res <- cie_lookup("E11", expandir = TRUE))
  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 1)
})

test_that("cie_lookup acepta arg deprecado 'normalizar'", {
  lifecycle::expect_deprecated(res <- cie_lookup("E110", normalizar = TRUE))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
})

test_that("cie_lookup acepta arg deprecado 'descripcion_completa'", {
  lifecycle::expect_deprecated(
    res <- cie_lookup("E11.0", descripcion_completa = TRUE)
  )
  expect_s3_class(res, "data.frame")
  expect_true("descripcion_completa" %in% names(res))
})

# --- cie_short / cie_siglas ------------------------------------------------

test_that("cie_short acepta arg deprecado 'categoria' con warning", {
  lifecycle::expect_deprecated(res <- cie_short(categoria = "oncologica"))
  expect_equal(res, cie_short("oncologica"))
})

test_that("cie_siglas emite warning de deprecacion completa", {
  lifecycle::expect_deprecated(res <- cie_siglas())
  expect_equal(res, cie_short())
})

test_that("cie_siglas pasa categoria al backend", {
  lifecycle::expect_deprecated(res <- cie_siglas("cardiovascular"))
  expect_equal(res, cie_short("cardiovascular"))
  expect_true(all(res$categoria == "cardiovascular"))
})
