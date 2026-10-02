# ============================================================
# Flags include_uso_cl / only_uso_cl en cie_lookup() y cie_search().
# Verifican:
#   1) Default preserva backward compat.
#   2) include_uso_cl agrega/quita la columna sin cambiar las filas.
#   3) only_uso_cl excluye codigos con uso_cl == "legado".
#   4) Combinacion both = TRUE filtra Y muestra la columna.
# ============================================================

# --- cie_lookup -----------------------------------------------------------

# canario CRAN: lookup exacto ligero sobre la DB cacheada; corre sin skip.
test_that("cie_lookup default mantiene columna uso_cl (compat v<=0.9.6)", {
  r <- cie_lookup("E11.0")
  expect_true("uso_cl" %in% names(r))
  expect_equal(r$codigo[1], "E11.0")
})

test_that("cie_lookup(include_uso_cl = FALSE) omite la columna", {
  skip_on_cran()

  r <- cie_lookup("E11.0", include_uso_cl = FALSE)
  expect_false("uso_cl" %in% names(r))
  expect_equal(r$codigo[1], "E11.0")
})

test_that("cie_lookup(only_uso_cl = TRUE) excluye codigos 'legado'", {
  skip_on_cran()

  # E11 expandido trae varios subcodigos; algunos pueden ser 'legado'.
  r_all <- cie_lookup("E11", expand = TRUE)
  r_only <- cie_lookup("E11", expand = TRUE, only_uso_cl = TRUE)

  expect_true(nrow(r_only) <= nrow(r_all))
  expect_false("legado" %in% r_only$uso_cl)
})

test_that("cie_lookup(only_uso_cl = TRUE, include_uso_cl = FALSE) combina", {
  skip_on_cran()

  r <- cie_lookup(
    "E11",
    expand = TRUE,
    only_uso_cl = TRUE, include_uso_cl = FALSE
  )
  expect_false("uso_cl" %in% names(r))
  # Verificar via consulta paralela que filtro si ocurrio
  r_check <- cie_lookup("E11", expand = TRUE, only_uso_cl = TRUE)
  expect_equal(nrow(r), nrow(r_check))
})

test_that("cie_lookup retorna vacio limpio con only_uso_cl si nada matchea", {
  skip_on_cran()

  # Codigo legado conocido: A00 (categoria sin subcodigos vigentes en CL)
  r <- cie_lookup("A00", only_uso_cl = TRUE)
  # A00.0 y A00.1 son 'principal', A00 a secas es 'legado'.
  # only_uso_cl = TRUE sobre lookup exacto de "A00" debe retornar 0 filas.
  expect_equal(nrow(r), 0)
})

# --- cie_search -----------------------------------------------------------

test_that("cie_search default NO incluye uso_cl (compat v<=0.9.6)", {
  skip_on_cran()

  r <- cie_search("diabetes", max_results = 3, verbose = FALSE)
  expect_false("uso_cl" %in% names(r))
  expect_true(all(c("codigo", "descripcion", "score") %in% names(r)))
})

test_that("cie_search(include_uso_cl = TRUE) agrega la columna", {
  skip_on_cran()

  r <- cie_search(
    "diabetes",
    max_results = 3,
    include_uso_cl = TRUE, verbose = FALSE
  )
  expect_true("uso_cl" %in% names(r))
  expect_true(all(c("codigo", "descripcion", "score") %in% names(r)))
})

test_that("cie_search(only_uso_cl = TRUE) excluye codigos 'legado'", {
  skip_on_cran()

  r_all <- cie_search(
    "colera",
    max_results = 50,
    include_uso_cl = TRUE, verbose = FALSE
  )
  r_only <- cie_search(
    "colera",
    max_results = 50,
    only_uso_cl = TRUE, include_uso_cl = TRUE, verbose = FALSE
  )

  expect_true(nrow(r_only) <= nrow(r_all))
  expect_false("legado" %in% r_only$uso_cl)
})

test_that("cie_search(only_uso_cl, include_uso_cl=FALSE) filtra sin exponer col", {
  skip_on_cran()

  r <- cie_search(
    "colera",
    max_results = 50,
    only_uso_cl = TRUE, include_uso_cl = FALSE, verbose = FALSE
  )
  expect_false("uso_cl" %in% names(r))
})

test_that("cie_search en field='inclusion' tambien respeta los flags", {
  skip_on_cran()

  r <- cie_search(
    "bacteriana",
    max_results = 5, field = "inclusion",
    include_uso_cl = TRUE, verbose = FALSE
  )
  expect_true("uso_cl" %in% names(r))
  expect_true("inclusion" %in% names(r))
})
