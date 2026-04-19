# Tests de rendimiento a gran escala
# Activar con: Sys.setenv(CIECL_LARGE_TESTS = "true")

skip_on_cran()
skip_if_not(
  identical(Sys.getenv("CIECL_LARGE_TESTS"), "true"),
  message = "Tests de escala desactivados (CIECL_LARGE_TESTS != 'true')"
)

# --- Setup comun ---

data("cie10_cl", package = "ciecl", envir = environment())
todos_codigos <- cie10_cl$codigo
set.seed(42)

top_500 <- sample(todos_codigos, min(500, length(todos_codigos)))
resto <- setdiff(todos_codigos, top_500)

generar_codigos <- function(n, prop_top = 0.8) {
  n_top <- round(n * prop_top)
  n_resto <- n - n_top
  c(
    sample(top_500, n_top, replace = TRUE),
    sample(resto, n_resto, replace = TRUE)
  )
}

# --- Tests ---

test_that("Patron GITTESIS 1M registros: lookup + join < 10s", {
  codigos_sim <- generar_codigos(1e6)
  df_sim <- dplyr::tibble(id = seq_len(1e6), codigo = codigos_sim)

  t <- system.time({
    unicos <- unique(codigos_sim)
    resultado <- cie_lookup(unicos)
    df_final <- dplyr::left_join(df_sim, resultado, by = "codigo")
  })

  expect_true(t["elapsed"] < 10,
    label = sprintf("GITTESIS pattern: %.2fs (limite 10s)", t["elapsed"]))
  expect_equal(nrow(df_final), 1e6)
  expect_true("descripcion" %in% names(df_final))
})

test_that("cie_validate_vector 1M codigos < 10s", {
  codigos_sim <- generar_codigos(1e6)

  t <- system.time({
    resultado <- cie_validate_vector(codigos_sim)
  })

  expect_true(t["elapsed"] < 10,
    label = sprintf("validate_vector 1M: %.2fs (limite 10s)", t["elapsed"]))
  expect_length(resultado, 1e6)
  expect_type(resultado, "logical")
})

test_that("cie_normalizar 500K codigos < 5s", {
  codigos_sim <- generar_codigos(5e5)
  # Agregar variaciones realistas
  codigos_sim[1:500] <- gsub("\\.", "", codigos_sim[1:500])
  codigos_sim[501:1000] <- paste0(" ", codigos_sim[501:1000], " ")

  t <- system.time({
    resultado <- cie_normalizar(codigos_sim, buscar_db = TRUE)
  })

  expect_true(t["elapsed"] < 5,
    label = sprintf("normalizar 500K: %.2fs (limite 5s)", t["elapsed"]))
  expect_length(resultado, 5e5)
  # Verificar que normalizo correctamente
  expect_false(any(grepl("^\\s|\\s$", resultado, perl = TRUE), na.rm = TRUE))
})

test_that("cie_map_comorbid 100K codigos vectorizado", {
  codigos_sim <- generar_codigos(1e5)

  t <- system.time({
    resultado <- cie_map_comorbid(codigos_sim)
  })

  expect_true(t["elapsed"] < 5,
    label = sprintf("map_comorbid 100K: %.2fs (limite 5s)", t["elapsed"]))
  expect_equal(nrow(resultado), 1e5)
  expect_true(all(c("codigo", "categoria") %in% names(resultado)))
  # Verificar que hay categorias diversas
  expect_true(length(unique(resultado$categoria)) > 1)
})

test_that("cie_comorbid 10K pacientes < 60s", {
  skip_if_not_installed("comorbidity")

  n_pac <- 1e4
  n_diag <- n_pac * 5
  df_sim <- dplyr::tibble(
    id_pac = rep(seq_len(n_pac), each = 5),
    diag = generar_codigos(n_diag)
  )

  t <- system.time({
    resultado <- cie_comorbid(df_sim, id = "id_pac", code = "diag", map = "charlson")
  })

  expect_true(t["elapsed"] < 60,
    label = sprintf("comorbid 10K pac: %.2fs (limite 60s)", t["elapsed"]))
  expect_equal(nrow(resultado), n_pac)
  expect_true("score_charlson" %in% names(resultado))
})
