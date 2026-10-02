# test-encoding.R
# Pruebas de compatibilidad con diferentes encodings y caracteres especiales

# ============================================================
# PRUEBAS DE ENCODING PARA CARACTERES ESPANOLES
# ============================================================

test_that("cie_search fuzzy tolera typo sin tilde ('diabetis')",
  {
  skip_on_cran()

  # Ambas versiones deben encontrar resultados
  resultado_sin <- cie_search("diabetes", threshold = 0.70)
  resultado_con <- cie_search("diabetis", threshold = 0.70)

  expect_gt(nrow(resultado_sin), 0)
  expect_gt(nrow(resultado_con), 0)
})

test_that("base de datos contiene descripciones con tildes correctas", {
  skip_on_cran()

  # Verificar que las descripciones con tildes estan correctas
  resultado <- cie10_sql("SELECT * FROM cie10 WHERE descripcion LIKE '%neumon%' LIMIT 5")
  expect_s3_class(resultado, "tbl_df")

  # Verificar que no hay caracteres corruptos tipicos de encoding incorrecto
  if (nrow(resultado) > 0) {
    # Buscar caracteres corruptos comunes
    descripciones <- resultado$descripcion
    tiene_corruptos <- any(stringr::str_detect(descripciones, "\ufffd|\u00c3\u00a1|\u00c3\u00a9|\u00c3\u00ad|\u00c3\u00b3|\u00c3\u00ba|\u00c3\u00b1"))
    expect_false(tiene_corruptos, info = "Las descripciones no deben tener caracteres corruptos")
  }
})

# ============================================================
# PRUEBAS DE CARACTERES ESPECIALES
# ============================================================

test_that("cie_search maneja parentesis y corchetes", {
  skip_on_cran()

  # Texto con parentesis
  expect_no_error({
    suppressMessages(cie_search("diabetes (tipo 2)", threshold = 0.5))
  })

  # Texto con corchetes
  expect_no_error({
    suppressMessages(cie_search("diabetes [mellitus]", threshold = 0.5))
  })
})

test_that("cie_search maneja signos de puntuacion", {
  skip_on_cran()

  # Coma
  expect_no_error({
    suppressMessages(cie_search("diabetes, tipo 2", threshold = 0.5))
  })

  # Punto y coma
  expect_no_error({
    suppressMessages(cie_search("diabetes; insulino", threshold = 0.5))
  })

  # Dos puntos
  expect_no_error({
    suppressMessages(cie_search("diabetes: tipo 2", threshold = 0.5))
  })
})

test_that("cie_search maneja guiones y barras", {
  skip_on_cran()

  # Guion
  resultado <- cie_search("insulino-dependiente", threshold = 0.50)
  expect_s3_class(resultado, "tbl_df")

  # Barra
  expect_no_error({
    suppressMessages(cie_search("diabetes mellitus/tipo", threshold = 0.5))
  })
})

# ============================================================
# PRUEBAS DE CONSISTENCIA ENTRE PLATAFORMAS
# ============================================================

test_that("cie_validate_vector es case-insensitive", {
  # Debe validar independientemente del case
  expect_true(cie_validate_vector("E11.0"))
  expect_true(cie_validate_vector("e11.0"))
  expect_true(cie_validate_vector("i10"))
})

test_that("cie_lookup es case-insensitive", {
  skip_on_cran()

  resultado_may <- cie_lookup("E11.0")
  resultado_min <- cie_lookup("e11.0")
  resultado_i10 <- cie_lookup("i10")

  expect_equal(resultado_may$codigo, resultado_min$codigo)
  expect_equal(resultado_i10$codigo, "I10")
})

# ============================================================
# PRUEBAS DE LOCALE
# ============================================================

test_that("funciones operan con locale C (collate distinto)", {
  skip_on_cran()

  # Forzar locale C scoped (withr restaura automaticamente); cie_lookup
  # y cie_search no deben depender del collation del sistema
  withr::local_locale(c(LC_COLLATE = "C"))

  resultado <- cie_lookup("E11.0")
  expect_equal(nrow(resultado), 1)

  resultado2 <- cie_search("diabetes", threshold = 0.70)
  expect_gt(nrow(resultado2), 0)
})

# ============================================================
# PRUEBAS DE COMPARACION STRING
# ============================================================

test_that("cie_search usa comparacion case-insensitive", {
  skip_on_cran()

  resultado_lower <- cie_search("diabetes mellitus", threshold = 0.70)
  resultado_upper <- cie_search("DIABETES MELLITUS", threshold = 0.70)
  resultado_mixed <- cie_search("Diabetes Mellitus", threshold = 0.70)

  # Todos deben encontrar resultados similares
  expect_gt(nrow(resultado_lower), 0)
  expect_gt(nrow(resultado_upper), 0)
  expect_gt(nrow(resultado_mixed), 0)

  # Los codigos encontrados deben ser similares
  expect_true(any(resultado_lower$codigo %in% resultado_upper$codigo))
})

test_that("similitud Jaro-Winkler funciona con tildes", {
  skip_on_cran()

  # La similitud debe funcionar aunque haya diferencias de tildes
  resultado1 <- cie_search("neumonia", threshold = 0.60)
  resultado2 <- cie_search("neumon\u00eda", threshold = 0.60)

  # Ambos deben encontrar resultados (aunque no exactamente los mismos)
  expect_s3_class(resultado1, "tbl_df")
  expect_s3_class(resultado2, "tbl_df")
})
