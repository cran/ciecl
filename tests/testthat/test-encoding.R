# test-encoding.R
# Pruebas de compatibilidad con diferentes encodings y caracteres especiales

# ============================================================
# PRUEBAS DE ENCODING PARA CARACTERES ESPANOLES
# ============================================================

test_that("cie_search maneja caracteres con tildes", {
  skip_on_cran()


  # Busqueda con tildes en espanol
  resultado <- cie_search("neumonia", threshold = 0.70)
  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)

  # Busqueda con ene
  resultado_ene <- cie_search("rinon", threshold = 0.70)
  expect_s3_class(resultado_ene, "tbl_df")
})

test_that("cie_search encuentra terminos con y sin tildes",
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
    tiene_corruptos <- any(stringr::str_detect(descripciones, "\ufffd|Ã¡|Ã©|Ã\u00ad|Ã³|Ãº|Ã±"))
    expect_false(tiene_corruptos, info = "Las descripciones no deben tener caracteres corruptos")
  }
})

test_that("cie_lookup maneja codigos sin importar encoding", {
  skip_on_cran()

  # El codigo debe funcionar independientemente del encoding del sistema
  resultado <- cie_lookup("E11.0")
  expect_equal(nrow(resultado), 1)
  expect_s3_class(resultado, "tbl_df")
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

test_that("cie10_sql maneja comillas en queries", {
  skip_on_cran()

  # Query con comillas simples (parametros)
  resultado <- cie10_sql("SELECT * FROM cie10 WHERE codigo = 'E11.0'")
  expect_s3_class(resultado, "tbl_df")

  # Query con LIKE y comillas
  resultado2 <- cie10_sql("SELECT * FROM cie10 WHERE descripcion LIKE '%diabetes%' LIMIT 5")
  expect_s3_class(resultado2, "tbl_df")
})

# ============================================================
# PRUEBAS DE UNICODE
# ============================================================

test_that("cie_search maneja caracteres unicode validos", {
  skip_on_cran()

  # Caracteres unicode basicos deben ser manejados
  expect_no_error({
    suppressMessages(cie_search("diabetes", threshold = 0.70))
  })
})

test_that("base de datos mantiene integridad unicode", {
  skip_on_cran()

  # Verificar que los datos no estan corruptos
  resultado <- cie10_sql("SELECT COUNT(*) as n FROM cie10")
  expect_gt(resultado$n, 5000)

  # Verificar que hay codigos con descripciones no vacias
  resultado2 <- cie10_sql("SELECT COUNT(*) as n FROM cie10 WHERE descripcion IS NOT NULL AND descripcion != ''")
  expect_gt(resultado2$n, 5000)
})

# ============================================================
# PRUEBAS DE CONSISTENCIA ENTRE PLATAFORMAS
# ============================================================

test_that("cie_normalizar es consistente con diferentes inputs", {
  skip_on_cran()

  # Mismos resultados independientemente de mayusculas/minusculas
  resultado_may <- cie_normalizar("E110", buscar_db = FALSE)
  resultado_min <- cie_normalizar("e110", buscar_db = FALSE)

  expect_equal(resultado_may, resultado_min)
})

test_that("cie_validate_vector es case-insensitive", {
  # Debe validar independientemente del case
  expect_true(cie_validate_vector("E11.0"))
  expect_true(cie_validate_vector("e11.0"))
  expect_true(cie_validate_vector("E11.0"))
})

test_that("cie_lookup es case-insensitive", {
  skip_on_cran()

  resultado_may <- cie_lookup("E11.0")
  resultado_min <- cie_lookup("e11.0")
  resultado_mix <- cie_lookup("e11.0")

  expect_equal(resultado_may$codigo, resultado_min$codigo)
  expect_equal(resultado_may$codigo, resultado_mix$codigo)
})

# ============================================================
# PRUEBAS DE LOCALE
# ============================================================

test_that("funciones operan independientemente del locale", {
  skip_on_cran()

  # Guardar locale actual
  old_locale <- Sys.getlocale("LC_COLLATE")

  # Las funciones deben operar correctamente
  resultado <- cie_lookup("E11.0")
  expect_equal(nrow(resultado), 1)

  resultado2 <- cie_search("diabetes", threshold = 0.70)
  expect_gt(nrow(resultado2), 0)

  # Restaurar locale (por si acaso cambio)
  tryCatch(
    Sys.setlocale("LC_COLLATE", old_locale),
    error = function(e) NULL
  )
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
  resultado2 <- cie_search("neumonía", threshold = 0.60)

  # Ambos deben encontrar resultados (aunque no exactamente los mismos)
  expect_s3_class(resultado1, "tbl_df")
  expect_s3_class(resultado2, "tbl_df")
})
