# test-robustness.R
# Pruebas de robustez, concurrencia y estabilidad del paquete

# ============================================================
# PRUEBAS DE ROBUSTEZ DE CONEXION SQLite
# ============================================================

test_that("multiples conexiones consecutivas funcionan", {
  skip_on_cran()

  # Abrir y cerrar multiples conexiones seguidas
  for (i in 1:10) {
    resultado <- cie_lookup("E11.0")
    expect_shape(resultado, nrow = 1)
  }
})

test_that("multiples queries consecutivas funcionan", {
  skip_on_cran()

  # Ejecutar multiples queries seguidas
  queries <- c(
    "SELECT COUNT(*) as n FROM cie10",
    "SELECT * FROM cie10 WHERE codigo = 'E11.0'",
    "SELECT * FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 5",
    "SELECT capitulo, COUNT(*) as n FROM cie10 GROUP BY capitulo"
  )

  for (query in queries) {
    resultado <- cie10_sql(query)
    expect_s3_class(resultado, "tbl_df")
  }
})

test_that("funciones diferentes pueden ejecutarse intercaladas", {
  skip_on_cran()

  # Intercalar diferentes funciones
  resultado1 <- cie_lookup("E11.0")
  resultado2 <- cie_search("diabetes", threshold = 0.70, max_results = 5)
  resultado3 <- cie10_sql("SELECT COUNT(*) as n FROM cie10")
  resultado4 <- cie_lookup("Z00")
  resultado5 <- cie_expand("E11")

  expect_equal(nrow(resultado1), 1)
  expect_gt(nrow(resultado2), 0)
  expect_gt(resultado3$n, 5000)
  expect_s3_class(resultado4, "tbl_df")
  expect_true(length(resultado5) > 0)
})

# ============================================================
# PRUEBAS DE MEMORIA Y RECURSOS
# ============================================================

test_that("queries grandes no causan problemas de memoria", {
  skip_on_cran()

  # Query que retorna muchos resultados
  resultado <- cie10_sql("SELECT * FROM cie10 LIMIT 5000")
  expect_s3_class(resultado, "tbl_df")
  expect_lte(nrow(resultado), 5000)

  rm(resultado)
})

test_that("cie_search con muchos resultados funciona", {
  skip_on_cran()

  # Busqueda con threshold bajo que retorne muchos resultados
  # Nota: texto debe tener al menos 3 caracteres
  resultado <- cie_search("diabetes", threshold = 0.01, max_results = 1000)
  expect_s3_class(resultado, "tbl_df")
  expect_lte(nrow(resultado), 1000)
})

test_that("cie_lookup con vector grande funciona", {
  skip_on_cran()

  # Vector de codigos grande
  codigos <- c(
    paste0("E11.", 0:9),
    paste0("E10.", 0:9),
    paste0("I10.", 0:9),
    paste0("Z00.", 0:9)
  )

  suppressMessages({
    resultado <- cie_lookup(codigos)
  })

  expect_s3_class(resultado, "tbl_df")
})

# ============================================================
# PRUEBAS DE CACHE Y ESTADO
# ============================================================

test_that("cie10_clear_cache funciona sin base de datos", {
  skip_on_cran()

  # No debe dar error si la cache no existe
  # (puede que ya exista, pero el mensaje debe ser apropiado)
  expect_no_error({
    suppressMessages(cie10_clear_cache())
  })
})

test_that("base de datos se reconstruye despues de clear_cache", {
  skip_on_cran()

  # Limpiar cache
  suppressMessages(cie10_clear_cache())

  # La siguiente query debe inicializar la base (mensajes solo en interactive)
  resultado <- cie_lookup("E11.0")

  expect_shape(resultado, nrow = 1)
})

test_that("base de datos persiste entre llamadas", {
  skip_on_cran()

  # Primera llamada
  resultado1 <- cie_lookup("E11.0")

  # Segunda llamada no debe mostrar mensaje de inicializacion
  expect_silent({
    resultado2 <- cie_lookup("E11.0")
  })

  expect_equal(resultado1$codigo, resultado2$codigo)
})

# ============================================================
# PRUEBAS DE INTEGRIDAD DE DATOS
# ============================================================

test_that("todos los codigos tienen descripcion", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT COUNT(*) as n FROM cie10 WHERE descripcion IS NULL OR descripcion = ''")
  expect_equal(resultado$n, 0, info = "Todos los codigos deben tener descripcion")
})

test_that("todos los codigos son unicos", {
  skip_on_cran()

  resultado_total <- cie10_sql("SELECT COUNT(*) as n FROM cie10")
  resultado_unique <- cie10_sql("SELECT COUNT(DISTINCT codigo) as n FROM cie10")

  expect_equal(resultado_total$n, resultado_unique$n)
})

test_that("codigos principales siguen formato CIE-10 valido", {
  skip_on_cran()

  # Obtener codigos principales (excluyendo morfologia M80xx/x que tienen formato especial)
  codigos <- cie10_sql("SELECT codigo FROM cie10 WHERE codigo NOT LIKE 'M80%/%'")$codigo

  # Validar formato
  validos <- cie_validate_vector(codigos)

  # La mayoria deben ser validos (permitir algunos especiales)
  porcentaje_validos <- sum(validos) / length(validos)
  expect_gt(porcentaje_validos, 0.95)
})

test_that("capitulos estan correctamente asignados", {
  skip_on_cran()

  # Verificar que los capitulos existen y son consistentes
  resultado <- cie10_sql("SELECT DISTINCT capitulo FROM cie10 WHERE capitulo IS NOT NULL")

  expect_gt(nrow(resultado), 0)
})

# ============================================================
# PRUEBAS DE RECUPERACION DE ERRORES
# ============================================================

test_that("error en query no corrompe estado", {
  skip_on_cran()

  # Query invalida debe dar error
  expect_error(cie10_sql("DROP TABLE cie10"))

  # Pero la siguiente query valida debe funcionar
  resultado <- cie10_sql("SELECT COUNT(*) as n FROM cie10")
  expect_gt(resultado$n, 5000)
})

test_that("codigo invalido no afecta siguientes busquedas", {
  skip_on_cran()

  # Buscar codigo invalido
  suppressMessages({
    resultado_invalido <- cie_lookup("XXXXXXXXX")
  })
  expect_length(resultado_invalido$codigo, 0)

  # Buscar codigo valido despues
  resultado_valido <- cie_lookup("E11.0")
  expect_equal(nrow(resultado_valido), 1)
})

test_that("texto invalido en search no afecta siguientes busquedas", {
  skip_on_cran()

  # Busqueda que no encuentra nada
  suppressMessages({
    resultado_vacio <- cie_search("xyzxyzxyzxyz", threshold = 0.99)
  })

  # Busqueda normal debe funcionar
  resultado_normal <- cie_search("diabetes", threshold = 0.70)
  expect_gt(nrow(resultado_normal), 0)
})

# ============================================================
# PRUEBAS DE TIPOS DE RETORNO
# ============================================================

test_that("cie_lookup siempre retorna tibble", {
  skip_on_cran()

  # Con resultado
  resultado1 <- cie_lookup("E11.0")
  expect_s3_class(resultado1, "tbl_df")

  # Sin resultado
  suppressMessages({
    resultado2 <- cie_lookup("XXXXX")
  })
  expect_s3_class(resultado2, "tbl_df")

  # Con vector
  resultado3 <- cie_lookup(c("E11.0", "Z00"))
  expect_s3_class(resultado3, "tbl_df")
})

test_that("cie_search siempre retorna tibble", {
  skip_on_cran()

  # Con resultados
  resultado1 <- cie_search("diabetes", threshold = 0.70)
  expect_s3_class(resultado1, "tbl_df")

  # Sin resultados
  suppressMessages({
    resultado2 <- cie_search("xyzxyzxyz", threshold = 0.99)
  })
  expect_s3_class(resultado2, "tbl_df")
})

test_that("cie10_sql siempre retorna tibble", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT * FROM cie10 LIMIT 1")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_map_comorbid siempre retorna tibble", {
  resultado <- cie_map_comorbid(c("E11.0", "I50.9"))
  expect_s3_class(resultado, "tbl_df")

  resultado_vacio <- cie_map_comorbid(character(0))
  expect_s3_class(resultado_vacio, "tbl_df")
})

# ============================================================
# PRUEBAS DE COLUMNAS ESPERADAS
# ============================================================

test_that("cie_lookup retorna columnas esperadas", {
  skip_on_cran()

  resultado <- cie_lookup("E11.0")

  columnas_esperadas <- c("codigo", "descripcion", "categoria")
  expect_true(all(columnas_esperadas %in% names(resultado)))
})

test_that("cie_search retorna columnas esperadas", {
  skip_on_cran()

  resultado <- cie_search("diabetes", threshold = 0.70)

  columnas_esperadas <- c("codigo", "descripcion", "score")
  expect_true(all(columnas_esperadas %in% names(resultado)))
})

test_that("cie_map_comorbid retorna columnas esperadas", {
  resultado <- cie_map_comorbid(c("E11.0"))

  columnas_esperadas <- c("codigo", "categoria")
  expect_named(resultado, columnas_esperadas)
})

# ============================================================
# PRUEBAS DE LIMITES Y CASOS EXTREMOS
# ============================================================

test_that("cie_search con threshold 1 retorna solo exactos", {
  skip_on_cran()

  # Threshold = 1 significa match exacto
  suppressMessages({
    resultado <- cie_search("diabetes mellitus", threshold = 1.0)
  })
  expect_s3_class(resultado, "tbl_df")

  # Si hay resultados, todos deben tener score = 1
  if (nrow(resultado) > 0) {
    expect_true(all(resultado$score == 1))
  }
})

test_that("cie_lookup con expansion de categoria completa", {
  skip_on_cran()

  # Expandir categoria E (muy grande)
  hijos <- cie_lookup("E", expand = TRUE)
  expect_s3_class(hijos, "tbl_df")
  expect_gt(nrow(hijos), 100)
})

test_that("cie_validate_vector con vector muy grande", {
  # Vector grande de codigos mixtos
  codigos <- c(
    rep("E11.0", 100),
    rep("INVALIDO", 100),
    rep("Z00", 100)
  )

  resultado <- cie_validate_vector(codigos)
  expect_length(resultado, 300)
  expect_equal(sum(resultado), 200) # 100 E11.0 + 100 Z00
})

# ============================================================
# PRUEBAS cie10_sql() ADICIONALES
# ============================================================

test_that("cie10_sql bloquea DROP TABLE", {
  testthat::local_reproducible_output()
  skip_on_cran()

  expect_snapshot(cie10_sql("DROP TABLE cie10"), error = TRUE)
})

test_that("cie10_sql bloquea DELETE", {
  testthat::local_reproducible_output()
  skip_on_cran()

  expect_snapshot(
    cie10_sql("DELETE FROM cie10 WHERE codigo = 'E11.0'"),
    error = TRUE
  )
})

test_that("cie10_sql bloquea UPDATE", {
  testthat::local_reproducible_output()
  skip_on_cran()

  expect_snapshot(
    cie10_sql("UPDATE cie10 SET descripcion = 'test' WHERE codigo = 'E11.0'"),
    error = TRUE
  )
})

test_that("cie10_sql bloquea INSERT", {
  testthat::local_reproducible_output()
  skip_on_cran()

  expect_snapshot(
    cie10_sql("INSERT INTO cie10 VALUES ('X99', 'test', NULL, NULL, NULL, NULL, NULL, NULL, 0, 0)"),
    error = TRUE
  )
})

test_that("cie10_sql bloquea multiples statements", {
  skip_on_cran()

  # El error puede ser por palabra clave no permitida (DROP) o por multiples sentencias
  expect_error(
    cie10_sql("SELECT * FROM cie10; DROP TABLE cie10")
  )
})

test_that("cie10_sql permite SELECT con subconsulta", {
  skip_on_cran()

  # Subconsulta valida
  resultado <- cie10_sql("SELECT * FROM cie10 WHERE codigo IN (SELECT codigo FROM cie10 LIMIT 5)")

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie10_sql permite punto y coma dentro de strings", {
  skip_on_cran()

  # Punto y coma dentro de string no debe ser bloqueado
  resultado <- cie10_sql("SELECT * FROM cie10 WHERE descripcion LIKE '%test; test%' LIMIT 1")

  expect_s3_class(resultado, "tbl_df")
})

# El warning de deprecacion se emite por la presencia del argumento
# close (cualquier valor), ver R/cie-sql.R:307-313.
test_that("cie10_sql emite advertencia de deprecacion al pasar close", {
  skip_on_cran()

  expect_warning(
    cie10_sql("SELECT COUNT(*) as n FROM cie10", close = TRUE),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("cie10_sql normaliza espacios al inicio", {
  skip_on_cran()

  # Query con espacios y saltos de linea al inicio
  resultado <- cie10_sql("
    SELECT COUNT(*) as n FROM cie10
  ")

  expect_s3_class(resultado, "tbl_df")
  expect_gt(resultado$n, 0)
})
