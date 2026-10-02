test_that("SQLite DB inicializa correctamente", {
  skip_on_cran()

  con <- get_cie10_db()
  expect_s4_class(con, "SQLiteConnection")
  expect_true(DBI::dbExistsTable(con, "cie10"))
})

test_that("cie10_sql ejecuta queries SELECT", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT COUNT(*) AS n FROM cie10")
  expect_s3_class(resultado, "tbl_df")
  expect_gt(resultado$n, 5000)  # Minimo 5k codigos
})

test_that("cie10_sql bloquea queries peligrosas", {
  skip_on_cran()

  expect_error(
    cie10_sql("DROP TABLE cie10"),
    class = "ciecl_unsafe_query"
  )
})

test_that("cie10_sql emite deprecation warning para argumento close", {
  skip_on_cran()

  expect_warning(
    cie10_sql("SELECT COUNT(*) AS n FROM cie10", close = FALSE),
    class = "lifecycle_warning_deprecated"
  )
})

# ============================================================
# PRUEBAS ADICIONALES cie10_sql()
# ============================================================

test_that("cie10_sql ejecuta queries SQL con clausulas WHERE/LIKE/GROUP/ORDER", {
  skip_on_cran()

  # WHERE retorna codigo exacto con contenido correcto
  r_where <- cie10_sql("SELECT * FROM cie10 WHERE codigo = 'E11.0'")
  expect_equal(nrow(r_where), 1)
  expect_match(r_where$descripcion, "iabetes", ignore.case = TRUE)

  # LIKE filtra por prefijo
  r_like <- cie10_sql("SELECT * FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 10")
  expect_true(all(grepl("^E11", r_like$codigo)))

  # GROUP BY retorna capitulos
  r_group <- cie10_sql("SELECT capitulo, COUNT(*) as n FROM cie10 GROUP BY capitulo")
  expect_gt(nrow(r_group), 10)

  # ORDER BY ordena correctamente
  r_order <- cie10_sql("SELECT codigo FROM cie10 ORDER BY codigo LIMIT 5")
  expect_equal(r_order$codigo, sort(r_order$codigo))
})

test_that("cie10_sql bloquea ALTER TABLE", {
  skip_on_cran()

  # Bloquea por keyword peligroso o por no ser SELECT
  expect_error(
    cie10_sql("ALTER TABLE cie10 ADD COLUMN test TEXT")
  )
})

test_that("cie10_sql bloquea CREATE TABLE", {
  skip_on_cran()

  expect_error(
    cie10_sql("CREATE TABLE test (id INTEGER)")
  )
})

test_that("cie10_sql bloquea TRUNCATE", {
  skip_on_cran()

  expect_error(
    cie10_sql("TRUNCATE TABLE cie10")
  )
})

test_that("cie10_sql bloquea ATTACH DATABASE", {
  skip_on_cran()

  expect_error(
    cie10_sql("ATTACH DATABASE 'test.db' AS test")
  )
})

test_that("cie10_sql bloquea load_extension", {
  skip_on_cran()

  expect_error(
    cie10_sql("SELECT load_extension('evil.dll')"),
    class = "ciecl_unsafe_query"
  )
})

test_that("cie10_sql bloquea PRAGMA", {
  skip_on_cran()

  expect_error(
    cie10_sql("PRAGMA table_info(cie10)")
  )
})

test_that("cie10_sql bloquea keywords peligrosos case-insensitive", {
  skip_on_cran()

  expect_error(cie10_sql("SELECT * FROM cie10; dRoP TABLE cie10"))
  expect_error(cie10_sql("SELECT * FROM cie10; Pragma table_info(cie10)"))
  expect_error(cie10_sql("SELECT * FROM cie10; aTTaCH DATABASE 'x' AS y"))
})

test_that("cie10_sql permite DISTINCT", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT DISTINCT capitulo FROM cie10")
  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)
})

test_that("cie10_sql permite subqueries y UNION", {
  skip_on_cran()

  # Subquery en WHERE con IN (SELECT ...)
  r_sub <- cie10_sql("SELECT * FROM cie10 WHERE codigo IN (SELECT codigo FROM cie10 WHERE codigo = 'E11.0')")
  expect_equal(nrow(r_sub), 1)

  # UNION de dos SELECT validos
  r_union <- cie10_sql(paste(
    "SELECT codigo, descripcion FROM cie10 WHERE codigo = 'E11.0'",
    "UNION SELECT codigo, descripcion FROM cie10 WHERE codigo = 'I10'"
  ))
  expect_gte(nrow(r_union), 1)
})

test_that("cie10_sql no rechaza keywords del blocklist dentro de literales string", {
  skip_on_cran()

  # Regresion F4: el blocklist escanea el SQL ya limpio de literales y
  # comentarios; un LIKE legitimo con texto bloqueable no debe gatillar
  # la proteccion
  res <- cie10_sql(
    "SELECT codigo, descripcion FROM cie10 WHERE descripcion LIKE '%drop%'"
  )
  expect_s3_class(res, "tbl_df")

  # La proteccion se mantiene: keyword fuera del literal aborta
  expect_error(
    cie10_sql("SELECT codigo FROM cie10 WHERE codigo = 'E11.0'; DROP TABLE cie10"),
    class = "ciecl_unsafe_query"
  )
})

test_that("cie10_sql permite COUNT con condicion", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT COUNT(*) as n FROM cie10 WHERE codigo LIKE 'E%'")
  expect_s3_class(resultado, "tbl_df")
  expect_gt(resultado$n, 100)
})

test_that("cie10_sql maneja query con saltos de linea", {
  skip_on_cran()

  query <- "
    SELECT
      codigo,
      descripcion
    FROM cie10
    WHERE codigo = 'E11.0'
  "
  resultado <- cie10_sql(query)
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
})

# ============================================================
# PRUEBAS get_cie10_db()
# ============================================================

test_that("get_cie10_db retorna conexion DBI valida", {
  skip_on_cran()

  con <- get_cie10_db()
  expect_true(DBI::dbIsValid(con))
  expect_s4_class(con, "SQLiteConnection")
})

test_that("get_cie10_db crea tabla cie10 si no existe", {
  skip_on_cran()

  con <- get_cie10_db()
  expect_true(DBI::dbExistsTable(con, "cie10"))
})

test_that("get_cie10_db tabla tiene indices", {
  skip_on_cran()

  con <- get_cie10_db()
  indices <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='index'")
  expect_gt(nrow(indices), 0)
})

test_that("get_cie10_db usa directorio cache correcto", {
  skip_on_cran()

  cache_dir <- get_cache_dir()
  db_path <- file.path(cache_dir, "cie10.db")

  con <- get_cie10_db()
  expect_true(file.exists(db_path))
})

test_that("get_cie10_db tabla tiene columnas esperadas", {
  skip_on_cran()

  con <- get_cie10_db()
  columnas <- DBI::dbListFields(con, "cie10")
  expect_true("codigo" %in% columnas)
  expect_true("descripcion" %in% columnas)
})

# ============================================================
# PRUEBAS cie10_clear_cache()
# ============================================================

test_that("cie10_clear_cache elimina archivo db", {
  skip_on_cran()

  cache_dir <- get_cache_dir()
  db_path <- file.path(cache_dir, "cie10.db")

  # Asegurar que existe
  get_cie10_db()
  expect_true(file.exists(db_path))

  # Limpiar cache
  suppressMessages(cie10_clear_cache())
  expect_false(file.exists(db_path))
})

test_that("cie10_clear_cache es idempotente", {
  skip_on_cran()

  expect_no_error({
    suppressMessages(cie10_clear_cache())
    suppressMessages(cie10_clear_cache())
  })
})

test_that("cie10_clear_cache emite mensaje apropiado", {
  skip_on_cran()

  # Asegurar que existe cache
  get_cie10_db()

  expect_message(cie10_clear_cache(), "eliminado")
  expect_message(cie10_clear_cache(), "no existe")
})

test_that("cie10_clear_cache retorna invisible NULL", {
  skip_on_cran()

  res <- withVisible(suppressMessages(cie10_clear_cache()))
  expect_null(res$value)
  expect_false(res$visible)
})

# ============================================================
# PRUEBAS ADICIONALES COBERTURA - Semicolon dentro de strings
# ============================================================

test_that("cie10_sql permite semicolon dentro de strings", {
  skip_on_cran()

  # Semicolon dentro de comillas simples no debe bloquearse
  resultado <- cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE descripcion LIKE '%tipo;%' LIMIT 1")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie10_sql bloquea semicolon fuera de strings", {

  skip_on_cran()

  # Multiples sentencias separadas por semicolon (sin keywords peligrosos)
  expect_error(
    cie10_sql("SELECT * FROM cie10; SELECT * FROM cie10"),
    "sentencias SQL no permitidas"
  )
})

test_that("cie10_sql maneja comentarios SQL sin falsos positivos", {
  skip_on_cran()

  # Comentario de linea no genera falso positivo
  resultado <- cie10_sql("SELECT codigo FROM cie10 WHERE codigo = 'E11.0' -- comentario")
  expect_equal(nrow(resultado), 1)

  # Comentario de bloque no genera falso positivo
  resultado2 <- cie10_sql("SELECT codigo /* columna */ FROM cie10 WHERE codigo = 'E11.0'")
  expect_equal(nrow(resultado2), 1)

  # Semicolon dentro de comentario no bloquea
  resultado3 <- cie10_sql("SELECT codigo FROM cie10 WHERE codigo = 'E11.0' -- ;test")
  expect_equal(nrow(resultado3), 1)
})

test_that("cie10_sql bloquea INSERT", {
  skip_on_cran()

  expect_error(
    cie10_sql("INSERT INTO cie10 VALUES ('X99', 'Test')"),
    class = "ciecl_unsafe_query"
  )
})

test_that("cie10_sql bloquea UPDATE", {
  skip_on_cran()

  expect_error(
    cie10_sql("UPDATE cie10 SET descripcion = 'test' WHERE codigo = 'E11.0'"),
    class = "ciecl_unsafe_query"
  )
})

test_that("cie10_sql bloquea DELETE", {
  skip_on_cran()

  expect_error(
    cie10_sql("DELETE FROM cie10 WHERE codigo = 'E11.0'"),
    class = "ciecl_unsafe_query"
  )
})

test_that("cie10_sql bloquea DETACH", {
  skip_on_cran()

  expect_error(
    cie10_sql("DETACH DATABASE main"),
    class = "ciecl_unsafe_query"
  )
})

test_that("cie10_sql bloquea EXEC", {
  skip_on_cran()

  expect_error(
    cie10_sql("EXEC sp_help"),
    class = "ciecl_unsafe_query"
  )
})

# ============================================================
# PRUEBAS CONNECTION POOLING
# ============================================================

test_that("get_cie10_db reutiliza conexion (pooling)", {
  skip_on_cran()

  con1 <- get_cie10_db()
  con2 <- get_cie10_db()

  # Misma referencia de objeto
  expect_identical(con1, con2)
})

test_that("cie10_clear_cache invalida pool", {
  skip_on_cran()

  env <- getFromNamespace(".ciecl_env", "ciecl")

  con1 <- get_cie10_db()
  suppressMessages(cie10_clear_cache())

  # Pool debe estar vacio
  expect_null(env$con)
  expect_null(env$db_path)

  # Nueva conexion debe ser diferente
  con2 <- get_cie10_db()
  expect_true(DBI::dbIsValid(con2))
})

test_that("cie10_disconnect cierra conexion pooled", {
  skip_on_cran()

  env <- getFromNamespace(".ciecl_env", "ciecl")

  # Asegurar conexion activa
  get_cie10_db()
  expect_false(is.null(env$con))

  # Desconectar
  cie10_disconnect()

  expect_null(env$con)
  expect_null(env$db_path)
})

test_that("cie10_disconnect es idempotente", {
  skip_on_cran()

  expect_no_error({
    cie10_disconnect()
    cie10_disconnect()
  })
})

# ============================================================
# PRUEBAS .ciecl_env y .onUnload (zzz.R)
# ============================================================

test_that(".ciecl_env tiene estructura correcta", {
  env <- .ciecl_env
  expect_type(env, "environment")
  expect_true("con" %in% ls(env, all.names = TRUE))
  expect_true("db_path" %in% ls(env, all.names = TRUE))
})

test_that(".onUnload cierra conexion y limpia env", {
  skip_on_cran()
  get_cie10_db()
  env <- .ciecl_env
  expect_false(is.null(env$con))

  onUnload <- getFromNamespace(".onUnload", "ciecl")
  onUnload(libpath = .libPaths()[1])

  expect_null(env$con)
  expect_null(env$db_path)
})

test_that(".onUnload no falla sin conexion activa", {
  cie10_disconnect()
  onUnload <- getFromNamespace(".onUnload", "ciecl")
  expect_no_error(onUnload(libpath = .libPaths()[1]))
})

# ============================================================
# PRUEBAS BRANCHES ADICIONALES cie-sql.R
# ============================================================

test_that("get_cie10_db limpia conexion invalida", {
  skip_on_cran()
  on.exit(ciecl::cie10_disconnect(), add = TRUE)
  con <- get_cie10_db()
  DBI::dbDisconnect(con)
  # .ciecl_env aun tiene referencia invalida
  con2 <- get_cie10_db()
  expect_true(DBI::dbIsValid(con2))
  ciecl::cie10_disconnect()
})

test_that("cie10_sql maneja error de ejecucion SQL", {
  skip_on_cran()
  expect_error(ciecl::cie10_sql("SELECT * FROM tabla_inexistente"))
})

test_that("cie10_sql relanza errores SQL con clase ciecl_sql_error", {
  skip_on_cran()
  err <- tryCatch(
    ciecl::cie10_sql("SELECT * FROM tabla_inexistente"),
    error = function(e) e
  )
  expect_s3_class(err, "ciecl_sql_error")
})

test_that("cie10_sql valida tipos de query invalidos", {
  expect_error(ciecl::cie10_sql(123), class = "ciecl_invalid_input")
  expect_error(ciecl::cie10_sql(c("SELECT 1", "SELECT 2")),
               class = "ciecl_invalid_input")
  expect_error(ciecl::cie10_sql(NA_character_), class = "ciecl_invalid_input")
  expect_error(ciecl::cie10_sql(), class = "ciecl_invalid_input")
})

test_that("cie10_sql acepta SELECT que comienza con comentario (F13)", {
  res <- cie10_sql("-- conteo\nSELECT COUNT(*) AS n FROM cie10")
  expect_equal(nrow(res), 1)
  expect_true(res$n > 0)

  res_bloque <- cie10_sql("/* conteo */\nSELECT COUNT(*) AS n FROM cie10")
  expect_equal(nrow(res_bloque), 1)

  # Query que es solo comentario sigue abortando
  expect_error(cie10_sql("-- solo comentario"), class = "ciecl_unsafe_query")
})
