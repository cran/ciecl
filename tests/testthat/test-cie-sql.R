test_that("SQLite DB inicializa correctamente", {
  skip_on_cran()
  
  con <- get_cie10_db()
  expect_s4_class(con, "SQLiteConnection")
  expect_true(DBI::dbExistsTable(con, "cie10"))
  DBI::dbDisconnect(con)
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
    "Solo queries SELECT"
  )
})

# ==============================================================================
# PRUEBAS ADICIONALES cie10_sql()
# ==============================================================================

test_that("cie10_sql ejecuta queries con WHERE", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT * FROM cie10 WHERE codigo = 'E11.0'")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
})

test_that("cie10_sql ejecuta queries con LIKE", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT * FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 10")
  expect_s3_class(resultado, "tbl_df")
  expect_lte(nrow(resultado), 10)
})

test_that("cie10_sql ejecuta queries con GROUP BY", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT capitulo, COUNT(*) as n FROM cie10 GROUP BY capitulo")
  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 10)
})

test_that("cie10_sql ejecuta queries con ORDER BY", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT codigo, descripcion FROM cie10 ORDER BY codigo LIMIT 5")
  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 5)
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

test_that("cie10_sql bloquea PRAGMA", {
  skip_on_cran()

  expect_error(
    cie10_sql("PRAGMA table_info(cie10)")
  )
})

test_that("cie10_sql permite DISTINCT", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT DISTINCT capitulo FROM cie10")
  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)
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

# ==============================================================================
# PRUEBAS get_cie10_db()
# ==============================================================================

test_that("get_cie10_db retorna conexion DBI valida", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  on.exit(DBI::dbDisconnect(con))

  expect_true(DBI::dbIsValid(con))
  expect_s4_class(con, "SQLiteConnection")
})

test_that("get_cie10_db crea tabla cie10 si no existe", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  on.exit(DBI::dbDisconnect(con))

  expect_true(DBI::dbExistsTable(con, "cie10"))
})

test_that("get_cie10_db tabla tiene indices", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  on.exit(DBI::dbDisconnect(con))

  indices <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='index'")
  expect_gt(nrow(indices), 0)
})

test_that("get_cie10_db usa directorio cache correcto", {
  skip_on_cran()

  cache_dir <- tools::R_user_dir("ciecl", "data")
  db_path <- file.path(cache_dir, "cie10.db")

  con <- ciecl:::get_cie10_db()
  DBI::dbDisconnect(con)

  expect_true(file.exists(db_path))
})

test_that("get_cie10_db tabla tiene columnas esperadas", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  on.exit(DBI::dbDisconnect(con))

  columnas <- DBI::dbListFields(con, "cie10")
  expect_true("codigo" %in% columnas)
  expect_true("descripcion" %in% columnas)
})

# ==============================================================================
# PRUEBAS cie10_clear_cache()
# ==============================================================================

test_that("cie10_clear_cache elimina archivo db", {
  skip_on_cran()

  cache_dir <- tools::R_user_dir("ciecl", "data")
  db_path <- file.path(cache_dir, "cie10.db")

  # Asegurar que existe
  con <- ciecl:::get_cie10_db()
  DBI::dbDisconnect(con)
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
  con <- ciecl:::get_cie10_db()
  DBI::dbDisconnect(con)

  expect_message(cie10_clear_cache(), "eliminado")
  expect_message(cie10_clear_cache(), "no existe")
})

test_that("cie10_clear_cache retorna invisible NULL", {
  skip_on_cran()

  resultado <- suppressMessages(cie10_clear_cache())
  expect_null(resultado)
})

# ==============================================================================
# PRUEBAS ADICIONALES COBERTURA - Semicolon dentro de strings
# ==============================================================================

test_that("cie10_sql permite semicolon dentro de strings", {
  skip_on_cran()

  # Semicolon dentro de comillas simples no debe bloquearse
  resultado <- cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE descripcion LIKE '%tipo;%' LIMIT 1")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie10_sql bloquea semicolon fuera de strings", {

  skip_on_cran()

  # Multiples statements separados por semicolon (sin keywords peligrosos)
  expect_error(
    cie10_sql("SELECT * FROM cie10; SELECT * FROM cie10"),
    "Multiples statements"
  )
})

test_that("cie10_sql bloquea INSERT", {
  skip_on_cran()

  expect_error(
    cie10_sql("INSERT INTO cie10 VALUES ('X99', 'Test')"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql bloquea UPDATE", {
  skip_on_cran()

  expect_error(
    cie10_sql("UPDATE cie10 SET descripcion = 'test' WHERE codigo = 'E11.0'"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql bloquea DELETE", {
  skip_on_cran()

  expect_error(
    cie10_sql("DELETE FROM cie10 WHERE codigo = 'E11.0'"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql bloquea DETACH", {
  skip_on_cran()

  expect_error(
    cie10_sql("DETACH DATABASE main"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql bloquea EXEC", {
  skip_on_cran()

  expect_error(
    cie10_sql("EXEC sp_help"),
    "Solo queries SELECT"
  )
})

# ==============================================================================
# PRUEBAS get_cie10_db() creacion directorio cache
# ==============================================================================

test_that("get_cie10_db crea directorio cache si no existe", {
  skip_on_cran()

  # Limpiar cache para forzar recreacion
  suppressMessages(cie10_clear_cache())

  cache_dir <- tools::R_user_dir("ciecl", "data")

  # Conectar - debe crear directorio si no existe
  con <- ciecl:::get_cie10_db()
  DBI::dbDisconnect(con)

  # Verificar que directorio existe
expect_true(dir.exists(cache_dir))
})

test_that("get_cie10_db inicializa indices en DB nueva", {
  skip_on_cran()

  # Limpiar cache
  suppressMessages(cie10_clear_cache())

  # Conectar - debe crear DB e indices
  expect_message(
    con <- ciecl:::get_cie10_db(),
    "Inicializada SQLite"
  )
  on.exit(DBI::dbDisconnect(con))

  # Verificar indices
  indices <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'idx_%'")
  expect_true("idx_codigo" %in% indices$name)
  expect_true("idx_desc" %in% indices$name)
})
