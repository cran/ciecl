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
    "Solo queries SELECT"
  )
})

test_that("cie10_sql emite deprecation warning para argumento close", {
  skip_on_cran()

  expect_warning(
    cie10_sql("SELECT COUNT(*) AS n FROM cie10", close = FALSE),
    "deprecado"
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
  expect_true(grepl("iabetes", r_where$descripcion, ignore.case = TRUE))

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
  r_union <- cie10_sql("SELECT codigo, descripcion FROM cie10 WHERE codigo = 'E11.0' UNION SELECT codigo, descripcion FROM cie10 WHERE codigo = 'I10'")
  expect_gte(nrow(r_union), 1)
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

  con <- ciecl:::get_cie10_db()
  expect_true(DBI::dbIsValid(con))
  expect_s4_class(con, "SQLiteConnection")
})

test_that("get_cie10_db crea tabla cie10 si no existe", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  expect_true(DBI::dbExistsTable(con, "cie10"))
})

test_that("get_cie10_db tabla tiene indices", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  indices <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='index'")
  expect_gt(nrow(indices), 0)
})

test_that("get_cie10_db usa directorio cache correcto", {
  skip_on_cran()

  cache_dir <- tools::R_user_dir("ciecl", "data")
  db_path <- file.path(cache_dir, "cie10.db")

  con <- ciecl:::get_cie10_db()
  expect_true(file.exists(db_path))
})

test_that("get_cie10_db tabla tiene columnas esperadas", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  columnas <- DBI::dbListFields(con, "cie10")
  expect_true("codigo" %in% columnas)
  expect_true("descripcion" %in% columnas)
})

# ============================================================
# PRUEBAS cie10_clear_cache()
# ============================================================

test_that("cie10_clear_cache elimina archivo db", {
  skip_on_cran()

  cache_dir <- tools::R_user_dir("ciecl", "data")
  db_path <- file.path(cache_dir, "cie10.db")

  # Asegurar que existe
  ciecl:::get_cie10_db()
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
  ciecl:::get_cie10_db()

  expect_message(cie10_clear_cache(), "eliminado")
  expect_message(cie10_clear_cache(), "no existe")
})

test_that("cie10_clear_cache retorna invisible NULL", {
  skip_on_cran()

  resultado <- suppressMessages(cie10_clear_cache())
  expect_null(resultado)
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

  # Multiples statements separados por semicolon (sin keywords peligrosos)
  expect_error(
    cie10_sql("SELECT * FROM cie10; SELECT * FROM cie10"),
    "Multiples statements"
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

# ============================================================
# PRUEBAS get_cie10_db() creacion directorio cache
# ============================================================

test_that("get_cie10_db crea directorio cache si no existe", {
  skip_on_cran()

  # Limpiar cache para forzar recreacion
  suppressMessages(cie10_clear_cache())

  cache_dir <- tools::R_user_dir("ciecl", "data")

  # Conectar - debe crear directorio si no existe
  ciecl:::get_cie10_db()

  # Verificar que directorio existe
  expect_true(dir.exists(cache_dir))
})

test_that("get_cie10_db inicializa indices en DB nueva", {
  skip_on_cran()

  # Limpiar cache
  suppressMessages(cie10_clear_cache())

  # Conectar - debe inicializar DB (mensajes solo en interactive)
  con <- ciecl:::get_cie10_db()

  # Verificar indices
  indices <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'idx_%'")
  expect_true("idx_codigo" %in% indices$name)
  expect_true("idx_desc" %in% indices$name)
})

# ============================================================
# PRUEBAS CONNECTION POOLING
# ============================================================

test_that("get_cie10_db reutiliza conexion (pooling)", {
  skip_on_cran()

  con1 <- ciecl:::get_cie10_db()
  con2 <- ciecl:::get_cie10_db()

  # Misma referencia de objeto
  expect_identical(con1, con2)
})

test_that("cie10_clear_cache invalida pool", {
  skip_on_cran()

  env <- getFromNamespace(".ciecl_env", "ciecl")

  con1 <- ciecl:::get_cie10_db()
  suppressMessages(cie10_clear_cache())

  # Pool debe estar vacio
  expect_null(env$con)
  expect_null(env$db_path)

  # Nueva conexion debe ser diferente
  con2 <- ciecl:::get_cie10_db()
  expect_true(DBI::dbIsValid(con2))
})

test_that("cie10_disconnect cierra conexion pooled", {
  skip_on_cran()

  env <- getFromNamespace(".ciecl_env", "ciecl")

  # Asegurar conexion activa
  ciecl:::get_cie10_db()
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
# PRUEBAS CACHE VERSIONADO
# ============================================================

test_that("cache incluye tabla cie10_meta con version", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()

  expect_true(DBI::dbExistsTable(con, "cie10_meta"))

  meta <- DBI::dbGetQuery(con, "SELECT * FROM cie10_meta WHERE key = 'cache_version'")
  expect_equal(nrow(meta), 1)
  expect_equal(meta$value, as.character(utils::packageVersion("ciecl")))
})

test_that("cache_is_current retorna TRUE para cache actual", {
  skip_on_cran()

  con <- ciecl:::get_cie10_db()
  expect_true(ciecl:::cache_is_current(con))
})

test_that("cache_is_current retorna FALSE si no hay tabla meta", {
  skip_on_cran()

  # Crear db temporal sin tabla meta
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  con_tmp <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con_tmp), add = TRUE)

  DBI::dbWriteTable(con_tmp, "cie10", data.frame(codigo = "E11.0"))

  expect_false(ciecl:::cache_is_current(con_tmp))
})

test_that("version mismatch fuerza rebuild", {
  skip_on_cran()

  # Obtener conexion valida
  con <- ciecl:::get_cie10_db()

  # Manipular version en meta
  DBI::dbExecute(con, "UPDATE cie10_meta SET value = '0.0.0' WHERE key = 'cache_version'")

  # Desconectar correctamente antes de forzar rebuild
  # (necesario en Windows para liberar lock del archivo)
  cie10_disconnect()

  # get_cie10_db debe detectar version mismatch y reconstruir
  con_new <- ciecl:::get_cie10_db()
  expect_true(DBI::dbIsValid(con_new))

  # Version debe ser correcta ahora
  meta <- DBI::dbGetQuery(con_new, "SELECT value FROM cie10_meta WHERE key = 'cache_version'")
  expect_equal(meta$value, as.character(utils::packageVersion("ciecl")))
})

# ============================================================
# PRUEBAS BUILD ATOMICO
# ============================================================

test_that("build_cache_atomic crea cache completo", {
  skip_on_cran()

  # Limpiar
  suppressMessages(cie10_clear_cache())

  cache_dir <- tools::R_user_dir("ciecl", "data")
  db_path <- file.path(cache_dir, "cie10.db")

  # No debe existir .tmp residual despues de build exitoso
  expect_false(file.exists(paste0(db_path, ".tmp")))

  con <- ciecl:::get_cie10_db()
  expect_true(DBI::dbExistsTable(con, "cie10"))
  expect_true(DBI::dbExistsTable(con, "cie10_fts"))
  expect_true(DBI::dbExistsTable(con, "cie10_meta"))
})

# ============================================================
# PRUEBAS .ciecl_env y .onUnload (zzz.R)
# ============================================================

test_that(".ciecl_env tiene estructura correcta", {
  env <- ciecl:::.ciecl_env
  expect_true(is.environment(env))
  expect_true("con" %in% ls(env, all.names = TRUE))
  expect_true("db_path" %in% ls(env, all.names = TRUE))
})

test_that(".onUnload cierra conexion y limpia env", {
  skip_on_cran()
  ciecl:::get_cie10_db()
  env <- ciecl:::.ciecl_env
  expect_false(is.null(env$con))

  onUnload <- getFromNamespace(".onUnload", "ciecl")
  onUnload(libpath = .libPaths()[1])

  expect_null(env$con)
  expect_null(env$db_path)
})

test_that(".onUnload no falla sin conexion activa", {
  ciecl:::cie10_disconnect()
  onUnload <- getFromNamespace(".onUnload", "ciecl")
  expect_no_error(onUnload(libpath = .libPaths()[1]))
})

# ============================================================
# PRUEBAS BRANCHES ADICIONALES cie-sql.R
# ============================================================

test_that("get_cie10_db reconstruye FTS5 si falta en pooled", {
  skip_on_cran()
  con <- ciecl:::get_cie10_db()
  DBI::dbExecute(con, "DROP TABLE IF EXISTS cie10_fts")
  # Forzar re-check via pooling (misma conexion valida, mismo path)
  con2 <- ciecl:::get_cie10_db()
  tablas <- DBI::dbListTables(con2)
  expect_true(any(grepl("cie10_fts", tablas)))
  ciecl::cie10_disconnect()
})

test_that("get_cie10_db reconstruye si version no coincide en pooled", {
  skip_on_cran()
  con <- ciecl:::get_cie10_db()
  # Modificar version en pooled connection -> trigger rebuild en pooling path
  DBI::dbExecute(con, "UPDATE cie10_meta SET value = '0.0.0' WHERE key = 'cache_version'")
  con2 <- ciecl:::get_cie10_db()
  ver <- DBI::dbGetQuery(con2, "SELECT value FROM cie10_meta WHERE key = 'cache_version'")
  expect_equal(ver$value, as.character(utils::packageVersion("ciecl")))
  ciecl::cie10_disconnect()
})

test_that("get_cie10_db limpia conexion invalida", {
  skip_on_cran()
  on.exit(ciecl::cie10_disconnect(), add = TRUE)
  con <- ciecl:::get_cie10_db()
  DBI::dbDisconnect(con)
  # .ciecl_env aun tiene referencia invalida
  con2 <- ciecl:::get_cie10_db()
  expect_true(DBI::dbIsValid(con2))
  ciecl::cie10_disconnect()
})

test_that("get_cie10_db reconstruye si tabla cie10 falta", {
  skip_on_cran()
  ciecl::cie10_disconnect()
  con_direct <- DBI::dbConnect(RSQLite::SQLite(),
    file.path(tools::R_user_dir("ciecl", "data"), "cie10.db"))
  on.exit(try(DBI::dbDisconnect(con_direct), silent = TRUE), add = TRUE)
  DBI::dbExecute(con_direct, "DROP TABLE IF EXISTS cie10")
  DBI::dbDisconnect(con_direct)
  con2 <- ciecl:::get_cie10_db()
  expect_true(DBI::dbExistsTable(con2, "cie10"))
  ciecl::cie10_disconnect()
})

test_that("cie10_sql maneja error de ejecucion SQL", {
  skip_on_cran()
  expect_error(ciecl::cie10_sql("SELECT * FROM tabla_inexistente"))
})

test_that("cache_is_current retorna FALSE cuando query falla", {
  skip_on_cran()
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  # Crear tabla cie10_meta con schema incorrecto para que el SELECT falle
  DBI::dbExecute(con, "CREATE TABLE cie10_meta (x INTEGER)")
  expect_false(ciecl:::cache_is_current(con))
})

test_that("build_cache_atomic limpia .tmp residual", {
  skip_on_cran()
  on.exit(ciecl::cie10_disconnect(), add = TRUE)
  ciecl::cie10_disconnect()
  cache_dir <- tools::R_user_dir("ciecl", "data")
  tmp_file <- file.path(cache_dir, "cie10.db.tmp")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  file.create(tmp_file)
  expect_true(file.exists(tmp_file))
  suppressMessages(ciecl:::cie10_clear_cache())
  con <- ciecl:::get_cie10_db()
  expect_false(file.exists(tmp_file))
  ciecl::cie10_disconnect()
})
