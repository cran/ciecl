# ============================================================
# COBERTURA: ciclo de vida del cache SQLite en cie-sql.R
# Cubre build_cache_atomic (creacion de cache_dir, cleanup
# de .tmp residual) + build_fts (failsafe) + cache_is_current
# (sin metadata, version mismatch) + reconstruccion automatica
# en get_cie10_db por version mismatch.
#
# Patrones r-lib usados:
# - withr::local_tempdir(): cache_dir aislado por test
# - withr::local_envvar(): R_USER_CACHE_DIR aislado
# - withr::local_db_connection(): cleanup automatico de DBI
#   (en lugar de defer manual de dbDisconnect)
# - llamada directa a helpers internos (build_fts, cache_is_current,
#   build_cache_atomic) — devtools::test() carga el namespace via
#   pkgload::load_all(), asi que getFromNamespace() es innecesario.
# ============================================================

# --- build_fts: failsafe FTS5 missing -------------------------------------

test_that("build_fts crea tabla FTS5 sobre conexion existente", {
  # Politica CRAN: construccion FTS5 siempre con skip
  skip_on_cran()

  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )

  # Crear tabla cie10 minima (build_fts requiere que exista)
  DBI::dbExecute(con, "CREATE TABLE cie10 (codigo TEXT, descripcion TEXT,
                                            inclusion TEXT, exclusion TEXT)")
  DBI::dbExecute(con, "INSERT INTO cie10 VALUES
                       ('E11.0', 'diabetes con coma', NULL, NULL)")

  expect_silent(build_fts(con))
  expect_true(DBI::dbExistsTable(con, "cie10_fts"))
})

# --- cache_is_current ------------------------------------------------------
# canario CRAN: estos 4 tests corren sin skip en CRAN; usan SQLite
# :memory: y no construyen el cache del paquete.

test_that("cache_is_current retorna FALSE si no existe tabla cie10_meta", {
  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )
  expect_false(cache_is_current(con))
})

test_that("cache_is_current retorna FALSE si tabla cie10_meta esta vacia", {
  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )
  DBI::dbExecute(con, "CREATE TABLE cie10_meta (key TEXT PRIMARY KEY,
                                                  value TEXT)")

  expect_false(cache_is_current(con))
})

test_that("cache_is_current retorna FALSE si version no coincide", {
  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )
  DBI::dbExecute(con, "CREATE TABLE cie10_meta (key TEXT PRIMARY KEY,
                                                  value TEXT)")
  DBI::dbExecute(
    con,
    "INSERT INTO cie10_meta (key, value) VALUES ('cache_version', '0.0.0')"
  )

  expect_false(cache_is_current(con))
})

test_that("cache_is_current retorna TRUE cuando version coincide", {
  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )
  DBI::dbExecute(con, "CREATE TABLE cie10_meta (key TEXT PRIMARY KEY,
                                                  value TEXT)")
  pkg_v <- as.character(utils::packageVersion("ciecl"))
  DBI::dbExecute(
    con,
    sprintf("INSERT INTO cie10_meta (key, value)
             VALUES ('cache_version', '%s')", pkg_v)
  )

  expect_true(cache_is_current(con))
})

# canario CRAN: rama de error de cache_is_current (tryCatch,
# R/cie-sql.R:270-272); usa SQLite temporal, no el cache del paquete.
# (Movido desde test-cie-sql.R en fase 6D; sin skip_on_cran)
test_that("cache_is_current retorna FALSE cuando la query falla", {
  tmp_db <- tempfile(fileext = ".db")
  withr::defer(unlink(tmp_db))
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
  withr::defer(
    if (DBI::dbIsValid(con)) suppressWarnings(DBI::dbDisconnect(con))
  )

  # Tabla cie10_meta con schema incorrecto: el SELECT falla
  DBI::dbExecute(con, "CREATE TABLE cie10_meta (x INTEGER)")
  expect_false(cache_is_current(con))
})

# --- build_cache_atomic: ciclo completo en directorio aislado --------------

test_that("build_cache_atomic crea cache_dir y construye DB completa", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  db_path <- file.path(cache_dir, "test.db")

  build_cache_atomic(cache_dir, db_path)

  expect_true(file.exists(db_path))

  # Verificar contenido
  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), db_path)
  )

  expect_true(DBI::dbExistsTable(con, "cie10"))
  expect_true(DBI::dbExistsTable(con, "cie10_fts"))
  expect_true(DBI::dbExistsTable(con, "cie10_meta"))

  # Cache version sincronizada
  v <- DBI::dbGetQuery(
    con,
    "SELECT value FROM cie10_meta WHERE key = 'cache_version'"
  )$value[1]
  expect_equal(v, as.character(utils::packageVersion("ciecl")))

  # Indices inicializados en la DB construida (absorbido de
  # test-cie-sql.R en fase 6D)
  indices <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'idx_%'"
  )
  expect_true("idx_codigo" %in% indices$name)
  expect_true("idx_desc" %in% indices$name)
})

test_that("build_cache_atomic crea cache_dir cuando no existe", {
  skip_on_cran()

  parent <- withr::local_tempdir()
  cache_dir <- file.path(parent, "subdir_no_existe")
  db_path <- file.path(cache_dir, "test.db")

  expect_false(dir.exists(cache_dir))

  build_cache_atomic(cache_dir, db_path)

  expect_true(dir.exists(cache_dir))
  expect_true(file.exists(db_path))
})

test_that("build_cache_atomic limpia .tmp residual antes de empezar", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  db_path <- file.path(cache_dir, "test.db")
  tmp_path <- paste0(db_path, ".tmp")

  # Simular un .tmp residual de un build anterior interrumpido
  writeLines("residuo", tmp_path)
  expect_true(file.exists(tmp_path))

  build_cache_atomic(cache_dir, db_path)

  expect_true(file.exists(db_path))
  expect_false(file.exists(tmp_path))
})

# ============================================================
# COBERTURA: cli_progress en sesion interactiva
# rlang::local_interactive(TRUE) fuerza is_interactive() = TRUE
# scoped al test (helper canonico r-lib, ver gargle/httr2).
# Cubre las ramas show_progress = TRUE en build_cache_atomic y
# build_fts sin requerir una TTY real.
# ============================================================

test_that("build_fts emite cli_progress en sesion interactiva", {
  skip_on_cran()

  rlang::local_interactive(TRUE)

  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE cie10 (codigo TEXT, descripcion TEXT,
                          inclusion TEXT, exclusion TEXT)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO cie10 VALUES ('E11.0', 'diabetes', NULL, NULL)"
  )

  # Capturar mensajes cli; el step debe nombrar FTS5.
  msgs <- capture_messages(build_fts(con, .progress = TRUE))
  expect_true(any(grepl("FTS5", msgs)))
  expect_true(DBI::dbExistsTable(con, "cie10_fts"))
})

test_that("build_fts permanece silencioso con .progress = FALSE aunque interactivo", {
  skip_on_cran()

  rlang::local_interactive(TRUE)

  con <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE cie10 (codigo TEXT, descripcion TEXT,
                          inclusion TEXT, exclusion TEXT)"
  )

  expect_silent(build_fts(con, .progress = FALSE))
})

test_that("build_cache_atomic emite los 4 cli_progress_step en sesion interactiva", {
  skip_on_cran()

  rlang::local_interactive(TRUE)

  cache_dir <- withr::local_tempdir()
  db_path <- file.path(cache_dir, "test.db")

  msgs <- capture_messages(build_cache_atomic(cache_dir, db_path))

  # Los 4 pasos del pipeline (dataset, tabla, indices, FTS5)
  texto <- paste(msgs, collapse = "\n")
  expect_match(texto, "cie10_cl")
  expect_match(texto, "cie10")
  expect_match(texto, "[Ii]ndices")
  expect_match(texto, "FTS5")
  expect_true(file.exists(db_path))
})

test_that("build_cache_atomic recupera limpiamente si build_fts falla en sesion interactiva", {
  skip_on_cran()

  rlang::local_interactive(TRUE)

  cache_dir <- withr::local_tempdir()
  db_path <- file.path(cache_dir, "test.db")
  tmp_path <- paste0(db_path, ".tmp")

  # Forzar fallo dentro del pipeline para ejercitar el handler
  # de error que cierra el progress con result = "failed".
  local_mocked_bindings(
    dbWriteTable = function(...) stop("fallo simulado en escritura"),
    .package = "DBI"
  )

  expect_error(
    suppressMessages(build_cache_atomic(cache_dir, db_path)),
    "Error construyendo cache SQLite"
  )

  # El .tmp debe haberse limpiado y el .db nunca debe existir
  expect_false(file.exists(tmp_path))
  expect_false(file.exists(db_path))
})

# --- get_cie10_db: rebuild condicional según versión (tabla sentinela) -----
# Una tabla extra ("sentinela") insertada a mano en el .db permite
# distinguir rebuild de reuso: el rebuild parte de un archivo nuevo, así
# que la sentinela desaparece; si el cache se reusa, la sentinela sobrevive.

test_that("get_cie10_db no reconstruye cuando la version coincide", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  # CIECL_CACHE_DIR tiene precedencia en get_cache_dir() y ya viene fijada
  # por setup.R; la sobreescribimos scoped para aislar este test.
  withr::local_envvar(CIECL_CACHE_DIR = cache_dir)
  cie10_disconnect()
  withr::defer(cie10_disconnect())

  con1 <- get_cie10_db()
  expect_true(DBI::dbIsValid(con1))

  # Sentinela: tabla ajena al paquete; sobrevive solo si NO hay rebuild
  DBI::dbExecute(con1, "CREATE TABLE sentinel_table (x INTEGER)")
  DBI::dbExecute(con1, "INSERT INTO sentinel_table VALUES (42)")
  cie10_disconnect()

  con2 <- get_cie10_db()
  expect_true(DBI::dbExistsTable(con2, "sentinel_table"))
  expect_equal(DBI::dbGetQuery(con2, "SELECT x FROM sentinel_table")$x, 42L)
})

test_that("get_cie10_db reconstruye cuando falta la tabla cie10_meta", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  withr::local_envvar(CIECL_CACHE_DIR = cache_dir)
  cie10_disconnect()
  withr::defer(cie10_disconnect())

  con1 <- get_cie10_db()
  DBI::dbExecute(con1, "CREATE TABLE sentinel_table (x INTEGER)")
  # Simular un cache antiguo (previo al versionado) o corrupto
  DBI::dbExecute(con1, "DROP TABLE cie10_meta")
  cie10_disconnect()

  con2 <- get_cie10_db()
  expect_false(DBI::dbExistsTable(con2, "sentinel_table"))
  expect_true(DBI::dbExistsTable(con2, "cie10_meta"))
  v <- DBI::dbGetQuery(
    con2,
    "SELECT value FROM cie10_meta WHERE key = 'cache_version'"
  )$value[1]
  expect_equal(v, as.character(utils::packageVersion("ciecl")))
})

# --- version-mismatch via local_mocked_bindings ---------------------------
# Simula que el paquete se actualizo DESPUES de construir el cache
# (mockeando utils::packageVersion, sugerencia de Maelle en rOpenSci
# #765), y verifica que cache_is_current() detecta el desfase y
# get_cie10_db() dispara el rebuild.

test_that("get_cie10_db reconstruye cuando packageVersion difiere (mock)", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  withr::local_envvar(CIECL_CACHE_DIR = cache_dir)
  cie10_disconnect()
  withr::defer(cie10_disconnect())

  con1 <- get_cie10_db()
  DBI::dbExecute(con1, "CREATE TABLE sentinel_table (x INTEGER)")
  cie10_disconnect()

  # Simular actualizacion del paquete: packageVersion("ciecl") reporta
  # una version distinta de la registrada en cie10_meta
  local_mocked_bindings(
    packageVersion = function(pkg, ...) package_version("999.0.0"),
    .package = "utils"
  )

  con2 <- get_cie10_db()
  # El rebuild parte de un .db nuevo: la sentinela desaparece
  expect_false(DBI::dbExistsTable(con2, "sentinel_table"))
  # La metadata registra la version "nueva" (mockeada) del paquete
  v <- DBI::dbGetQuery(
    con2,
    "SELECT value FROM cie10_meta WHERE key = 'cache_version'"
  )$value[1]
  expect_equal(v, "999.0.0")
})

# --- get_cie10_db: failsafes sobre conexion pooled y fresh ------------------
# Movidos desde test-cie-sql.R en fase 6D y reescritos con aislamiento
# (CIECL_CACHE_DIR + local_tempdir): los originales operaban sobre el
# cache real del usuario.

test_that("get_cie10_db reconstruye FTS5 si falta (pooled y fresh)", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  withr::local_envvar(CIECL_CACHE_DIR = cache_dir)
  cie10_disconnect()
  withr::defer(cie10_disconnect())

  con1 <- get_cie10_db()
  expect_true(DBI::dbExistsTable(con1, "cie10_fts"))

  # Rama pooled (R/cie-sql.R:40-42): borrar FTS5 sobre la conexion
  # pooled viva; la siguiente llamada la reconstruye sin reconectar
  DBI::dbExecute(con1, "DROP TABLE cie10_fts")
  con2 <- get_cie10_db()
  expect_identical(con2, con1)
  expect_true(DBI::dbExistsTable(con2, "cie10_fts"))

  # Rama fresh-connect (R/cie-sql.R:82-84): pool vacio y FTS5 borrada
  # por conexion directa; se reconstruye al conectar
  cie10_disconnect()
  db_path <- file.path(cache_dir, "cie10.db")
  con_direct <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con_direct, "DROP TABLE cie10_fts")
  DBI::dbDisconnect(con_direct)

  con3 <- get_cie10_db()
  expect_true(DBI::dbExistsTable(con3, "cie10_fts"))
})

test_that("get_cie10_db reconstruye si la tabla cie10 falta", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  withr::local_envvar(CIECL_CACHE_DIR = cache_dir)
  cie10_disconnect()
  withr::defer(cie10_disconnect())

  # Construir cache y borrar la tabla principal por conexion directa
  get_cie10_db()
  cie10_disconnect()
  db_path <- file.path(cache_dir, "cie10.db")
  con_direct <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con_direct, "DROP TABLE cie10")
  DBI::dbDisconnect(con_direct)

  # Al conectar detecta la integridad rota y reconstruye
  # (R/cie-sql.R:75-79)
  con <- get_cie10_db()
  expect_true(DBI::dbExistsTable(con, "cie10"))
})

# cie10_clear_cache tambien limpia .tmp residual (R/cie-sql.R:432-435).
# Reescritura aislada del test eliminado de test-cie-sql.R en 6D: el
# original operaba sobre el cache real; la rama NO la cubre el test de
# .tmp de build_cache_atomic (es un helper distinto).
test_that("cie10_clear_cache elimina .tmp residual junto al .db", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  withr::local_envvar(CIECL_CACHE_DIR = cache_dir)
  cie10_disconnect()
  withr::defer(cie10_disconnect())

  # Simular cache construido + .tmp residual de un build interrumpido
  get_cie10_db()
  cie10_disconnect()
  db_path <- file.path(cache_dir, "cie10.db")
  tmp_path <- paste0(db_path, ".tmp")
  file.create(tmp_path)
  expect_true(file.exists(db_path))
  expect_true(file.exists(tmp_path))

  suppressMessages(cie10_clear_cache())

  expect_false(file.exists(db_path))
  expect_false(file.exists(tmp_path))
})

# --- get_cache_dir: env var vacia -------------------------------------------
# canario CRAN: solo lee una env var y consulta tools::R_user_dir();
# no construye cache ni escribe en disco.

test_that("get_cache_dir trata CIECL_CACHE_DIR vacia como no definida", {
  # Regresion F10: "" no debe derivar en file.path("", "cie10.db"),
  # que escribiria el cache en el directorio de trabajo (CRAN policy)
  withr::local_envvar(CIECL_CACHE_DIR = "")

  expect_equal(get_cache_dir(), tools::R_user_dir("ciecl", "data"))
})

# --- F9: retornos de file.rename()/file.remove() no se silencian -------------

test_that("build_cache_atomic advierte si el rename final falla", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  db_path <- file.path(cache_dir, "test.db")

  local_mocked_bindings(
    file.rename = function(...) FALSE,
    .package = "base"
  )

  expect_warning(
    build_cache_atomic(cache_dir, db_path),
    "No se pudo renombrar"
  )
  # El .tmp construido queda en disco al no poder renombrarse
  expect_true(file.exists(paste0(db_path, ".tmp")))
})

test_that("cie10_clear_cache advierte si file.remove falla", {
  skip_on_cran()

  cache_dir <- withr::local_tempdir()
  withr::local_envvar(CIECL_CACHE_DIR = cache_dir)
  cie10_disconnect()
  withr::defer(cie10_disconnect())

  get_cie10_db()
  cie10_disconnect()

  local_mocked_bindings(
    file.remove = function(...) FALSE,
    .package = "base"
  )

  expect_warning(cie10_clear_cache(), "No se pudo eliminar")
})
