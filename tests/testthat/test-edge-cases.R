# test-edge-cases.R
# Pruebas exhaustivas para edge cases y entradas problematicas

# ==============================================================================
# PRUEBAS PARA cie_search()
# ==============================================================================

test_that("cie_search rechaza NA, vectores y tipos invalidos", {
  skip_on_cran()

  # NA debe dar error
  expect_error(cie_search(NA), "texto debe ser un string character no-NA")
  expect_error(cie_search(NA_character_), "texto debe ser un string character no-NA")

  # Vectores de longitud != 1 deben dar error
  expect_error(cie_search(c("diabetes", "cancer")), "texto debe ser un string character no-NA")
  expect_error(cie_search(character(0)), "texto debe ser un string character no-NA")

  # Tipos invalidos deben dar error
  expect_error(cie_search(123), "texto debe ser un string character no-NA")
  expect_error(cie_search(list("diabetes")), "texto debe ser un string character no-NA")
})

test_that("cie_search maneja cadenas muy cortas", {
  skip_on_cran()


  # Texto de 2 caracteres ahora es valido (para siglas como DM, FA, TB)
  # Sigla DM debe expandirse a "diabetes mellitus"
  expect_no_error(suppressMessages(cie_search("DM")))

  # Texto de 1 caracter o vacio debe dar error
  expect_error(cie_search("a"), "Texto minimo 2 caracteres")
  expect_error(cie_search(""), "Texto minimo 2 caracteres")
  expect_error(cie_search(" "), "Texto minimo 2 caracteres")
})

test_that("cie_search maneja threshold invalido", {
  skip_on_cran()

  # Threshold debe estar entre 0 y 1
  # Con threshold muy alto no deberia encontrar nada
  resultado <- cie_search("diabetes mellitus", threshold = 0.99)
  expect_s3_class(resultado, "tbl_df")

  # Con threshold = 0 debe encontrar muchos (todos tienen score >= 0)
  resultado_bajo <- cie_search("diabetes", threshold = 0, max_results = 5)
  expect_s3_class(resultado_bajo, "tbl_df")
})

test_that("cie_search maneja max_results extremos", {
  skip_on_cran()

  # max_results = 1

  resultado <- cie_search("diabetes", threshold = 0.70, max_results = 1)
  expect_lte(nrow(resultado), 1)

  # max_results muy grande
  resultado_grande <- cie_search("diabetes", threshold = 0.60, max_results = 10000)
  expect_s3_class(resultado_grande, "tbl_df")
})

test_that("cie_search maneja caracteres especiales en texto", {
  skip_on_cran()

  # Texto con caracteres especiales SQL (inyeccion)
  expect_no_error({
    suppressMessages(cie_search("diabetes'; DROP TABLE cie10;--", threshold = 0.5))
  })

  # Texto con comillas
  expect_no_error({
    suppressMessages(cie_search("diabetes \"tipo 1\"", threshold = 0.5))
  })

  # Texto con tildes y caracteres especiales espanol
  expect_no_error({
    resultado <- cie_search("diarrea infecciosa", threshold = 0.70)
  })
})

test_that("cie_search maneja espacios multiples", {
  skip_on_cran()

  # Texto con espacios multiples debe funcionar igual
  resultado1 <- cie_search("diabetes mellitus", threshold = 0.80)
  resultado2 <- cie_search("diabetes   mellitus", threshold = 0.80)

  # Ambos deben retornar resultados
  expect_gt(nrow(resultado1), 0)
  expect_gt(nrow(resultado2), 0)
})

test_that("cie_search maneja campo invalido", {
  skip_on_cran()

  # Campo invalido debe usar default o dar error
  expect_error(cie_search("diabetes", campo = "inexistente"))
})

# ==============================================================================
# PRUEBAS PARA cie_lookup()
# ==============================================================================

test_that("cie_lookup maneja NA en entrada", {
  skip_on_cran()

  # NA como codigo
  suppressMessages({
    resultado <- cie_lookup(NA_character_)
  })
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup maneja vector vacio", {
  skip_on_cran()

  # Vector vacio
  resultado <- cie_lookup(character(0))
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup maneja cadena vacia", {
  skip_on_cran()

  suppressMessages({
    resultado <- cie_lookup("")
  })
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup maneja codigo con espacios", {
  skip_on_cran()

  # Codigo con espacios debe ser normalizado
  resultado1 <- cie_lookup("E11.0")
  resultado2 <- cie_lookup(" E11.0 ")

  expect_equal(nrow(resultado1), nrow(resultado2))
  expect_equal(resultado1$codigo, resultado2$codigo)
})

test_that("cie_lookup maneja minusculas", {
  skip_on_cran()

  # Codigo en minusculas debe funcionar
  resultado1 <- cie_lookup("E11.0")
  resultado2 <- cie_lookup("e11.0")

  expect_equal(nrow(resultado1), nrow(resultado2))
  expect_equal(resultado1$codigo, resultado2$codigo)
})

test_that("cie_lookup maneja rangos invalidos", {
  skip_on_cran()

  # Rango invertido
  suppressWarnings(
    resultado <- cie_lookup("E14-E10")
  )
  # Puede no encontrar resultados pero no debe crashear
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_lookup maneja vector con NAs mezclados", {
  skip_on_cran()

  # Vector con NAs mezclados
  codigos <- c("E11.0", NA, "Z00", NA)
  suppressMessages({
    resultado <- cie_lookup(codigos)
  })

  # Debe procesar los codigos validos
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_lookup maneja codigo con caracteres SQL peligrosos", {
  skip_on_cran()

  # Intento de SQL injection - debe ser rechazado con mensaje, no error
  codigos_peligrosos <- c(
    "E11'; DROP TABLE cie10;--",
    "E11.0 OR 1=1",
    "E11%",
    "E11*"
  )

  for (cod in codigos_peligrosos) {
    # Debe retornar tibble vacio sin crashear
    suppressMessages({
      resultado <- cie_lookup(cod)
    })
    expect_s3_class(resultado, "tbl_df")
    expect_equal(nrow(resultado), 0)
  }
})

test_that("cie_lookup expandir con codigo inexistente", {
  skip_on_cran()

  # Expandir codigo que no existe
  suppressMessages({
    resultado <- cie_lookup("ZZZ", expandir = TRUE)
  })
  expect_equal(nrow(resultado), 0)
})

# ==============================================================================
# PRUEBAS PARA cie_normalizar()
# ==============================================================================

test_that("cie_normalizar maneja NA", {
  skip_on_cran()

  # NA como entrada
  resultado <- cie_normalizar(NA_character_, buscar_db = FALSE)
  expect_true(is.na(resultado))
})

test_that("cie_normalizar maneja vector con NAs", {
  skip_on_cran()

  codigos <- c("E110", NA, "I100")
  resultado <- cie_normalizar(codigos, buscar_db = FALSE)

  expect_equal(resultado[1], "E11.0")
  expect_true(is.na(resultado[2]))
  expect_equal(resultado[3], "I10.0")
})

test_that("cie_normalizar maneja cadena vacia", {
  skip_on_cran()

  resultado <- cie_normalizar("", buscar_db = FALSE)
  expect_equal(resultado, "")
})

test_that("cie_normalizar maneja codigos ya normalizados", {
  skip_on_cran()

  # Codigos ya con punto no deben cambiar
  codigos <- c("E11.0", "I10.0", "Z00.0")
  resultado <- cie_normalizar(codigos, buscar_db = FALSE)
  expect_equal(resultado, codigos)
})

test_that("cie_normalizar maneja codigos de 3 caracteres", {
  skip_on_cran()

  # Codigos de categoria (3 chars) no deben modificarse
  resultado <- cie_normalizar("E11", buscar_db = FALSE)
  expect_equal(resultado, "E11")
})

test_that("cie_normalizar maneja codigos largos", {
  skip_on_cran()

  # Codigos de 5 digitos (E11.00 formato)
  resultado <- cie_normalizar("E1100", buscar_db = FALSE)
  expect_equal(resultado, "E11.00")
})

# ==============================================================================
# PRUEBAS PARA cie_validate_vector()
# ==============================================================================

test_that("cie_validate_vector maneja NA", {
  # NA debe ser FALSE
  resultado <- cie_validate_vector(NA_character_)
  expect_false(resultado)
})

test_that("cie_validate_vector maneja vector vacio", {
  resultado <- cie_validate_vector(character(0))
  expect_length(resultado, 0)
})

test_that("cie_validate_vector maneja cadena vacia", {
  resultado <- cie_validate_vector("")
  expect_false(resultado)
})

test_that("cie_validate_vector acepta formatos validos", {
  # Todos los formatos validos CIE-10
  codigos_validos <- c(
    "E11",      # Categoria (3 chars)
    "E11.0",    # Subcategoria con punto
    "E11.00",   # Subcategoria con 2 decimales
    "E110",     # Formato MINSAL sin punto
    "Z00",
    "A00.0",
    "B99.9"
  )

  resultado <- cie_validate_vector(codigos_validos)
  expect_true(all(resultado))
})

test_that("cie_validate_vector rechaza formatos invalidos", {
  codigos_invalidos <- c(
    "INVALIDO",   # Texto
    "123",        # Solo numeros
    "E1",         # Muy corto
    "EE11",       # Letra duplicada
    "E11.111",    # Demasiados decimales
    "E11..0",     # Punto duplicado
    "1E11",       # Numero al inicio
    ""            # Vacio
  )

  resultado <- cie_validate_vector(codigos_invalidos)
  expect_true(all(!resultado))
})

test_that("cie_validate_vector maneja minusculas", {
  # Debe funcionar con minusculas
  resultado <- cie_validate_vector("e11.0")
  expect_true(resultado)
})

test_that("cie_validate_vector strict mode", {
  skip_on_cran()

  # Modo estricto verifica existencia en DB
  codigos <- c("E11.0", "Z00", "XXXX")

  expect_warning({
    resultado <- cie_validate_vector(codigos, strict = TRUE)
  })

  # Los dos primeros deben ser validos, el tercero no
  expect_equal(resultado[1:2], c(TRUE, TRUE))
  expect_false(resultado[3])
})

# ==============================================================================
# PRUEBAS PARA cie_expand()
# ==============================================================================

test_that("cie_expand maneja codigo vacio", {
  skip_on_cran()

  suppressMessages({
    resultado <- cie_expand("")
  })
  expect_length(resultado, 0)
})

test_that("cie_expand maneja NA", {
  skip_on_cran()

  suppressMessages({
    resultado <- cie_expand(NA_character_)
  })
  expect_length(resultado, 0)
})

test_that("cie_expand maneja codigo inexistente", {
  skip_on_cran()

  suppressMessages({
    resultado <- cie_expand("ZZZ")
  })
  expect_length(resultado, 0)
})

test_that("cie_expand retorna hijos correctos", {
  skip_on_cran()

  # E11 debe tener multiples hijos
  hijos <- cie_expand("E11")

  # Todos deben empezar con E11
  expect_true(all(stringr::str_starts(hijos, "E11")))

  # Debe haber al menos E11.0
  expect_true("E11.0" %in% hijos || any(stringr::str_detect(hijos, "^E11\\.0")))
})

# ==============================================================================
# PRUEBAS PARA cie_map_comorbid()
# ==============================================================================

test_that("cie_map_comorbid maneja vector vacio", {
  resultado <- cie_map_comorbid(character(0))
  expect_equal(nrow(resultado), 0)
  expect_true("codigo" %in% names(resultado))
  expect_true("categoria" %in% names(resultado))
})

test_that("cie_map_comorbid maneja NA", {
  resultado <- cie_map_comorbid(NA_character_)
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$categoria, "Otra")
})

test_that("cie_map_comorbid categoriza correctamente multiples codigos", {
  codigos <- c(
    "E11.0",   # Diabetes
    "I50.9",   # Insuficiencia cardiaca
    "I21.0",   # Infarto miocardio
    "C50.9",   # Neoplasia maligna
    "J44.0",   # EPOC
    "N18.0",   # Enfermedad renal cronica
    "F32.0",   # Trastornos mentales
    "Z00.0"    # Otra (no mapeada)
  )

  resultado <- cie_map_comorbid(codigos)

  expect_equal(resultado$categoria[resultado$codigo == "E11.0"], "Diabetes")
  expect_equal(resultado$categoria[resultado$codigo == "I50.9"], "Insuficiencia cardiaca")
  expect_equal(resultado$categoria[resultado$codigo == "I21.0"], "Infarto miocardio")
  expect_equal(resultado$categoria[resultado$codigo == "C50.9"], "Neoplasia maligna")
  expect_equal(resultado$categoria[resultado$codigo == "J44.0"], "EPOC")
  expect_equal(resultado$categoria[resultado$codigo == "N18.0"], "Enfermedad renal cronica")
  expect_equal(resultado$categoria[resultado$codigo == "F32.0"], "Trastornos mentales")
  expect_equal(resultado$categoria[resultado$codigo == "Z00.0"], "Otra")
})

# ==============================================================================
# PRUEBAS PARA cie_comorbid()
# ==============================================================================

test_that("cie_comorbid rechaza dataframe vacio", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df_vacio <- data.frame(
    id = character(0),
    diag = character(0)
  )

  # El paquete comorbidity no acepta datos vacios, esto es comportamiento esperado
  expect_error(
    cie_comorbid(df_vacio, id = "id", code = "diag", map = "charlson"),
    "non-missing data"
  )
})

test_that("cie_comorbid detecta columnas inexistentes", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    paciente = c(1, 2),
    codigo = c("E11.0", "I50.9")
  )

  # Columna id incorrecta
  expect_error(
    cie_comorbid(df, id = "id_paciente", code = "codigo"),
    "no existen en data"
  )

  # Columna code incorrecta
  expect_error(
    cie_comorbid(df, id = "paciente", code = "diagnostico"),
    "no existen en data"
  )
})

test_that("cie_comorbid funciona con map elixhauser", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 2),
    diag = c("E11.0", "I50.9", "C50.9")
  )

  resultado <- cie_comorbid(df, id = "id", code = "diag", map = "elixhauser")
  expect_s3_class(resultado, "tbl_df")
  expect_false("score_charlson" %in% names(resultado))
})

test_that("cie_comorbid maneja codigos con NA", {
  skip_on_cran()
  skip_if_not_installed("comorbidity")

  df <- data.frame(
    id = c(1, 1, 2, 2),
    diag = c("E11.0", NA, "I50.9", "C50.9")
  )

  # No debe crashear con NAs (warning esperado por NA values)
  expect_no_error({
    suppressWarnings({
      resultado <- cie_comorbid(df, id = "id", code = "diag", map = "charlson")
    })
  })
})

# ==============================================================================
# PRUEBAS PARA cie10_sql()
# ==============================================================================

test_that("cie10_sql bloquea queries UPDATE", {
  expect_error(
    cie10_sql("UPDATE cie10 SET codigo = 'X' WHERE codigo = 'E11.0'"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql bloquea queries DELETE", {
  expect_error(
    cie10_sql("DELETE FROM cie10 WHERE codigo = 'E11.0'"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql bloquea queries INSERT", {
  expect_error(
    cie10_sql("INSERT INTO cie10 (codigo) VALUES ('TEST')"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql bloquea queries ALTER", {
  expect_error(
    cie10_sql("ALTER TABLE cie10 ADD COLUMN test TEXT"),
    "Solo queries SELECT"
  )
})

test_that("cie10_sql maneja queries con espacios", {
  skip_on_cran()

  # Query con espacios al inicio
  resultado <- cie10_sql("   SELECT COUNT(*) as n FROM cie10")
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie10_sql maneja queries complejas", {
  skip_on_cran()

  # Query con JOIN, GROUP BY, ORDER BY
  query <- "
    SELECT capitulo, COUNT(*) as n
    FROM cie10
    WHERE codigo LIKE 'E%'
    GROUP BY capitulo
    ORDER BY n DESC
    LIMIT 5
  "

  resultado <- cie10_sql(query)
  expect_s3_class(resultado, "tbl_df")
  expect_true("capitulo" %in% names(resultado))
  expect_true("n" %in% names(resultado))
})

test_that("cie10_sql maneja queries con LIKE", {
  skip_on_cran()

  resultado <- cie10_sql("SELECT * FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 5")
  expect_s3_class(resultado, "tbl_df")
  expect_true(all(stringr::str_starts(resultado$codigo, "E11")))
})

test_that("cie10_sql bloquea keywords peligrosos", {
  # DROP (detectado como keyword peligroso)
  expect_error(
    cie10_sql("SELECT * FROM cie10; DROP TABLE cie10;--"),
    "keyword no permitido"
  )

  # Multiples statements sin keyword peligroso
  expect_error(
    cie10_sql("SELECT * FROM cie10; SELECT * FROM cie10"),
    "Multiples statements"
  )

  # ATTACH (SQLite specific attack)
  expect_error(
    cie10_sql("SELECT * FROM cie10 WHERE 1=1 ATTACH DATABASE"),
    "keyword no permitido"
  )

  # PRAGMA (SQLite metadata)
  expect_error(
    cie10_sql("SELECT * FROM cie10 WHERE 1=1 PRAGMA table_info"),
    "keyword no permitido"
  )
})

test_that("cie10_sql permite queries legitimas con strings especiales", {
  skip_on_cran()

  # Query con string que contiene ;
  resultado <- cie10_sql("SELECT * FROM cie10 WHERE descripcion LIKE '%a;b%' LIMIT 1")
  expect_s3_class(resultado, "tbl_df")
})
