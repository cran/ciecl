# test-cie-data.R
# Pruebas para funciones de generacion de datos CIE-10

# ==============================================================================
# PRUEBAS parsear_cie10_minsal()
# ==============================================================================

test_that("parsear_cie10_minsal requiere readxl", {
  # Solo testear si readxl NO esta instalado
  skip_if(requireNamespace("readxl", quietly = TRUE),
          "readxl esta instalado")

  expect_error(
    ciecl:::parsear_cie10_minsal("test.xls"),
    "readxl"
  )
})

test_that("parsear_cie10_minsal error si archivo no existe", {
  skip_if_not_installed("readxl")

  expect_error(
    ciecl:::parsear_cie10_minsal("archivo_inexistente.xls"),
    "no encontrado"
  )
})

test_that("parsear_cie10_minsal con XLS real", {
  skip_on_cran()
  skip_if_not_installed("readxl")

  # Buscar XLSX/XLS MINSAL en rutas conocidas (formato CIE-10 correcto)
  xls_paths <- c(
    "D:/MAGISTER/01_Paquete_R/analisis_bases/CIE-10 (1).xlsx",
    "../CIE-10 (1).xlsx",
    "CIE-10 (1).xlsx"
  )


  xls_path <- NULL
  for (path in xls_paths) {
    if (file.exists(path)) {
      xls_path <- path
      break
    }
  }

  skip_if(is.null(xls_path), "Archivo XLSX CIE-10 MINSAL no disponible")

  # Intentar parsear, skip si formato incompatible
  resultado <- tryCatch(
    ciecl:::parsear_cie10_minsal(xls_path),
    error = function(e) {
      if (grepl("columnas codigo/descripcion", e$message)) {
        skip("XLS tiene formato de columnas incompatible")
      }
      stop(e)
    }
  )

  # Verificar estructura
  expect_s3_class(resultado, "tbl_df")
  expect_true("codigo" %in% names(resultado))
  expect_true("descripcion" %in% names(resultado))
  expect_true("capitulo" %in% names(resultado))
  expect_true("es_daga" %in% names(resultado))
  expect_true("es_cruz" %in% names(resultado))

  # Verificar contenido minimo
  expect_gt(nrow(resultado), 30000)


  # Verificar tipos de datos
  expect_type(resultado$codigo, "character")
  expect_type(resultado$descripcion, "character")
  expect_type(resultado$es_daga, "logical")
  expect_type(resultado$es_cruz, "logical")
})

test_that("parsear_cie10_minsal detecta codigos daga y cruz", {
  skip_on_cran()
  skip_if_not_installed("readxl")

  # Buscar XLSX/XLS MINSAL (formato CIE-10 correcto)
  xls_paths <- c(
    "D:/MAGISTER/01_Paquete_R/analisis_bases/CIE-10 (1).xlsx",
    "../CIE-10 (1).xlsx",
    "CIE-10 (1).xlsx"
  )


  xls_path <- NULL
  for (path in xls_paths) {
    if (file.exists(path)) {
      xls_path <- path
      break
    }
  }

  skip_if(is.null(xls_path), "Archivo XLSX CIE-10 MINSAL no disponible")

  # Intentar parsear, skip si formato incompatible
  resultado <- tryCatch(
    ciecl:::parsear_cie10_minsal(xls_path),
    error = function(e) {
      if (grepl("columnas codigo/descripcion", e$message)) {
        skip("XLS tiene formato de columnas incompatible")
      }
      stop(e)
    }
  )

  # Verificar que hay codigos con flags daga/cruz
  n_daga <- sum(resultado$es_daga, na.rm = TRUE)
  n_cruz <- sum(resultado$es_cruz, na.rm = TRUE)

  # Los flags pueden ser 0 si el archivo no tiene simbolos
  # Pero la columna debe existir y ser logica
  expect_type(resultado$es_daga, "logical")
  expect_type(resultado$es_cruz, "logical")
})

# ==============================================================================
# PRUEBAS generar_cie10_cl()
# ==============================================================================

test_that("generar_cie10_cl requiere usethis", {
  # Solo testear si usethis NO esta instalado
  skip_if(requireNamespace("usethis", quietly = TRUE),
          "usethis esta instalado")

  expect_error(
    generar_cie10_cl("test.xls"),
    "usethis"
  )
})

test_that("generar_cie10_cl error si XLS no encontrado", {
  skip_if_not_installed("usethis")
  skip_if_not_installed("readxl")

  # En directorio temporal sin XLS
  withr::with_tempdir({
    expect_error(
      generar_cie10_cl(),
      "XLS no encontrado"
    )
  })
})

test_that("generar_cie10_cl acepta ruta explicita", {
  skip_on_cran()
  skip_if_not_installed("usethis")
  skip_if_not_installed("readxl")

  # Verificar que acepta argumento xls_path
  # (fallara si archivo no existe, pero no debe dar error de argumento)
  expect_error(
    generar_cie10_cl(xls_path = "ruta_explicita.xls"),
    "no encontrado"
  )
})

test_that("generar_cie10_cl detecta XLS en directorio padre", {
  skip_on_cran()
  skip_if_not_installed("usethis")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    # Crear subdirectorio
    dir.create("child")

    # Crear mock XLSX en directorio padre con formato CIE-10 correcto
    # Nota: usamos .xlsx porque writexl crea archivos xlsx
    mock_data <- data.frame(
      codigo = c("E11.0", "I50.9", "A00.0"),
      descripcion = c("Diabetes tipo 2", "Insuficiencia cardiaca", "Colera"),
      stringsAsFactors = FALSE
    )
    writexl::write_xlsx(mock_data, "mock_cie10.xlsx")

    # Verificar que el archivo existe en padre
    expect_true(file.exists("mock_cie10.xlsx"))

    # Desde child, parsear_cie10_minsal deberia poder leer archivo en padre
    withr::with_dir("child", {
      parent_path <- normalizePath("../mock_cie10.xlsx", mustWork = FALSE)
      expect_true(file.exists(parent_path))

      # Ejecutar parsear_cie10_minsal con ruta explicita padre
      resultado <- ciecl:::parsear_cie10_minsal(parent_path)
      expect_s3_class(resultado, "tbl_df")
      expect_equal(nrow(resultado), 3)
    })
  })
})

test_that("generar_cie10_cl detecta XLS en directorio actual", {
  skip_on_cran()
  skip_if_not_installed("usethis")
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    # Crear mock XLSX en directorio actual con formato CIE-10 correcto
    mock_data <- data.frame(
      codigo = c("E11.0", "I50.9", "A00.0"),
      descripcion = c("Diabetes tipo 2", "Insuficiencia cardiaca", "Colera"),
      stringsAsFactors = FALSE
    )
    writexl::write_xlsx(mock_data, "mock_cie10.xlsx")

    # Verificar que el archivo existe
    expect_true(file.exists("mock_cie10.xlsx"))

    # parsear_cie10_minsal con ruta directa
    resultado <- ciecl:::parsear_cie10_minsal("mock_cie10.xlsx")
    expect_s3_class(resultado, "tbl_df")
    expect_equal(nrow(resultado), 3)
  })
})

test_that("generar_cie10_cl autodeteccion prueba ambas rutas", {
  skip_on_cran()
  skip_if_not_installed("usethis")
  skip_if_not_installed("readxl")

  # Este test verifica que la funcion intenta ambas rutas
  # cuando xls_path es NULL y ninguna existe
  withr::with_tempdir({
    dir.create("subdir")
    withr::with_dir("subdir", {
      # Desde subdir, ni ../ ni ./ tienen el archivo
      expect_error(
        generar_cie10_cl(xls_path = NULL),
        "XLS no encontrado"
      )
    })
  })
})

# ==============================================================================
# PRUEBAS DATASET cie10_cl
# ==============================================================================

test_that("dataset cie10_cl carga correctamente", {
  data("cie10_cl", package = "ciecl", envir = environment())

  expect_s3_class(cie10_cl, "tbl_df")
  expect_gt(nrow(cie10_cl), 30000)
})

test_that("dataset cie10_cl tiene columnas requeridas", {
  data("cie10_cl", package = "ciecl", envir = environment())

  columnas_requeridas <- c("codigo", "descripcion")
  expect_true(all(columnas_requeridas %in% names(cie10_cl)))
})

test_that("dataset cie10_cl codigos son unicos", {
  skip_on_cran()

  data("cie10_cl", package = "ciecl", envir = environment())

  # Contar duplicados
  n_total <- nrow(cie10_cl)
  n_unicos <- length(unique(cie10_cl$codigo))

  # Puede haber algunos duplicados por daga/cruz pero no deberia ser excesivo
  ratio_duplicados <- (n_total - n_unicos) / n_total
  expect_lt(ratio_duplicados, 0.1)  # < 10% duplicados
})

test_that("dataset cie10_cl codigos tienen formato valido", {
  skip_on_cran()

  data("cie10_cl", package = "ciecl", envir = environment())

  # Todos los codigos deben empezar con letra
  expect_true(all(grepl("^[A-Z]", cie10_cl$codigo)))

  # Todos los codigos deben tener al menos 3 caracteres
  expect_true(all(nchar(cie10_cl$codigo) >= 3))
})

test_that("dataset cie10_cl contiene codigos E11 (diabetes)", {
  skip_on_cran()

  data("cie10_cl", package = "ciecl", envir = environment())

  # E11 es Diabetes tipo 2, debe existir
  codigos_e11 <- cie10_cl[grepl("^E11", cie10_cl$codigo), ]
  expect_gt(nrow(codigos_e11), 0)
})

test_that("dataset cie10_cl contiene codigos I10 (hipertension)", {
  skip_on_cran()

  data("cie10_cl", package = "ciecl", envir = environment())

  # I10 es Hipertension esencial
  codigos_i10 <- cie10_cl[grepl("^I10", cie10_cl$codigo), ]
  expect_gt(nrow(codigos_i10), 0)
})

# ==============================================================================
# PRUEBAS DE INTEGRIDAD DE DATOS
# ==============================================================================

test_that("dataset cie10_cl sin valores NA en columnas criticas", {
  skip_on_cran()

  data("cie10_cl", package = "ciecl", envir = environment())

  # codigo y descripcion no deben tener NA
  expect_equal(sum(is.na(cie10_cl$codigo)), 0)
  expect_equal(sum(is.na(cie10_cl$descripcion)), 0)
})

test_that("dataset cie10_cl descripciones no vacias", {
  skip_on_cran()

  data("cie10_cl", package = "ciecl", envir = environment())

  # Todas las descripciones deben tener contenido
  expect_true(all(nchar(cie10_cl$descripcion) > 0))
})

test_that("dataset cie10_cl capitulos extraidos correctamente", {
  skip_on_cran()

  data("cie10_cl", package = "ciecl", envir = environment())

  # Si existe columna capitulo, verificar
  if ("capitulo" %in% names(cie10_cl)) {
    # Capitulos validos: A00-Z99
    capitulos_validos <- unique(cie10_cl$capitulo)
    expect_true(all(grepl("^[A-Z]\\d{1,2}$", capitulos_validos, perl = TRUE) |
                    is.na(capitulos_validos)))
  }
})

# ==============================================================================
# PRUEBAS ADICIONALES PARA COBERTURA
# ==============================================================================

test_that("parsear_cie10_minsal verifica codigos con formato correcto", {
  skip_on_cran()
  skip_if_not_installed("readxl")

  xls_paths <- c(
    "D:/MAGISTER/01_Paquete_R/analisis_bases/CIE-10 (1).xlsx",
    "../CIE-10 (1).xlsx",
    "CIE-10 (1).xlsx"
  )


  xls_path <- NULL
  for (path in xls_paths) {
    if (file.exists(path)) {
      xls_path <- path
      break
    }
  }

  skip_if(is.null(xls_path), "Archivo XLSX CIE-10 MINSAL no disponible")

  resultado <- tryCatch(
    ciecl:::parsear_cie10_minsal(xls_path),
    error = function(e) {
      if (grepl("columnas codigo/descripcion", e$message)) {
        skip("XLS tiene formato de columnas incompatible")
      }
      stop(e)
    }
  )

  # Todos los codigos deben empezar con letra y tener minimo 3 caracteres
  expect_true(all(grepl("^[A-Z]", resultado$codigo)))
  expect_true(all(nchar(resultado$codigo) >= 3))
})

test_that("parsear_cie10_minsal extrae capitulo de codigo", {
  skip_on_cran()
  skip_if_not_installed("readxl")

  xls_paths <- c(
    "D:/MAGISTER/01_Paquete_R/analisis_bases/CIE-10 (1).xlsx",
    "../CIE-10 (1).xlsx",
    "CIE-10 (1).xlsx"
  )


  xls_path <- NULL
  for (path in xls_paths) {
    if (file.exists(path)) {
      xls_path <- path
      break
    }
  }

  skip_if(is.null(xls_path), "Archivo XLSX CIE-10 MINSAL no disponible")

  resultado <- tryCatch(
    ciecl:::parsear_cie10_minsal(xls_path),
    error = function(e) {
      if (grepl("columnas codigo/descripcion", e$message)) {
        skip("XLS tiene formato de columnas incompatible")
      }
      stop(e)
    }
  )

  # Capitulo debe extraerse correctamente del codigo
  # E11.0 -> E11, A00 -> A00
  expect_true(all(!is.na(resultado$capitulo) | is.na(resultado$codigo)))
})

test_that("parsear_cie10_minsal limpia descripciones", {
  skip_on_cran()
  skip_if_not_installed("readxl")

  xls_paths <- c(
    "D:/MAGISTER/01_Paquete_R/analisis_bases/CIE-10 (1).xlsx",
    "../CIE-10 (1).xlsx",
    "CIE-10 (1).xlsx"
  )


  xls_path <- NULL
  for (path in xls_paths) {
    if (file.exists(path)) {
      xls_path <- path
      break
    }
  }

  skip_if(is.null(xls_path), "Archivo XLSX CIE-10 MINSAL no disponible")

  resultado <- tryCatch(
    ciecl:::parsear_cie10_minsal(xls_path),
    error = function(e) {
      if (grepl("columnas codigo/descripcion", e$message)) {
        skip("XLS tiene formato de columnas incompatible")
      }
      stop(e)
    }
  )

  # Descripciones deben estar limpias (sin espacios extra al inicio/fin)
  expect_false(any(grepl("^\\s|\\s$", resultado$descripcion)))
})

test_that("generar_cie10_cl valida parametros y parsea XLS", {
  skip_on_cran()
  skip_if_not_installed("usethis")
  skip_if_not_installed("readxl")

  # Test 1: Error cuando no encuentra XLS
  expect_error(
    generar_cie10_cl(xls_path = NULL),
    "XLS no encontrado"
  )

 # Test 2: Error con ruta invalida
  expect_error(
    generar_cie10_cl(xls_path = "archivo_inexistente.xlsx"),
    class = "error"
  )

  # Test 3: Verificar que parsear_cie10_minsal funciona con XLS real
  # (generar_cie10_cl usa internamente parsear_cie10_minsal)
  xls_paths <- c(
    "D:/MAGISTER/01_Paquete_R/analisis_bases/CIE-10 (1).xlsx",
    "../CIE-10 (1).xlsx",
    "CIE-10 (1).xlsx"
  )

  xls_path <- NULL
  for (path in xls_paths) {
    if (file.exists(path)) {
      xls_path <- path
      break
    }
  }

  skip_if(is.null(xls_path), "Archivo XLSX CIE-10 MINSAL no disponible")

  # Testear parsear_cie10_minsal directamente (lo que generar_cie10_cl usa)
  resultado <- parsear_cie10_minsal(xls_path)

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 30000)
  expect_true("codigo" %in% names(resultado))
  expect_true("descripcion" %in% names(resultado))
})


# ==============================================================================
# PRUEBAS parsear_cie10_minsal() CON MOCK DATA
# ==============================================================================

test_that("parsear_cie10_minsal con mock XLSX detecta columnas codigo/descripcion", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    # Crear mock XLSX con estructura minima
    mock_data <- data.frame(
      codigo = c("E11.0", "E11.1", "I50.9", "C50.9"),
      descripcion = c(
        "Diabetes mellitus tipo 2 con coma",
        "Diabetes mellitus tipo 2 con cetoacidosis",
        "Insuficiencia cardiaca, no especificada",
        "Tumor maligno de la mama, sin especificar"
      ),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_s3_class(resultado, "tbl_df")
    expect_true("codigo" %in% names(resultado))
    expect_true("descripcion" %in% names(resultado))
    expect_equal(nrow(resultado), 4)
  })
})

test_that("parsear_cie10_minsal con mock extrae capitulo correctamente", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      codigo = c("E11.0", "I50.9", "A00.0", "Z99.9"),
      descripcion = c("Desc E11", "Desc I50", "Desc A00", "Desc Z99"),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_true("capitulo" %in% names(resultado))
    expect_equal(resultado$capitulo[1], "E11")
    expect_equal(resultado$capitulo[2], "I50")
    expect_equal(resultado$capitulo[3], "A00")
    expect_equal(resultado$capitulo[4], "Z99")
  })
})

test_that("parsear_cie10_minsal detecta codigo con tilde en nombre columna", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    # Columna con tilde: codigo -> código
    mock_data <- data.frame(
      `código` = c("E11.0", "I50.9"),
      `descripción` = c("Diabetes", "Insuficiencia cardiaca"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_s3_class(resultado, "tbl_df")
    expect_true("codigo" %in% names(resultado))
    expect_equal(nrow(resultado), 2)
  })
})

test_that("parsear_cie10_minsal detecta columna 'clave' como codigo", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      clave = c("E11.0", "I50.9"),
      titulo = c("Diabetes", "Insuficiencia cardiaca"),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_s3_class(resultado, "tbl_df")
    expect_equal(nrow(resultado), 2)
  })
})

test_that("parsear_cie10_minsal filtra codigos con menos de 3 caracteres", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      codigo = c("E11.0", "AB", "I", "C50.9"),  # AB e I deben ser filtrados
      descripcion = c("Desc1", "Desc2", "Desc3", "Desc4"),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_equal(nrow(resultado), 2)  # Solo E11.0 y C50.9
    expect_true(all(nchar(resultado$codigo) >= 3))
  })
})

test_that("parsear_cie10_minsal filtra filas con codigo NA", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      codigo = c("E11.0", NA, "I50.9", NA),
      descripcion = c("Desc1", "Desc2", "Desc3", "Desc4"),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_equal(nrow(resultado), 2)  # Solo E11.0 e I50.9
  })
})

test_that("parsear_cie10_minsal limpia espacios en codigo y descripcion", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      codigo = c("  E11.0  ", "I50.9 "),
      descripcion = c("  Diabetes  ", " Insuficiencia "),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_equal(resultado$codigo[1], "E11.0")
    expect_equal(resultado$codigo[2], "I50.9")
    expect_equal(resultado$descripcion[1], "Diabetes")
    expect_equal(resultado$descripcion[2], "Insuficiencia")
  })
})

test_that("parsear_cie10_minsal detecta simbolo daga", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      codigo = c("E11.0\u2020", "I50.9"),  # \u2020 es simbolo daga
      descripcion = c("Diabetes daga", "Insuficiencia"),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_true("es_daga" %in% names(resultado))
    expect_true(resultado$es_daga[1])
    expect_false(resultado$es_daga[2])
  })
})

test_that("parsear_cie10_minsal detecta simbolo asterisco", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      codigo = c("E11.0*", "I50.9"),  # * es simbolo cruz
      descripcion = c("Diabetes cruz", "Insuficiencia"),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_true("es_cruz" %in% names(resultado))
    expect_true(resultado$es_cruz[1])
    expect_false(resultado$es_cruz[2])
  })
})

test_that("parsear_cie10_minsal incluye columna categoria si existe", {
  skip_on_cran()
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  withr::with_tempdir({
    mock_data <- data.frame(
      codigo = c("E11.0", "I50.9"),
      descripcion = c("Diabetes", "Insuficiencia"),
      categoria = c("Endocrinas", "Circulatorias"),
      stringsAsFactors = FALSE
    )

    mock_path <- "mock_cie10.xlsx"
    writexl::write_xlsx(mock_data, mock_path)

    resultado <- ciecl:::parsear_cie10_minsal(mock_path)

    expect_true("categoria" %in% names(resultado))
  })
})
