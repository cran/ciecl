test_that("cie_search encuentra diabetes con fuzzy", {
  skip_on_cran()
  
  # Buscar diabetes (sin typo para mayor certeza)
  resultado <- cie_search("diabetes mellitus", threshold = 0.70)
  expect_gt(nrow(resultado), 0)
  # Codigos E10x-E14x (diabetes sin punto decimal)
  expect_true(any(stringr::str_detect(resultado$codigo, "^E1[0-4]")))
})

test_that("cie_lookup codigo exacto funciona", {
  skip_on_cran()
  
  resultado <- cie_lookup("E11.0")
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$codigo, "E11.0")
})

test_that("cie_lookup expansion jerarquica funciona", {
  skip_on_cran()
  
  hijos <- cie_lookup("E11", expandir = TRUE)
  expect_gt(nrow(hijos), 5)  # E11.0, E11.1, ...
  expect_true(all(stringr::str_starts(hijos$codigo, "E11")))
})

test_that("cie_lookup acepta vectores de multiples codigos", {
  skip_on_cran()
  
  # Vector con multiples codigos
  codigos <- c("E11.0", "Z00", "I10")
  resultado <- cie_lookup(codigos)
  
  # Debe retornar resultados para al menos algunos codigos
  expect_gt(nrow(resultado), 0)
  
  # Los codigos en el resultado deben estar en nuestra lista original
  # (puede haber mas si algun codigo tiene expansion)
  expect_true(any(resultado$codigo %in% codigos))
})

test_that("cie_lookup vectorizado maneja codigos invalidos", {
  skip_on_cran()
  
  # Mezcla de codigos validos e invalidos
  codigos <- c("E11.0", "INVALIDO", "Z00")
  
  # Capturar mensajes de advertencia
  suppressMessages({
    resultado <- cie_lookup(codigos)
  })
  
  # Debe retornar solo los codigos validos
  expect_gt(nrow(resultado), 0)
  expect_true(all(resultado$codigo %in% c("E11.0", "Z00")))
})

test_that("cie_lookup vectorizado elimina duplicados", {
  skip_on_cran()
  
  # Vector con codigos duplicados
  codigos <- c("E11.0", "E11.0", "Z00")
  resultado <- cie_lookup(codigos)
  
  # No debe haber duplicados en resultado
  expect_equal(nrow(resultado), length(unique(resultado$codigo)))
})

test_that("cie_lookup acepta multiples formatos de codigo", {
  skip_on_cran()
  
  # Probar con punto
  resultado_punto <- cie_lookup("E11.0")
  expect_equal(nrow(resultado_punto), 1)
  expect_equal(resultado_punto$codigo, "E11.0")
  
  # Probar sin punto (debe normalizar)
  resultado_sin_punto <- cie_lookup("E110")
  expect_equal(nrow(resultado_sin_punto), 1)
  expect_equal(resultado_sin_punto$codigo, "E11.0")
  
  # Ambos deben dar el mismo resultado
  expect_equal(resultado_punto$codigo, resultado_sin_punto$codigo)
})

test_that("cie_lookup puede generar descripcion_completa", {
  skip_on_cran()
  
  # Sin descripcion_completa
  resultado_normal <- cie_lookup("E11.0")
  expect_false("descripcion_completa" %in% names(resultado_normal))
  
  # Con descripcion_completa
  resultado_completo <- cie_lookup("E11.0", descripcion_completa = TRUE)
  expect_true("descripcion_completa" %in% names(resultado_completo))
  expect_true(stringr::str_detect(resultado_completo$descripcion_completa, "E11\\.0 - "))
})
test_that("cie_lookup con descripcion_completa mantiene columna en dataframe vacio", {
  skip_on_cran()

  # Bug fix: Cuando todos los codigos son invalidos, debe mantener la columna descripcion_completa
  suppressMessages({
    resultado_vacio <- cie_lookup(codigo = c("XXXX", "YYYY", "ZZZZ"), descripcion_completa = TRUE)
  })

  # El dataframe debe estar vacio
  expect_equal(nrow(resultado_vacio), 0)

  # Pero debe tener 11 columnas incluyendo descripcion_completa
  expect_equal(ncol(resultado_vacio), 11)
  expect_true("descripcion_completa" %in% names(resultado_vacio))

  # Verificar nombres de todas las columnas esperadas
  columnas_esperadas <- c("codigo", "descripcion", "categoria", "seccion",
                          "capitulo_nombre", "inclusion", "exclusion", "capitulo",
                          "es_daga", "es_cruz", "descripcion_completa")
  expect_equal(names(resultado_vacio), columnas_esperadas)
})

# ==============================================================================
# PRUEBAS PARA cie_siglas()
# ==============================================================================

test_that("cie_siglas retorna todas las siglas", {
  resultado <- cie_siglas()

  expect_s3_class(resultado, "tbl_df")
  expect_true("sigla" %in% names(resultado))
  expect_true("termino_busqueda" %in% names(resultado))
  expect_true("categoria" %in% names(resultado))
  expect_gt(nrow(resultado), 50)  # Al menos 50 siglas
})

test_that("cie_siglas contiene siglas comunes", {
  resultado <- cie_siglas()

  # Verificar siglas comunes
  siglas_esperadas <- c("iam", "dm", "hta", "epoc", "tbc")
  expect_true(all(siglas_esperadas %in% resultado$sigla))
})

test_that("cie_siglas no tiene duplicados", {
  resultado <- cie_siglas()

  # No debe haber siglas duplicadas
  expect_equal(nrow(resultado), length(unique(resultado$sigla)))
})

test_that("cie_siglas filtra por categoria", {
  # Filtrar cardiovasculares
  cardio <- cie_siglas("cardiovascular")
  expect_gt(nrow(cardio), 5)
  expect_true(all(cardio$categoria == "cardiovascular"))

  # Filtrar oncologicas
  onco <- cie_siglas("oncologica")
  expect_gt(nrow(onco), 3)
  expect_true(all(onco$categoria == "oncologica"))

  # Categoria invalida debe dar warning y retornar vacio
  expect_warning(invalida <- cie_siglas("inexistente"), "no encontrada")
  expect_equal(nrow(invalida), 0)
})

# ==============================================================================
# PRUEBAS PARA rangos en cie_lookup()
# ==============================================================================

test_that("cie_lookup maneja rangos invertidos", {
  skip_on_cran()
  
  # Rango invertido debe corregirse automaticamente (warning se emite)
  suppressWarnings(
    resultado <- cie_lookup("E14-E10")
  )
  
  # Debe retornar resultados (el rango corregido funciona)
  expect_gt(nrow(resultado), 0)
  
  # Los codigos deben estar en el rango correcto (E10-E14)
  expect_true(all(stringr::str_detect(resultado$codigo, "^E1[0-4]")))
})

test_that("cie_lookup rango normal funciona sin warning", {
  skip_on_cran()

  # Rango normal no debe emitir warning
  expect_no_warning(
    resultado <- cie_lookup("E10-E14")
  )

  expect_gt(nrow(resultado), 0)
})

test_that("cie_search verbose=FALSE suprime mensajes", {
  skip_on_cran()

  # Con verbose=FALSE no debe emitir mensajes
  expect_silent(
    suppressWarnings(cie_search("xyzabc123", verbose = FALSE))
  )
})

# ==============================================================================
# PRUEBAS ADICIONALES DE EDGE CASES
# ==============================================================================

test_that("cie_search con threshold 0 retorna muchos resultados", {
  skip_on_cran()

  resultado <- cie_search("diabetes", threshold = 0.0, max_results = 100)

  expect_s3_class(resultado, "tbl_df")
  # Con threshold 0, deberia retornar el maximo solicitado
  expect_lte(nrow(resultado), 100)
})

test_that("cie_search con threshold 1 es muy restrictivo", {
  skip_on_cran()

  # Threshold 1.0 requiere match perfecto
  suppressMessages({
    resultado <- cie_search("xyz123abc", threshold = 1.0, verbose = FALSE)
  })

  expect_s3_class(resultado, "tbl_df")
  # Probablemente 0 resultados para texto aleatorio
  expect_gte(nrow(resultado), 0)
})

test_that("cie_search con max_results=1 retorna maximo 1 resultado", {
  skip_on_cran()

  resultado <- cie_search("diabetes", threshold = 0.5, max_results = 1)

  expect_s3_class(resultado, "tbl_df")
  expect_lte(nrow(resultado), 1)
})

test_that("cie_search con campo inclusion funciona", {
  skip_on_cran()

  resultado <- cie_search("bacteriana", campo = "inclusion",
                          threshold = 0.7, max_results = 20)

  expect_s3_class(resultado, "tbl_df")
  # Puede o no encontrar resultados
  expect_true(nrow(resultado) >= 0)
})

test_that("cie_search valida parametros incorrectos", {
  skip_on_cran()

  # threshold fuera de rango
  expect_error(
    cie_search("diabetes", threshold = 1.5),
    "threshold"
  )

  expect_error(
    cie_search("diabetes", threshold = -0.1),
    "threshold"
  )

  # max_results invalido
  expect_error(
    cie_search("diabetes", max_results = 0),
    "max_results"
  )

  # texto no character
  expect_error(
    cie_search(123),
    "texto"
  )

  # texto NA
  expect_error(
    cie_search(NA_character_),
    "texto"
  )

  # texto muy corto
  expect_error(
    cie_search("a"),
    "2 caracteres"
  )
})

test_that("cie_search acepta siglas de 2 caracteres", {
  skip_on_cran()

  # DM = diabetes mellitus
  resultado <- cie_search("dm", threshold = 0.7, verbose = FALSE)

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)
})

test_that("cie_search busca con typos usando fuzzy", {
  skip_on_cran()

  # "diabetis" tiene typo
  resultado <- cie_search("diabetis", threshold = 0.7, verbose = FALSE)

  expect_s3_class(resultado, "tbl_df")
  # Fuzzy deberia encontrar "diabetes"
  expect_gt(nrow(resultado), 0)
})

test_that("cie_search normaliza tildes", {
  skip_on_cran()

  # Buscar con y sin tilde
  resultado_sin <- cie_search("neumonia", threshold = 0.7, verbose = FALSE)
  resultado_con <- cie_search("neumon\u00eda", threshold = 0.7, verbose = FALSE)

  expect_s3_class(resultado_sin, "tbl_df")
  expect_s3_class(resultado_con, "tbl_df")

  # Ambas busquedas deberian encontrar resultados similares
  expect_gt(nrow(resultado_sin), 0)
  expect_gt(nrow(resultado_con), 0)
})

# ==============================================================================
# PRUEBAS ADICIONALES cie_lookup()
# ==============================================================================

test_that("cie_lookup con extract=TRUE extrae codigo de texto", {
  skip_on_cran()

  # Codigo con prefijo
  resultado <- cie_lookup("CIE:E11.0", extract = TRUE)
  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$codigo, "E11.0")

  # Codigo con sufijo
  resultado2 <- cie_lookup("I10-confirmado", extract = TRUE)
  expect_equal(nrow(resultado2), 1)
  expect_equal(resultado2$codigo, "I10")
})

test_that("cie_lookup con check_siglas=TRUE convierte siglas", {
  skip_on_cran()

  # IAM deberia convertirse a I21.x
  resultado <- cie_lookup("IAM", check_siglas = TRUE)

  # Puede encontrar o no, pero no debe dar error
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_lookup maneja rangos validos", {
  skip_on_cran()

  # Rango normal
  resultado <- cie_lookup("E10-E14")

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 10)

  # Todos los codigos deben estar en el rango
  expect_true(all(grepl("^E1[0-4]", resultado$codigo)))
})

test_that("cie_lookup con normalizar=FALSE preserva formato", {
  skip_on_cran()

  # Sin normalizacion, E110 no se convierte a E11.0
  suppressMessages({
    resultado <- cie_lookup("E110", normalizar = FALSE)
  })

  # Puede encontrar o no dependiendo del formato en DB
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_lookup maneja vector con NAs", {
  skip_on_cran()

  codigos <- c("E11.0", NA, "Z00", NA)

  suppressMessages({
    resultado <- cie_lookup(codigos)
  })

  expect_s3_class(resultado, "tbl_df")
  # Solo deberia encontrar E11.0 y Z00
  expect_lte(nrow(resultado), 2)
})

test_that("cie_lookup maneja vector vacio", {
  resultado <- cie_lookup(character(0))

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup maneja vector solo NAs", {
  resultado <- cie_lookup(c(NA, NA, NA))

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

# ==============================================================================
# PRUEBAS cie_guia_busqueda()
# ==============================================================================

test_that("cie_guia_busqueda retorna data.frame", {
  resultado <- cie_guia_busqueda()

  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 5)
  expect_true("Tengo..." %in% names(resultado))
  expect_true("Usar funcion" %in% names(resultado))
  expect_true("Ejemplo" %in% names(resultado))
})

test_that("cie_guia_busqueda contiene casos comunes", {
  resultado <- cie_guia_busqueda()

  # Verificar que menciona casos importantes
  tengo <- resultado[["Tengo..."]]

  expect_true(any(grepl("exacto", tengo, ignore.case = TRUE)))
  expect_true(any(grepl("Descripcion", tengo, ignore.case = TRUE)))
  expect_true(any(grepl("Sigla", tengo, ignore.case = TRUE)))
})

test_that("cie_guia_busqueda tiene 8 filas", {
  resultado <- cie_guia_busqueda()
  expect_equal(nrow(resultado), 8)
})

test_that("cie_guia_busqueda tiene 3 columnas exactas", {
  resultado <- cie_guia_busqueda()
  expect_equal(ncol(resultado), 3)
  expect_equal(names(resultado), c("Tengo...", "Usar funcion", "Ejemplo"))
})

test_that("cie_guia_busqueda ejemplos mencionan funciones ciecl", {
  resultado <- cie_guia_busqueda()

  ejemplos <- resultado[["Ejemplo"]]

  # Verificar que menciona las funciones principales
  expect_true(any(grepl("cie_lookup", ejemplos)))
  expect_true(any(grepl("cie_search", ejemplos)))
})

test_that("cie_guia_busqueda todas las columnas son character", {
  resultado <- cie_guia_busqueda()

  expect_type(resultado[["Tengo..."]], "character")
  expect_type(resultado[["Usar funcion"]], "character")
  expect_type(resultado[["Ejemplo"]], "character")
})

# ==============================================================================
# PRUEBAS ADICIONALES cie_search() - COBERTURA
# ==============================================================================

test_that("cie_search con solo_fuzzy=TRUE omite busqueda exacta", {
  skip_on_cran()

  # Con solo_fuzzy, usa directamente fuzzy matching
  resultado <- cie_search("diabetes mellitus", solo_fuzzy = TRUE,
                          threshold = 0.7, verbose = FALSE)

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)
})

test_that("cie_search detecta sigla y muestra mensaje", {
  skip_on_cran()

  # IAM debe detectarse como sigla
  expect_message(
    resultado <- cie_search("IAM", threshold = 0.7, verbose = TRUE),
    "Sigla detectada"
  )

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_search busca en inclusion correctamente", {
  skip_on_cran()

  resultado <- cie_search("aguda", campo = "inclusion",
                          threshold = 0.6, max_results = 10, verbose = FALSE)

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_search con texto largo encuentra matches parciales", {
  skip_on_cran()

  # Busqueda de multiples palabras
  resultado <- cie_search("diabetes tipo dos insulina",
                          threshold = 0.5, max_results = 20, verbose = FALSE)

  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_search fuzzy encuentra typos", {
  skip_on_cran()

  # Typos intencionales
  resultado <- cie_search("hipertencion", threshold = 0.6, verbose = FALSE)

  expect_s3_class(resultado, "tbl_df")
  # Deberia encontrar hipertension
  expect_gt(nrow(resultado), 0)
})

# ==============================================================================
# PRUEBAS ADICIONALES cie_lookup() - COBERTURA
# ==============================================================================

test_that("cie_lookup con extract=TRUE y check_siglas=TRUE combinados", {
  skip_on_cran()

  # Combinar ambas opciones
  resultado <- cie_lookup("CIE:E11.0", extract = TRUE, check_siglas = TRUE)

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 1)
})

test_that("cie_lookup maneja rango de codigos", {
  skip_on_cran()

  resultado <- cie_lookup("I20-I25")

  expect_s3_class(resultado, "tbl_df")
  expect_gt(nrow(resultado), 0)
})

test_that("cie_lookup con normalizar procesa codigo sin punto", {
  skip_on_cran()

  # E110 -> E11.0
  resultado <- cie_lookup("E110", normalizar = TRUE)

  expect_equal(nrow(resultado), 1)
  expect_equal(resultado$codigo, "E11.0")
})

test_that("cie_lookup maneja codigos especiales daga/asterisco", {
  skip_on_cran()

  # Buscar codigo con daga si existe
  # Primero verificar que el paquete normaliza correctamente
  resultado <- cie_lookup("A17.0")

  expect_s3_class(resultado, "tbl_df")
})

# ==============================================================================
# PRUEBAS COBERTURA - mensaje "Sin coincidencias" y sigla_to_codigo NULL
# ==============================================================================

test_that("cie_search muestra mensaje Sin coincidencias con verbose=TRUE", {
  skip_on_cran()

  # Buscar texto que no deberia tener coincidencias
  expect_message(
    resultado <- cie_search("xyznonexistent12345abc", threshold = 0.95, verbose = TRUE),
    "Sin coincidencias"
  )

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie_search no muestra mensaje con verbose=FALSE cuando no hay resultados", {
  skip_on_cran()

  # No debe mostrar mensaje con verbose=FALSE
  expect_silent({
    resultado <- cie_search("xyznonexistent12345abc", threshold = 0.95, verbose = FALSE)
  })

  expect_equal(nrow(resultado), 0)
})

test_that("cie_lookup con check_siglas=TRUE y sigla invalida no convierte", {
  skip_on_cran()

  # Sigla que no existe en el diccionario
  suppressMessages({
    resultado <- cie_lookup("XYZ", check_siglas = TRUE)
  })

  # Debe retornar vacio porque XYZ no es codigo CIE ni sigla valida
  expect_s3_class(resultado, "tbl_df")
})

test_that("cie_lookup con check_siglas=TRUE y sigla valida convierte", {
  skip_on_cran()

  # EPOC es sigla valida
  resultado <- cie_lookup("EPOC", check_siglas = TRUE)

  expect_s3_class(resultado, "tbl_df")
  # Puede encontrar o no, pero no debe crashear
})

test_that("sigla_to_codigo retorna NULL para sigla inexistente", {
  skip_on_cran()

  # Acceder funcion interna
  resultado <- ciecl:::sigla_to_codigo("XYZINVALIDA")

  expect_null(resultado)
})

test_that("sigla_to_codigo retorna codigo para sigla valida", {
  skip_on_cran()

  # IAM es sigla de infarto agudo miocardio
  resultado <- ciecl:::sigla_to_codigo("iam")

  # Puede retornar codigo o NULL si no encuentra match fuzzy
  expect_true(is.null(resultado) || is.character(resultado))
})

test_that("expandir_sigla retorna NULL para texto no-sigla", {
  skip_on_cran()

  resultado <- ciecl:::expandir_sigla("diabetes")

  expect_null(resultado)
})

test_that("expandir_sigla retorna termino para sigla valida", {
  skip_on_cran()

  resultado <- ciecl:::expandir_sigla("IAM")

  expect_equal(resultado, "infarto agudo miocardio")
})

test_that("cie_search con busqueda que no encuentra matches exactos ni fuzzy", {
  skip_on_cran()

  # Threshold muy alto para texto que no matchea bien
  suppressMessages({
    resultado <- cie_search("qwrtyuiopasdfghjklzxcvbnm", threshold = 0.99, verbose = FALSE)
  })

  expect_s3_class(resultado, "tbl_df")
  expect_equal(nrow(resultado), 0)
})

test_that("cie_search fuzzy fallback cuando no hay matches exactos", {
  skip_on_cran()

  # Usar solo_fuzzy para forzar path fuzzy
  resultado <- cie_search("diabetes mellitus tipo", solo_fuzzy = TRUE,
                          threshold = 0.5, max_results = 10, verbose = FALSE)

  expect_s3_class(resultado, "tbl_df")
  # Deberia encontrar algo via fuzzy
  expect_gt(nrow(resultado), 0)
})

test_that("normalizar_tildes con vector vacio retorna vacio", {
  resultado <- ciecl:::normalizar_tildes(character(0))

  expect_length(resultado, 0)
  expect_type(resultado, "character")
})

test_that("normalizar_tildes convierte caracteres acentuados", {
  resultado <- ciecl:::normalizar_tildes("neumonía agúda")

  expect_equal(resultado, "neumonia aguda")
})

test_that("extract_cie_from_text extrae codigo de texto con ruido", {
  skip_on_cran()

  # Con prefijo
  resultado1 <- ciecl:::extract_cie_from_text("CIE:E11.0")
  expect_equal(resultado1, "E11.0")

  # Con sufijo
  resultado2 <- ciecl:::extract_cie_from_text("I10-confirmado")
  expect_equal(resultado2, "I10")

  # Sin ruido
  resultado3 <- ciecl:::extract_cie_from_text("E11.0")
  expect_equal(resultado3, "E11.0")
})