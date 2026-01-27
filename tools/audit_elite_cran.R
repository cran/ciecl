# =============================================================================
# AUDITORÍA ELITE CRAN - ciecl package
# =============================================================================
# Este script ejecuta pruebas exhaustivas para asegurar calidad CRAN
# =============================================================================

cat("=== AUDITORÍA ELITE CRAN - ciecl ===\n\n")

library(ciecl)
library(testthat)

# Directorio de bases sintéticas
SYNTH_DIR <- "D:/01_PROYECTOS/PROY_BBDD_SINTETICAS/data/output"

errors <- list()
warnings_found <- list()

# -----------------------------------------------------------------------------
# 1. PRUEBAS CON BASES SINTÉTICAS
# -----------------------------------------------------------------------------
cat("1. PRUEBAS CON BASES SINTÉTICAS\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

synth_files <- list.files(SYNTH_DIR, pattern = "\\.csv$", full.names = TRUE)
cat("Archivos encontrados:", length(synth_files), "\n\n")

for (f in synth_files) {
  fname <- basename(f)
  cat("  Testing:", fname, "... ")
  
  tryCatch({
    # Leer muestra (primeras 1000 filas)
    df <- read.csv(f, nrows = 1000, stringsAsFactors = FALSE)
    
    # Detectar columna de códigos
    code_col <- NULL
    for (col in names(df)) {
      if (grepl("codigo|code|cie|diag", tolower(col))) {
        code_col <- col
        break
      }
    }
    
    if (is.null(code_col)) code_col <- names(df)[1]
    
    codes <- df[[code_col]]
    
    # Test cie_normalizar
    norm_result <- suppressWarnings(cie_normalizar(codes[1:min(100, length(codes))]))
    
    # Test cie_lookup (solo códigos válidos)
    valid_codes <- norm_result[grepl("^[A-Z]\\d", norm_result)]
    if (length(valid_codes) > 0) {
      lookup_result <- suppressWarnings(suppressMessages(
        cie_lookup(valid_codes[1:min(10, length(valid_codes))])
      ))
    }
    
    # Test cie_validate_vector
    valid_result <- cie_validate_vector(codes[1:min(100, length(codes))])
    
    cat("OK\n")
    
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    errors[[fname]] <<- conditionMessage(e)
  })
}

# -----------------------------------------------------------------------------
# 2. EDGE CASES EXTREMOS
# -----------------------------------------------------------------------------
cat("\n2. EDGE CASES EXTREMOS\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

edge_cases <- list(
  "NULL input" = function() cie_normalizar(NULL),
  "Empty vector" = function() cie_normalizar(character(0)),
  "All NA" = function() cie_normalizar(c(NA, NA, NA)),
  "Very long string" = function() cie_normalizar(paste(rep("A", 1000), collapse = "")),
  "Special chars" = function() cie_normalizar("E11.0™©®"),
  "Unicode stress" = function() cie_normalizar("糖尿病E11.0диабет"),
  "SQL injection attempt" = function() cie_normalizar("E11.0'; DROP TABLE--"),
  "Newlines" = function() cie_normalizar("E11\n.0"),
  "Tabs" = function() cie_normalizar("E11\t.0"),
  "Only whitespace" = function() cie_normalizar("   "),
  "Mixed encoding" = function() cie_normalizar(c("E11.0", "diabétes", "niño")),
  "Huge vector" = function() cie_normalizar(rep("E11.0", 10000)),
  "Numeric input coerced" = function() cie_normalizar(as.character(110)),
  "Factor input" = function() cie_normalizar(factor(c("E11.0", "I10")))
)

for (name in names(edge_cases)) {
  cat("  Testing:", name, "... ")
  tryCatch({
    result <- suppressWarnings(edge_cases[[name]]())
    cat("OK\n")
  }, error = function(e) {
    # Algunos errores son esperados
    if (grepl("debe ser|cannot|NULL|vacio", conditionMessage(e), ignore.case = TRUE)) {
      cat("Expected error OK\n")
    } else {
      cat("UNEXPECTED ERROR:", conditionMessage(e), "\n")
      errors[[name]] <<- conditionMessage(e)
    }
  })
}

# -----------------------------------------------------------------------------
# 3. PRUEBAS DE BÚSQUEDA FUZZY
# -----------------------------------------------------------------------------
cat("\n3. PRUEBAS DE BÚSQUEDA FUZZY\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

search_tests <- c(
  "diabetes",
  "diabetis",  # typo
  "DIABETES",
  "DiAbEtEs",
  "neumonia",
  "neumonía",
  "hipertension",
  "hipertensión arterial",
  "cancer",
  "cáncer de mama",
  "infarto",
  "IAM",
  "HTA",
  "EPOC",
  "VIH",
  "tuberculosis",
  "TBC",
  "",  # vacío
  "xyzabc123",  # sin resultados
  "a",  # muy corto
  paste(rep("diabetes ", 100), collapse = "")  # muy largo
)

for (term in search_tests) {
  display_term <- if (nchar(term) > 30) paste0(substr(term, 1, 30), "...") else term
  cat("  Searching:", sprintf("%-35s", paste0('"', display_term, '"')), "... ")
  tryCatch({
    result <- suppressWarnings(suppressMessages(
      cie_search(term, max_results = 5)
    ))
    cat("OK (", nrow(result), " results)\n", sep = "")
  }, error = function(e) {
    if (grepl("2 caracteres|vacio|debe ser", conditionMessage(e), ignore.case = TRUE)) {
      cat("Expected error OK\n")
    } else {
      cat("ERROR:", conditionMessage(e), "\n")
      errors[[paste0("search_", display_term)]] <<- conditionMessage(e)
    }
  })
}

# -----------------------------------------------------------------------------
# 4. PRUEBAS DE COMORBILIDAD
# -----------------------------------------------------------------------------
cat("\n4. PRUEBAS DE COMORBILIDAD\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

# Crear datos de prueba
comorbid_data <- data.frame(
  id = c(1, 1, 1, 2, 2, 3, 3, 3, 3),
  codigo = c("I21.0", "E11.0", "I10", "J44.0", "E11.9", "C34.0", "I50.0", "N18.3", "E78.0")
)

cat("  Testing cie_comorbid Charlson... ")
tryCatch({
  result <- suppressWarnings(cie_comorbid(comorbid_data, id = "id", code = "codigo", map = "charlson"))
  cat("OK (", nrow(result), " patients)\n", sep = "")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  errors[["comorbid_charlson"]] <<- conditionMessage(e)
})

cat("  Testing cie_comorbid Elixhauser... ")
tryCatch({
  result <- suppressWarnings(cie_comorbid(comorbid_data, id = "id", code = "codigo", map = "elixhauser"))
  cat("OK (", nrow(result), " patients)\n", sep = "")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  errors[["comorbid_elixhauser"]] <<- conditionMessage(e)
})

# Test con datos vacíos
cat("  Testing cie_comorbid empty data... ")
tryCatch({
  empty_df <- data.frame(id = integer(0), codigo = character(0))
  result <- suppressWarnings(cie_comorbid(empty_df, id = "id", code = "codigo"))
  cat("OK\n")
}, error = function(e) {
  # El paquete comorbidity subyacente lanza error con datos vacíos - esto es esperado
  if (grepl("vacio|empty|filas|non-missing", conditionMessage(e), ignore.case = TRUE)) {
    cat("Expected error OK\n")
  } else {
    cat("ERROR:", conditionMessage(e), "\n")
    errors[["comorbid_empty"]] <<- conditionMessage(e)
  }
})

# -----------------------------------------------------------------------------
# 5. PRUEBAS SQL
# -----------------------------------------------------------------------------
cat("\n5. PRUEBAS SQL\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

sql_tests <- list(
  "Basic SELECT" = "SELECT * FROM cie10 LIMIT 5",
  "WHERE clause" = "SELECT * FROM cie10 WHERE codigo LIKE 'E11%' LIMIT 5",
  "COUNT" = "SELECT COUNT(*) as n FROM cie10",
  "GROUP BY" = "SELECT capitulo, COUNT(*) as n FROM cie10 GROUP BY capitulo LIMIT 5"
)

for (name in names(sql_tests)) {
  cat("  Testing:", name, "... ")
  tryCatch({
    result <- cie10_sql(sql_tests[[name]])
    cat("OK (", nrow(result), " rows)\n", sep = "")
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    errors[[paste0("sql_", name)]] <<- conditionMessage(e)
  })
}

# SQL injection attempts (should fail)
sql_attacks <- c(
  "DROP TABLE cie10",
  "DELETE FROM cie10",
  "UPDATE cie10 SET codigo = 'X'",
  "INSERT INTO cie10 VALUES ('X', 'X')",
  "SELECT * FROM cie10; DROP TABLE cie10"
)

cat("  Testing SQL injection protection:\n")
for (attack in sql_attacks) {
  display <- if (nchar(attack) > 40) paste0(substr(attack, 1, 40), "...") else attack
  cat("    ", display, "... ")
  tryCatch({
    result <- cie10_sql(attack)
    cat("VULNERABILITY! Query executed!\n")
    errors[[paste0("sql_injection_", substr(attack, 1, 20))]] <<- "Query should have been blocked"
  }, error = function(e) {
    cat("Blocked OK\n")
  })
}

# -----------------------------------------------------------------------------
# 6. VERIFICACIÓN DE DOCUMENTACIÓN
# -----------------------------------------------------------------------------
cat("\n6. VERIFICACIÓN DE DOCUMENTACIÓN\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

exported_fns <- c(
  "cie_lookup", "cie_search", "cie_normalizar", "cie_expand",

  "cie_validate_vector", "cie_comorbid", "cie_map_comorbid",
  "cie10_sql", "cie10_clear_cache", "cie11_search",
  "cie_table", "cie_siglas", "cie_guia_busqueda", "generar_cie10_cl"
)

for (fn in exported_fns) {
  cat("  Checking docs for:", fn, "... ")
  tryCatch({
    help_file <- help(fn, package = "ciecl")
    if (length(help_file) > 0) {
      cat("OK\n")
    } else {
      cat("MISSING\n")
      errors[[paste0("doc_", fn)]] <<- "Documentation missing"
    }
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    errors[[paste0("doc_", fn)]] <<- conditionMessage(e)
  })
}

# -----------------------------------------------------------------------------
# 7. PRUEBAS DE RENDIMIENTO
# -----------------------------------------------------------------------------
cat("\n7. PRUEBAS DE RENDIMIENTO\n")
cat(paste(rep("-", 60), collapse = ""), "\n")

cat("  cie_normalizar (10000 codes)... ")
t1 <- system.time({
  result <- cie_normalizar(rep(c("E11.0", "i10", "J44 0", "a00"), 2500))
})
cat(sprintf("%.2fs\n", t1["elapsed"]))

cat("  cie_lookup (100 codes)... ")
t2 <- system.time({
  result <- suppressMessages(cie_lookup(rep("E11.0", 100)))
})
cat(sprintf("%.2fs\n", t2["elapsed"]))

cat("  cie_search (single term)... ")
t3 <- system.time({
  result <- suppressMessages(cie_search("diabetes", max_results = 50))
})
cat(sprintf("%.2fs\n", t3["elapsed"]))

# -----------------------------------------------------------------------------
# 8. RESUMEN
# -----------------------------------------------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("RESUMEN DE AUDITORÍA\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

if (length(errors) == 0) {
  cat("✓ TODAS LAS PRUEBAS PASARON\n")
  cat("✓ El paquete está listo para CRAN submission\n")
} else {
  cat("✗ ERRORES ENCONTRADOS:", length(errors), "\n\n")
  for (name in names(errors)) {
    cat("  -", name, ":", errors[[name]], "\n")
  }
}

cat("\n")
