## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ciecl)
library(dplyr)

## ----datos--------------------------------------------------------------------
set.seed(42)

egresos <- data.frame(
  ID_EGRESO = 1:200,
  ANO       = sample(2018:2022, 200, replace = TRUE),
  DIAG1     = sample(
    c(
      # Codigos frecuentes en egresos DEIS (sin punto, formato original)
      "J189", "O800", "Z380", "K359", "N390",
      "I10X", "J449", "E119", "O829", "J069",
      "K922", "N185", "I509", "C509", "A099",
      # Algunos con sufijo X (convencion DEIS)
      "N40X", "K800", "I259", "J180", "E149"
    ),
    size    = 200,
    replace = TRUE,
    prob    = c(
      0.12, 0.10, 0.09, 0.07, 0.06,
      0.08, 0.06, 0.07, 0.05, 0.04,
      0.04, 0.03, 0.05, 0.03, 0.03,
      0.03, 0.03, 0.04, 0.03, 0.05
    )
  ),
  stringsAsFactors = FALSE
)

cat("Registros totales:", nrow(egresos), "\n")
cat("Codigos unicos:", length(unique(egresos$DIAG1)), "\n")
cat("Periodo:", min(egresos$ANO), "-", max(egresos$ANO), "\n\n")
head(egresos)

## ----lookup-------------------------------------------------------------------
# Filtrar registros validos (sin NA en diagnostico)
datos_filtrados <- egresos %>%
  filter(!is.na(DIAG1))

# Codigos unicos a procesar
codigos_unicos <- unique(datos_filtrados$DIAG1)
cat("Codigos unicos a procesar:", length(codigos_unicos), "\n")

# Busqueda vectorizada con cie_lookup
inicio <- Sys.time()

resultado_ciecl <- cie_lookup(
  codigo             = codigos_unicos,
  normalizar         = TRUE,
  descripcion_completa = TRUE
)

tiempo <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 2)

cat("Codigos encontrados:", nrow(resultado_ciecl), "de", length(codigos_unicos), "\n")
cat("Tiempo de busqueda:", tiempo, "segundos\n")
cat("Tasa de exito:", round(nrow(resultado_ciecl) / length(codigos_unicos) * 100, 1), "%\n\n")

# Muestra de resultados
head(resultado_ciecl[, c("codigo", "descripcion")], 5)

## ----unir---------------------------------------------------------------------
# Tabla de lookup: codigo sin punto -> descripcion completa
# cie_lookup() devuelve con punto (J18.9), la BBDD tiene sin punto (J189)
tabla_lookup <- resultado_ciecl %>%
  mutate(
    codigo_sin_punto = gsub("\\.", "", codigo),
    DIAG_COMPLETO    = paste0(codigo, " - ", descripcion)
  ) %>%
  select(codigo_sin_punto, DIAG_COMPLETO)

# Funcion para normalizar el codigo de la BBDD antes del join:
# 1. Quitar puntos si los hubiera
# 2. Quitar sufijo X final (I10X -> I10, N40X -> N40)
normalizar_codigo_bbdd <- function(codigo) {
  codigo <- gsub("\\.", "", codigo)
  codigo <- gsub("X$", "", codigo)
  return(codigo)
}

# Unir con los datos originales
datos_con_desc <- datos_filtrados %>%
  mutate(codigo_busqueda = normalizar_codigo_bbdd(DIAG1)) %>%
  left_join(tabla_lookup, by = c("codigo_busqueda" = "codigo_sin_punto")) %>%
  mutate(
    DIAG_COMPLETO = if_else(is.na(DIAG_COMPLETO), DIAG1, DIAG_COMPLETO)
  ) %>%
  select(-codigo_busqueda)

# Verificar resultado
n_con_desc <- sum(grepl(" - ", datos_con_desc$DIAG_COMPLETO), na.rm = TRUE)
pct_exito  <- round(n_con_desc / nrow(datos_con_desc) * 100, 1)

cat("Registros con descripcion CIE-10:", n_con_desc, "de", nrow(datos_con_desc), "\n")
cat("Tasa de exito:", pct_exito, "%\n\n")

# Ejemplos de diagnosticos procesados
datos_con_desc %>%
  select(DIAG1, DIAG_COMPLETO) %>%
  distinct() %>%
  head(10)

## ----perfil-------------------------------------------------------------------
perfil_diag <- datos_con_desc %>%
  filter(grepl(" - ", DIAG_COMPLETO)) %>%
  count(DIAG_COMPLETO, sort = TRUE) %>%
  mutate(
    pct       = round(n / sum(n) * 100, 1),
    pct_acum  = cumsum(pct)
  )

cat("Top 10 diagnosticos:\n\n")
head(perfil_diag, 10)

## ----cobertura----------------------------------------------------------------
top10_pct <- sum(head(perfil_diag, 10)$pct)
cat("Los 10 diagnosticos mas frecuentes representan el", top10_pct, "% del total de egresos\n")

## ----busqueda_fuzzy-----------------------------------------------------------
# Terminos clinicos comunes en egresos (con y sin typos)
terminos <- c("neumonia", "diabetis", "hipertension")

for (termino in terminos) {
  cat("Buscando:", termino, "\n")
  res <- cie_search(termino, threshold = 0.70, max_results = 3)
  if (nrow(res) > 0) {
    print(res[, c("codigo", "descripcion")])
  }
  cat("\n")
}

## ----validacion---------------------------------------------------------------
validos <- cie_validate_vector(codigos_unicos)

cat("Codigos validos:  ", sum(validos), "\n")
cat("Codigos invalidos:", sum(!validos), "\n")

if (any(!validos)) {
  cat("\nCodigos no reconocidos:\n")
  print(codigos_unicos[!validos])
}

