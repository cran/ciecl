## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ciecl)
library(dplyr)

## ----datos--------------------------------------------------------------------
set.seed(42)

# Simulación de 200 registros con formatos típicos del DEIS Chile
egresos <- data.frame(
  ID_EGRESO = 1:200,
  PACIENTE_ID = sample(1:50, 200, replace = TRUE),
  ANO       = sample(2018:2022, 200, replace = TRUE),
  DIAG1     = sample(
    c(
      "J189", "O800", "Z380", "K359", "N390",
      "I10X", "J449", "E119", "O829", "J069",
      "K922", "N185", "I509", "C509", "A099",
      "N40X", "K800", "I259", "J180", "E149"
    ),
    size    = 200,
    replace = TRUE
  ),
  stringsAsFactors = FALSE
)

head(egresos)

## ----normalizacion------------------------------------------------------------
# Limpieza y estandarización de diagnósticos en el flujo de trabajo
egresos <- egresos |>
  mutate(
    DIAG1_NORM = cie_norm(codes = DIAG1)
  )

# Comparación entre formato original y normalizado
egresos |>
  select(DIAG1, DIAG1_NORM) |>
  distinct() |>
  head(5)

## ----describe-----------------------------------------------------------------
# Integración directa de descripciones al dataframe principal
egresos_full <- egresos |>
  mutate(
    descripcion = cie_describe(DIAG1_NORM)
  )

head(egresos_full |> select(ID_EGRESO, DIAG1, descripcion))

## ----lookup-------------------------------------------------------------------
# Obtención de metadata completa vía lookup + join
metadata <- cie_lookup(
  code = unique(egresos$DIAG1_NORM),
  full_description = TRUE
)

egresos_metadata <- egresos |>
  left_join(metadata, by = c("DIAG1_NORM" = "codigo"))

## ----busqueda-----------------------------------------------------------------
# Búsqueda tolerante: "diabetis" en lugar de "diabetes"
# (por defecto se muestran los 50 resultados más parecidos;
#  ampliamos el límite porque el catálogo tiene muchos códigos de diabetes)
resultados_busqueda <- cie_search(text = "diabetis", threshold = 0.7, max_results = 100)

resultados_busqueda

## ----cruce--------------------------------------------------------------------
# ¿Qué códigos de diabetes están realmente en mi base?
codigos_diabetes <- intersect(
  resultados_busqueda$codigo,
  unique(egresos$DIAG1_NORM)
)

codigos_diabetes

## ----reporte-diabetes---------------------------------------------------------
# Reporte final: egresos por diabetes, resumidos por tipo
egresos_full |>
  filter(DIAG1_NORM %in% codigos_diabetes) |>
  count(descripcion, sort = TRUE)

## ----sin-resultados-lookup----------------------------------------------------
cie_lookup("XYZ123")

## ----sin-resultados-search----------------------------------------------------
cie_search("zzzqwerty", threshold = 0.95)

## ----validacion---------------------------------------------------------------
cie_validate_vector(c("E11.0", "XYZ123", "I10X"))

## ----comorbilidad, eval=rlang::is_installed("comorbidity")--------------------
# Requiere el paquete 'comorbidity' instalado
# Cálculo del Índice de Charlson consolidado por paciente
comorbilidades <- cie_comorbid(
  data = egresos,
  id = "PACIENTE_ID",
  code = "DIAG1",
  map = "charlson"
)

head(comorbilidades, 10)

