
> **Idioma / Language:** **Español** \|
> [English](https://github.com/ropensci/ciecl/blob/main/README.en.md)

<!-- README generado desde README.Rmd. Editar este archivo, no los .md -->

# ciecl <img src="man/figures/logo-CDSP_color.png" align="right" height="60" alt="CDSP"> <img src="man/figures/logo.png" align="right" height="139" alt="ciecl logo">

**Grupo de Ciencia de Datos para Salud Pública** \| Universidad de Chile

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/ciecl)](https://CRAN.R-project.org/package=ciecl)
[![GitHub
release](https://img.shields.io/github/v/release/ropensci/ciecl)](https://github.com/ropensci/ciecl/releases)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22906553.svg)](https://doi.org/10.5281/zenodo.22906553)
[![R-CMD-check](https://github.com/ropensci/ciecl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/ciecl/actions/workflows/R-CMD-check.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ciecl)](https://cran.r-project.org/package=ciecl)
[![Coverage](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/ropensci/ciecl/main/badges/coverage.json)](https://github.com/ropensci/ciecl/actions/workflows/test-coverage.yaml)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue)](https://docs.ropensci.org/ciecl/)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/765_status.svg)](https://github.com/ropensci/software-review/issues/765)
<!-- badges: end -->

**Clasificación Internacional de Enfermedades (CIE-10) oficial de Chile
para R**.

Paquete especializado para búsqueda, validación y análisis de códigos
CIE-10 en el contexto chileno. Incluye el catálogo oficial MINSAL/DEIS
v2018 con búsqueda optimizada, cálculo de comorbilidades y acceso a la
API CIE-11 de la OMS.

## Finalidad

`ciecl` facilita el trabajo con códigos CIE-10 en investigación y
análisis de datos de salud en Chile, eliminando la necesidad de
manipular archivos Excel manualmente. Los registros clínicos chilenos
suelen contener códigos con inconsistencias de formato (espacios,
mayúsculas, puntos faltantes); el paquete automatiza la **corrección
vectorizada de estas inconsistencias** de forma eficiente, valida los
códigos contra el catálogo oficial y permite el **cálculo de índices de
comorbilidad** (Charlson, Elixhauser) directamente sobre los
diagnósticos. Está dirigido a epidemiólogos, bioestadísticos y
científicos de datos que trabajan con registros clínicos chilenos.

Características principales:

- **Catálogo oficial CIE-10 Chile** (MINSAL/DEIS v2018) embebido como
  dataset
- **Siglas médicas chilenas** (IAM, EPOC, DM2, HTA, TBC, …)
- **API CIE-11 OMS** para búsqueda en la clasificación internacional
  vigente
- **Validación y normalización vectorizada**: acepta `E110`, `E11.0`,
  `e 11 0`, `I10-0`, etc.
- **Búsqueda fuzzy Jaro-Winkler** tolerante a errores tipográficos
- **Cálculo de comorbilidades Charlson/Elixhauser** con `comorbidity`
- **Consultas SQL directas** sobre el catálogo completo con SQLite +
  FTS5
- **Expansión jerárquica de categorías** (ej: `E11` → `E11.0`, `E11.1`,
  …, `E11.9`)

El dataset está establecido por el [Decreto
356/2017](https://www.bcn.cl/leychile/navegar?i=1112064) del MINSAL como
clasificación oficial de enfermedades. No es modificable por el paquete;
solo se actualiza por decreto institucional.

## Instalación

``` r
# CRAN
install.packages("ciecl")

# rOpenSci R-universe
install.packages("ciecl", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))

# GitHub (desarrollo)
# install.packages("pak")
pak::pak("ropensci/ciecl")
```

## Uso rápido

``` r
library(ciecl)

# Busqueda exacta por codigo
cie_lookup("E11.0")
#> # A tibble: 1 × 11
#>   codigo descripcion       categoria seccion capitulo_nombre inclusion exclusion
#>   <chr>  <chr>             <chr>     <chr>   <chr>           <chr>     <chr>    
#> 1 E11.0  Diabetes mellitu… E11 DIAB… E08-E1… Cap.04  ENFERM… <NA>      <NA>     
#> # ℹ 4 more variables: capitulo <chr>, es_daga <int>, es_cruz <int>,
#> #   uso_cl <chr>

# Multiples codigos
cie_lookup(c("E11.0", "I10", "Z00"))
#> # A tibble: 3 × 11
#>   codigo descripcion       categoria seccion capitulo_nombre inclusion exclusion
#>   <chr>  <chr>             <chr>     <chr>   <chr>           <chr>     <chr>    
#> 1 E11.0  Diabetes mellitu… E11 DIAB… E08-E1… Cap.04  ENFERM… <NA>      <NA>     
#> 2 I10    Hipertensión ese… I10 HIPE… I10-I1… Cap.09  ENFERM… <NA>      <NA>     
#> 3 Z00    Examen general e… Z00 EXAM… Z00-Z1… Cap.21  FACTOR… <NA>      <NA>     
#> # ℹ 4 more variables: capitulo <chr>, es_daga <int>, es_cruz <int>,
#> #   uso_cl <chr>

# Descripcion directa (vectorizada) para usar en mutate()
cie_describe(c("E11.0", "I10"))
#> [1] "Diabetes mellitus tipo 2 con coma" "Hipertensión esencial (primaria)"

# Ejemplo en flujo dplyr
# egresos |> dplyr::mutate(desc = cie_describe(codigo))

# Busqueda fuzzy tolerante a errores
cie_search("diabetis mellitus")
#> # A tibble: 50 × 4
#>    codigo descripcion                                            score categoria
#>    <chr>  <chr>                                                  <dbl> <chr>    
#>  1 E10    Diabetes mellitus insulinodependiente                    0.5 E10 DIAB…
#>  2 E10.0  Diabetes mellitus tipo 1 con coma                        0.5 E10 DIAB…
#>  3 E10.1  Diabetes mellitus tipo 1 con cetoacidosis                0.5 E10 DIAB…
#>  4 E10.2  Diabetes mellitus tipo 1 con complicaciones renales      0.5 E10 DIAB…
#>  5 E10.3  Diabetes mellitus tipo 1 con complicaciones oftálmicas   0.5 E10 DIAB…
#>  6 E10.4  Diabetes mellitus tipo 1 con complicaciones neurológi…   0.5 E10 DIAB…
#>  7 E10.5  Diabetes mellitus tipo 1 con complicaciones  circulat…   0.5 E10 DIAB…
#>  8 E10.6  Diabetes mellitus tipo 1 con otras complicaciones esp…   0.5 E10 DIAB…
#>  9 E10.7  Diabetes mellitus tipo 1 con complicaciones múltiples    0.5 E10 DIAB…
#> 10 E10.8  Diabetes mellitus tipo 1 con complicaciones no especi…   0.5 E10 DIAB…
#> # ℹ 40 more rows

# Siglas medicas chilenas
cie_search("IAM")
#> ℹ Sigla detectada: "IAM" -> "infarto agudo miocardio"
#> # A tibble: 50 × 4
#>    codigo descripcion                                            score categoria
#>    <chr>  <chr>                                                  <dbl> <chr>    
#>  1 I21    Infarto agudo del miocardio                                1 I21 INFA…
#>  2 I21.0  Infarto transmural agudo del miocardio de la pared an…     1 I21 INFA…
#>  3 I21.1  Infarto transmural agudo del miocardio de la pared in…     1 I21 INFA…
#>  4 I21.2  Infarto agudo transmural del miocardio de otros sitios     1 I21 INFA…
#>  5 I21.3  Infarto transmural agudo del miocardio, de sitio no e…     1 I21 INFA…
#>  6 I21.4  Infarto subendocárdico agudo del miocardio                 1 I21 INFA…
#>  7 I21.9  Infarto agudo del miocardio, sin otra especificación       1 I21 INFA…
#>  8 I23    Ciertas complicaciones presentes posteriores al infar…     1 I23 CIER…
#>  9 I23.0  Hemopericardio como complicación presente posterior a…     1 I23 CIER…
#> 10 I23.3  Ruptura de la pared cardíaca sin hemopericardio como …     1 I23 CIER…
#> # ℹ 40 more rows
```

¿No sabes cuál función usar? `cie_guide()` muestra una tabla comparativa
con el escenario, la función recomendada y un ejemplo para cada caso.

``` r
# Comorbilidades (requiere: install.packages("comorbidity"))
df |> cie_comorbid(id = "paciente", code = "diagnostico", map = "charlson")
```

## API CIE-11 (opcional)

Para usar `cie11_search()` necesitas credenciales gratuitas de la OMS
(<https://icd.who.int/icdapi>). Recomendamos guardarlas con el paquete
`keyring`:

``` r
keyring::key_set("ciecl_icd11")  # client_id:client_secret
Sys.setenv(ICD_API_KEY = keyring::key_get("ciecl_icd11"))
cie11_search("diabetes mellitus")
```

Las funciones de CIE-10 (núcleo del paquete) funcionan sin API key.

## Datos

Catálogo oficial **CIE-10 MINSAL/DEIS v2018**:

- Fuente: [DEIS](https://deis.minsal.cl) — [Centro FIC
  Chile](https://deis.minsal.cl/centrofic/)
- Dominio público según [Decreto
  356/2017](https://www.bcn.cl/leychile/navegar?i=1112064)

## Contribuir

- Reportar bugs: <https://github.com/ropensci/ciecl/issues>
- Contribuir: ver
  [CONTRIBUTING.md](https://github.com/ropensci/ciecl/blob/main/CONTRIBUTING.md)

## Licencia

MIT + datos MINSAL de dominio público.

## Autor

**Rodolfo Tasso Suazo** — <rtasso@uchile.cl> Grupo de Ciencia de Datos
para Salud Pública, Escuela de Salud Pública, Facultad de Medicina,
Universidad de Chile.

## Agradecimientos

- Diseño del logotipo del paquete: [@fje1](https://github.com/fje1).

## Enlaces

- Repositorio: <https://github.com/ropensci/ciecl>
- DEIS MINSAL: <https://deis.minsal.cl>
- API CIE-11: <https://icd.who.int/icdapi>

------------------------------------------------------------------------

<img src="man/figures/logo-CDSP_color.png" height="120" alt="Grupo de Ciencia de Datos para Salud Pública">

**Grupo de Ciencia de Datos para Salud Pública**<br> Escuela de Salud
Pública, Facultad de Medicina<br> Universidad de Chile
