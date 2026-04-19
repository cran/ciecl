# ciecl 0.9.6 (2026-04-04)

*English summary below*

## Preparacion rOpenSci

Version candidata para submission a rOpenSci. Cumple con rOpenSci Dev Guide
2025 (Capitulos 1, 5, 6, 20). R CMD check: 0 errors, 0 warnings.
Tests: 1148 PASS, 95.6% cobertura.

### Nuevas funcionalidades

* **Connection pooling SQLite**: Cache atomico con versionado en
  `get_cie10_db()`. Reutiliza conexiones activas, reconstruye si la
  BD esta corrupta o desactualizada.

* **Vectorizacion mejorada**: `cie_map_comorbid()` y `cie_normalizar()`
  refactorizados para procesamiento batch eficiente.

* **pkgdown site**: Tema limpio compatible con rOpenSci, modo oscuro
  (light-switch), logo hexagonal, favicons.

* **Vignette caso de uso**: `caso-uso-egresos` con datos simulados
  de egresos hospitalarios usando columnas esenciales DEIS.

### Documentacion y comunidad

* CONTRIBUTING.md y CODE_OF_CONDUCT.md bilingues (ingles + espanol)
* SECURITY.md bilingue
* PR template para rOpenSci
* `@family` y `@seealso` en todas las funciones exportadas
* Documentacion English-first con traducciones completas

### CI/CD

* GitHub Actions R-CMD-check multiplataforma (Windows, macOS, Ubuntu)
* Workflow de test coverage
* pkgdown deployment automatico desde main
* R-hub workflow

### Seguridad

* FTS5 parametros sanitizados contra inyeccion SQL
* Stripeo de comentarios SQL
* Validacion estricta de inputs en funciones publicas

### Fixes

* Estandarizar pipes a `%>%` (magrittr) en todo el paquete
* Corregir sintaxis invalida en `.gitignore`
* Sincronizar `codemeta.json` con DESCRIPTION
* `_pkgdown.yml` lang alineado con contenido (es)
* Eliminar `inst/extdata/cie10.db` bundled (causaba NOTE de 21.3MB)
* Compatibilidad multiplataforma: rutas absolutas, encoding, line endings
* Logo hexagonal actualizado a version G16

### Heredado de v0.9.3

* **Breaking**: `generar_cie10_cl()` ya no exportada (marcada `@noRd`)
* `comorbidity` y `gt` movidos de Imports a Suggests
* Mensajes de BD solo en sesiones interactivas (`if (interactive())`)
* `generar_cie10_cl()`: prioriza XLSX completo (39K+) sobre XLS legado
* `cie_guia_busqueda()`: corregida referencia a funcion inexistente

## English Summary

### rOpenSci Preparation

Release candidate for rOpenSci submission. Complies with rOpenSci Dev Guide
2025 (Chapters 1, 5, 6, 20). R CMD check: 0 errors, 0 warnings.
Tests: 1148 PASS, 95.6% coverage.

* **SQLite connection pooling** with atomic versioned cache
* **Vectorized** `cie_map_comorbid()` and `cie_normalizar()`
* **pkgdown site** with dark mode, hex logo, favicons
* **Hospital discharge vignette** with simulated DEIS data
* Bilingual community files (CONTRIBUTING, CODE_OF_CONDUCT, SECURITY)
* Multi-platform CI/CD (Windows, macOS, Ubuntu)
* FTS5 SQL injection protection
* Pipe standardization (`%>%`), gitignore fixes, codemeta sync

---

# ciecl 0.9.2 (2026-01-20)

## CRAN Resubmission - Example Timing

* Ejemplos de `cie_search()` envueltos en `\donttest{}` para reducir tiempo de check
* Tiempo de ejemplos reducido de ~12s a ~2s

---

# ciecl 0.9.1 (2026-01-19)
*English summary below*

## CRAN Resubmission Fixes

Correccion de errores reportados por CRAN en revision de v0.9.0.

### Bug Fixes

* **FTS5 tabla faltante**: Agregar verificacion independiente de `cie10_fts`
  - Si la BD tiene `cie10` pero no `cie10_fts` (cache corrupto), ahora se recrea
  - Soluciona error "no such table: cie10_fts" en examples/tests
  - Afecta `get_cie10_db()` en `R/cie-sql.R`

* **Link invalido README**: Cambiar link relativo `DEPENDENCIAS.md` a URL absoluta
  - CRAN no incluye archivos de raiz en paquete instalado
  - Ahora apunta a GitHub: `https://github.com/Rodotasso/ciecl/blob/main/DEPENDENCIAS.md`

* **Script audit removido de tests/**: Mover `audit_elite_cran.R` a `tools/`
  - Contenia rutas hardcodeadas locales
  - CRAN no ejecuta scripts en `tools/`

### Tests y Validacion

* 0 errores, 0 warnings en R CMD check (Windows, macOS, Linux)
* Examples de `cie_lookup()` y `cie_search()` ejecutan sin error

## English Summary

### CRAN Resubmission Fixes

* **Missing FTS5 table**: Independent verification of `cie10_fts` table
* **Invalid README link**: Changed relative link to absolute GitHub URL
* **Audit script moved**: `audit_elite_cran.R` moved from tests/ to tools/

---

# ciecl 0.9.0 (2026-01-18)
*English summary below*

## Optimizacion de Rendimiento - FTS5

Version candidata a release con mejoras significativas de rendimiento.

### Nuevas Funcionalidades

* **FTS5 Full-Text Search**: Busquedas de texto ~100x mas rapidas
  - Tabla virtual SQLite FTS5 para pre-filtrado en SQL
  - `cie_search()` ahora filtra en SQLite antes de traer datos a R
  - Reduccion de tiempo de busqueda de ~30s a <1s

* **Vectorizacion de `cie_lookup()`**: Query batch con IN clause
  - Una sola conexion para multiples codigos (antes N conexiones)
  - Mejora significativa para procesamiento de datasets grandes

### Seguridad

* **Sanitizacion SQL injection en FTS5**: Solo caracteres alfanumericos permitidos
* Proteccion contra inyeccion de comandos FTS5 maliciosos

### Limpieza de Proyecto

* Fixtures de prueba movidos fuera del paquete (reduccion ~2.5 MB)
* Eliminado `helper-fixtures.R` no utilizado

### Tests y Validacion

* **1088 tests** pasando
* **0 errores, 0 warnings** en R CMD check
* Cobertura de tests mantenida

## English Summary

### Performance Optimization - FTS5

Release candidate with significant performance improvements.

* **FTS5 Full-Text Search**: ~100x faster text searches
* **Vectorized `cie_lookup()`**: Batch queries with IN clause
* **SQL injection protection**: Sanitized FTS5 inputs
* **Project cleanup**: Test fixtures moved outside package

Tests: 1088 passing | R CMD check: 0 errors, 0 warnings

---

# ciecl 0.8.0 (2026-01-17)

*English summary below*

## Suite de Tests Exhaustiva - Cobertura CRAN

Aumento masivo de cobertura de tests para cumplir estandares CRAN:

* **850 tests** (antes 305)
* **96.41% cobertura** (meta CRAN >80%)
* **0 errores, 0 warnings, 0 notes** en R CMD check

### Nuevos Archivos de Tests

* `test-cie-data.R`: Tests para funciones de generacion de datos
  - `parsear_cie10_minsal()`: Parsing de XLS MINSAL
  - `generar_cie10_cl()`: Generacion de dataset
  - Validacion de dataset cie10_cl cargado
  - Verificacion de estructura, unicidad y formatos

* `test-data-integrity.R`: Validacion de integridad del dataset cie10_cl
  - Estructura de columnas correcta
  - 39,877 codigos unicos
  - Sin NAs ni valores vacios en campos criticos
  - Formato CIE-10 valido (>95% de codigos)
  - Codigos especificos conocidos (diabetes, hipertension, neoplasias, IAM)

* `test-utils-internal.R`: Tests de funciones internas
  - `normalizar_tildes()`: Remocion de tildes y caracteres especiales
  - `get_siglas_medicas()`: Diccionario de 88 siglas medicas
  - `expandir_sigla()`: Expansion de siglas a terminos de busqueda
  - `extract_cie_from_text()`: Extraccion de codigos de texto con ruido
  - `cie10_empty_tibble()`: Tibble vacio con estructura correcta
  - `sigla_to_codigo()`: Conversion de siglas a codigos CIE-10
  - `cie_lookup_single()`: Busqueda interna de codigo unico

* `test-comorbid-validation.R`: Validacion exhaustiva de comorbilidades
  - Calculo Charlson con datos sinteticos
  - Calculo Elixhauser con categorias validadas
  - Manejo de NA y codigos vacios
  - Categorizacion correcta por patologia

* `test-api-mock.R`: Tests de API sin conexion real
  - Validacion de parametros
  - Manejo de errores
  - Formato de API key
  - Estructura de respuesta

### Tests Expandidos

* `test-robustness.R`: +40 nuevos tests
  - `get_cie10_db()`: Conexion DBI, creacion de indices
  - `cie10_clear_cache()`: Eliminacion idempotente
  - `cie10_sql()`: Bloqueo de keywords peligrosos (DROP, DELETE, UPDATE, etc.)
  - Proteccion contra SQL injection

* `test-cie-search.R`: +15 nuevos tests
  - Edge cases de threshold (0.0, 1.0)
  - Modo `solo_fuzzy=TRUE`
  - Busqueda en campo `inclusion`
  - Validacion de parametros incorrectos
  - Normalizacion de tildes
  - `cie_guia_busqueda()`

* `test-cie-table.R`: +8 nuevos tests
  - Parametro `interactive`
  - Expansion jerarquica
  - Columnas esperadas
  - Manejo de codigos invalidos

* `test-cie-utils.R`: +36 nuevos tests
  - `cie_normalizar()` con `buscar_db=TRUE` y edge cases
  - `cie_validate_vector()` con `strict=TRUE` y validacion DB
  - `cie_expand()` con valores vacios/NA/NULL
  - Manejo de caracteres especiales Unicode
  - Cobertura: 94.55%

* `test-cie-api.R`: +12 nuevos tests
  - Tests con API real (skip_on_cran, requiere ICD_API_KEY)
  - Parsing de resultados JSON
  - Manejo de busquedas sin resultados
  - Limpieza de tags HTML
  - Soporte idiomas es/en

* `test-cie-sql.R`: +15 nuevos tests
  - Queries con WHERE, LIKE, GROUP BY, ORDER BY
  - Bloqueo de ALTER, CREATE, TRUNCATE, ATTACH, PRAGMA
  - Queries con saltos de linea

## Mejoras de Estabilidad

* API estable sin breaking changes
* Manejo robusto de errores en todas las funciones
* Conexiones DB con `on.exit()` garantizado
* Validacion de inputs en funciones publicas

## English Summary

### First Stable Release

First stable version of `ciecl`, the R package for working with Chile's
official ICD-10 classification (MINSAL/DEIS v2018).

### Comprehensive Test Suite - CRAN Ready

* **850 tests** (up from 305)
* **96.41% coverage** (CRAN target >80%)
* **0 errors, 0 warnings, 0 notes** in R CMD check

New test files for:
- Data generation functions (parsear_cie10_minsal, generar_cie10_cl)
- Dataset integrity validation
- Internal functions
- Comorbidity validation
- API mocking and real API tests
- Expanded robustness tests
- SQL security tests
- Edge cases coverage

Coverage by file:
- cie-search.R: 99.38%
- cie-sql.R: 97.37%
- cie-comorbid.R: 96.77%
- cie-utils.R: 94.55%
- cie-table.R: 92.00%


# ciecl 0.1.0 (2026-01-17)

*English summary below*

## Tests de Comorbilidad

Validacion exhaustiva de funciones `cie_comorbid()` y `cie_map_comorbid()` con bases sinteticas:

### Bases de Prueba Generadas
* `15_comorbid_charlson.csv`: 500 pacientes, 2007 diagnosticos
  - Codigos de 17 categorias Charlson (MI, CHF, PVD, CEVD, demencia, EPOC, etc.)
  - Scores validados: rango 1-15 puntos
* `16_comorbid_elixhauser.csv`: 500 pacientes, 3219 diagnosticos
  - Codigos de 31 categorias Elixhauser
  - Categorias por paciente: 1-8
* `17_comorbid_mixto.csv`: 200 pacientes, edge cases
  - 50 pacientes con NA en diagnosticos (ignorados correctamente)
  - 50 pacientes con codigos invalidos mezclados
  - 50 pacientes con un solo codigo
  - Validacion de robustez ante datos sucios

### Resultados de Validacion
* `cie_comorbid(map="charlson")`: Calculo correcto de score_charlson
* `cie_comorbid(map="elixhauser")`: Calculo correcto de categorias booleanas
* Manejo robusto de NA y codigos vacios (warnings informativos)
* Codigos invalidos no afectan calculo de pacientes validos

### Codigos CIE-10 Testeados por Categoria
* **Infarto miocardio**: I21.0-I22.9, I25.2
* **Insuficiencia cardiaca**: I50.0-I50.9, I11.0, I13.0
* **Diabetes**: E10.0-E14.9 (sin/con complicaciones)
* **Enfermedad renal**: N18.1-N18.9, N19
* **EPOC**: J40-J44.9
* **Cancer**: C18.x, C34.x, C50.x, C61, C67.x, C71.x, C73
* **VIH/SIDA**: B20-B24

## Bug Fixes

* `cie_lookup_single()`: Corregir emisión de warning para rangos invertidos
  - Usar `paste0()` en lugar de concatenación con comas en `warning()`
  - Tests ahora suprimen warnings esperados con `suppressWarnings()`

## Mejoras

* `cie_lookup()`: Documentación mejorada del parámetro `extract`:
  - Advertencia clara de usar `extract=TRUE` solo con códigos escalares
  - Ejemplos de uso correcto con prefijos/sufijos
  - Evita confusiones con vectores múltiples

* `extract_cie_from_text()`: Función interna para extraer código CIE-10 de texto con ruido
  - Soporta prefijos: "CIE:E11.0" -> "E11.0"
  - Soporta sufijos: "E11.0-confirmado" -> "E11.0"
  - Soporta ambos: "CIE:E11.0-prov" -> "E11.0"

## Tests

* `test-cie-search.R`: Modificar test de rangos invertidos para usar `suppressWarnings()`
* `test-edge-cases.R`: Modificar test de rangos para usar `suppressWarnings()`

## English Summary

### Comorbidity Tests
Exhaustive validation of `cie_comorbid()` and `cie_map_comorbid()` with synthetic datasets:
* 500-patient Charlson test base with 2007 diagnoses
* 500-patient Elixhauser test base with 3219 diagnoses
* 200-patient mixed test base with edge cases (NA values, invalid codes)

### Bug Fixes
* `cie_lookup_single()`: Fix warning emission for inverted ranges

### Improvements
* `cie_lookup()`: Improved documentation for `extract` parameter
* `extract_cie_from_text()`: Internal function to extract ICD-10 codes from noisy text


# ciecl 0.1.0.9000 (desarrollo - historico)

## Nuevas funcionalidades

* `cie_search()`: Soporte para **88 siglas medicas** comunes en Chile:
  - Cardiovasculares: IAM, HTA, ACV, FA, ICC, TEP, TVP
  - Respiratorias: TBC, EPOC, NAC, SDRA
  - Metabolicas: DM, DM1, DM2, ERC, IRC
  - Infecciosas: VIH, ITU, ITS
  - Oncologicas: CA, LMA, LMC, LLA, LLC
  - Neurologicas: TEC, EPI, EM, ELA
  - Y muchas mas (ver `cie_siglas()`)

* `cie_search()`: Tolerancia a tildes mejorada:
  - "neumonia" ahora encuentra "neumonía"
  - "rinon" encuentra "riñón"
  - "corazon" encuentra "corazón"

* `cie_search()`: Busqueda por subcadena como estrategia principal:
  - Mas rapido y preciso para terminos exactos
  - Fuzzy search como fallback para typos

* `cie_siglas()`: Nueva funcion para listar todas las siglas medicas soportadas

## Datos

* Dataset `cie10_cl`: Agregados 4 codigos COVID-19 de actualizaciones OMS 2021:
  - U08.9: Historia personal de COVID-19
  - U09.9: Condicion post COVID-19 (COVID prolongado)
  - U10.9: Sindrome inflamatorio multisistemico (PIMS)
  - U12.9: Efecto adverso de vacunas COVID-19 (ESAVI)
  - Total codigos: 39,877 (antes 39,873)

## Mejoras

* `cie_normalizar()`: Manejo robusto de caracteres especiales comunes en datos clinicos:
  - Espacios internos (E 11 0 -> E11.0)
  - Guiones en lugar de puntos (I10-0 -> I10.0)
  - Puntos multiples consecutivos (E..11 -> E.11)
  - Puntos iniciales (.I10 -> I10)
  - Simbolos daga y asterisco de codificacion dual (A17.0† -> A17.0, G01* -> G01)

* `cie_normalizar()`: Ahora elimina automaticamente el sufijo "X" de codigos CIE-10
  (ej. I10X -> I10, J00X -> J00). Esto permite trabajar con codigos que usan "X"
  para indicar ausencia de subcategoria adicional.

* `cie_normalizar()`: Preserva X en codigos largos (>5 chars) donde es placeholder
  obligatorio del 7o caracter de extension en trauma/lesiones (ej. S72X01A).
  Esto asegura compatibilidad con codificacion ICD-10-CM.

# ciecl 0.1.0 (BETA)

> **NOTA**: Esta es una version beta en desarrollo activo. La API puede cambiar
> antes de la version estable 1.0.0.

## Funcionalidades principales

* Sistema de busqueda SQL optimizado con SQLite persistente
* Busqueda fuzzy con algoritmo Jaro-Winkler para matching de terminos medicos
* Calculo de comorbilidades Charlson y Elixhauser adaptadas a codigos chilenos
* Integracion con API CIE-11 de la OMS
* Validacion y expansion de codigos jerarquicos CIE-10
* Tablas interactivas con paquete gt (opcional)
* Dataset completo CIE-10 Chile oficial MINSAL/DEIS v2018

## Funciones exportadas

### Busqueda y consulta
* `cie_lookup()` - Busqueda exacta por codigo (vectorizada)
* `cie_search()` - Busqueda fuzzy de terminos medicos
* `cie10_sql()` - Consultas SQL directas sobre CIE-10
* `cie11_search()` - Busqueda en API CIE-11 OMS (requiere credenciales)

### Utilidades
* `cie_expand()` - Expansion jerarquica de categorias
* `cie_validate_vector()` - Validacion formato de codigos
* `cie_normalizar()` - Normalizacion de codigos
* `cie10_clear_cache()` - Limpiar cache SQLite

### Comorbilidades (requiere paquete comorbidity)
* `cie_comorbid()` - Calculo de indices de comorbilidad
* `cie_map_comorbid()` - Mapeo de categorias de comorbilidad

### Visualizacion (requiere paquete gt)
* `cie_table()` - Tablas interactivas GT

### Generacion de datos
* `generar_cie10_cl()` - Generar dataset desde XLS/XLSX MINSAL (requiere readxl)

## Dataset incluido

* `cie10_cl` - **39,873 codigos** CIE-10 Chile oficial MINSAL/DEIS v2018
  - Incluye categorias (3 digitos) y subcategorias (4+ digitos)
  - Columnas: codigo, descripcion, categoria, inclusion, exclusion, capitulo, es_daga, es_cruz

## Estado del desarrollo

* Dataset generado y validado: 39,873 registros
* Tests unitarios: 8 archivos de tests
* R CMD check: 0 errores, 0 warnings
* Documentacion completa con roxygen2
* Vignette funcional incluida

## Notas para usuarios beta

Esta version esta siendo probada activamente. Por favor reporta cualquier
problema en: <https://github.com/RodoTasso/ciecl/issues>

## Primera release

Primera version beta del paquete ciecl para trabajar con Clasificacion
Internacional de Enfermedades CIE-10 de Chile en R.

Fuente de datos: [Centro FIC Chile DEIS](https://deis.minsal.cl/centrofic/)
