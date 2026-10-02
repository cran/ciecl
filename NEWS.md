# ciecl 1.0.0 (2026-09-22)

*English summary below*

Primera versión del paquete aprobada en la [revisión por pares de
rOpenSci](https://github.com/ropensci/software-review/issues/765) y
transferida a la organización [ropensci](https://github.com/ropensci/ciecl).
El paquete se instala además desde el [R-universe de
rOpenSci](https://ropensci.r-universe.dev) y su documentación se publica en
<https://docs.ropensci.org/ciecl/>.

Cambios al usuario consolidados de la revisión:

* **Validación de entradas inválidas**: las funciones públicas abortan con
  errores tipados de clase `ciecl_invalid_input` (mensajes claros en lugar
  de errores base de R): `cie_search()` valida `threshold`/`max_results`;
  `cie_lookup()` exige `extract` escalar; `cie_expand()` valida `code` (con
  `NA` abortando en lugar de retornar `character(0)` silenciosamente);
  `cie_table()` exige un único código y rechaza `NA`; `cie_short()` valida
  `category`; `cie_describe()` valida y coacciona `default`.
* **Orden determinista en `cie_lookup()`**: el modo vector exacto
  (`IN (...)`) ahora incluye `ORDER BY codigo`, igual que los demás caminos
  de la función.
* **Rangos inclusivos en `cie_lookup()`**: un rango como `"E10-E14"` ya no
  excluye silenciosamente las subcategorías del límite superior.
* **Robustez de `cie10_sql()`**: una consulta `SELECT` legítima que
  comienza con un comentario ya no aborta como "no SELECT".
* **`cie_search()` con texto de solo símbolos** (p. ej. `"!!"`): ya no
  deriva en un error interno.

*First package version approved at [rOpenSci peer
review](https://github.com/ropensci/software-review/issues/765) and
transferred to the [ropensci](https://github.com/ropensci/ciecl)
organization. The package can also be installed from the [rOpenSci
R-universe](https://ropensci.r-universe.dev) and its documentation is
published at <https://docs.ropensci.org/ciecl/>. Consolidated user-facing
changes from the review: invalid inputs now abort with typed errors of
class `ciecl_invalid_input` across public functions; `cie_lookup()` is
deterministic (`ORDER BY codigo`) in exact vector mode and its ranges now
include the upper limit's subcategories; `cie10_sql()` no longer rejects a
legitimate `SELECT` starting with a comment; and `cie_search()` handles
symbol-only text.*

El ciclo de desarrollo previo (0.9.8 en desarrollo, 2026-04-25 →
2026-09-16) se publica íntegramente como 1.0.0; su detalle se conserva en
las secciones fechadas siguientes.

## Correcciones menores — nits de auditoría (2026-09-16)

Sin cambios en la API pública; dos cambios de comportamiento acotados:
`cie_short()` ahora valida `category` y `cie_table(NA)` aborta como
input inválido en lugar de reportar "código no encontrado: NA".

* **Validaciones endurecidas** (clase `ciecl_invalid_input`):
  `cie_short()` valida que `category` sea NULL o un string escalar no-NA
  (antes un vector o numérico derivaba en un error duro no tipado);
  `cie_table()` rechaza `code = NA` como input inválido (antes caía en
  "Código no encontrado: NA").
* **Orden determinista en `cie_lookup()`**: el modo vector exacto
  (`IN (...)`) ahora incluye `ORDER BY codigo`, igual que los demás
  caminos de la función.
* **Documentación interna corregida**: el `@returns` de
  `sigla_to_codigo()` refleja que retorna el primer código (string de
  longitud 1) o NULL.
* **Limpieza de metadatos**: se completa la eliminación del
  `@importFrom dplyr select` sin uso real (los usos en `R/` ya eran
  calificados `dplyr::select()`); `writexl` sale de `Suggests` (sin uso
  ejecutable en el paquete) y de las tablas de instalación de las
  viñetas; se verificó que `Config/roxygen2/version` en DESCRIPTION es
  el mecanismo estándar del roxygen2 vigente (reemplaza a
  `RoxygenNote`; `document()` la re-escribe automáticamente) y se
  mantiene con el valor correcto; `codemeta.json` registra
  `dateModified`.
* **README y viñetas bilingües**: los chunks de instalación y
  comorbilidad del README ahora respetan la condicional de idioma
  (el README en inglés ya no muestra comentarios en español); el
  encabezado final de la viñeta en inglés se alinea al registro de la
  española ("Collaboration and Support").
* **Tests**: nuevas aserciones para el warning de rango invertido en
  `cie_lookup()`, los errores tipados de `threshold`/`max_results` en
  `cie_search()`, la coerción de `default = NA_real_`/`NaN` en
  `cie_describe()` y el NULL del camino sin match de `sigla_to_codigo()`;
  literales non-ASCII de `test-encoding.R` convertidos a escapes
  `\uXXXX`.

*Input validation hardened (class `ciecl_invalid_input`): `cie_short()`
requires `category` to be NULL or a non-NA length-1 string, and
`cie_table(NA)` now aborts as invalid input instead of reporting
"code not found: NA". Exact vector mode in `cie_lookup()` now sorts by
`codigo` for deterministic output. Metadata cleanup: the unused
`@importFrom dplyr select` removal is completed (all uses were already
qualified `dplyr::select()`), `writexl` leaves `Suggests` (no executable
use in the package), `Config/roxygen2/version` in DESCRIPTION was
verified to be the standard mechanism of the current roxygen2 (it
replaces `RoxygenNote` and is rewritten automatically by `document()`)
and is kept with its correct value, and `codemeta.json` records
`dateModified`. README install and
comorbidity chunks now follow the language conditional, and the English
vignette's closing heading matches the Spanish register
("Collaboration and Support"). Tests added for the inverted-range
warning, typed `threshold`/`max_results` errors, `default` coercion in
`cie_describe()`, and the NULL no-match path of `sigla_to_codigo()`.*

## Correcciones menores — nits de auditoría (2026-09-15)

Sin cambios en la API pública; un cambio de comportamiento acotado:
`cie_table()` ahora requiere un único código; pasar un vector de largo
>1 (uso nunca documentado) aborta con error claro en lugar del error
de `gt`.

* **Bug fix — `cie10_sql()`**: una consulta `SELECT` legítima que
  comenzaba con un comentario (`--` o `/* */`) abortaba como "no
  SELECT"; la validación ahora corre después de remover comentarios y
  strings.
* **Validaciones endurecidas** (clase `ciecl_invalid_input`):
  `cie_table()` valida que `code` sea un string de longitud 1;
  `cie_describe()` valida que `default` sea character escalar o `NA`.

*Bug fix: `cie10_sql()` no longer rejects a legitimate `SELECT` that
starts with a comment (`--` or `/* */`); the check now runs after
stripping comments and string literals. Input validation hardened
(class `ciecl_invalid_input`): `cie_table()` requires `code` to be a
length-1 string (a behavior change: a length >1 vector, never a
documented use, now aborts with a clear error instead of the `gt`
error) and `cie_describe()` requires `default` to be a scalar
character or `NA`.*

## Correcciones de robustez — auditoría interna (2026-09-13)

Primera tanda (Fase A) de la auditoría interna de `R/`. Sin cambios en la
API pública; un cambio de comportamiento acotado: `cie_expand(NA)` ahora
aborta con error claro en lugar de retornar `character(0)` silenciosamente.

* **Bug fix — rangos en `cie_lookup()`**: un rango como `"E10-E14"`
  excluía silenciosamente las subcategorías del límite superior
  (`E14.0`–`E14.9`), porque `BETWEEN` con collation BINARY compara
  lexicográficamente (`'E14.9' > 'E14'`). La consulta ahora cubre el
  prefijo del límite superior, por lo que el rango incluye todas las
  subcategorías.
* **Validaciones endurecidas** (errores claros con clase
  `ciecl_invalid_input` en lugar de errores base de R): `cie_lookup()`
  aborta si `extract = TRUE` recibe más de un código (el contrato
  documentado es escalar); `cie_expand()` valida que `code` sea un string
  escalar; `cie_search()` valida tipo y ausencia de `NA` en `threshold` y
  `max_results`; `cie11_search()` valida `NA` en `max_results`.
* **`cie_search()` — esquema de salida estable**: la columna `uso_cl`
  ahora está presente en todos los caminos internos (FTS y fallbacks), y
  `only_uso_cl = TRUE` filtra los códigos legado **antes** de aplicar el
  límite `max_results` (antes podían consumir cupo del límite y truncar
  resultados vigentes).
* **`cie_lookup()` vectorial**: los códigos con caracteres inválidos o no
  encontrados se informan en un mensaje agregado (como ya hacía el modo
  escalar), en lugar de descartarse en silencio.
* **`cie_guide()`**: la tabla de orientación recomendaba el argumento
  deprecado `expandir = TRUE`; ahora indica el vigente `expand = TRUE`.

Segunda tanda (Fase B) de la misma auditoría: ítems menores de bajo
riesgo. Sin cambios en la API pública.

* **Hardening de `cie10_sql()`**: el escaneo de palabras clave bloqueadas
  ahora se aplica sobre la consulta ya limpia de literales de texto y
  comentarios, eliminando falsos positivos en `SELECT` legítimas (p. ej.
  `LIKE '%drop%'`); las protecciones vigentes no cambian.
* **`CIECL_CACHE_DIR=""`**: una variable de entorno definida pero vacía
  ahora se trata como no definida (fallback a `tools::R_user_dir()`);
  antes derivaba en escribir la caché en el directorio de trabajo.
* **`cie_norm()`**: `"E11.X"` (convención "no especificada") ya no queda
  como `"E11."` malformado; al remover la `X` final se remueve también el
  punto previo si lo hay.
* **Caché SQLite**: la construcción y eliminación de la caché ahora
  chequean el retorno de `file.rename()`/`file.remove()` y advierten con
  un mensaje informativo si el sistema operativo rechaza la operación
  (típico lock de archivo en Windows), en lugar de continuar en silencio.
* **`cie_comorbid()`**: la documentación del valor de retorno ahora indica
  correctamente `tibble` (antes decía `data.frame`), y `data`, `id` y
  `code` se validan al inicio con errores tipados `ciecl_invalid_input`.
* **`cie_search()` con texto de solo símbolos** (p. ej. `"!!"`): el camino
  sin candidatos para la búsqueda difusa ahora retorna de inmediato un
  tibble vacío con el esquema correcto; antes calculaba
  `mean(numeric(0))` y propagaba un `NaN` silencioso en los scores.
* **Limpieza interna**: eliminados `@importFrom` sin uso real
  (`pull`, `rowwise`, `tribble`, `matches`, `select`); `NAMESPACE`
  regenerado.

*Bug fix: code ranges in `cie_lookup()` (e.g. `"E10-E14"`) now include the
upper bound's subcategories (`E14.x`), previously dropped silently by the
lexicographic `BETWEEN`. Input validation hardened (`extract = TRUE`
scalar-only, scalar `cie_expand()` — with `NA` now aborting instead of
silently returning `character(0)` —, typed `threshold`/`max_results` with
explicit `NA` guards in `cie_search()` and `cie11_search()`).
`cie_search()` output schema is now stable (`uso_cl` present on every
internal path) and `only_uso_cl = TRUE` filters legacy codes before the
`max_results` cap. Vector-mode `cie_lookup()` reports invalid/not-found
codes in a single aggregate message, and `cie_guide()` now points to the
current `expand` argument instead of the deprecated `expandir`.*

*Phase B (same audit, low-risk items): `cie10_sql()` keyword scanning now
ignores string literals and comments (no more false positives on
legitimate `SELECT`s, same protections); an empty `CIECL_CACHE_DIR` falls
back to `tools::R_user_dir()` instead of writing to the working directory;
`cie_norm("E11.X")` no longer leaves a dangling dot; cache build/clear now
warn if `file.rename()`/`file.remove()` fail; `cie_comorbid()` docs
correctly state the tibble return and `data`/`id`/`code` are validated up
front; `cie_search()` with a symbols-only text returns an empty tibble
with the correct schema instead of a silent `NaN`; unused `@importFrom`
entries removed.*

## Respuesta a comentarios rOpenSci #765 (2026-09-08)

Cambios derivados del comentario de Maëlle del 2026-09-07. Sin cambios en
la API pública.

* **Ejemplos ejecutados en el sitio pkgdown**: los ejemplos que solo
  corrían en sesiones interactivas ahora también se ejecutan al construir
  el sitio (`rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"),
  "true")`), de modo que las páginas de referencia muestran la salida
  real de las funciones. Los ejemplos de `cie11_search()` usan cassettes
  de `vcr` (sin red ni credenciales); se agregó un cassette con causas
  frecuentes de egreso en Chile (diabetes mellitus, hipertensión
  esencial, neumonía).
* **Nuevo test del rebuild de caché por versión**: simula una
  actualización del paquete con `testthat::local_mocked_bindings()`
  sobre `utils::packageVersion()` y verifica que la caché se reconstruye
  (sugerencia de la revisora).
* `Config/Needs/website` ahora incluye `vcr` explícitamente, garantizando
  su instalación en el build del sitio.

*Examples on the pkgdown site now run at build time (interactive or
`IN_PKGDOWN`), reference pages show real output, `cie11_search()`
examples replay enriched vcr cassettes, and a new test pins the
cache-rebuild-on-version-mismatch behaviour using
`testthat::local_mocked_bindings()`.*

## Poda y endurecimiento de la suite de tests (2026-09-05 / 2026-09-06)

Reestructuración completa de la suite de tests, en el contexto de la
revisión rOpenSci #765. Sin cambios en `R/` ni en la API pública.

* **457 → 372 tests** con la cobertura subiendo de 97,72 % a 98,62 %:
  cero líneas de `R/` perdidas, verificado con covr línea por línea en
  cada fase de la poda.
* **Cero HTTP real en la suite**: los tests de la API CIE-11 de la OMS
  migraron a mocks de `httr2`; el único test con red real queda protegido
  con `skip_on_cran()` + `skip_if_offline()` + credencial de la API.
* **Tests de caché aislados**: los que operaban sobre el caché real de la
  usuaria ahora corren en directorios temporales vía `CIECL_CACHE_DIR` +
  `withr`; los tests de ciclo de vida del caché viven en
  `test-cie-sql-cache.R`.
* **Aserciones endurecidas**: expectativas débiles (`expect_gte()`,
  conteos genéricos) reemplazadas por valores exactos verificados
  empíricamente contra el paquete `comorbidity`; duplicados y flujos E2E
  redundantes eliminados; un snapshot por mensaje de error.
* **Política "canario CRAN"**: documentada en `tests/testthat/setup.R`;
  cada archivo clave mantiene al menos un test ligero que corre también en
  CRAN (sin red, sin paquetes opcionales, escribiendo solo en tempdir).
* Nuevo archivo `test-cie-map-comorbid.R` con los tests de
  `cie_map_comorbid()`, extraídos de `test-cie-comorbid.R` (que baja del
  límite de 600 líneas).

## Revisión rOpenSci — seguimiento de comentarios de Maëlle Salmon (2026-08-27)

Tercera tanda, a partir del seguimiento de @maelle en el issue rOpenSci
#765. Sin cambios en la API pública.

* **Guía de inicio mejorada**: la frase sobre `mutate()` quedó en lenguaje
  simple; el cruce entre la búsqueda difusa y los códigos presentes en la
  base ahora se muestra con código ejecutable (`intersect()`), explicitando
  de dónde salen `E11.9` y `E14.9`; la guía cierra el caso de uso filtrando
  los egresos y resumiéndolos por tipo de diabetes. Los mismos cambios se
  aplicaron a la gemela en inglés `case-study-discharges.Rmd`, que queda
  sincronizada con `ciecl.Rmd`.
* **Caché SQLite**: la ayuda de `cie10_clear_cache()` ahora indica que la
  reconstrucción tras una actualización del paquete es automática (la caché
  guarda la versión del paquete en la tabla `cie10_meta` y se reconstruye
  sola en el primer uso si difiere); los casos de uso manual que quedan son
  caché corrupta o liberación de disco. La ayuda de `cie10_disconnect()`
  aclara que la desconexión manual solo es necesaria al borrar el `.db` por
  fuera del paquete o al cerrar procesos batch largos. Tres tests nuevos
  cubren el ciclo de reconstrucción por versión.
* **`cie11_search()`**: la condición del ejemplo con `vcr` se movió a
  `@examplesIf`, de modo que ya no aparece en la documentación visible.
* **Viñetas de instalación (ES/EN)**: párrafo inicial que explica para
  quién es la guía (personas que trabajan con bases de datos de salud y
  pueden estar aprendiendo R) y quién puede saltársela.

## Revisión rOpenSci — comentarios de Maëlle Salmon ms01–ms20 (2026-08-17)

Segunda tanda de correcciones de la revisión formal en el issue rOpenSci
#765, a partir de los comentarios de @maelle. Cierra todos los ítems
pendientes de la revisión.

* **`cie11_search()`**: nuevo argumento `api_key = get_icd_api_key()` con la
  nueva función exportada `get_icd_api_key()`, que centraliza la lectura de
  las credenciales de la API OMS siguiendo el patrón de `httr2`. La ayuda
  gana una subsección "Seguridad de la API key" y las viñetas de instalación
  (ES/EN) agregan la nota correspondiente (ms17).
* **Ejemplos offline con `vcr`**: los ejemplos de `cie11_search()` ahora
  corren sin conexión usando cassettes de `vcr` (>= 2.0.0, agregado a
  `Suggests`); cassette en `inst/_vcr/cie11_search.yml` y
  `tests/testthat/setup-vcr.R` con `filter_sensitive_data` (ms19).
* **Documentación bilingüe**: español primero en CONTRIBUTING y README;
  enlaces y orden de bullets corregidos; tildes en el índice de referencia
  del sitio pkgdown y en mensajes de `cli` (ms01–ms08).
* **Viñetas**: instalación solo con `pak`; nueva guía de inicio con caso de
  uso real que reemplaza a `caso-uso-egresos.Rmd`; nueva viñeta en inglés
  `vignettes/ciecl-en.Rmd` (ms10–ms13).
* **Sitio pkgdown**: footer de `_pkgdown.yml` corregido según el PR #15 de
  la revisora (`developed_by` escalar) (ms09).
* **CONTRIBUTING.md**: nueva sección "Declaración de asistencia con IA"
  (español e inglés) (ms14).
* **Mejoras de código interno**: uso de `anyNA()`, `nzchar()` y variables
  explicativas en las funciones revisadas (ms15, ms16, ms18).
* **Tests**: aserciones más específicas (`expect_named()`, `expect_length()`,
  `expect_type()`, `expect_all_false()`) y eliminación de `:::` en la suite
  de tests (ms20).

## Revisión rOpenSci — comentarios de yabellini ybs08–ybs36 (2026-06-23)

Tanda de correcciones a partir de la revisión formal de empaquetado de
@yabellini en el issue rOpenSci #765. Sin cambios en la API pública: no se
agregan ni eliminan funciones exportadas, no cambian firmas ni se rompe
compatibilidad.

* **Bug de documentación (`cie10_cl`)**: la columna `uso_cl` estaba
  documentada como lógica, pero en realidad es `character` con las categorías
  `"principal"`, `"legado"`, `"etiologico"`, `"causa_externa"` y
  `"causa_externa | principal"`. Corregido el `@format` y el ejemplo roto
  `subset(cie10_cl, uso_cl == TRUE)`, que devolvía un conjunto vacío (ybs08,
  ybs09).
* **`cie10_empty_tibble()`**: se agrega la columna `uso_cl` al esquema del
  tibble vacío interno, evitando inconsistencias al concatenar resultados
  (ybs36).
* **Validación de parámetros obligatorios**: `cie_norm()`,
  `cie_validate_vector()`, `cie_expand()`, `cie_table()`, `cie_lookup()`
  y `cie_search()` ahora emiten un error en castellano vía
  `cli::cli_abort()` cuando se las llama sin su argumento obligatorio,
  en lugar del mensaje en inglés de `rlang`/R base (ybs19, ybs22, ybs24,
  ybs27, ybs30).
* **`cie10_sql()`**: validación explícita de `query` (tipo character,
  longitud 1, no NA), captura de errores de `DBI::dbGetQuery()` con
  `tryCatch()` relanzados como clase `ciecl_sql_error`, descripción y
  documentación de `query` ampliadas (`FROM`, `ORDER BY`, `GROUP BY`,
  `HAVING`) y `@seealso [cie10_cl]` (ybs12–ybs17). Terminología de los
  mensajes homogeneizada al castellano (consulta/palabra clave/sentencias)
  (ybs14).
* **Ortografía de la ayuda**: se corrigen las tildes faltantes en la
  documentación roxygen de `cie_norm()`, `cie_normalizar()`,
  `cie_validate_vector()`, `cie_expand()`, `cie_search()`, `cie_lookup()`,
  `cie_table()`, `cie_guide()`, `cie_describe()` y `cie11_search()`
  (ybs18, ybs20, ybs21, ybs23, ybs29, ybs32, ybs33, ybs35). Las tildes
  de `cie_siglas()` —deprecated pero exportada— corresponden a ybs25,
  y las de `cie_short()` y `cie_search()` a ybs26.
* **`cie10_clear_cache()` / `cie10_disconnect()`**: ayuda ampliada explicando
  qué es la caché SQLite, cuándo conviene forzar la reconstrucción, qué es el
  bloqueo del archivo `.db`, por qué ocurre y qué pasa si no se libera
  (ybs10, ybs11).
* **Mensajes en castellano consistente**: en `cie_short()` y
  `cie_validate_vector()` el listado de valores usa el conector `" y "` en
  lugar del `" and "` que `cli` aplica por defecto al colapsar vectores; el
  mensaje de `cie_validate_vector()` reemplaza `"DB"` por
  `"base de datos"` y agrega la tilde de `"Códigos"` mediante escape unicode,
  según lo sugerido en la revisión (ybs14, ybs20, ybs27).
* **`cie_map_comorbid()`**: ahora emite un `cli::cli_warn()` cuando alguna
  entrada no tiene formato CIE-10 válido (p. ej. `"hola"` o `35`), indicando
  cuántas se clasificaron como `"Otra"`. El comportamiento de clasificación
  no cambia: los códigos CIE-10 válidos no mapeados siguen en `"Otra"`
  (ybs34).
* **`cie_guide()`**: se mantiene como función, se agrega un enlace cruzado
  desde el README hacia ella y `@seealso` a ella desde las ayudas de las
  funciones de búsqueda (ybs31).
* **Tests**: nuevos casos de validación para `cie10_sql()` y refuerzo de las
  aserciones de las dos ramas "sin palabras válidas" de `cie_search()`, que
  ahora verifican que la consulta de respaldo devuelve datos reales, no solo
  que no falla (ybs17, ybs28).

## Documentación — Correcciones revisión rOpenSci, comentarios de yabellini (2026-06-20)

Trabajo preparado originalmente el 2026-05-29, comiteado recien el 2026-06-20:
quedo guardado en `git stash` desde el 2026-06-09 hasta su recuperacion.
Sin cambios en la API publica.

* **README**: fusion de las secciones "Finalidad" y "Caracteristicas" para
  eliminar redundancia entre ambas; se agrega mencion explicita de la
  correccion vectorizada de inconsistencias de formato y del publico
  objetivo (epidemiologos, bioestadisticos, cientificos de datos).
* **README.en.md**: comentarios de los ejemplos de codigo traducidos al
  ingles (estaban en espanol pese a ser la version en ingles).
* **README**: agregado enlace markdown a `CONTRIBUTING.md` (antes se
  mencionaba sin enlazar).
* **CONTRIBUTING.md**: agregada seccion "Tipos de issue" (ES/EN) que
  explica las plantillas disponibles (bug/feature/question) y corrige la
  instruccion de usar la etiqueta `question`, que el usuario no puede
  aplicar al crear un issue; ahora remite a la plantilla **Consulta**.
  Tambien se sincronizo la politica de idioma y la regla de encoding
  entre las secciones EN y ES, que estaban desactualizadas entre si.

## Sprint F4 — Documentación, limpieza interna y estilo (2026-06-09)

Sin cambios en la API pública.

* **`cie10_cl`**: documentación roxygen completada — columna `uso_cl` documentada,
  descripción de dominio para las 11 columnas, tildes corregidas, `@family datasets`,
  `@seealso [cie_lookup()], [cie_search()], [cie10_sql()]` y ejemplos adicionales.
* **Interno**: argumentos `texto` renombrados a `text` en las funciones internas
  `normalizar_tildes()` y `expandir_sigla()` (no exportadas, sin impacto en API).
* **Estilo**: lambdas de una línea migradas a sintaxis `\(x)` en `cie-siglas.R`
  y `cie-table.R`.

## Sprint F2 — Flags `include_uso_cl` / `only_uso_cl` (2026-05-20)

Expone la columna `uso_cl` del dataset CIE-10 MINSAL/DEIS v2018 como
filtro y como salida opcional en las dos funciones de busqueda principales.

* **`cie_lookup(include_uso_cl = TRUE, only_uso_cl = FALSE)`**:
  - `include_uso_cl = TRUE` (default) preserva el contrato historico: la
    columna `uso_cl` sigue apareciendo en el output.
  - `only_uso_cl = TRUE` filtra a codigos vigentes en Chile excluyendo
    `uso_cl == "legado"`.
* **`cie_search(include_uso_cl = FALSE, only_uso_cl = FALSE)`**:
  - `include_uso_cl = FALSE` (default) preserva el contrato historico (no
    incluye `uso_cl` en el output). Pasarlo a `TRUE` agrega la columna.
  - `only_uso_cl = TRUE` filtra el resultado a codigos vigentes (excluye
    `legado`) en cualquier `field` (`descripcion` o `inclusion`).
* **Semantica de `uso_cl`** (textual, no booleano): `principal` (8.898),
  `legado` (27.335), `causa_externa` (3.295), `etiologico` (330),
  `causa_externa | principal` (19). `only_uso_cl = TRUE` conserva todo lo
  que NO es `legado` (12.542 codigos vigentes).
* **Asimetria documentada**: los defaults difieren entre `cie_lookup`
  (TRUE) y `cie_search` (FALSE) para no romper backward compat. El
  roxygen de ambas funciones lo menciona explicitamente.
* Tests: `tests/testthat/test-uso-cl-flags.R` cubre los 4 paths (default,
  include solo, only solo, ambos) en ambas funciones. +18 tests, sin
  regresion sobre los 1069 existentes.

## Sprint F1 — Cobertura post-Bloque A (2026-05-20)

Recuperación de cobertura sobre los paths agregados en Bloque A y refactor
de mocks de API CIE-11 al patrón canónico de `httr2`. Sin cambios de API
pública.

* **Cobertura total: 94,20 % → 97,31 %** (`R/cie-sql.R`: 81,07 → 96,60;
  `R/cie-api.R`: 90,76 → 100,00). Suite: 1069 tests (+20), 0 fails.
* **`R/cie-sql.R`**: `interactive()` → `rlang::is_interactive()` (2 sitios)
  para que `rlang::local_interactive(TRUE)` pueda activar el path
  `cli_progress` en tests sin TTY real.
* **`R/cie-api.R`**:
  - `who_error_body()` extraído del cuerpo de `cie11_search()` como helper
    interno `@noRd` testeable directamente. Misma lógica de extracción
    (`error_description` → `error` → `message`).
  - El warning del `tryCatch` externo pasa de `{e$message}` a
    `{conditionMessage(e)}`, propagando al usuario el detalle del body OMS
    cuando la API responde 4xx (antes se mostraba solo `"HTTP 401 ..."`).
* **`tests/testthat/test-api-mock.R`** migrado al patrón canónico:
  - `local_mocked_bindings(req_perform, .package = "httr2")` →
    `httr2::local_mocked_responses(mock_who_api(...))`. El flow real de
    `httr2` corre hasta `handle_resp()` → `resp_failure_cnd()`, lo que
    permite verificar end-to-end el callback
    `req_error(body = who_error_body)`.
  - Respuestas fake construidas con `httr2::response_json()` /
    `httr2::response()` en lugar de `structure(list(), class =
    "httr2_response")` (resistente a cambios internos de `httr2`).
  - Nuevo helper `mock_who_api()` que distingue por URL el endpoint OAuth
    de token del endpoint de búsqueda OMS.

## Modernización r-lib — Bloque A (2026-05-17)

Iteración técnica sobre patrones r-lib identificados en auditoría
post-revisión. Sin cambios de API pública.

* **OAuth en `cie11_search()` simplificado** (`R/cie-api.R`):
  - Migración del flujo OAuth manual (POST al token endpoint, parseo JSON,
    header `Authorization` armado a mano) a `httr2::oauth_client()` +
    `httr2::req_oauth_client_credentials()`. `httr2` ahora gestiona la
    obtención, cacheo y refresh del token de acceso de forma transparente.
  - Errores HTTP de la OMS se tipifican vía `httr2::resp_check_status()` y
    `httr2::req_error(body = ...)`, exponiendo `error_description` / `error`
    / `message` del cuerpo de respuesta cuando viene en JSON.
* **Indicadores de progreso en construcción de cache** (`R/cie-sql.R`):
  - `build_cache_atomic()` muestra ahora cuatro pasos vía
    `cli::cli_progress_step()` (cargar dataset, escribir tabla, construir
    índices, crear FTS5) durante la primera invocación (~16 s). En sesiones
    no interactivas se mantiene completamente silencioso.
* **Argumentos requeridos uniformados** (`cie-api.R`, `cie-search.R`,
  `cie-comorbid.R`): `rlang::check_required()` estandariza el mensaje cuando
  faltan args obligatorios (`text`, `data`, `id`, `code`, `codes`).
* **Tests reproducibles**: `testthat::local_reproducible_output()` en cada
  `test_that()` que usa `expect_snapshot`, aislando ancho de terminal, color
  y locale para evitar falsos positivos cross-platform.

## Sprint cobertura y tooling rOpenSci (2026-05-08 / 2026-05-09)

* **Cobertura de tests subida de 86,45 % a 96,61 %** mediante:
  - Tests nuevos para `cie_describe()`, `cie_guide()` y `cie_guia_busqueda()`
    (+42 tests).
  - Tests para rutas de deprecación, cache SQLite y manejo de errores
    (+107 tests). Total acumulado: **1049 tests**, 0 fails, 0 warnings.
  - Refactor a helpers canónicos r-lib:
    `lifecycle::expect_deprecated()` en lugar de `expect_warning(class = ...)`,
    `withr::local_db_connection()` en lugar de helpers custom, llamadas
    directas a internos en lugar de `getFromNamespace()`.
* **Workflows rOpenSci/CRAN agregados**:
  - `.github/workflows/pkgcheck.yaml` (`ropensci-review-tools/pkgcheck-action`)
    con `post-to-issue: false` para evitar 403 en creación de issue.
  - `.github/workflows/urlchecker.yaml` con scheduled run mensual.
  - `.github/workflows/codemeta.yaml` regenera `codemeta.json` cuando cambia
    `DESCRIPTION`.
* **Badge de cobertura dinámico**: README y README.en consultan endpoint JSON
  del workflow `test-coverage.yaml` en lugar de un porcentaje hardcoded.
* **Bug fix `cie_search()`**: sigla `IRA` desambiguada (ver detalle más
  abajo). Aliases `IRA_RESP` e `IRA_RENAL` agregados.
* **Limpieza técnica menor**:
  - `@importFrom` duplicado eliminado en `cie-search.R`.
  - `sapply()` reemplazado por `vapply(..., logical(1))` en `cie_table()`.
  - Fix de asociación roxygen rota en `cie_guia_busqueda()` (línea en blanco
    entre bloque roxygen y firma).
  - Badge rOpenSci Software Peer Review (#765) agregado a los README.

---

## Respuesta a Revisión rOpenSci (rev-ropensci)

Versión de cumplimiento editorial y técnico tras la revisión de Maëlle Salmon.
Esta versión unifica la interfaz del paquete bajo estándares internacionales de R,
manteniendo la documentación pedagógica en español para el contexto local.

### Correcciones funcionales

* **Sigla `IRA` desambiguada** (`cie_search()`): la sigla `IRA` es ambigua en
  contexto clínico chileno (puede ser **infección respiratoria aguda** o
  **insuficiencia renal aguda**). Ahora `cie_search("IRA")` mantiene el default
  respiratorio (uso más frecuente en pediatría y atención primaria) pero emite
  una advertencia explícita orientando al usuario hacia los nuevos alias
  inequívocos `IRA_RESP` (respiratoria) e `IRA_RENAL` (renal). Cierra bug
  detectado durante auditoría de seguridad post-split.

### Infraestructura de calidad de código

* **Workflow de `lintr`** agregado en `.github/workflows/lint.yaml` (template
  oficial r-lib). Reporta lints sin bloquear el CI.
* **`.lintr` modernizado** a sintaxis lintr 3.x (`linters_with_defaults`),
  removiendo el deprecado `with_defaults` y el linter inexistente
  `cyclocomp_linter`.
* **Cleanup de espacios en blanco al final de línea** en 18 archivos
  (R/, tests/, vignettes/, NEWS, README, data-raw, tools).

### Remediación r-lib y Calidad Técnica

* **Migración total a `cli`**: Todos los errores, advertencias y mensajes informativos usan ahora `cli::cli_abort()`, `cli_warn()` y `cli_inform()` con clases de error personalizadas (`ciecl_invalid_input`, `ciecl_api_error`, etc.) para captura programática.
* **Modernización de Roxygen**:
  - Migración masiva de `@return` a `@returns`.
  - Uso de sintaxis markdown para enlaces internos y externos (`[fun()]`).
  - Normalización de etiquetas `@family` a snake_case sin tildes.
* **Refactor de Estructura**: El archivo `cie-search.R` se ha dividido en 4 módulos (`cie-search.R`, `cie-siglas.R`, `cie-lookup.R`, `cie-guide.R`) para mejorar la mantenibilidad.
* **Pipe Nativo**: Migración de `%>%` al pipe nativo de R `|>` en todo el paquete (código, vignettes y tests).
* **Aislamiento de Tests**: Implementación de `withr` para garantizar que las pruebas no contaminen el cache del usuario, utilizando directorios temporales aislados.
* **Snapshots de Mensajes**: Uso de `testthat::expect_snapshot()` para asegurar la consistencia de los mensajes en español generados por el paquete.
* **Limpieza de Suggests**: Removidos `usethis` y `litedown` de `Suggests:` por no ser referenciados en código, tests ni vignettes del paquete.

### Cambios que rompen compatibilidad (Deprecaciones)

* **Internacionalización de la API**: Todos los argumentos de las funciones
  públicas han sido migrados al inglés para consistencia con el ecosistema
  tidyverse (dplyr, httr2). Los argumentos antiguos en español emiten
  `lifecycle::deprecate_warn()` y serán eliminados en la versión 1.0.0.

  - `cie_lookup()`: `codigo` -> `code`, `expandir` -> `expand`, `normalizar` -> `normalize`.
  - `cie_search()`: `texto` -> `text`, `campo` -> `field`, `solo_fuzzy` -> `only_fuzzy`.
  - `cie_short()`: Reemplaza a `cie_siglas()`. Argumento `category`.

### Cambios internos

* El script generador del dataset (`generar_cie10_cl()`, `parsear_cie10_minsal()`) se movió a `data-raw/` siguiendo la convención de R packages. No afecta el uso del paquete: `data(cie10_cl)` sigue funcionando igual.
* Tests del generador trasladados a sanity-check ejecutable desde `data-raw/`.
* Eliminación masiva de `ciecl:::` en tests para favorecer `devtools::load_all()`.

  - `cie11_search()`: `texto` -> `text`.

* **Motor de Normalización Único**: Se establece `cie_norm()` como la función
  canónica. `cie_normalize()` y `cie_normalizar()` están ahora deprecadas.

### Nuevas funcionalidades

* **`cie_describe()`**: Nueva función vectorizada para obtener descripciones
  CIE-10 directamente, facilitando su uso dentro de `dplyr::mutate()` sin
  necesidad de cruces manuales.

### Auditoría Técnica (Compliance)

* **API Resiliente (CIE-11)**: Refactorización de `cie11_search()` usando
  `httr2` con mejores prácticas de consumo web:
  - Identificación clara mediante `User-Agent`.
  - Política de reintentos (`req_retry`) para fallas temporales.
  - Control de tasa (`req_throttle`) para respetar servidores de la OMS.
  - `httr2` promovido de Suggests a **Imports**.

* **Robustez de Tests**: Limpieza masiva de la suite de pruebas. Se eliminaron
  warnings de deprecación internos y se migró a `withr` para la gestión de
  variables de entorno.

### Documentación y Homologación

* **Guías en Espejo**: Creación de versiones en inglés de todas las guías de
  usuario (`Case Study`, `Installation Guide`) para consistencia en el sitio
  `pkgdown`.
* **README paramétrico**: Unificación de `README.Rmd` para generar versiones
  sincronizadas en español e inglés. El README principal es ahora en español.
* **Narrativa Profesional**: Redacción mejorada en todas las vignettes,
  eliminando tonos informales por un lenguaje técnico de ingeniería de datos.

## English Summary

### Test Suite Overhaul (2026-09-06)

Complete restructuring of the test suite, in the context of rOpenSci
review #765. No changes to `R/` or the public API.

* **457 → 372 tests** while coverage rose from 97.72% to 98.62%: zero
  `R/` lines lost, verified line by line with covr at each phase.
* **No real HTTP in the suite**: WHO ICD-11 API tests now use `httr2`
  mocks; the single live-network test is gated by `skip_on_cran()` +
  `skip_if_offline()` + API credentials.
* **Isolated cache tests**: tests that used to operate on the user's real
  cache now run in temporary directories via `CIECL_CACHE_DIR` + `withr`;
  cache lifecycle tests live in `test-cie-sql-cache.R`.
* **Hardened assertions**: weak expectations (`expect_gte()`, generic
  counts) replaced with exact values empirically verified against the
  `comorbidity` package; duplicate and redundant end-to-end flows
  removed; one snapshot per error message.
* **"CRAN canary" policy**: documented in `tests/testthat/setup.R`; each
  key test file keeps at least one lightweight test that also runs on
  CRAN (no network, no optional packages, writes only to tempdir).
* New `test-cie-map-comorbid.R` file holding the `cie_map_comorbid()`
  tests, split out of `test-cie-comorbid.R` (now under the 600-line
  limit).

### rOpenSci Review Follow-up (2026-08-27)

* **Getting-started guide improved**: plain-language `mutate()` wording, an
  executable `intersect()` step showing where `E11.9`/`E14.9` come from, and
  a closing section that filters and summarizes discharges by diabetes type.
  The English twin `case-study-discharges.Rmd` is now in sync with
  `ciecl.Rmd`.
* **SQLite cache**: `cie10_clear_cache()` docs now state that rebuilding
  after a package update is automatic (cache stores the package version in
  `cie10_meta` and rebuilds on first use when it differs);
  `cie10_disconnect()` docs clarify when manual disconnection is actually
  needed. Three new tests cover the version-based rebuild cycle.
* **`cie11_search()`**: the `vcr` example guard moved to `@examplesIf`, so it
  no longer shows in the visible documentation.
* **Installation vignettes (ES/EN)**: opening paragraph explaining who the
  guide is for and who can skip it.

### rOpenSci Review Response

Comprehensive update focused on API consistency and technical compliance
following rOpenSci peer review.

* **Breaking Changes (Deprecations)**: Full API migration to English arguments
  (e.g., `code`, `expand`, `normalize`, `text`) using the `lifecycle` package.
  Old Spanish arguments remain functional but emit warnings.
* **Streamlined Normalization**: `cie_norm()` is now the single canonical
  normalization motor.
* **New Function**: Added `cie_describe()` for direct, vectorized description
  lookup within `mutate()` calls.
* **Technical Compliance**:
  - `cie11_search()` now uses `httr2` with User-Agent, retry, and throttle
    policies.
  - `httr2` promoted to **Imports**.
  - Test suite cleaned of deprecation noise and updated to use `withr`.
* **Documentation**: Mirror versions of user guides provided in both
  English and Spanish. `pkgdown` site navigation localized to Spanish.

---

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
