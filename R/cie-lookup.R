#' @importFrom stringr str_trim str_detect str_extract str_split
#' @importFrom tibble as_tibble add_column
#' @importFrom DBI dbGetQuery
NULL

#' Extraer codigo CIE-10 de texto con ruido
#'
#' @param text Character vector que puede contener prefijos/sufijos
#' @returns Character vector con codigo CIE-10 extraido o original
#' @keywords internal
#' @noRd
extract_cie_from_text <- function(text) {
  # Fix #2: Patron estricto para extraer codigo CIE-10:
  # letra + 2-3 digitos + punto opcional + 0-2 digitos
  # Solo extrae si esta rodeado de no-alfanumericos o en extremos
  patron <- "(?:^|[^A-Z0-9])([A-Z][0-9]{2}[0-9]?\\.?[0-9X]{0,2})(?:$|[^A-Z0-9])"

  extraido <- stringr::str_extract(toupper(text), patron)

  # Extraer grupo capturado (quitar prefijos/sufijos)
  if (!is.na(extraido) && extraido != "") {
    extraido <- gsub("^[^A-Z]+|[^A-Z0-9]+$", "", extraido)
  }

  # Si se extrajo algo, usarlo; si no, devolver original
  resultado <- ifelse(
    !is.na(extraido) & extraido != "",
    extraido,
    text
  )

  return(resultado)
}

#' Búsqueda exacta por código CIE-10
#'
#' @param code Character vector de códigos
#'   (ej. "E11", "E11.0", c("E11.0", "Z00"))
#'   o rango (ej. "E10-E14"). Acepta vectores.
#'   Soporta formatos: con punto (E11.0),
#'   sin punto (E110), o solo categoría (E11).
#' @param expand Logical, expandir jerarquía completa (default FALSE)
#' @param normalize Logical, normalizar formato de códigos
#'   automáticamente (default TRUE)
#' @param full_description Logical, agregar columna `descripcion_completa`
#'   con formato "CODIGO - DESCRIPCION" (default FALSE)
#' @param extract Logical, extraer código CIE-10 de texto con
#'   prefijos/sufijos (default FALSE).
#'   IMPORTANTE: Solo usar con código ESCALAR (longitud 1).
#'   Ejemplo: "CIE:E11.0" -> "E11.0", "E11.0-confirmado" -> "E11.0".
#'   Para vectores múltiples usar extract=FALSE (default).
#' @param check_siglas Logical, buscar siglas médicas comunes (default FALSE).
#'   Ejemplo: "IAM" -> I21.0 (Infarto agudo miocardio)
#' @param include_uso_cl Logical, incluir columna `uso_cl` en el output
#'   (default TRUE). El default difiere de [cie_search()] (FALSE) para
#'   preservar el contrato histórico de cada función. Valores posibles:
#'   `"principal"`, `"legado"`, `"causa_externa"`, `"etiologico"`,
#'   `"causa_externa | principal"`.
#' @param only_uso_cl Logical, filtrar a códigos vigentes de uso clínico
#'   en Chile (default FALSE). Cuando es TRUE, excluye los códigos con
#'   `uso_cl == "legado"` (es decir, conserva `principal`,
#'   `causa_externa`, `etiologico` y sus combinaciones).
#' @param codigo `r lifecycle::badge("deprecated")` Use `code`.
#' @param expandir `r lifecycle::badge("deprecated")` Use `expand`.
#' @param normalizar `r lifecycle::badge("deprecated")` Use `normalize`.
#' @param descripcion_completa `r lifecycle::badge("deprecated")` Use `full_description`.
#' @returns tibble con codigo(s) matcheado(s)
#' @family search
#' @seealso [cie_search()], [cie_norm()], [cie_expand()], [cie_guide()]
#' @export
#' @examples
#' # Búsqueda directa por código
#' cie_lookup("E11.0")
#'
#' @examplesIf rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' cie_lookup("E110") # Sin punto
#' cie_lookup("E11") # Solo categoría
#' cie_lookup("E11", expand = TRUE) # Todos E11.x
#' # Vectorizado - múltiples códigos y formatos
#' cie_lookup(c("E11.0", "Z00", "I10"))
#' # Con descripción completa
#' cie_lookup("E110", full_description = TRUE)
#' # Extraer código de texto con ruido (solo código escalar)
#' cie_lookup("CIE:E11.0", extract = TRUE)
#' cie_lookup("E11.0-confirmado", extract = TRUE)
#' # Buscar por siglas médicas
#' cie_lookup("IAM", check_siglas = TRUE)
#' cie_lookup("DM2", check_siglas = TRUE)
#' # Filtrar a códigos vigentes de uso clínico Chile (excluye 'legado')
#' cie_lookup("E11", expand = TRUE, only_uso_cl = TRUE)
#' # Omitir columna uso_cl en el output
#' cie_lookup("E11.0", include_uso_cl = FALSE)
cie_lookup <- function(code, expand = FALSE, normalize = TRUE,
                       full_description = FALSE, extract = FALSE,
                       check_siglas = FALSE,
                       include_uso_cl = TRUE, only_uso_cl = FALSE,
                       codigo = lifecycle::deprecated(),
                       expandir = lifecycle::deprecated(),
                       normalizar = lifecycle::deprecated(),
                       descripcion_completa = lifecycle::deprecated()) {
  # Deprecation: argumentos en espanol -> ingles
  if (lifecycle::is_present(codigo)) {
    lifecycle::deprecate_warn(
      "0.9.8", "cie_lookup(codigo = )", "cie_lookup(code = )"
    )
    code <- codigo
  }
  if (lifecycle::is_present(expandir)) {
    lifecycle::deprecate_warn(
      "0.9.8", "cie_lookup(expandir = )", "cie_lookup(expand = )"
    )
    expand <- expandir
  }
  if (lifecycle::is_present(normalizar)) {
    lifecycle::deprecate_warn(
      "0.9.8", "cie_lookup(normalizar = )", "cie_lookup(normalize = )"
    )
    normalize <- normalizar
  }
  if (lifecycle::is_present(descripcion_completa)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_lookup(descripcion_completa = )",
      "cie_lookup(full_description = )"
    )
    full_description <- descripcion_completa
  }

  check_required_es(missing(code), "code")

  # Alias internos para mantener el cuerpo de la funcion sin cambios
  codigo <- code
  expandir <- expand
  normalizar <- normalize
  descripcion_completa <- full_description

  # Manejar vector vacio
  if (length(codigo) == 0) {
    return(cie10_empty_tibble(add_descripcion_completa = descripcion_completa))
  }

  # Filtrar NAs antes de procesar
  codigo_sin_na <- codigo[!is.na(codigo)]
  if (length(codigo_sin_na) == 0) {
    return(cie10_empty_tibble(add_descripcion_completa = descripcion_completa))
  }

  # Normalizar entrada
  codigo_input <- stringr::str_trim(toupper(codigo_sin_na))
  # Fix #1: Normalizar espacios en codigos
  codigo_input <- gsub("\\s+", "", codigo_input)

  # extract = TRUE solo opera en modo escalar (contrato de @param extract);
  # sin esta guarda, el && interno sobre un vector sería error duro (R >= 4.3)
  if (extract && length(codigo_input) != 1L) {
    cli::cli_abort(
      "{.arg code} debe ser un solo c\u00f3digo (longitud 1) cuando {.code extract = TRUE}.",
      class = "ciecl_invalid_input"
    )
  }

  # Extraer codigo de texto con ruido (prefijos/sufijos)
  if (extract) {
    codigo_input <- extract_cie_from_text(codigo_input)
  }

  # Buscar siglas medicas
  if (check_siglas) {
    codigo_input <- vapply(codigo_input, function(x) {
      codigo_sigla <- sigla_to_codigo(x)
      if (!is.null(codigo_sigla)) {
        return(codigo_sigla)
      }
      return(x)
    }, character(1), USE.NAMES = FALSE)
  }

  # Normalizar formato si se solicita
  # (elimina sufijo X DEIS, agrega punto, etc.)
  # Preservar rangos (guion entre codigos) antes de normalizar,
  # ya que cie_normalizar convierte guiones a puntos
  if (normalizar) {
    es_rango_input <- stringr::str_detect(
      codigo_input,
      "^[A-Z][0-9.]{2,}-[A-Z][0-9.]{2,}$"
    )
    codigo_norm <- ifelse(
      es_rango_input,
      codigo_input,
      cie_norm(codigo_input, search_db = FALSE)
    )
  } else {
    codigo_norm <- codigo_input
  }

  # Si es vector de multiples codigos, procesar con query vectorizada
  if (length(codigo_norm) > 1) {
    # Optimizacion: usar unique() y query batch
    codigo_unique <- unique(codigo_norm)

    # Separar codigos normales de rangos (contienen "-")
    es_rango <- stringr::str_detect(codigo_unique, "-")
    codigos_normales <- codigo_unique[!es_rango]
    codigos_rango <- codigo_unique[es_rango]

    resultado <- cie10_empty_tibble()

    # Query batch para codigos normales (sin rangos)
    if (length(codigos_normales) > 0) {
      # Sanitizar codigos (solo alfanumericos y punto)
      codigos_safe <- codigos_normales[
        stringr::str_detect(
          codigos_normales, "^[A-Za-z0-9.]+$"
        )
      ]

      # Informar en un solo mensaje los códigos descartados por caracteres
      # inválidos (equivalente vectorial del informe del modo escalar)
      invalidos <- codigos_normales[
        !stringr::str_detect(codigos_normales, "^[A-Za-z0-9.]+$")
      ]
      if (length(invalidos) > 0) {
        invalidos_vec <- cli::cli_vec(
          invalidos,
          style = list("vec-last" = " y ")
        )
        cli::cli_inform(c(
          "x" = "C\u00f3digos con caracteres inv\u00e1lidos: {.val {invalidos_vec}}"
        ))
      }

      if (length(codigos_safe) > 0) {
        con <- get_cie10_db()

        if (expandir) {
          # Expandir: LIKE parametrizado por cada codigo
          placeholders <- paste(
            rep("codigo LIKE ?", length(codigos_safe)),
            collapse = " OR "
          )
          query <- sprintf(
            "SELECT * FROM cie10 WHERE %s ORDER BY codigo",
            placeholders
          )
          params <- paste0(codigos_safe, "%")
          resultado <- DBI::dbGetQuery(
            con, query,
            params = as.list(params)
          ) |> tibble::as_tibble()
        } else {
          # Exacto: IN clause parametrizada
          placeholders <- paste(rep("?", length(codigos_safe)),
            collapse = ","
          )
          query <- sprintf(
            "SELECT * FROM cie10 WHERE codigo IN (%s) ORDER BY codigo",
            placeholders
          )
          resultado <- DBI::dbGetQuery(
            con, query,
            params = as.list(codigos_safe)
          ) |> tibble::as_tibble()
        }

        # Informar en un solo mensaje los códigos no encontrados en la base
        if (expandir) {
          # En modo expansión un código se considera encontrado si algún
          # código del resultado tiene ese prefijo
          encontrados <- vapply(
            codigos_safe,
            function(cod) any(startsWith(resultado$codigo, cod)),
            logical(1)
          )
        } else {
          encontrados <- codigos_safe %in% resultado$codigo
        }
        no_encontrados <- codigos_safe[!encontrados]
        if (length(no_encontrados) > 0) {
          no_encontrados_vec <- cli::cli_vec(
            no_encontrados,
            style = list("vec-last" = " y ")
          )
          cli::cli_inform(c(
            "x" = "C\u00f3digos no encontrados: {.val {no_encontrados_vec}}"
          ))
        }
      }
    }

    # Procesar rangos individualmente (poco comun)
    if (length(codigos_rango) > 0) {
      resultados_rango <- lapply(codigos_rango, function(cod) {
        cie_lookup_single(cod, expandir = expandir)
      })
      resultado <- dplyr::bind_rows(
        resultado, dplyr::bind_rows(resultados_rango)
      )
    }

    # Eliminar duplicados
    resultado <- dplyr::distinct(resultado)
  } else {
    # Codigo unico - usar funcion interna
    resultado <- cie_lookup_single(codigo_norm, expandir = expandir)
  }

  # Filtrar a codigos vigentes de uso clinico Chile (excluye 'legado')
  if (only_uso_cl && "uso_cl" %in% names(resultado) && nrow(resultado) > 0) {
    resultado <- dplyr::filter(resultado, .data$uso_cl != "legado")
  }

  # Omitir columna uso_cl del output si include_uso_cl = FALSE
  if (!include_uso_cl) {
    resultado <- dplyr::select(resultado, -dplyr::any_of("uso_cl"))
  }

  # Agregar columna descripcion_completa si se solicita
  if (descripcion_completa) {
    if (nrow(resultado) > 0) {
      resultado <- resultado |>
        dplyr::mutate(
          descripcion_completa = paste0(codigo, " - ", descripcion)
        )
    } else {
      # Asegurar que la columna existe incluso cuando el resultado esta vacio
      # Necesitamos usar tibble::add_column()
      # para mantener la estructura de tibble
      resultado <- resultado |>
        tibble::add_column(descripcion_completa = character(0))
    }
  }

  return(resultado)
}

#' Busqueda interna de un solo codigo CIE-10
#' @keywords internal
#' @noRd
cie_lookup_single <- function(codigo_norm, expandir = FALSE) {
  # Asegurar que codigo_norm es un escalar (longitud 1)
  if (length(codigo_norm) != 1) {
    cli::cli_abort("{.fn cie_lookup_single} solo acepta un c\u00f3digo a la vez.", class = "ciecl_invalid_input")
  }

  # Manejar NA
  if (is.na(codigo_norm)) {
    return(cie10_empty_tibble())
  }

  # Manejar cadena vacia
  if (nchar(stringr::str_trim(codigo_norm)) == 0) {
    return(cie10_empty_tibble())
  }

  # Sanitizar entrada para prevenir SQL injection
  # Solo permitir caracteres validos para codigos CIE-10:
  # letras, numeros, punto, guion
  if (!stringr::str_detect(codigo_norm, "^[A-Za-z0-9.\\-]+$")) {
    cli::cli_inform(c("x" = "C\u00f3digo con caracteres inv\u00e1lidos: {.val {codigo_norm}}"))
    return(cie10_empty_tibble())
  }

  # Conexion pooled (queries parametrizadas, previene SQL injection)
  con <- get_cie10_db()

  if (expandir) {
    # Buscar jerarquia completa (E11 -> E11.x)
    query <- "SELECT * FROM cie10 WHERE codigo LIKE ? ORDER BY codigo"
    resultado <- DBI::dbGetQuery(
      con, query,
      params = list(paste0(codigo_norm, "%"))
    )
  } else if (stringr::str_detect(codigo_norm, "-")) {
    # Rango (ej. "E10-E14")
    partes <- stringr::str_split(codigo_norm, "-")[[1]]
    inicio <- partes[1]
    fin <- partes[2]

    # Advertir y corregir rangos invertidos (ej. "E14-E10" -> "E10-E14")
    if (inicio > fin) {
      cli::cli_warn(c(
        "Rango invertido detectado: {.val {codigo_norm}}.",
        "i" = "Corrigiendo a {.val {fin}-{inicio}}."
      ))
      temp <- inicio
      inicio <- fin
      fin <- temp
    }

    # Incluir las subcategorías del límite superior del rango:
    # 'E14.9' BETWEEN 'E10' AND 'E14' es falso con collation BINARY
    # (comparación lexicográfica), así que se agrega un LIKE sobre el
    # prefijo del límite superior para cubrir 'E14.x'
    query <- paste(
      "SELECT * FROM cie10",
      "WHERE codigo BETWEEN ? AND ? OR codigo LIKE ?",
      "ORDER BY codigo"
    )
    resultado <- DBI::dbGetQuery(
      con, query,
      params = list(inicio, fin, paste0(fin, ".%"))
    )
  } else {
    # Exacto
    query <- "SELECT * FROM cie10 WHERE codigo = ?"
    resultado <- DBI::dbGetQuery(con, query, params = list(codigo_norm))
  }

  resultado <- tibble::as_tibble(resultado)

  # Fix #3: Validar estrictamente codigos invalidos
  # Solo codigos CIE-10 validos existen en la base
  if (nrow(resultado) == 0) {
    cli::cli_inform(c("x" = "C\u00f3digo no encontrado: {.val {codigo_norm}}"))
    return(cie10_empty_tibble())
  }

  return(resultado)
}
