#' Normalizar códigos CIE-10 a formato con punto
#'
#' @description
#' Convierte códigos CIE-10 de diferentes formatos al
#' formato estándar (con punto).
#' Maneja múltiples variaciones de entrada comunes en datos clínicos.
#'
#' @details
#' La normalización incluye:
#' \itemize{
#'   \item Conversión a mayúsculas
#'   \item Eliminación de espacios (inicio, fin e internos)
#'   \item Eliminación de símbolos daga y asterisco (codificación dual)
#'   \item Conversión de guiones a puntos (I10-0 -> I10.0)
#'   \item Eliminación de puntos iniciales (.I10 -> I10)
#'   \item Corrección de puntos múltiples (E..11 -> E.11)
#'   \item Eliminación de sufijo X en códigos cortos, incluido el punto
#'     previo si lo hay (I10X -> I10, E11.X -> E11)
#'   \item Preservación de X en códigos largos (placeholder 7o carácter)
#'   \item Agregado de punto en posición correcta (E110 -> E11.0)
#' }
#'
#' El sistema de daga/asterisco indica codificación dual donde la daga
#' marca la enfermedad subyacente y el asterisco la manifestación.
#' Ambos símbolos se eliminan para normalización.
#'
#' @param codes Character vector de códigos en cualquier formato
#' @param search_db Logical, buscar código en base de datos
#'   si no se encuentra exacto (default TRUE)
#' @param codigos `r lifecycle::badge("deprecated")` Use `codes`.
#' @param buscar_db `r lifecycle::badge("deprecated")` Use `search_db`.
#' @returns Character vector con códigos normalizados al formato con punto
#' @family validacion
#' @seealso [cie_validate_vector()],
#'   [cie_expand()], [cie_lookup()], [cie_guide()]
#' @export
#' @examples
#' cie_norm("E110")     # Retorna "E11.0"
#' cie_norm("E11")      # Retorna "E11" (categoría)
#' cie_norm("I10X")     # Retorna "I10" (elimina X)
#' cie_norm("E 11 0")   # Retorna "E11.0" (espacios internos)
#' cie_norm("I10-0")    # Retorna "I10.0" (guion a punto)
#' cie_norm(paste0("A17.0", intToUtf8(0x2020)))  # "A17.0" (elimina daga)
#' cie_norm("G01*")                              # "G01"   (elimina asterisco)
#' cie_norm(c("E110", "I10X", "Z00"))  # Vectorizado
cie_norm <- function(codes,
                          search_db = TRUE,
                          codigos = lifecycle::deprecated(),
                          buscar_db = lifecycle::deprecated()) {
  # Deprecation: argumentos en espanol -> ingles
  if (lifecycle::is_present(codigos)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_norm(codigos = )",
      "cie_norm(codes = )"
    )
    codes <- codigos
  }
  if (lifecycle::is_present(buscar_db)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_norm(buscar_db = )",
      "cie_norm(search_db = )"
    )
    search_db <- buscar_db
  }

  check_required_es(missing(codes), "codes")

  # Manejar NULL y vectores vacios
  if (is.null(codes) || length(codes) == 0) {
    return(character(0))
  }

  # Convertir factores a character
  if (is.factor(codes)) {
    codes <- as.character(codes)
  }

  # Manejar vector todo NA
  if (all(is.na(codes))) {
    return(rep(NA_character_, length(codes)))
  }

  # Normalizar a mayusculas y trim
  codigos_norm <- stringr::str_trim(toupper(codes))

  # === LIMPIEZA DE CARACTERES ESPECIALES ===
  # Eliminar simbolos de codificacion dual MINSAL/DEIS:
  # daga U+2020 (enfermedad subyacente), medio-punto U+00B7,
  # asterisco (manifestacion), mas (+, etiologia alternativa).
  # Usamos intToUtf8() para mantener el fuente ASCII-puro (requisito CRAN).
  pattern_duales <- paste0("[", intToUtf8(c(0x2020, 0x00B7)), "*+]")
  codigos_norm <- stringr::str_replace_all(codigos_norm, pattern_duales, "")

  # Convertir guiones a puntos (comun en sistemas que no aceptan puntos)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "-", ".")

  # Eliminar espacios internos (E 11 0 -> E110)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "\\s+", "")

  # Eliminar puntos iniciales (.I10 -> I10)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "^\\.+", "")

  # Corregir puntos multiples consecutivos (E..11 -> E.11)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "\\.{2,}", ".")

  # === MANEJO DE SUFIJO X ===
  # Eliminar X final solo en codigos cortos (<=5 chars); si la X venia
  # precedida de punto (E11.X, convencion "no especificada"), remover
  # tambien el punto para no dejar "E11." malformado
  # Preservar X en codigos largos donde es placeholder obligatorio (ej. S72X01A)
  codigos_norm <- ifelse(
    nchar(codigos_norm) <= 5 & stringr::str_detect(codigos_norm, "X$"),
    stringr::str_remove(codigos_norm, "\\.?X$"),
    codigos_norm
  )

  # === FORMATO CON PUNTO ===
  # Agregar punto si no lo tiene (E110 -> E11.0)
  # Solo para codigos de 4+ caracteres sin punto: [A-Z]\d{3,}
  codigos_norm <- ifelse(
    stringr::str_detect(codigos_norm, "^[A-Z]\\d{3,}$") &
      !stringr::str_detect(codigos_norm, "\\."),
    stringr::str_replace(codigos_norm, "^([A-Z]\\d{2})(\\d.*)$", "\\1.\\2"),
    codigos_norm
  )

  if (search_db) {
    # Verificar que existan en la base de datos
    con <- get_cie10_db()

    codigos_db <- DBI::dbGetQuery(
      con, "SELECT DISTINCT codigo FROM cie10"
    )$codigo

    # Verificacion vectorizada contra DB
    no_na <- !is.na(codigos_norm)
    en_db <- codigos_norm %in% codigos_db
    es_3_dig <- no_na & !en_db & nchar(codigos_norm) == 3 &
      stringr::str_detect(codigos_norm, "^[A-Z]\\d{2}$")
    cod_con_0 <- paste0(codigos_norm, "0")
    fallback_ok <- es_3_dig & (cod_con_0 %in% codigos_db)

    resultado <- codigos_norm
    resultado[fallback_ok] <- cod_con_0[fallback_ok]

    return(resultado)
  } else {
    return(codigos_norm)
  }
}

#' Normalizar códigos CIE-10 (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Alias en español de [cie_norm()]. Se mantiene por compatibilidad
#' con código existente en CRAN. Usar [cie_norm()] en código nuevo.
#'
#' @param codigos `r lifecycle::badge("deprecated")` Character vector de códigos.
#'   Use [cie_norm()] con `codes` en código nuevo.
#' @param buscar_db `r lifecycle::badge("deprecated")` Logical, buscar código en DB
#'   (default TRUE). Use [cie_norm()] con `search_db` en código nuevo.
#' @returns Character vector con códigos normalizados
#' @family validacion
#' @keywords internal
#' @export
#' @examples
#' # Deprecated: usar cie_norm()
#' cie_normalizar("E110")

cie_normalizar <- function(codigos = lifecycle::deprecated(),
                           buscar_db = lifecycle::deprecated()) {
  lifecycle::deprecate_warn(
    "0.9.8",
    "cie_normalizar()",
    "cie_norm()"
  )
  check_required_es(missing(codigos), "codigos")
  search_db <- if (lifecycle::is_present(buscar_db)) buscar_db else TRUE
  cie_norm(codes = codigos, search_db = search_db)
}

#' @rdname cie_norm
#' @export
cie_normalize <- function(codes, search_db = TRUE,
                          codigos = lifecycle::deprecated(),
                          buscar_db = lifecycle::deprecated()) {
  lifecycle::deprecate_warn(
    "0.9.8",
    "cie_normalize()",
    "cie_norm()"
  )
  if (lifecycle::is_present(codigos)) {
    lifecycle::deprecate_warn("0.9.8", "cie_normalize(codigos = )", "cie_normalize(codes = )")
    codes <- codigos
  }
  if (lifecycle::is_present(buscar_db)) {
    lifecycle::deprecate_warn("0.9.8", "cie_normalize(buscar_db = )", "cie_normalize(search_db = )")
    search_db <- buscar_db
  }
  cie_norm(codes = codes, search_db = search_db)
}

#' Validar vector de códigos CIE-10 formato
#'
#' @param codes Character vector códigos (ej. c("E11.0", "Z00.0"))
#' @param strict Logical, validar existencia en DB (default FALSE)
#' @param codigos `r lifecycle::badge("deprecated")` Use `codes`.
#' @returns Logical vector de la misma longitud que `codes`. TRUE si el
#'   código tiene formato CIE-10 válido (y existe en DB si `strict = TRUE`).
#' @family validacion
#' @seealso [cie_norm()], [cie_expand()]
#' @export
#' @examples
#' cie_validate_vector(c("E11.0", "INVALIDO", "Z00"))
cie_validate_vector <- function(codes,
                                strict = FALSE,
                                codigos = lifecycle::deprecated()) {
  if (lifecycle::is_present(codigos)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_validate_vector(codigos = )",
      "cie_validate_vector(codes = )"
    )
    codes <- codigos
  }

  check_required_es(missing(codes), "codes")

  # Regex CIE-10 Chile: acepta formato MINSAL (E110) y estandar (E11.0)
  # MINSAL: [A-Z]\d{2}\d? (3-4 chars: E11, E110)
  # Estandar: [A-Z]\d{2}\.\d{1,2} (con punto: E11.0, E11.00)
  patron <- "^[A-Z]\\d{2}(\\d|\\.\\d{1,2})?$"

  # Normalizar antes de validar formato (I10X -> I10, N10X -> N10)
  codigos_norm <- cie_norm(codes, search_db = FALSE)

  # Manejar NAs: retornar FALSE para NAs
  validos_formato <- ifelse(
    is.na(codigos_norm),
    FALSE,
    stringr::str_detect(toupper(codigos_norm), patron)
  )

  if (strict) {
    # Validar existencia en DB (conexion pooled)
    con <- get_cie10_db()

    codigos_db <- DBI::dbGetQuery(
      con, "SELECT DISTINCT codigo FROM cie10"
    )$codigo

    validos_db <- codigos_norm %in% codigos_db
    resultado <- validos_formato & validos_db

    invalidos <- codes[!resultado & !is.na(codes)]
    if (length(invalidos) > 0) {
      invalidos_vec <- cli::cli_vec(invalidos, style = list("vec-last" = " y "))
      cli::cli_warn(c(
        "C\u00f3digos no encontrados en la base de datos MINSAL:",
        "x" = "{.val {invalidos_vec}}"
      ))
    }

    return(resultado)
  } else {
    return(validos_formato)
  }
}

#' Expandir código jerárquico (ej. E11 -> E11.0-E11.9)
#'
#' @param code String código padre (ej. "E11")
#' @param codigo `r lifecycle::badge("deprecated")` Use `code`.
#' @returns Character vector con todos los códigos hijos del código padre.
#'   Vector vacío si el código no existe en la base de datos.
#' @family validacion
#' @seealso [cie_norm()], [cie_lookup()], [cie_guide()]
#' @export
#' @examples
#' cie_expand("E11")
cie_expand <- function(code, codigo = lifecycle::deprecated()) {
  if (lifecycle::is_present(codigo)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_expand(codigo = )",
      "cie_expand(code = )"
    )
    code <- codigo
  }

  check_required_es(missing(code), "code")

  # Validación escalar: el contrato documentado es un único código padre.
  # Sin esta guarda, el || sobre un vector sería error duro (R >= 4.3).
  if (!rlang::is_string(code)) {
    cli::cli_abort(
      "{.arg code} debe ser un string character no-NA de longitud 1, no {.obj_type_friendly {code}}.",
      class = "ciecl_invalid_input"
    )
  }

  # Manejar cadena vacía
  if (nchar(stringr::str_trim(code)) == 0) {
    return(character(0))
  }

  hijos <- cie_lookup(code, expand = TRUE)$codigo
  return(hijos)
}
