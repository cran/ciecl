#' Normalizar codigos CIE-10 a formato con punto
#'
#' @description
#' Convierte codigos CIE-10 de diferentes formatos al formato estandar (con punto).
#' Maneja multiples variaciones de entrada comunes en datos clinicos.
#' 
#' @details
#' La normalizacion incluye:
#' \itemize{
#'   \item Conversion a mayusculas
#'   \item Eliminacion de espacios (inicio, fin e internos)
#'   \item Eliminacion de simbolos daga y asterisco (codificacion dual)
#'   \item Conversion de guiones a puntos (I10-0 -> I10.0)
#'   \item Eliminacion de puntos iniciales (.I10 -> I10)
#'   \item Correccion de puntos multiples (E..11 -> E.11)
#'   \item Eliminacion de sufijo X en codigos cortos (I10X -> I10)
#'   \item Preservacion de X en codigos largos (placeholder 7o caracter)
#'   \item Agregado de punto en posicion correcta (E110 -> E11.0)
#' }
#' 
#' El sistema de daga/asterisco indica codificacion dual donde la daga
#' marca la enfermedad subyacente y el asterisco la manifestacion.
#' Ambos simbolos se eliminan para normalizacion.
#' 
#' @param codigos Character vector de codigos en cualquier formato
#' @param buscar_db Logical, buscar codigo en base de datos si no se encuentra exacto (default TRUE)
#' @return Character vector con codigos normalizados al formato con punto
#' @export
#' @examples
#' cie_normalizar("E110")     # Retorna "E11.0"
#' cie_normalizar("E11")      # Retorna "E11" (categoria)
#' cie_normalizar("I10X")     # Retorna "I10" (elimina X)
#' cie_normalizar("E 11 0")   # Retorna "E11.0" (espacios internos)
#' cie_normalizar("I10-0")    # Retorna "I10.0" (guion a punto)
#' cie_normalizar("A17.0\u2020") # Retorna "A17.0" (elimina daga)
#' cie_normalizar("G01*")     # Retorna "G01" (elimina asterisco)
#' cie_normalizar(c("E110", "I10X", "Z00"))  # Vectorizado
cie_normalizar <- function(codigos, buscar_db = TRUE) {
  # Manejar NULL y vectores vacios

  if (is.null(codigos) || length(codigos) == 0) {
    return(character(0))
  }
  
  # Convertir factores a character
  if (is.factor(codigos)) {
    codigos <- as.character(codigos)
  }
  
  # Manejar vector todo NA
  if (all(is.na(codigos))) {
    return(rep(NA_character_, length(codigos)))
  }
  
  # Normalizar a mayusculas y trim
  codigos_norm <- stringr::str_trim(toupper(codigos))
  
  # === LIMPIEZA DE CARACTERES ESPECIALES ===
  # Eliminar simbolos de codificacion dual (daga U+2020, asterisco)
  # Daga: enfermedad subyacente, Asterisco: manifestacion
  codigos_norm <- stringr::str_replace_all(codigos_norm, "[\u2020\u00B7*]", "")
  
  # Convertir guiones a puntos (comun en sistemas que no aceptan puntos)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "-", ".")
  
  # Eliminar espacios internos (E 11 0 -> E110)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "\\s+", "")
  
  # Eliminar puntos iniciales (.I10 -> I10)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "^\\.+", "")
  
  # Corregir puntos multiples consecutivos (E..11 -> E.11)
  codigos_norm <- stringr::str_replace_all(codigos_norm, "\\.{2,}", ".")
  
  # === MANEJO DE SUFIJO X ===
  # Eliminar X final solo en codigos cortos (<=5 chars)
  # Preservar X en codigos largos donde es placeholder obligatorio (ej. S72X01A)
  codigos_norm <- ifelse(
    nchar(codigos_norm) <= 5 & stringr::str_detect(codigos_norm, "X$"),
    stringr::str_remove(codigos_norm, "X$"),
    codigos_norm
  )
  
  # === FORMATO CON PUNTO ===
  # Agregar punto si no lo tiene (E110 -> E11.0)
  # Solo para codigos de 4+ caracteres sin punto: [A-Z]\d{3,}
  codigos_norm <- ifelse(
    stringr::str_detect(codigos_norm, "^[A-Z]\\d{3,}$") & !stringr::str_detect(codigos_norm, "\\."),
    stringr::str_replace(codigos_norm, "^([A-Z]\\d{2})(\\d.*)$", "\\1.\\2"),
    codigos_norm
  )
  
  if (buscar_db) {
    # Verificar que existan en la base de datos
    con <- get_cie10_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    codigos_db <- DBI::dbGetQuery(con, "SELECT DISTINCT codigo FROM cie10")$codigo
    
    # Para cada codigo, verificar si existe, sino buscar variaciones
    resultado <- sapply(codigos_norm, function(cod) {
      if (cod %in% codigos_db) {
        return(cod)
      }
      # Si no existe, intentar agregar 0 al final para subcategorias de 3 digitos
      if (nchar(cod) == 3 && stringr::str_detect(cod, "^[A-Z]\\d{2}$")) {
        cod_con_0 <- paste0(cod, "0")
        if (cod_con_0 %in% codigos_db) {
          return(cod_con_0)
        }
      }
      # Retornar codigo original si no se encuentra
      return(cod)
    }, USE.NAMES = FALSE)
    
    return(resultado)
  } else {
    return(codigos_norm)
  }
}

#' Validar vector de codigos CIE-10 formato
#'
#' @param codigos Character vector codigos (ej. c("E11.0", "Z00.0"))
#' @param strict Logical, validar existencia en DB (default FALSE)
#' @return Logical vector misma longitud input
#' @export
#' @examples
#' cie_validate_vector(c("E11.0", "INVALIDO", "Z00"))
cie_validate_vector <- function(codigos, strict = FALSE) {
  # Regex CIE-10 Chile: acepta formato MINSAL (E110) y estandar (E11.0)
  # MINSAL: [A-Z]\d{2}\d? (3-4 chars: E11, E110)
  # Estandar: [A-Z]\d{2}\.\d{1,2} (con punto: E11.0, E11.00)
  patron <- "^[A-Z]\\d{2}(\\d|\\.\\d{1,2})?$"

  # Manejar NAs: retornar FALSE para NAs
  validos_formato <- ifelse(
    is.na(codigos),
    FALSE,
    stringr::str_detect(toupper(codigos), patron)
  )
  
  if (strict) {
    # Validar existencia en DB con conexion segura
    con <- get_cie10_db()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    codigos_db <- DBI::dbGetQuery(con, "SELECT DISTINCT codigo FROM cie10")$codigo
    
    validos_db <- codigos %in% codigos_db
    resultado <- validos_formato & validos_db
    
    invalidos <- codigos[!resultado]
    if (length(invalidos) > 0) {
      warning("Codigos no encontrados en DB MINSAL: ", paste(invalidos, collapse = ", "))
    }
    
    return(resultado)
  } else {
    return(validos_formato)
  }
}

#' Expandir codigo jerarquico (ej. E11 -> E11.0-E11.9)
#'
#' @param codigo String codigo padre (ej. "E11")
#' @return Character vector codigos hijos
#' @export
#' @examples
#' cie_expand("E11")
cie_expand <- function(codigo) {
  # Manejar NA o cadena vacia
  if (length(codigo) == 0 || is.na(codigo) || nchar(stringr::str_trim(codigo)) == 0) {
    return(character(0))
  }

  hijos <- cie_lookup(codigo, expandir = TRUE)$codigo
  return(hijos)
}
