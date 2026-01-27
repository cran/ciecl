#' @importFrom stringr str_trim str_replace_all str_detect str_extract str_split
#' @importFrom dplyr select mutate filter all_of any_of bind_rows distinct %>%
#' @importFrom tibble as_tibble
NULL

#' Normalizar texto removiendo tildes y caracteres especiales
#'
#' @param texto Character vector a normalizar
#' @return Character vector sin tildes ni caracteres especiales
#' @keywords internal
#' @noRd
normalizar_tildes <- function(texto) {
  # Manejar vector vacio o NA
  if (length(texto) == 0) return(character(0))

  # Usar chartr() que es mas rapido para sustituciones multiples
  # Caracteres con tilde -> sin tilde
  chartr(
    "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1",
    "aeiouunAEIOUUN",
    texto
  )
}

#' Diccionario de siglas medicas comunes en Chile
#'
#' @return Named list con siglas como keys y terminos de busqueda como values
#' @keywords internal
#' @noRd
get_siglas_medicas <- function() {
  # Retorna lista con categoria incluida: list(sigla = list(termino, categoria))
  list(
    # Cardiovasculares
    "iam" = list(termino = "infarto agudo miocardio", categoria = "cardiovascular"),
    "iamcest" = list(termino = "infarto agudo miocardio", categoria = "cardiovascular"),
    "iamsest" = list(termino = "infarto agudo miocardio", categoria = "cardiovascular"),
    "sca" = list(termino = "sindrome coronario agudo", categoria = "cardiovascular"),
    "hta" = list(termino = "hipertension arterial", categoria = "cardiovascular"),
    "aha" = list(termino = "hipertension arterial", categoria = "cardiovascular"),
    "icc" = list(termino = "insuficiencia cardiaca", categoria = "cardiovascular"),
    "ic" = list(termino = "insuficiencia cardiaca", categoria = "cardiovascular"),
    "fa" = list(termino = "fibrilacion auricular", categoria = "cardiovascular"),
    "tep" = list(termino = "embolia pulmonar", categoria = "cardiovascular"),
    "tvp" = list(termino = "trombosis venosa profunda", categoria = "cardiovascular"),
    "eap" = list(termino = "edema agudo pulmon", categoria = "cardiovascular"),
    "acv" = list(termino = "accidente cerebrovascular", categoria = "cardiovascular"),
    "ave" = list(termino = "accidente vascular encefalico", categoria = "cardiovascular"),
    "ait" = list(termino = "isquemico transitorio", categoria = "cardiovascular"),

    # Respiratorias
    "tbc" = list(termino = "tuberculosis", categoria = "respiratoria"),
    "tb" = list(termino = "tuberculosis", categoria = "respiratoria"),
    "epoc" = list(termino = "enfermedad pulmonar obstructiva cronica", categoria = "respiratoria"),
    "asma" = list(termino = "asma", categoria = "respiratoria"),
    "nac" = list(termino = "neumonia", categoria = "respiratoria"),
    "ira" = list(termino = "infeccion respiratoria aguda", categoria = "respiratoria"),
    "sdra" = list(termino = "sindrome distres respiratorio", categoria = "respiratoria"),
    "covid" = list(termino = "covid", categoria = "respiratoria"),
    "sars" = list(termino = "coronavirus", categoria = "respiratoria"),

    # Metabolicas/Endocrinas
    "dm" = list(termino = "diabetes mellitus", categoria = "metabolica"),
    "dm1" = list(termino = "diabetes mellitus tipo 1", categoria = "metabolica"),
    "dm2" = list(termino = "diabetes mellitus tipo 2", categoria = "metabolica"),
    "dbt" = list(termino = "diabetes", categoria = "metabolica"),
    "hipo" = list(termino = "hipotiroidismo", categoria = "metabolica"),
    "hiper" = list(termino = "hipertiroidismo", categoria = "metabolica"),
    "erc" = list(termino = "enfermedad renal cronica", categoria = "metabolica"),
    "irc" = list(termino = "insuficiencia renal cronica", categoria = "metabolica"),
    "ira_renal" = list(termino = "insuficiencia renal aguda", categoria = "metabolica"),
    "lra" = list(termino = "lesion renal aguda", categoria = "metabolica"),

    # Gastrointestinales
    "hda" = list(termino = "hemorragia digestiva alta", categoria = "gastrointestinal"),
    "hdb" = list(termino = "hemorragia digestiva baja", categoria = "gastrointestinal"),
    "rge" = list(termino = "reflujo gastroesofagico", categoria = "gastrointestinal"),
    "erge" = list(termino = "reflujo gastroesofagico", categoria = "gastrointestinal"),
    "eii" = list(termino = "enfermedad inflamatoria intestinal", categoria = "gastrointestinal"),
    "cu" = list(termino = "colitis ulcerosa", categoria = "gastrointestinal"),
    "ec" = list(termino = "enfermedad crohn", categoria = "gastrointestinal"),
    "dhc" = list(termino = "dano hepatico cronico", categoria = "gastrointestinal"),
    "cirrosis" = list(termino = "cirrosis", categoria = "gastrointestinal"),

    # Infecciosas
    "vih" = list(termino = "vih", categoria = "infecciosa"),
    "sida" = list(termino = "sida", categoria = "infecciosa"),
    "its" = list(termino = "infeccion transmision sexual", categoria = "infecciosa"),
    "ets" = list(termino = "enfermedad transmision sexual", categoria = "infecciosa"),
    "itu" = list(termino = "infeccion tracto urinario", categoria = "infecciosa"),
    "ivu" = list(termino = "infeccion vias urinarias", categoria = "infecciosa"),
    "meningitis" = list(termino = "meningitis", categoria = "infecciosa"),
    "sepsis" = list(termino = "sepsis", categoria = "infecciosa"),

    # Oncologicas
    "ca" = list(termino = "carcinoma", categoria = "oncologica"),
    "neo" = list(termino = "neoplasia", categoria = "oncologica"),
    "lma" = list(termino = "leucemia mieloide aguda", categoria = "oncologica"),
    "lmc" = list(termino = "leucemia mieloide cronica", categoria = "oncologica"),
    "lla" = list(termino = "leucemia linfoblastica aguda", categoria = "oncologica"),
    "llc" = list(termino = "leucemia linfocitica cronica", categoria = "oncologica"),
    "lnh" = list(termino = "linfoma no hodgkin", categoria = "oncologica"),
    "lh" = list(termino = "linfoma hodgkin", categoria = "oncologica"),
    "mm" = list(termino = "mieloma multiple", categoria = "oncologica"),

    # Reumatologicas
    "ar" = list(termino = "artritis reumatoide", categoria = "reumatologica"),
    "les" = list(termino = "lupus eritematoso", categoria = "reumatologica"),
    "fm" = list(termino = "fibromialgia", categoria = "reumatologica"),
    "ea" = list(termino = "espondilitis anquilosante", categoria = "reumatologica"),

    # Neurologicas
    "epi" = list(termino = "epilepsia", categoria = "neurologica"),
    "parkinson" = list(termino = "parkinson", categoria = "neurologica"),
    "alzheimer" = list(termino = "alzheimer", categoria = "neurologica"),
    "em" = list(termino = "esclerosis multiple", categoria = "neurologica"),
    "ela" = list(termino = "esclerosis lateral amiotrofica", categoria = "neurologica"),
    "cefalea" = list(termino = "cefalea", categoria = "neurologica"),
    "migrana" = list(termino = "migrana", categoria = "neurologica"),

    # Psiquiatricas
    "tdah" = list(termino = "deficit atencion hiperactividad", categoria = "psiquiatrica"),
    "toc" = list(termino = "obsesivo compulsivo", categoria = "psiquiatrica"),
    "tag" = list(termino = "ansiedad generalizada", categoria = "psiquiatrica"),
    "tept" = list(termino = "estres postraumatico", categoria = "psiquiatrica"),
    "edm" = list(termino = "depresion mayor", categoria = "psiquiatrica"),
    "tab" = list(termino = "trastorno bipolar", categoria = "psiquiatrica"),

    # Traumatologicas
    "tec" = list(termino = "traumatismo craneoencefalico", categoria = "traumatologica"),
    "fx" = list(termino = "fractura", categoria = "traumatologica"),
    "lca" = list(termino = "ligamento cruzado anterior", categoria = "traumatologica"),

    # Pediatricas
    "sbo" = list(termino = "sindrome bronquial obstructivo", categoria = "pediatrica"),
    "eda" = list(termino = "enfermedad diarreica aguda", categoria = "pediatrica"),
    "gea" = list(termino = "gastroenteritis aguda", categoria = "pediatrica"),

    # Gineco-obstetricias
    "sop" = list(termino = "sindrome ovario poliquistico", categoria = "gineco_obstetrica"),
    "epi_gineco" = list(termino = "enfermedad pelvica inflamatoria", categoria = "gineco_obstetrica"),
    "hie" = list(termino = "hipertension embarazo", categoria = "gineco_obstetrica"),
    "pe" = list(termino = "preeclampsia", categoria = "gineco_obstetrica"),
    "dpp" = list(termino = "desprendimiento prematuro placenta", categoria = "gineco_obstetrica"),
    "rciu" = list(termino = "restriccion crecimiento intrauterino", categoria = "gineco_obstetrica")
  )
}

#' Expandir siglas medicas a terminos de busqueda
#'
#' @param texto Texto que puede contener siglas
#' @return Texto con siglas expandidas o NULL si no es sigla
#' @keywords internal
#' @noRd
expandir_sigla <- function(texto) {
  siglas <- get_siglas_medicas()
  texto_lower <- tolower(stringr::str_trim(texto))

  if (texto_lower %in% names(siglas)) {
    return(siglas[[texto_lower]]$termino)
  }

  return(NULL)
}

#' Obtener codigo CIE-10 desde sigla medica
#'
#' @param sigla Character sigla medica (ej. "IAM", "DM2")
#' @return Character vector con codigos CIE-10 o NULL
#' @keywords internal
#' @noRd
sigla_to_codigo <- function(sigla) {
  siglas <- get_siglas_medicas()
  sigla_lower <- tolower(stringr::str_trim(sigla))

  if (!(sigla_lower %in% names(siglas))) {
    return(NULL)
  }

  termino <- siglas[[sigla_lower]]$termino
  resultado <- cie_search(termino, solo_fuzzy = TRUE, verbose = FALSE)

  if (nrow(resultado) > 0) {
    return(resultado$codigo[1])
  }

  return(NULL)
}

#' Extraer codigo CIE-10 de texto con ruido
#'
#' @param texto Character vector que puede contener prefijos/sufijos
#' @return Character vector con codigo CIE-10 extraido o original
#' @keywords internal
#' @noRd
extract_cie_from_text <- function(texto) {
  # Fix #2: Patrón estricto para extraer código CIE-10: letra + 2-3 digitos + punto opcional + 0-2 digitos
  # Solo extrae si está rodeado de no-alfanuméricos o en extremos
  patron <- "(?:^|[^A-Z0-9])([A-Z][0-9]{2}[0-9]?\\.?[0-9X]{0,2})(?:$|[^A-Z0-9])"
  
  extraido <- stringr::str_extract(toupper(texto), patron)
  
  # Extraer grupo capturado (quitar prefijos/sufijos)
  if (!is.na(extraido) && extraido != "") {
    extraido <- gsub("^[^A-Z]+|[^A-Z0-9]+$", "", extraido)
  }
  
  # Si se extrajo algo, usarlo; si no, devolver original
  resultado <- ifelse(
    !is.na(extraido) & extraido != "",
    extraido,
    texto
  )
  
  return(resultado)
}

#' Listar siglas medicas soportadas
#'
#' Muestra todas las siglas medicas que pueden usarse en cie_search().
#'
#' @param categoria Character opcional, filtrar por categoria. Valores validos:
#'   "cardiovascular", "respiratoria", "metabolica", "gastrointestinal",
#'   "infecciosa", "oncologica", "reumatologica", "neurologica",
#'   "psiquiatrica", "traumatologica", "pediatrica", "gineco_obstetrica".
#'   Si es NULL (default), retorna todas las siglas.
#' @return tibble con columnas: sigla, termino_busqueda, categoria
#' @export
#' @examples
#' # Ver todas las siglas
#' cie_siglas()
#'
#' # Filtrar por categoria
#' cie_siglas("cardiovascular")
#' cie_siglas("oncologica")
#'
#' # Buscar una sigla especifica
#' cie_siglas() |> dplyr::filter(sigla == "iam")
cie_siglas <- function(categoria = NULL) {
  siglas <- get_siglas_medicas()

  resultado <- tibble::tibble(
    sigla = names(siglas),
    termino_busqueda = sapply(siglas, function(x) x$termino),
    categoria = sapply(siglas, function(x) x$categoria)
  )

  # Filtrar por categoria si se especifica
  if (!is.null(categoria)) {
    categoria <- tolower(categoria)
    categorias_validas <- unique(resultado$categoria)

    if (!categoria %in% categorias_validas) {
      warning("Categoria '", categoria, "' no encontrada. ",
              "Categorias validas: ", paste(categorias_validas, collapse = ", "))
      return(resultado[0, ])
    }

    resultado <- resultado[resultado$categoria == categoria, ]
  }

  return(resultado)
}

#' Busqueda difusa (fuzzy) de terminos medicos CIE-10
#'
#' Busca en descripciones CIE-10 usando multiples estrategias:
#' 1. Expansion de siglas medicas (IAM, TBC, DM, etc.)
#' 2. Busqueda exacta por subcadena (mas rapida)
#' 3. Busqueda fuzzy con Jaro-Winkler (tolera typos)
#'
#' La busqueda es tolerante a tildes: "neumonia" encuentra "neumonia".
#' Soporta siglas medicas comunes: "IAM" busca "infarto agudo miocardio".
#'
#' @param texto String termino medico en espanol o sigla (ej. "diabetes", "IAM", "TBC")
#' @param threshold Numeric entre 0 y 1, umbral similitud Jaro-Winkler (default 0.70)
#' @param max_results Integer, maximo resultados a retornar (default 50)
#' @param campo Character, campo busqueda ("descripcion" o "inclusion")
#' @param solo_fuzzy Logical, usar solo busqueda fuzzy sin busqueda exacta (default FALSE)
#' @param verbose Logical, mostrar mensajes informativos (default TRUE). Usar FALSE en scripts.
#' @return tibble ordenado por score descendente (1.0 = coincidencia exacta).
#'   Incluye atributo "sigla_expandida" si se uso una sigla.
#' @export
#' @importFrom stringdist stringsim
#' @importFrom dplyr mutate filter arrange desc slice_head select everything %>%
#' @examples
#' # Busqueda basica
#' cie_search("diabetes")
#'
#' \donttest{
#' cie_search("neumonia")
#'
#' # Busqueda por siglas medicas
#' cie_search("IAM")
#' cie_search("DM2")
#'
#' # Tolerante a tildes y typos
#' cie_search("diabetis")
#'
#' # Buscar en inclusiones
#' cie_search("bacteriana", campo = "inclusion")
#' }
cie_search <- function(texto, threshold = 0.70, max_results = 50,
                       campo = c("descripcion", "inclusion"),
                       solo_fuzzy = FALSE, verbose = TRUE) {
  campo <- match.arg(campo)

  # Validacion de parametros
  if (!is.character(texto) || length(texto) != 1 || is.na(texto)) {
    stop("texto debe ser un string character no-NA de longitud 1")
  }
  if (threshold < 0 || threshold > 1) {
    stop("threshold debe estar entre 0 y 1")
  }
  if (max_results < 1) {
    stop("max_results debe ser >= 1")
  }

  texto_limpio <- stringr::str_trim(texto)

  # Permitir siglas de 2 caracteres (DM, TB, FA, etc.)
  if (nchar(texto_limpio) < 2) {
    stop("Texto minimo 2 caracteres")
  }

  # Verificar si es una sigla medica y expandirla
  sigla_expandida <- expandir_sigla(texto_limpio)
  texto_busqueda <- if (!is.null(sigla_expandida)) {
    if (verbose) message("i Sigla detectada: ", toupper(texto_limpio), " -> ", sigla_expandida)
    sigla_expandida
  } else {
    texto_limpio
  }

  # Conexion segura con auto-cierre
  con <- get_cie10_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Normalizar texto de busqueda (minusculas + sin tildes)
  texto_norm <- tolower(texto_busqueda)
  texto_sin_tildes <- normalizar_tildes(texto_norm)

  # Dividir en palabras para FTS5
  palabras <- unlist(stringr::str_split(texto_sin_tildes, "\\s+"))
  palabras <- palabras[nchar(palabras) >= 2]

  # Pre-filtrar usando FTS5 para velocidad
  if (length(palabras) > 0) {
    # Sanitizar palabras para FTS5 (prevenir SQL injection)
    # Solo permitir alfanumericos y acentos normalizados
    palabras_fts <- gsub("[^a-z0-9]", "", palabras)
    palabras_fts <- palabras_fts[nchar(palabras_fts) >= 2]

    if (length(palabras_fts) > 0) {
      # Construir query FTS5: palabra1* OR palabra2*
      texto_fts <- paste0(palabras_fts, "*", collapse = " OR ")

      if (campo == "descripcion") {
        query_sql <- sprintf("
          SELECT c.codigo, c.descripcion, c.categoria
          FROM cie10 c
          WHERE c.rowid IN (SELECT rowid FROM cie10_fts WHERE cie10_fts MATCH '%s')
        ", texto_fts)
      } else {
        query_sql <- sprintf("
          SELECT c.codigo, c.descripcion, c.categoria, c.%s
          FROM cie10 c
          WHERE c.rowid IN (SELECT rowid FROM cie10_fts WHERE cie10_fts MATCH '%s')
        ", campo, texto_fts)
      }
    } else {
      # Sin palabras validas tras sanitizar, cargar todo
      if (campo == "descripcion") {
        query_sql <- "SELECT codigo, descripcion, categoria FROM cie10"
      } else {
        query_sql <- sprintf("SELECT codigo, descripcion, categoria, %s FROM cie10", campo)
      }
    }
  } else {
    # Sin palabras validas, cargar todo (fallback)
    if (campo == "descripcion") {
      query_sql <- "SELECT codigo, descripcion, categoria FROM cie10"
    } else {
      query_sql <- sprintf("SELECT codigo, descripcion, categoria, %s FROM cie10", campo)
    }
  }

  base <- DBI::dbGetQuery(con, query_sql) %>% tibble::as_tibble()

  # Si FTS5 no retorno resultados, intentar carga completa para fuzzy
  if (nrow(base) == 0 && length(palabras) > 0) {
    if (campo == "descripcion") {
      query_sql <- "SELECT codigo, descripcion, categoria FROM cie10"
    } else {
      query_sql <- sprintf("SELECT codigo, descripcion, categoria, %s FROM cie10", campo)
    }
    base <- DBI::dbGetQuery(con, query_sql) %>% tibble::as_tibble()
  }

  # Normalizar texto de la base (minusculas + sin tildes)
  base_texto <- tolower(stringr::str_trim(base[[campo]]))
  base_texto[is.na(base_texto)] <- ""
  base_texto_sin_tildes <- normalizar_tildes(base_texto)

  # ESTRATEGIA 1: Busqueda exacta por subcadena (mas rapida y precisa)
  if (!solo_fuzzy) {
    # Buscar coincidencias exactas (subcadena)
    matches_exactos <- stringr::str_detect(base_texto_sin_tildes,
                                           stringr::fixed(texto_sin_tildes))

    if (any(matches_exactos)) {
      resultado_exacto <- base[matches_exactos, ] %>%
        dplyr::mutate(score = 1.0) %>%
        dplyr::slice_head(n = max_results) %>%
        dplyr::select(codigo, descripcion, score, dplyr::everything())

      return(resultado_exacto)
    }
  }

  # ESTRATEGIA 2: Busqueda fuzzy palabra por palabra
  # palabras ya fue definido arriba para FTS5
  palabras_fuzzy <- palabras[nchar(palabras) >= 3]

  if (length(palabras_fuzzy) > 0) {
    # Calcular score basado en cuantas palabras coinciden
    scores_palabras <- sapply(seq_along(base_texto_sin_tildes), function(i) {
      texto_base <- base_texto_sin_tildes[i]
      # Contar palabras que aparecen en la descripcion
      matches <- sapply(palabras_fuzzy, function(p) {
        stringr::str_detect(texto_base, stringr::fixed(p))
      })
      sum(matches) / length(palabras_fuzzy)
    })

    # Si hay coincidencias parciales de palabras
    if (any(scores_palabras > 0)) {
      resultado <- base %>%
        dplyr::mutate(score = scores_palabras) %>%
        dplyr::filter(score > 0) %>%
        dplyr::arrange(dplyr::desc(score)) %>%
        dplyr::slice_head(n = max_results) %>%
        dplyr::select(codigo, descripcion, score, dplyr::everything())

      if (nrow(resultado) > 0) {
        return(resultado)
      }
    }
  }

  # ESTRATEGIA 3: Fuzzy matching con Jaro-Winkler (para typos)
  # Calcular similitud de cada palabra del texto con palabras de la descripcion
  scores_fuzzy <- sapply(seq_along(base_texto_sin_tildes), function(i) {
    texto_base <- base_texto_sin_tildes[i]
    palabras_base <- unlist(stringr::str_split(texto_base, "\\s+"))
    palabras_base <- palabras_base[nchar(palabras_base) >= 3]

    if (length(palabras_base) == 0) return(0)

    # Para cada palabra del texto buscar la mejor coincidencia en la descripcion
    best_scores <- sapply(palabras_fuzzy, function(p) {
      if (length(palabras_base) == 0) return(0)
      max(stringdist::stringsim(p, palabras_base, method = "jw"))
    })

    mean(best_scores)
  })

  # Filtrar + ordenar resultados fuzzy
  resultado <- base %>%
    dplyr::mutate(score = scores_fuzzy) %>%
    dplyr::filter(score >= threshold) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = max_results) %>%
    dplyr::select(codigo, descripcion, score, dplyr::everything())

  if (nrow(resultado) == 0 && verbose) {
    message("x Sin coincidencias >= threshold ", threshold)
  }

  return(resultado)
}

#' Busqueda exacta por codigo CIE-10
#'
#' @param codigo Character vector de codigos (ej. "E11", "E11.0", c("E11.0", "Z00"))
#'   o rango (ej. "E10-E14"). Acepta vectores de multiples codigos.
#'   Soporta formatos: con punto (E11.0), sin punto (E110), o solo categoria (E11).
#' @param expandir Logical, expandir jerarquia completa (default FALSE)
#' @param normalizar Logical, normalizar formato de codigos automaticamente (default TRUE)
#' @param descripcion_completa Logical, agregar columna descripcion_completa con formato "CODIGO - DESCRIPCION" (default FALSE)
#' @param extract Logical, extraer codigo CIE-10 de texto con prefijos/sufijos (default FALSE).
#'   IMPORTANTE: Solo usar con codigo ESCALAR (longitud 1). Ejemplo: "CIE:E11.0" -> "E11.0", "E11.0-confirmado" -> "E11.0".
#'   Para vectores multiples usar extract=FALSE (default).
#' @param check_siglas Logical, buscar siglas medicas comunes (default FALSE).
#'   Ejemplo: "IAM" -> I21.0 (Infarto agudo miocardio)
#' @return tibble con codigo(s) matcheado(s)
#' @export
#' @examples
#' cie_lookup("E11.0")       # Con punto
#' cie_lookup("E110")        # Sin punto
#' cie_lookup("E11")         # Solo categoria
#' cie_lookup("E11", expandir = TRUE)  # Todos E11.x
#' # Vectorizado - multiples codigos y formatos
#' cie_lookup(c("E11.0", "Z00", "I10"))
#' # Con descripcion completa
#' cie_lookup("E110", descripcion_completa = TRUE)
#' # Extraer codigo de texto con ruido (solo codigo escalar)
#' cie_lookup("CIE:E11.0", extract = TRUE)
#' cie_lookup("E11.0-confirmado", extract = TRUE)
#' # Nota: Para vectores multiples usar extract=FALSE (default)
#' # Buscar por siglas medicas
#' cie_lookup("IAM", check_siglas = TRUE)
#' cie_lookup("DM2", check_siglas = TRUE)
#' cie_lookup("EPOC", check_siglas = TRUE)
cie_lookup <- function(codigo, expandir = FALSE, normalizar = TRUE, descripcion_completa = FALSE, extract = FALSE, check_siglas = FALSE) {
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
  # Fix #1: Normalizar espacios en códigos
  codigo_input <- gsub("\\s+", "", codigo_input)
  
  # Extraer codigo de texto con ruido (prefijos/sufijos)
  if (extract) {
    codigo_input <- extract_cie_from_text(codigo_input)
  }
  
  # Buscar siglas medicas
  if (check_siglas) {
    codigo_input <- sapply(codigo_input, function(x) {
      codigo_sigla <- sigla_to_codigo(x)
      if (!is.null(codigo_sigla)) {
        return(codigo_sigla)
      }
      return(x)
    }, USE.NAMES = FALSE)
  }
  
  # Normalizar formato si se solicita: agregar punto si falta (E110 -> E11.0)
  if (normalizar) {
    codigo_norm <- ifelse(
      stringr::str_detect(codigo_input, "^[A-Z]\\d{3,}$") & !stringr::str_detect(codigo_input, "\\."),
      stringr::str_replace(codigo_input, "^([A-Z]\\d{2})(\\d.*)$", "\\1.\\2"),
      codigo_input
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
      codigos_safe <- codigos_normales[stringr::str_detect(codigos_normales, "^[A-Za-z0-9.]+$")]

      if (length(codigos_safe) > 0) {
        con <- get_cie10_db()
        on.exit(DBI::dbDisconnect(con), add = TRUE)

        if (expandir) {
          # Expandir: usar LIKE para cada codigo
          likes <- paste0("codigo LIKE '", codigos_safe, "%'", collapse = " OR ")
          query <- sprintf("SELECT * FROM cie10 WHERE %s ORDER BY codigo", likes)
        } else {
          # Exacto: usar IN clause
          codigos_sql <- paste0("'", codigos_safe, "'", collapse = ",")
          query <- sprintf("SELECT * FROM cie10 WHERE codigo IN (%s)", codigos_sql)
        }

        resultado <- DBI::dbGetQuery(con, query) %>% tibble::as_tibble()
      }
    }

    # Procesar rangos individualmente (poco comun)
    if (length(codigos_rango) > 0) {
      resultados_rango <- lapply(codigos_rango, function(cod) {
        cie_lookup_single(cod, expandir = expandir)
      })
      resultado <- dplyr::bind_rows(resultado, dplyr::bind_rows(resultados_rango))
    }

    # Eliminar duplicados
    resultado <- dplyr::distinct(resultado)
  } else {
    # Codigo unico - usar funcion interna
    resultado <- cie_lookup_single(codigo_norm, expandir = expandir)
  }
  
  # Agregar columna descripcion_completa si se solicita
  if (descripcion_completa) {
    if (nrow(resultado) > 0) {
      resultado <- resultado %>%
        dplyr::mutate(
          descripcion_completa = paste0(codigo, " - ", descripcion)
        )
    } else {
      # Asegurar que la columna existe incluso cuando el resultado está vacío
      # Necesitamos usar tibble::add_column() para mantener la estructura de tibble
      resultado <- resultado %>%
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
    stop("cie_lookup_single() solo acepta un codigo a la vez")
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
  # Solo permitir caracteres validos para codigos CIE-10: letras, numeros, punto, guion
  if (!stringr::str_detect(codigo_norm, "^[A-Za-z0-9.\\-]+$")) {
    message("x Codigo con caracteres invalidos: ", codigo_norm)
    return(cie10_empty_tibble())
  }

  # Conexion con queries parametrizadas (previene SQL injection)
  con <- get_cie10_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (expandir) {
    # Buscar jerarquia completa (E11 -> E11.x)
    query <- "SELECT * FROM cie10 WHERE codigo LIKE ? ORDER BY codigo"
    resultado <- DBI::dbGetQuery(con, query, params = list(paste0(codigo_norm, "%")))
  } else if (stringr::str_detect(codigo_norm, "-")) {
    # Rango (ej. "E10-E14")
    partes <- stringr::str_split(codigo_norm, "-")[[1]]
    inicio <- partes[1]
    fin <- partes[2]

    # Advertir y corregir rangos invertidos (ej. "E14-E10" -> "E10-E14")
    if (inicio > fin) {
      warning(paste0("Rango invertido detectado: '", codigo_norm, "'. ",
                   "Corrigiendo a '", fin, "-", inicio, "'"))
      temp <- inicio
      inicio <- fin
      fin <- temp
    }

    query <- "SELECT * FROM cie10 WHERE codigo BETWEEN ? AND ? ORDER BY codigo"
    resultado <- DBI::dbGetQuery(con, query, params = list(inicio, fin))
  } else {
    # Exacto
    query <- "SELECT * FROM cie10 WHERE codigo = ?"
    resultado <- DBI::dbGetQuery(con, query, params = list(codigo_norm))
  }

  resultado <- tibble::as_tibble(resultado)

  # Fix #3: Validar estrictamente códigos inválidos
  # Solo códigos CIE-10 válidos existen en la base
  if (nrow(resultado) == 0) {
    message("x Codigo no encontrado: ", codigo_norm)
    return(cie10_empty_tibble())
  }

  return(resultado)
}

#' Guia de funciones de busqueda CIE-10
#'
#' Muestra tabla comparativa de cuando usar cada funcion de busqueda.
#'
#' @return data.frame con guia de uso
#' @export
#' @examples
#' cie_guia_busqueda()
cie_guia_busqueda <- function() {
  guia <- data.frame(
    `Tengo...` = c(
      "Codigo exacto (E11.0)",
      "Codigo sin punto (e110)",
      "Codigo con espacios (E 11.0)",
      "Codigo con prefijos/sufijos",
      "Codigo categoria (E11)",
      "Descripcion (diabetes)",
      "Sigla medica (IAM, DM2)",
      "No se que tengo"
    ),
    `Usar funcion` = c(
      "cie_lookup()",
      "cie_lookup()",
      "cie_lookup()",
      "cie_lookup(extract = TRUE)",
      "cie_lookup() o cie_lookup(expandir = TRUE)",
      "cie_search()",
      "cie_lookup(check_siglas = TRUE)",
      "cie_buscar() (experimental)"
    ),
    `Ejemplo` = c(
      'cie_lookup("E11.0")',
      'cie_lookup("e110")',
      'cie_lookup("E 11.0")',
      'cie_lookup("CIE:E11.0", extract = TRUE)',
      'cie_lookup("E11")',
      'cie_search("diabetes")',
      'cie_lookup("IAM", check_siglas = TRUE)',
      'pronto disponible'
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  return(guia)
}
