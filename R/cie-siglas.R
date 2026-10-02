#' @importFrom stringr str_trim
#' @importFrom tibble tibble
NULL

#' Diccionario de siglas médicas comunes en Chile
#'
#' @returns Named list con siglas como nombres y términos de búsqueda como valores
#' @keywords internal
#' @noRd
get_siglas_medicas <- function() {
  # Retorna lista con categoria incluida: list(sigla = list(termino, categoria))
  list(
    # Cardiovasculares
    "iam" = list(
      termino = "infarto agudo miocardio",
      categoria = "cardiovascular"
    ),
    "iamcest" = list(
      termino = "infarto agudo miocardio",
      categoria = "cardiovascular"
    ),
    "iamsest" = list(
      termino = "infarto agudo miocardio",
      categoria = "cardiovascular"
    ),
    "sca" = list(
      termino = "sindrome coronario agudo",
      categoria = "cardiovascular"
    ),
    "hta" = list(
      termino = "hipertension arterial",
      categoria = "cardiovascular"
    ),
    "aha" = list(
      termino = "hipertension arterial",
      categoria = "cardiovascular"
    ),
    "icc" = list(
      termino = "insuficiencia cardiaca",
      categoria = "cardiovascular"
    ),
    "ic" = list(
      termino = "insuficiencia cardiaca",
      categoria = "cardiovascular"
    ),
    "fa" = list(
      termino = "fibrilacion auricular",
      categoria = "cardiovascular"
    ),
    "tep" = list(
      termino = "embolia pulmonar",
      categoria = "cardiovascular"
    ),
    "tvp" = list(
      termino = "trombosis venosa profunda",
      categoria = "cardiovascular"
    ),
    "eap" = list(
      termino = "edema agudo pulmon",
      categoria = "cardiovascular"
    ),
    "acv" = list(
      termino = "accidente cerebrovascular",
      categoria = "cardiovascular"
    ),
    "ave" = list(
      termino = "accidente vascular encefalico",
      categoria = "cardiovascular"
    ),
    "ait" = list(
      termino = "isquemico transitorio",
      categoria = "cardiovascular"
    ),

    # Respiratorias
    "tbc" = list(
      termino = "tuberculosis",
      categoria = "respiratoria"
    ),
    "tb" = list(
      termino = "tuberculosis",
      categoria = "respiratoria"
    ),
    "epoc" = list(
      termino = "enfermedad pulmonar obstructiva cronica",
      categoria = "respiratoria"
    ),
    "asma" = list(
      termino = "asma",
      categoria = "respiratoria"
    ),
    "nac" = list(
      termino = "neumonia",
      categoria = "respiratoria"
    ),
    "ira" = list(
      termino = "infeccion respiratoria aguda",
      categoria = "respiratoria",
      ambiguo = TRUE,
      aviso = paste(
        "{.val IRA} es ambigua en contexto clinico chileno:",
        "respiratoria (default) o renal. Use {.val IRA_RESP} o",
        "{.val IRA_RENAL} para evitar ambiguedad."
      )
    ),
    "ira_resp" = list(
      termino = "infeccion respiratoria aguda",
      categoria = "respiratoria"
    ),
    "sdra" = list(
      termino = "sindrome distres respiratorio",
      categoria = "respiratoria"
    ),
    "covid" = list(
      termino = "covid",
      categoria = "respiratoria"
    ),
    "sars" = list(
      termino = "coronavirus",
      categoria = "respiratoria"
    ),

    # Metabolicas/Endocrinas
    "dm" = list(
      termino = "diabetes mellitus",
      categoria = "metabolica"
    ),
    "dm1" = list(
      termino = "diabetes mellitus tipo 1",
      categoria = "metabolica"
    ),
    "dm2" = list(
      termino = "diabetes mellitus tipo 2",
      categoria = "metabolica"
    ),
    "dbt" = list(
      termino = "diabetes",
      categoria = "metabolica"
    ),
    "hipo" = list(
      termino = "hipotiroidismo",
      categoria = "metabolica"
    ),
    "hiper" = list(
      termino = "hipertiroidismo",
      categoria = "metabolica"
    ),
    "erc" = list(
      termino = "enfermedad renal cronica",
      categoria = "metabolica"
    ),
    "irc" = list(
      termino = "insuficiencia renal cronica",
      categoria = "metabolica"
    ),
    "ira_renal" = list(
      termino = "insuficiencia renal aguda",
      categoria = "metabolica"
    ),
    "lra" = list(
      termino = "lesion renal aguda",
      categoria = "metabolica"
    ),

    # Gastrointestinales
    "hda" = list(
      termino = "hemorragia digestiva alta",
      categoria = "gastrointestinal"
    ),
    "hdb" = list(
      termino = "hemorragia digestiva baja",
      categoria = "gastrointestinal"
    ),
    "rge" = list(
      termino = "reflujo gastroesofagico",
      categoria = "gastrointestinal"
    ),
    "erge" = list(
      termino = "reflujo gastroesofagico",
      categoria = "gastrointestinal"
    ),
    "eii" = list(
      termino = "enfermedad inflamatoria intestinal",
      categoria = "gastrointestinal"
    ),
    "cu" = list(
      termino = "colitis ulcerosa",
      categoria = "gastrointestinal"
    ),
    "ec" = list(
      termino = "enfermedad crohn",
      categoria = "gastrointestinal"
    ),
    "dhc" = list(
      termino = "dano hepatico cronico",
      categoria = "gastrointestinal"
    ),
    "cirrosis" = list(
      termino = "cirrosis",
      categoria = "gastrointestinal"
    ),

    # Infecciosas
    "vih" = list(
      termino = "vih",
      categoria = "infecciosa"
    ),
    "sida" = list(
      termino = "sida",
      categoria = "infecciosa"
    ),
    "its" = list(
      termino = "infeccion transmision sexual",
      categoria = "infecciosa"
    ),
    "ets" = list(
      termino = "enfermedad transmision sexual",
      categoria = "infecciosa"
    ),
    "itu" = list(
      termino = "infeccion tracto urinario",
      categoria = "infecciosa"
    ),
    "ivu" = list(
      termino = "infeccion vias urinarias",
      categoria = "infecciosa"
    ),
    "meningitis" = list(
      termino = "meningitis",
      categoria = "infecciosa"
    ),
    "sepsis" = list(
      termino = "sepsis",
      categoria = "infecciosa"
    ),

    # Oncologicas
    "ca" = list(
      termino = "carcinoma",
      categoria = "oncologica"
    ),
    "neo" = list(
      termino = "neoplasia",
      categoria = "oncologica"
    ),
    "lma" = list(
      termino = "leucemia mieloide aguda",
      categoria = "oncologica"
    ),
    "lmc" = list(
      termino = "leucemia mieloide cronica",
      categoria = "oncologica"
    ),
    "lla" = list(
      termino = "leucemia linfoblastica aguda",
      categoria = "oncologica"
    ),
    "llc" = list(
      termino = "leucemia linfocitica cronica",
      categoria = "oncologica"
    ),
    "lnh" = list(
      termino = "linfoma no hodgkin",
      categoria = "oncologica"
    ),
    "lh" = list(
      termino = "linfoma hodgkin",
      categoria = "oncologica"
    ),
    "mm" = list(
      termino = "mieloma multiple",
      categoria = "oncologica"
    ),

    # Reumatologicas
    "ar" = list(
      termino = "artritis reumatoide",
      categoria = "reumatologica"
    ),
    "les" = list(
      termino = "lupus eritematoso",
      categoria = "reumatologica"
    ),
    "fm" = list(
      termino = "fibromialgia",
      categoria = "reumatologica"
    ),
    "ea" = list(
      termino = "espondilitis anquilosante",
      categoria = "reumatologica"
    ),

    # Neurologicas
    "epi" = list(
      termino = "epilepsia",
      categoria = "neurologica"
    ),
    "parkinson" = list(
      termino = "parkinson",
      categoria = "neurologica"
    ),
    "alzheimer" = list(
      termino = "alzheimer",
      categoria = "neurologica"
    ),
    "em" = list(
      termino = "esclerosis multiple",
      categoria = "neurologica"
    ),
    "ela" = list(
      termino = "esclerosis lateral amiotrofica",
      categoria = "neurologica"
    ),
    "cefalea" = list(
      termino = "cefalea",
      categoria = "neurologica"
    ),
    "migrana" = list(
      termino = "migrana",
      categoria = "neurologica"
    ),

    # Psiquiatricas
    "tdah" = list(
      termino = "deficit atencion hiperactividad",
      categoria = "psiquiatrica"
    ),
    "toc" = list(
      termino = "obsesivo compulsivo",
      categoria = "psiquiatrica"
    ),
    "tag" = list(
      termino = "ansiedad generalizada",
      categoria = "psiquiatrica"
    ),
    "tept" = list(
      termino = "estres postraumatico",
      categoria = "psiquiatrica"
    ),
    "edm" = list(
      termino = "depresion mayor",
      categoria = "psiquiatrica"
    ),
    "tab" = list(
      termino = "trastorno bipolar",
      categoria = "psiquiatrica"
    ),

    # Traumatologicas
    "tec" = list(
      termino = "traumatismo craneoencefalico",
      categoria = "traumatologica"
    ),
    "fx" = list(
      termino = "fractura",
      categoria = "traumatologica"
    ),
    "lca" = list(
      termino = "ligamento cruzado anterior",
      categoria = "traumatologica"
    ),

    # Pediatricas
    "sbo" = list(
      termino = "sindrome bronquial obstructivo",
      categoria = "pediatrica"
    ),
    "eda" = list(
      termino = "enfermedad diarreica aguda",
      categoria = "pediatrica"
    ),
    "gea" = list(
      termino = "gastroenteritis aguda",
      categoria = "pediatrica"
    ),

    # Gineco-obstetricias
    "sop" = list(
      termino = "sindrome ovario poliquistico",
      categoria = "gineco_obstetrica"
    ),
    "epi_gineco" = list(
      termino = "enfermedad pelvica inflamatoria",
      categoria = "gineco_obstetrica"
    ),
    "hie" = list(
      termino = "hipertension embarazo",
      categoria = "gineco_obstetrica"
    ),
    "pe" = list(
      termino = "preeclampsia",
      categoria = "gineco_obstetrica"
    ),
    "dpp" = list(
      termino = "desprendimiento prematuro placenta",
      categoria = "gineco_obstetrica"
    ),
    "rciu" = list(
      termino = "restriccion crecimiento intrauterino",
      categoria = "gineco_obstetrica"
    )
  )
}

#' Expandir siglas medicas a terminos de busqueda
#'
#' @param text Texto que puede contener siglas
#' @returns Lista con `termino`, `ambiguo` (logical) y `aviso` (character o NULL),
#'   o NULL si `text` no es sigla.
#' @keywords internal
#' @noRd
expandir_sigla <- function(text) {
  siglas <- get_siglas_medicas()
  texto_lower <- tolower(stringr::str_trim(text))

  if (!(texto_lower %in% names(siglas))) {
    return(NULL)
  }

  entry <- siglas[[texto_lower]]
  list(
    termino = entry$termino,
    ambiguo = isTRUE(entry$ambiguo),
    aviso = entry$aviso
  )
}

#' Obtener codigo CIE-10 desde sigla medica
#'
#' @param sigla Character sigla medica (ej. "IAM", "DM2")
#' @returns String de longitud 1 con el primer codigo CIE-10 encontrado
#'   para el termino de la sigla, o NULL si `sigla` no esta en el
#'   diccionario o la busqueda fuzzy no encuentra match.
#' @keywords internal
#' @noRd
sigla_to_codigo <- function(sigla) {
  siglas <- get_siglas_medicas()
  sigla_lower <- tolower(stringr::str_trim(sigla))

  if (!(sigla_lower %in% names(siglas))) {
    return(NULL)
  }

  termino <- siglas[[sigla_lower]]$termino
  resultado <- cie_search(termino, only_fuzzy = TRUE, verbose = FALSE)

  if (nrow(resultado) > 0) {
    return(resultado$codigo[1])
  }

  return(NULL)
}

#' Listar siglas médicas soportadas
#'
#' Muestra todas las siglas médicas que pueden usarse en [cie_search()].
#' "Sigla" se conserva como término local (concepto médico chileno) en la
#' columna de salida; el nombre de la función usa `cie_short` por
#' consistencia con el ecosistema R (verbos cortos en inglés).
#'
#' @param category Character opcional, filtrar por categoría. Los valores
#'   (sin tildes, son tokens usados también para matching interno) son:
#'   "cardiovascular", "respiratoria", "metabolica", "gastrointestinal",
#'   "infecciosa", "oncologica", "reumatologica", "neurologica",
#'   "psiquiatrica", "traumatologica", "pediatrica", "gineco_obstetrica".
#'   Si es NULL (default), retorna todas las siglas.
#' @param categoria `r lifecycle::badge("deprecated")` Use `category`.
#' @returns tibble con columnas: sigla, termino_busqueda, categoria
#' @family busqueda
#' @seealso [cie_search()], [cie_lookup()]
#' @export
#' @examples
#' # Ver todas las siglas
#' cie_short()
#'
#' # Filtrar por categoría
#' cie_short("cardiovascular")
#' cie_short("oncologica")
#'
#' # Buscar una sigla específica
#' cie_short() |> dplyr::filter(sigla == "iam")
cie_short <- function(category = NULL,
                      categoria = lifecycle::deprecated()) {
  if (lifecycle::is_present(categoria)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_short(categoria = )",
      "cie_short(category = )"
    )
    category <- categoria
  }

  # Validación escalar: sin esta guarda, un vector llegaba a tolower() y
  # a %in% con error duro no tipado, y un numérico reventaba en tolower().
  if (!is.null(category) && !rlang::is_string(category)) {
    cli::cli_abort(
      "{.arg category} debe ser un string character no-NA de longitud 1 o {.code NULL}, no {.obj_type_friendly {category}}.",
      class = "ciecl_invalid_input"
    )
  }

  siglas <- get_siglas_medicas()

  resultado <- tibble::tibble(
    sigla = names(siglas),
    termino_busqueda = vapply(siglas, \(x) x$termino, character(1)),
    categoria = vapply(siglas, \(x) x$categoria, character(1))
  )

  if (!is.null(category)) {
    category <- tolower(category)
    categorias_validas <- unique(resultado$categoria)

    if (!category %in% categorias_validas) {
      categorias_validas_vec <- cli::cli_vec(
        categorias_validas,
        style = list("vec-last" = " y ")
      )
      cli::cli_warn(c(
        "Categor\u00eda {.val {category}} no encontrada.",
        "i" = "Categor\u00edas v\u00e1lidas: {.val {categorias_validas_vec}}"
      ))
      return(resultado[0, ])
    }

    resultado <- resultado[resultado$categoria == category, ]
  }

  return(resultado)
}

#' Listar siglas médicas (deprecated)
#'
#' `r lifecycle::badge("deprecated")` Use [cie_short()].
#'
#' @param categoria Character opcional, filtrar por categoría
#' @returns tibble con columnas: sigla, termino_busqueda, categoria
#' @family busqueda
#' @keywords internal
#' @export
cie_siglas <- function(categoria = NULL) {
  lifecycle::deprecate_warn(
    "0.9.8",
    "cie_siglas()",
    "cie_short()"
  )
  cie_short(category = categoria)
}
