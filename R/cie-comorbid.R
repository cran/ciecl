#' @importFrom stringr str_detect
#' @importFrom tibble tibble as_tibble
NULL

#' Calcular comorbilidades Charlson/Elixhauser para Chile
#'
#' @param data data.frame con columnas id paciente + codigos CIE-10
#' @param id String nombre columna identificador paciente
#' @param code String nombre columna con codigos CIE-10 (uno por fila)
#' @param map Character, esquema comorbilidad ("charlson" o "elixhauser")
#' @param assign0 Logical, asignar 0 si sin comorbilidad (default TRUE)
#' @returns tibble ancho con scores comorbilidad por paciente
#' @family comorbidities
#' @seealso [cie_map_comorbid()], [cie_norm()]
#' @export
#' @examples
#' # Ver documentacion de parametros
#' args(cie_comorbid)
#'
#' @examplesIf rlang::is_interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' df <- data.frame(
#'   id_pac = c(1, 1, 2, 2),
#'   diag = c("E11.0", "I21.0", "C50.9", "E10.9")
#' )
#' cie_comorbid(df, id = "id_pac", code = "diag", map = "charlson")
cie_comorbid <- function(data, id, code, map = c("charlson", "elixhauser"),
                         assign0 = TRUE) {
  rlang::check_required(data)
  rlang::check_required(id)
  rlang::check_required(code)

  # Validacion de inputs al inicio (patron del paquete: error tipado
  # en lugar del error base de R con vectores de largo > 1)
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} debe ser un data.frame, no {.obj_type_friendly {data}}.",
      class = "ciecl_invalid_input"
    )
  }
  if (!rlang::is_string(id)) {
    cli::cli_abort(
      "{.arg id} debe ser un string character no-NA de longitud 1, no {.obj_type_friendly {id}}.",
      class = "ciecl_invalid_input"
    )
  }
  if (!rlang::is_string(code)) {
    cli::cli_abort(
      "{.arg code} debe ser un string character no-NA de longitud 1, no {.obj_type_friendly {code}}.",
      class = "ciecl_invalid_input"
    )
  }

  # Verificar que comorbidity este instalado
  rlang::check_installed("comorbidity", reason = "para calcular scores de comorbilidad (Charlson/Elixhauser).")

  map <- rlang::arg_match(map)

  # Validar columnas existen
  if (!id %in% names(data) || !code %in% names(data)) {
    cli::cli_abort(
      "Columnas {.field {id}} y/o {.field {code}} no existen en {.arg data}.",
      class = "ciecl_invalid_input"
    )
  }

  # Advertir sobre NAs en columna de codigos
  n_na <- sum(is.na(data[[code]]))
  if (n_na > 0) {
    cli::cli_warn("Columna {.field {code}} contiene {.val {n_na}} valores NA que ser\u00e1n ignorados.")
    data <- data[!is.na(data[[code]]), ]
  }

  # Advertir sobre codigos vacios
  n_empty <- sum(nchar(trimws(as.character(data[[code]]))) == 0, na.rm = TRUE)
  if (n_empty > 0) {
    cli::cli_warn("Columna {.field {code}} contiene {.val {n_empty}} c\u00f3digos vac\u00edos que ser\u00e1n ignorados.")
    data <- data[nchar(trimws(as.character(data[[code]]))) > 0, ]
  }

  # Normalizar codigos (elimina sufijo X DEIS, agrega punto, etc.)
  data[[code]] <- cie_norm(data[[code]], search_db = FALSE)

  # Mapear a nomenclatura comorbidity package
  map_full <- switch(map,
    "charlson" = "charlson_icd10_quan",
    "elixhauser" = "elixhauser_icd10_quan"
  )

  # Mapeo Charlson adaptado Chile (usa comorbidity::comorbidity)
  # Nota: version mas reciente de comorbidity no requiere argumento 'icd'
  resultado <- comorbidity::comorbidity(
    x = data,
    id = id,
    code = code,
    map = map_full,
    assign0 = assign0,
    labelled = FALSE
  )

  # Score total Charlson (si aplica)
  if (map == "charlson") {
    resultado$score_charlson <- comorbidity::score(
      resultado,
      weights = "charlson",
      assign0 = assign0
    )
  }

  return(tibble::as_tibble(resultado))
}

#' Mapeo manual de grupos de comorbilidad específicos de Chile
#'
#' Agrupa códigos CIE-10 chilenos en categorías de comorbilidad MINSAL.
#' Basado en Decreto 1301/2016 MINSAL + icd::icd10_map_charlson.
#'
#' @param codes Character vector de codigos
#' @param codigos `r lifecycle::badge("deprecated")` Use `codes`.
#' @returns tibble con columnas: codigo, categoria
#' @family comorbidities
#' @seealso [cie_comorbid()], [cie_norm()]
#' @export
#' @examples
#' cie_map_comorbid(c("E11.0", "I50.9", "C50.9"))
cie_map_comorbid <- function(codes, codigos = lifecycle::deprecated()) {
  if (lifecycle::is_present(codigos)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie_map_comorbid(codigos = )",
      "cie_map_comorbid(codes = )"
    )
    codes <- codigos
  }

  rlang::check_required(codes)

  # Manejar vector vacio
  if (length(codes) == 0) {
    return(tibble::tibble(
      codigo = character(0),
      categoria = character(0)
    ))
  }

  # Advertir (sin cambiar el resultado) cuando una entrada no tiene
  # formato CIE-10 valido: se clasifica como "Otra" igual que un codigo
  # valido no mapeado, pero la primera es un problema de datos (ybs34).
  # Los NA no cuentan en este warning.
  formato_valido <- cie_validate_vector(codes)
  invalidos <- codes[!is.na(codes) & !formato_valido]
  if (length(invalidos) > 0) {
    invalidos_vec <- cli::cli_vec(invalidos, style = list("vec-last" = " y "))
    cli::cli_warn(c(
      "{length(invalidos)} c\u00f3digo{?s} sin formato CIE-10 v\u00e1lido, clasificado{?s} como {.val Otra}.",
      "i" = "Entradas: {.val {invalidos_vec}}"
    ))
  }

  # Categorizacion vectorizada con case_when
  resultado <- tibble::tibble(
    codigo = codes,
    categoria = dplyr::case_when(
      is.na(codes) ~ "Otra",
      stringr::str_detect(codes, "^E10|^E11") ~ "Diabetes",
      stringr::str_detect(codes, "^I50") ~ "Insuficiencia cardiaca",
      stringr::str_detect(codes, "^I21|^I22") ~ "Infarto miocardio",
      stringr::str_detect(codes, "^C[0-9]{2}") ~ "Neoplasia maligna",
      stringr::str_detect(codes, "^J4[0-4]") ~ "EPOC",
      stringr::str_detect(codes, "^N18") ~ "Enfermedad renal cronica",
      stringr::str_detect(codes, "^F[0-9]{2}") ~ "Trastornos mentales",
      .default = "Otra"
    )
  )

  return(resultado)
}
