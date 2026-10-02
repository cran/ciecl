#' Buscar códigos CIE-11 vía API OMS
#'
#' @param text String término búsqueda español/inglés
#' @param api_key String opcional, Client ID + Secret OMS separados ":".
#'   Por defecto se lee desde la variable de entorno `ICD_API_KEY` vía
#'   [get_icd_api_key()]. Obtener credenciales en: https://icd.who.int/icdapi
#' @param lang Character, idioma respuesta ("es" o "en")
#' @param max_results Integer, máximo resultados (default 10)
#' @param release Character, versión de release CIE-11 a consultar
#'   (default "2024-01"). Ver releases disponibles en la API OMS.
#' @param texto `r lifecycle::badge("deprecated")` Use `text`.
#' @returns tibble con códigos CIE-11 + títulos o vacío si error
#' @details
#' ## Seguridad de la API key
#'
#' NUNCA escribas la llave literal en tus scripts (p. ej.
#' `api_key = "tu_id:tu_secret"`): si compartes el código (repositorios,
#' correos, capturas) expondrías tus credenciales de forma accidental.
#'
#' La vía recomendada es guardar la llave en la variable de entorno
#' `ICD_API_KEY`, por ejemplo editando `~/.Renviron` con
#' `usethis::edit_r_environ()`; [get_icd_api_key()] la lee
#' automáticamente.
#'
#' El argumento `api_key` existe solo para circunstancias excepcionales
#' (p. ej. manejar múltiples llaves, o entornos donde no es posible fijar
#' una variable de entorno).
#' @family api_who
#' @seealso [cie_search()], [cie_lookup()], [cie_guide()]
#' @export
#' @importFrom tibble as_tibble
#' @examples
#' # Ver parámetros disponibles
#' args(cie11_search)
#'
#' @examplesIf requireNamespace("vcr", quietly = TRUE)
#' \dontshow{
#' # El cassette grabado (inst/_vcr/) reproduce la respuesta de la API sin
#' # conexión ni credenciales reales; la llave ficticia solo satisface el
#' # parseo interno del argumento api_key y se restaura al final.
#' .key_prev <- Sys.getenv("ICD_API_KEY", unset = NA)
#' if (is.na(.key_prev)) Sys.setenv(ICD_API_KEY = "client_id:client_secret")
#' vcr::insert_example_cassette("cie11_search", package = "ciecl",
#'                              match_requests_on = c("method", "uri"))
#' }
#' # Requiere credenciales OMS gratuitas (https://icd.who.int/icdapi)
#' cie11_search("depresion mayor")
#' \dontshow{
#' vcr::eject_cassette()
#' if (is.na(.key_prev)) Sys.unsetenv("ICD_API_KEY")
#' }
#'
#' @examplesIf requireNamespace("vcr", quietly = TRUE)
#' \dontshow{
#' .key_prev <- Sys.getenv("ICD_API_KEY", unset = NA)
#' if (is.na(.key_prev)) Sys.setenv(ICD_API_KEY = "client_id:client_secret")
#' vcr::insert_example_cassette("cie11_search_cronicos", package = "ciecl",
#'                              match_requests_on = c("method", "uri"))
#' }
#' # Causas frecuentes de egreso en Chile
#' cie11_search("diabetes mellitus")
#' cie11_search("hipertension esencial")
#' \dontshow{
#' vcr::eject_cassette()
#' if (is.na(.key_prev)) Sys.unsetenv("ICD_API_KEY")
#' }
cie11_search <- function(text, api_key = get_icd_api_key(),
                         lang = c("es", "en"),
                         max_results = 10, release = "2024-01",
                         texto = lifecycle::deprecated()) {
  # Deprecación: argumento en español -> inglés
  if (lifecycle::is_present(texto)) {
    lifecycle::deprecate_warn(
      "0.9.8",
      "cie11_search(texto = )",
      "cie11_search(text = )"
    )
    text <- texto
  }

  rlang::check_required(text)
  lang <- rlang::arg_match(lang)

  # Validación de inputs
  if (!rlang::is_string(text)) {
    cli::cli_abort(
      "{.arg text} debe ser un string de largo 1, no {.obj_type_friendly {text}}.",
      class = "ciecl_invalid_input"
    )
  }
  if (!nzchar(trimws(text))) {
    cli::cli_abort("{.arg text} no puede estar vac\u00edo.", class = "ciecl_invalid_input")
  }
  # Variables explicativas para la condición compuesta (legibilidad)
  is_numeric_scalar <- is.numeric(max_results) && length(max_results) == 1
  is_positive_integer <- is_numeric_scalar && !is.na(max_results) &&
    max_results >= 1 && max_results == as.integer(max_results)
  if (!is_positive_integer) {
    cli::cli_abort("{.arg max_results} debe ser un entero positivo.", class = "ciecl_invalid_input")
  }
  if (!is.character(release) || length(release) != 1 ||
    !grepl("^\\d{4}-\\d{2}$", release)) {
    cli::cli_abort(
      "{.arg release} debe ser formato {.val YYYY-MM} (ej. {.val 2024-01}).",
      class = "ciecl_invalid_input"
    )
  }

  # Verificar que httr2 esté instalado
  rlang::check_installed("httr2", reason = "para consultar la API oficial de la OMS (CIE-11).")

  # Construir cliente OAuth (parsea "client_id:client_secret" del api_key)
  credentials <- strsplit(api_key, ":")[[1]]
  if (length(credentials) != 2) {
    cli::cli_abort(
      "API key debe tener formato {.val client_id:client_secret}",
      class = "ciecl_invalid_input"
    )
  }

  who_client <- httr2::oauth_client(
    id = credentials[1],
    secret = credentials[2],
    token_url = "https://icdaccessmanagement.who.int/connect/token",
    name = "ciecl"
  )

  tryCatch(
    {
      # Buscar en CIE-11; req_oauth_client_credentials() obtiene y cachea
      # el token de acceso transparentemente antes de la peticion.
      search_url <- paste0(
        "https://id.who.int/icd/release/11/", release, "/mms/search"
      )

      search_req <- httr2::request(search_url) |>
        httr2::req_user_agent("ciecl (https://github.com/ropensci/ciecl)") |>
        httr2::req_timeout(30) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_throttle(rate = 10 / 60) |> # 10 req/min para ser conservador
        httr2::req_url_query(
          q = text,
          flatResults = "true",
          useFlexisearch = "true"
        ) |>
        httr2::req_headers(
          `API-Version` = "v2",
          `Accept-Language` = lang
        ) |>
        httr2::req_oauth_client_credentials(
          client = who_client,
          scope = "icdapi_access"
        ) |>
        httr2::req_error(body = who_error_body)

      search_resp <- httr2::req_perform(search_req)
      # Verifica explicitamente status 2xx; req_perform ya lanza httr2_http_*
      # en 4xx/5xx, este check defensivo asegura el tipado en cualquier ruta.
      httr2::resp_check_status(search_resp)
      json <- httr2::resp_body_json(search_resp, simplifyVector = TRUE)

      # Parsear resultados
      has_results <- "destinationEntities" %in% names(json) &&
        length(json$destinationEntities) > 0
      if (has_results) {
        # Limpiar HTML tags del titulo
        html_pattern <- "<em class='found'>|</em>"
        titulos_limpios <- gsub(html_pattern, "", json$destinationEntities$title)

        resultados <- tibble::tibble(
          codigo = json$destinationEntities$theCode,
          titulo = titulos_limpios,
          capitulo = json$destinationEntities$chapter
        ) |>
          dplyr::slice_head(n = max_results)

        return(resultados)
      } else {
        cli::cli_inform(c("i" = "Sin resultados CIE-11 para: {.val {text}}"))
        return(tibble::tibble(
          codigo = character(),
          titulo = character(),
          capitulo = character()
        ))
      }
    },
    error = function(e) {
      cli::cli_warn(c(
        "Error API CIE-11: {conditionMessage(e)}",
        "i" = "Retornando resultado vac\u00edo.",
        "i" = "Usa {.fn cie_search} para fallback local CIE-10."
      ))
      return(tibble::tibble(
        codigo = character(),
        titulo = character(),
        capitulo = character()
      ))
    }
  )
}

#' Obtener la API key de la OMS desde el entorno
#'
#' Lee la variable de entorno `ICD_API_KEY` y aborta con un error
#' informativo si no está configurada. Es el valor por defecto del
#' argumento `api_key` de [cie11_search()], siguiendo el patrón
#' recomendado por httr2 para envolver APIs.
#'
#' @details
#' Configura la variable editando `~/.Renviron` con
#' `usethis::edit_r_environ()`:
#'
#' ```
#' ICD_API_KEY=tu_client_id:tu_client_secret
#' ```
#'
#' Nunca escribas la llave literal en scripts que vayas a compartir.
#'
#' @returns String con la API key en formato "client_id:client_secret".
#' @family api_who
#' @seealso [cie11_search()]
#' @export
#' @examples
#' # Requiere ICD_API_KEY configurada (ver Details)
#' try(get_icd_api_key())
get_icd_api_key <- function() {
  api_key <- Sys.getenv("ICD_API_KEY", unset = NA)
  if (is.na(api_key)) {
    # call = caller_env() para que el error se atribuya a la funcion
    # llamante (p. ej. cie11_search) y no a este helper
    cli::cli_abort(
      "API key OMS requerida. Ver: {.url https://icd.who.int/icdapi}",
      class = "ciecl_api_error",
      call = rlang::caller_env()
    )
  }
  api_key
}

#' Extraer detalle de error desde body de respuesta OMS
#'
#' Pasado a `httr2::req_error(body = ...)` para enriquecer condiciones
#' tipadas `httr2_http_*` con el mensaje legible que la API OMS emite
#' en el body JSON (`error_description`, `error` o `message`).
#'
#' @param resp Objeto httr2_response.
#' @returns String con el detalle, o `NULL` si no se puede extraer.
#' @keywords internal
#' @noRd
who_error_body <- function(resp) {
  body <- tryCatch(
    httr2::resp_body_json(resp),
    error = function(e) NULL
  )
  if (is.null(body)) {
    return(NULL)
  }
  for (field in c("error_description", "error", "message")) {
    val <- body[[field]]
    if (!is.null(val) && nzchar(as.character(val))) {
      return(val)
    }
  }
  NULL
}
