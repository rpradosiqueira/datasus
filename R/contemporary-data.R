# Contemporary OpenDataSUS datasets --------------------------------------

.contemporary_year <- function(ano, minimum = 2020L) {
  if (identical(ano, "last")) {
    return(ano)
  }
  if (!is.numeric(ano) || length(ano) != 1L || is.na(ano) ||
      ano != as.integer(ano) || ano < minimum || ano > 9999L) {
    stop(
      "'ano' must be 'last' or one four-digit year from ",
      minimum, " onwards",
      call. = FALSE
    )
  }
  as.integer(ano)
}

.contemporary_format <- function(formato, choices) {
  if (!is.character(formato) || length(formato) != 1L ||
      is.na(formato) || !nzchar(formato)) {
    stop("'formato' must identify one resource format", call. = FALSE)
  }
  result <- toupper(formato)
  if (!result %in% choices) {
    stop(
      "'formato' must be one of: ",
      paste(choices, collapse = ", "),
      call. = FALSE
    )
  }
  result
}

.contemporary_latest_dataset <- function(busca, pattern, cache, atualizar) {
  catalog <- opendatasus_catalogo(
    busca = busca,
    limite = 100L,
    cache = cache,
    atualizar = atualizar
  )
  keep <- !is.na(catalog$conjunto) &
    grepl(pattern, catalog$conjunto, perl = TRUE)
  candidates <- catalog[keep, , drop = FALSE]
  if (!nrow(candidates)) {
    stop(
      "No annual OpenDataSUS dataset was found for '", busca, "'",
      call. = FALSE
    )
  }
  years <- vapply(
    paste(candidates$conjunto, candidates$titulo),
    .opendatasus_resource_year,
    integer(1)
  )
  if (all(is.na(years))) {
    stop(
      "Could not identify years in the OpenDataSUS catalog for '",
      busca, "'",
      call. = FALSE
    )
  }
  candidates$conjunto[[which.max(years)]]
}

.contemporary_resource <- function(resources, formato, keep = NULL,
                                   description = "requested") {
  selected <- resources[
    !is.na(resources$formato) &
      toupper(resources$formato) == toupper(formato),
    ,
    drop = FALSE
  ]
  if (!is.null(keep)) {
    if (!is.logical(keep) || length(keep) != nrow(resources) ||
        anyNA(keep)) {
      stop("Internal resource selection is invalid", call. = FALSE)
    }
    selected <- resources[
      keep &
        !is.na(resources$formato) &
        toupper(resources$formato) == toupper(formato),
      ,
      drop = FALSE
    ]
  }
  if (!nrow(selected)) {
    stop(
      "No ", description, " resource is currently published in ",
      formato, " format. Use opendatasus_recursos() to inspect the ",
      "dataset.",
      call. = FALSE
    )
  }
  if (nrow(selected) > 1L) {
    stop(
      "The ", description, " resource selection is ambiguous: ",
      paste(utils::head(selected$nome, 5L), collapse = "; "),
      call. = FALSE
    )
  }
  selected
}

.contemporary_read_resource <- function(resource, destino, cache,
                                        atualizar, n_max, colunas,
                                        sistema, normalizar, ...) {
  normalizar <- .opendatasus_validate_normalize(normalizar)
  result <- opendatasus_ler(
    conjunto = resource$conjunto[[1L]],
    recurso = resource$id[[1L]],
    ano = NULL,
    formato = resource$formato[[1L]],
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    n_max = n_max,
    colunas = colunas,
    ...
  )
  if (normalizar) {
    result <- datasus_padronizar(result, sistema)
  }
  result
}

.pni_month_names <- c(
  janeiro = 1L,
  fevereiro = 2L,
  marco = 3L,
  abril = 4L,
  maio = 5L,
  junho = 6L,
  julho = 7L,
  agosto = 8L,
  setembro = 9L,
  outubro = 10L,
  novembro = 11L,
  dezembro = 12L
)

.pni_month <- function(mes) {
  if (identical(mes, "last")) {
    return(mes)
  }
  if (is.character(mes) && length(mes) == 1L && !is.na(mes)) {
    normalized <- stringi::stri_trans_general(tolower(mes), "Latin-ASCII")
    if (normalized %in% names(.pni_month_names)) {
      return(unname(.pni_month_names[[normalized]]))
    }
  }
  if (!is.numeric(mes) || length(mes) != 1L || is.na(mes) ||
      mes != as.integer(mes) || mes < 1L || mes > 12L) {
    stop(
      "'mes' must be 'last', a month name, or an integer from 1 to 12",
      call. = FALSE
    )
  }
  as.integer(mes)
}

.pni_resource_month <- function(name) {
  if (!length(name) || is.na(name) || !nzchar(name)) {
    return(NA_integer_)
  }
  normalized <- stringi::stri_trans_general(tolower(name), "Latin-ASCII")
  found <- names(.pni_month_names)[vapply(
    names(.pni_month_names),
    function(month) grepl(
      paste0("(^|[^a-z])", month, "([^a-z]|$)"),
      normalized,
      perl = TRUE
    ),
    logical(1)
  )]
  if (!length(found)) NA_integer_ else unname(.pni_month_names[[found[[1L]]]])
}

.pni_dataset <- function(ano, cache, atualizar) {
  if (identical(ano, "last")) {
    return(.contemporary_latest_dataset(
      busca = "doses aplicadas PNI",
      pattern = paste0(
        "^(dataset-)?doses-aplicadas-pelo-programa-de-nacional-",
        "de-imunizacoes-pni[-_][0-9]{4}$"
      ),
      cache = cache,
      atualizar = atualizar
    ))
  }
  if (identical(ano, 2022L)) {
    return(paste0(
      "dataset-doses-aplicadas-pelo-programa-de-nacional-",
      "de-imunizacoes-pni_2022"
    ))
  }
  paste0(
    "doses-aplicadas-pelo-programa-de-nacional-de-imunizacoes-pni-",
    ano
  )
}

.syndrome_gripal_dataset <- function(ano, cache, atualizar) {
  if (identical(ano, "last")) {
    return(.contemporary_latest_dataset(
      busca = "sindrome gripal",
      pattern = paste0(
        "^notificacoes-de-sindrome-gripal-leve-[0-9]{4}$"
      ),
      cache = cache,
      atualizar = atualizar
    ))
  }
  paste0("notificacoes-de-sindrome-gripal-leve-", ano)
}

#' Read anonymous ESAVI notifications
#'
#' Downloads anonymous individual records from the e-SUS Notifica ESAVI
#' module. The source contains notifications since January 2021 and is
#' continuously updated. An event following immunization does not by itself
#' establish a causal association with a vaccine.
#'
#' @param formato File format, `"CSV"` or `"JSON"`.
#' @param destino Optional destination file or existing directory.
#' @param cache Whether to reuse the local OpenDataSUS cache.
#' @param atualizar Whether to force a fresh metadata query and download.
#' @param n_max Maximum number of rows to read.
#' @param colunas Optional character vector selecting columns during CSV
#'   parsing.
#' @param normalizar Whether to apply the curated contemporary schema with
#'   [datasus_padronizar()].
#' @param ... Additional arguments passed to the underlying reader.
#'
#' @return The parsed OpenDataSUS resource with provenance metadata.
#' @references OpenDataSUS. ESAVI - Dados sobre Eventos Supostamente
#'   Atribuíveis a Vacinação.
#'   \url{https://dadosabertos.saude.gov.br/dataset/esavi}
#' @export
#'
#' @examples
#' \dontrun{
#' eventos <- esavi(n_max = 1000)
#' datasus_proveniencia(eventos)
#' }
esavi <- function(formato = "CSV", destino = NULL, cache = TRUE,
                  atualizar = FALSE, n_max = Inf, colunas = NULL,
                  normalizar = FALSE, ...) {
  formato <- .contemporary_format(formato, c("CSV", "JSON"))
  normalizar <- .opendatasus_validate_normalize(normalizar)
  result <- opendatasus_ler(
    conjunto = "esavi",
    ano = NULL,
    formato = formato,
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    n_max = n_max,
    colunas = colunas,
    ...
  )
  if (normalizar) {
    result <- datasus_padronizar(result, "esavi")
  }
  result
}

#' Read e-SUS Notifica mild influenza-like illness records
#'
#' Downloads anonymous mild and moderate influenza-like illness notifications
#' published by annual dataset and state. These records are distinct from
#' hospitalized severe acute respiratory syndrome records returned by
#' [sivep_gripe()].
#'
#' @param uf One Brazilian state abbreviation, IBGE code or state name.
#' @param ano Dataset year from 2020 onwards, or `"last"` for the latest
#'   annual dataset currently published.
#' @param formato File format. The annual state resources are published as
#'   `"CSV"`.
#' @inheritParams esavi
#'
#' @return The parsed OpenDataSUS resource with provenance metadata.
#' @references OpenDataSUS. Notificações de Síndrome Gripal.
#'   \url{https://dadosabertos.saude.gov.br/dataset?query=sindrome+gripal}
#' @export
#'
#' @examples
#' \dontrun{
#' casos <- esus_sindrome_gripal(uf = "MS", ano = 2024, n_max = 1000)
#' }
esus_sindrome_gripal <- function(uf, ano = "last", formato = "CSV",
                                  destino = NULL, cache = TRUE,
                                  atualizar = FALSE, n_max = Inf,
                                  colunas = NULL, normalizar = FALSE, ...) {
  uf <- toupper(.tabnet_validate_uf(uf))
  ano <- .contemporary_year(ano)
  formato <- .contemporary_format(formato, "CSV")
  dataset <- .syndrome_gripal_dataset(ano, cache, atualizar)
  resources <- opendatasus_recursos(
    dataset,
    cache = cache,
    atualizar = atualizar
  )
  names <- toupper(trimws(resources$nome))
  keep <- !is.na(names) & grepl(
    paste0("^DADOS[[:space:]]+", uf, "([[:space:]]|-)"),
    names
  )
  resource <- .contemporary_resource(
    resources,
    formato,
    keep,
    paste0("syndrome-gripal resource for ", uf)
  )
  files <- opendatasus_arquivos(
    dataset,
    recurso = resource$id[[1L]],
    formato = formato,
    cache = cache,
    atualizar = atualizar
  )
  .contemporary_read_files(
    files = files,
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    n_max = n_max,
    colunas = colunas,
    sistema = "sindrome_gripal",
    normalizar = normalizar,
    ...
  )
}

#' Read individual PNI dose records
#'
#' Downloads anonymous vaccination records published in monthly files by the
#' National Immunization Program. This record-level source complements the
#' aggregated historical TABNET series returned by [pni_imunizacoes()].
#'
#' @param ano Dataset year from 2020 onwards, or `"last"` for the most recent
#'   annual dataset currently published.
#' @param mes Month number, Portuguese month name, or `"last"` for the latest
#'   month currently published in the selected year and format.
#' @param formato File format, `"CSV"` or `"JSON"`.
#' @inheritParams esavi
#'
#' @return The parsed OpenDataSUS resource with provenance metadata.
#' @references OpenDataSUS. Doses aplicadas pelo Programa Nacional de
#'   Imunizações.
#'   \url{https://dadosabertos.saude.gov.br/dataset?query=doses+aplicadas+PNI}
#' @export
#'
#' @examples
#' \dontrun{
#' doses <- pni_doses(ano = 2025, mes = 1, n_max = 1000)
#' }
pni_doses <- function(ano = "last", mes = "last", formato = "CSV",
                      destino = NULL, cache = TRUE, atualizar = FALSE,
                      n_max = Inf, colunas = NULL,
                      normalizar = FALSE, ...) {
  ano <- .contemporary_year(ano)
  mes <- .pni_month(mes)
  formato <- .contemporary_format(formato, c("CSV", "JSON"))
  dataset <- .pni_dataset(ano, cache, atualizar)
  resources <- opendatasus_recursos(
    dataset,
    cache = cache,
    atualizar = atualizar
  )
  resource_month <- vapply(
    resources$nome,
    .pni_resource_month,
    integer(1)
  )
  available <- !is.na(resource_month) &
    !is.na(resources$formato) &
    toupper(resources$formato) == formato
  if (identical(mes, "last")) {
    if (!any(available)) {
      stop(
        "No monthly PNI resource is currently published in ",
        formato, " format",
        call. = FALSE
      )
    }
    mes <- max(resource_month[available])
  }
  keep <- !is.na(resource_month) & resource_month == mes
  resource <- .contemporary_resource(
    resources,
    formato,
    keep,
    paste0("PNI resource for month ", mes)
  )
  .contemporary_read_resource(
    resource, destino, cache, atualizar, n_max, colunas,
    "pni_doses", normalizar, ...
  )
}

#' Read COVID-19 hospital occupancy records
#'
#' Downloads annual records from the e-SUS Notifica Hospital Admissions
#' module. The published files cover SUS clinical and intensive-care beds
#' allocated to suspected or confirmed COVID-19 cases. Fields added in 2022
#' are not populated in earlier records.
#'
#' @param ano Resource year or `"last"` for the latest year currently
#'   published.
#' @param formato File format, `"CSV"` or `"JSON"`.
#' @inheritParams esavi
#'
#' @return The parsed OpenDataSUS resource with provenance metadata.
#' @references OpenDataSUS. Registro de Ocupação Hospitalar COVID-19.
#'   \url{https://dadosabertos.saude.gov.br/dataset/registro-de-ocupacao-hospitalar-covid-19}
#' @export
#'
#' @examples
#' \dontrun{
#' leitos <- ocupacao_hospitalar(ano = 2022, n_max = 1000)
#' }
ocupacao_hospitalar <- function(ano = "last", formato = "CSV",
                                destino = NULL, cache = TRUE,
                                atualizar = FALSE, n_max = Inf,
                                colunas = NULL, normalizar = FALSE, ...) {
  ano <- .contemporary_year(ano)
  formato <- .contemporary_format(formato, c("CSV", "JSON"))
  normalizar <- .opendatasus_validate_normalize(normalizar)
  result <- opendatasus_ler(
    conjunto = "registro-de-ocupacao-hospitalar-covid-19",
    ano = ano,
    formato = formato,
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    n_max = n_max,
    colunas = colunas,
    ...
  )
  if (normalizar) {
    result <- datasus_padronizar(result, "ocupacao_hospitalar")
  }
  result
}
