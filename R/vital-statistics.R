# Unified vital-statistics API ---------------------------------------------

#' Query mortality data from SIM
#'
#' Provides a single interface to the mortality tables of the Mortality
#' Information System (SIM). Available datasets are listed by
#' `datasus_catalogo("sim")`; current row, column, measure, period and filter
#' choices can be inspected with `datasus_opcoes("sim", ...)`.
#'
#' @param conjunto SIM dataset: `"obitos"`, `"mortalidade_infantil"`,
#'   `"causas_evitaveis_0_4"` or `"causas_evitaveis_5_74"`.
#' @param abrangencia Geographic form: `"municipio"` for municipality-level
#'   results or `"uf"` for region/state-level results.
#' @param uf Optional state abbreviation, two-digit IBGE code or state name. With
#'   `abrangencia = "municipio"`, selects the state-specific municipal form.
#'   It must be `NULL` with `abrangencia = "uf"`.
#' @param linha Row dimension. `NULL` selects the form's first dimension.
#' @param coluna Column dimension. `NULL` selects `"Não ativa"`.
#' @param conteudo Measure label, raw TABNET value or one-based index.
#' @param periodo `"last"` for the latest available year, an exact period
#'   label or raw value, a four-digit year, or a vector of these values.
#' @param filtros Named list of filters. Use [datasus_opcoes()] to inspect
#'   their names and accepted values.
#'
#' @return A data frame containing the TABNET result, with query provenance
#'   available through [datasus_proveniencia()].
#' @export
#'
#' @examples
#' \donttest{
#' old_options <- options(
#'   datasus.timeout = 5,
#'   datasus.download_timeout = 15,
#'   datasus.max_tries = 1
#' )
#' try({
#' datasus_catalogo("sim")
#' op <- datasus_opcoes(
#'   "sim", "obitos", abrangencia = "uf"
#' )
#' obitos <- sim(
#'   conjunto = "obitos",
#'   abrangencia = "uf",
#'   periodo = 2024,
#'   filtros = list(sexo = "Masculino")
#' )
#' })
#' options(old_options)
#' }
sim <- function(conjunto = "obitos",
                abrangencia = c("municipio", "uf"),
                uf = NULL, linha = NULL, coluna = NULL,
                conteudo = 1, periodo = "last", filtros = list()) {
  abrangencia <- match.arg(abrangencia)
  .datasus_query(
    "sim", conjunto, uf, linha, coluna, conteudo, periodo, filtros,
    abrangencia = abrangencia
  )
}

#' Query live-birth data from SINASC
#'
#' Provides a single interface to the Live Birth Information System
#' (SINASC). Current row, column, measure, period and filter choices can be
#' inspected with `datasus_opcoes("sinasc", ...)`.
#'
#' @param conjunto SINASC dataset. Currently `"nascidos_vivos"`.
#' @inheritParams sim
#'
#' @return A data frame containing the TABNET result, with query provenance
#'   available through [datasus_proveniencia()].
#' @export
#'
#' @examples
#' \donttest{
#' old_options <- options(
#'   datasus.timeout = 5,
#'   datasus.download_timeout = 15,
#'   datasus.max_tries = 1
#' )
#' try({
#' op <- datasus_opcoes(
#'   "sinasc", abrangencia = "municipio", uf = "SP"
#' )
#' nascimentos <- sinasc(
#'   uf = "SP",
#'   periodo = 2024,
#'   filtros = list(sexo = "Masculino")
#' )
#' })
#' options(old_options)
#' }
sinasc <- function(conjunto = "nascidos_vivos",
                   abrangencia = c("municipio", "uf"),
                   uf = NULL, linha = NULL, coluna = NULL,
                   conteudo = 1, periodo = "last", filtros = list()) {
  abrangencia <- match.arg(abrangencia)
  .datasus_query(
    "sinasc", conjunto, uf, linha, coluna, conteudo, periodo, filtros,
    abrangencia = abrangencia
  )
}

.datasus_legacy_vital <- function(old, replacement, sistema, conjunto,
                                  abrangencia) {
  .Deprecated(replacement, package = "datasus", old = old)

  caller <- parent.frame()
  caller_function <- sys.function(sys.parent())
  argument_names <- names(formals(caller_function))
  arguments <- mget(argument_names, envir = caller, inherits = FALSE)

  uf <- if ("uf" %in% names(arguments)) arguments$uf else NULL
  primary <- c("uf", "linha", "coluna", "conteudo", "periodo")
  filtros <- arguments[setdiff(argument_names, primary)]

  .datasus_query(
    sistema = sistema,
    conjunto = conjunto,
    uf = uf,
    linha = arguments$linha,
    coluna = arguments$coluna,
    conteudo = arguments$conteudo,
    periodo = arguments$periodo,
    filtros = unname(filtros),
    abrangencia = abrangencia,
    filtros_posicionais = TRUE
  )
}
