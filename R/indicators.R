# Integrated epidemiological indicators ---------------------------------

.indicator_column <- function(dados, coluna, argument) {
  if (!is.character(coluna) || length(coluna) != 1L ||
      is.na(coluna) || !nzchar(coluna)) {
    stop("The '", argument, "' argument must name one column",
         call. = FALSE)
  }
  if (!coluna %in% names(dados)) {
    stop("Column '", coluna, "' was not found in 'dados'", call. = FALSE)
  }
  coluna
}

.indicator_groups <- function(dados, grupo) {
  if (is.null(grupo)) {
    return(list(
      rows = list(seq_len(nrow(dados))),
      values = NULL
    ))
  }
  if (!is.character(grupo) || !length(grupo) ||
      anyNA(grupo) || any(!nzchar(grupo))) {
    stop("'grupo' must contain valid column names", call. = FALSE)
  }
  if (anyDuplicated(grupo)) {
    stop("'grupo' must not contain duplicated column names",
         call. = FALSE)
  }
  missing <- setdiff(grupo, names(dados))
  if (length(missing)) {
    stop(
      "Grouping column(s) not found in 'dados': ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  values <- dados[grupo]
  if (anyNA(values)) {
    stop("Grouping columns must not contain missing values",
         call. = FALSE)
  }
  key <- do.call(
    paste,
    c(lapply(values, as.character), sep = "\u001f")
  )
  levels <- unique(key)
  first <- match(levels, key)
  list(
    rows = lapply(levels, function(value) which(key == value)),
    values = values[first, , drop = FALSE]
  )
}

.indicator_sum <- function(index, value, na_rm) {
  selected <- value[index]
  if (!na_rm && anyNA(selected)) {
    return(NA_real_)
  }
  if (na_rm && all(is.na(selected))) {
    return(NA_real_)
  }
  sum(selected, na.rm = na_rm)
}

.indicator_binomial_interval <- function(numerador, denominador, confianca,
                                         multiplicador) {
  lower <- upper <- rep(NA_real_, length(numerador))
  complete <- !is.na(numerador) & !is.na(denominador) & denominador > 0
  for (index in which(complete)) {
    interval <- stats::binom.test(
      numerador[[index]],
      denominador[[index]],
      conf.level = confianca
    )$conf.int
    lower[[index]] <- interval[[1L]] * multiplicador
    upper[[index]] <- interval[[2L]] * multiplicador
  }
  list(lower = lower, upper = upper)
}

#' Aggregate epidemiological indicators
#'
#' Aggregates numerator and denominator counts before calculating rates,
#' proportions, ratios or case fatality. Rates use exact Poisson confidence
#' intervals; proportions and case fatality use exact binomial intervals.
#'
#' @param dados A non-empty data frame.
#' @param numerador Name of the non-negative numerator column.
#' @param denominador Name of the non-negative denominator column.
#' @param grupo Optional character vector naming grouping columns.
#' @param tipo Indicator type: `"taxa"`, `"proporcao"`, `"razao"` or
#'   `"letalidade"`.
#' @param multiplicador Positive scale factor. When omitted, defaults to
#'   100,000 for rates, 100 for proportions and case fatality, and 1 for
#'   ratios.
#' @param confianca Optional confidence level. Exact intervals are available
#'   for rates, proportions and case fatality, but not for ratios.
#' @param na_rm Whether rows with a missing numerator or denominator should be
#'   removed pairwise within each group. A group containing no complete row
#'   remains missing.
#' @param zero_denominador How to handle zero denominators: `"na"` returns
#'   `NA`, while `"erro"` stops.
#'
#' @return A data frame containing the grouping columns, aggregated
#'   `numerador`, aggregated `denominador`, `indicador`, and optional
#'   confidence limits.
#' @export
#'
#' @examples
#' dados <- data.frame(
#'   ano = c(2024, 2024, 2025),
#'   casos = c(10, 20, 15),
#'   populacao = c(50000, 50000, 100000)
#' )
#' calcular_indicador(
#'   dados, "casos", "populacao", grupo = "ano", tipo = "taxa"
#' )
calcular_indicador <- function(dados, numerador, denominador, grupo = NULL,
                               tipo = c(
                                 "taxa", "proporcao", "razao", "letalidade"
                               ),
                               multiplicador = NULL, confianca = NULL,
                               na_rm = FALSE,
                               zero_denominador = c("na", "erro")) {
  if (!is.data.frame(dados) || !nrow(dados)) {
    stop("'dados' must be a non-empty data frame", call. = FALSE)
  }
  numerador <- .indicator_column(dados, numerador, "numerador")
  denominador <- .indicator_column(dados, denominador, "denominador")
  tipo <- match.arg(tipo)
  if (is.null(multiplicador)) {
    multiplicador <- switch(
      tipo,
      taxa = 100000,
      proporcao = 100,
      letalidade = 100,
      razao = 1
    )
  }
  multiplicador <- .analytics_validate_multiplier(multiplicador)
  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("'na_rm' must be TRUE or FALSE", call. = FALSE)
  }
  zero_denominador <- match.arg(zero_denominador)
  if (!is.null(confianca)) {
    confianca <- .analytics_validate_confidence(confianca)
    if (identical(tipo, "razao")) {
      stop("Confidence intervals are not available for ratios",
           call. = FALSE)
    }
  }

  numerator_values <- .analytics_numeric(dados[[numerador]], numerador)
  denominator_values <- .analytics_numeric(
    dados[[denominador]], denominador
  )
  groups <- .indicator_groups(dados, grupo)
  aggregation_rows <- groups$rows
  if (na_rm) {
    aggregation_rows <- lapply(aggregation_rows, function(index) {
      complete <- !is.na(numerator_values[index]) &
        !is.na(denominator_values[index])
      index[complete]
    })
  }
  numerator_sum <- vapply(
    aggregation_rows,
    .indicator_sum,
    numeric(1),
    value = numerator_values,
    na_rm = na_rm
  )
  denominator_sum <- vapply(
    aggregation_rows,
    .indicator_sum,
    numeric(1),
    value = denominator_values,
    na_rm = na_rm
  )

  if (tipo %in% c("proporcao", "letalidade") &&
      any(numerator_sum > denominator_sum, na.rm = TRUE)) {
    stop(
      "Aggregated numerators cannot exceed denominators for proportions ",
      "or case fatality",
      call. = FALSE
    )
  }
  if (!is.null(confianca)) {
    whole <- numerator_sum == floor(numerator_sum)
    if (tipo %in% c("proporcao", "letalidade")) {
      whole <- whole & denominator_sum == floor(denominator_sum)
    }
    if (any(!whole, na.rm = TRUE)) {
      stop("Exact confidence intervals require whole counts",
           call. = FALSE)
    }
  }

  zero <- !is.na(denominator_sum) & denominator_sum == 0
  if (any(zero) && identical(zero_denominador, "erro")) {
    stop("Indicators cannot be calculated with a zero denominator",
         call. = FALSE)
  }
  if (any(zero & !is.na(numerator_sum) & numerator_sum > 0)) {
    warning(
      "Positive numerators with a zero denominator were converted to NA",
      call. = FALSE
    )
  }
  indicator <- numerator_sum / denominator_sum * multiplicador
  indicator[zero] <- NA_real_

  result <- data.frame(
    numerador = numerator_sum,
    denominador = denominator_sum,
    indicador = indicator
  )
  if (!is.null(groups$values)) {
    result <- data.frame(
      groups$values,
      result,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(confianca)) {
    if (identical(tipo, "taxa")) {
      interval <- intervalo_taxa(
        numerator_sum,
        denominator_sum,
        multiplicador = multiplicador,
        confianca = confianca,
        zero_denominador = zero_denominador
      )
      result$limite_inferior <- interval$limite_inferior
      result$limite_superior <- interval$limite_superior
    } else {
      interval <- .indicator_binomial_interval(
        numerator_sum, denominator_sum, confianca, multiplicador
      )
      result$limite_inferior <- interval$lower
      result$limite_superior <- interval$upper
    }
  }
  row.names(result) <- NULL
  attr(result, "tipo") <- tipo
  attr(result, "multiplicador") <- multiplicador
  attr(result, "confianca") <- confianca
  result
}

.indicator_wrapper <- function(dados, numerador, denominador, grupo,
                               tipo, nome, multiplicador, confianca,
                               na_rm, zero_denominador) {
  result <- calcular_indicador(
    dados = dados,
    numerador = numerador,
    denominador = denominador,
    grupo = grupo,
    tipo = tipo,
    multiplicador = multiplicador,
    confianca = confianca,
    na_rm = na_rm,
    zero_denominador = zero_denominador
  )
  names(result)[names(result) == "indicador"] <- nome
  result
}

#' Calculate grouped mortality rates
#'
#' @inheritParams calcular_indicador
#' @param obitos Name of the deaths column.
#' @param populacao Name of the population denominator column.
#' @param multiplicador Positive rate scale, default 100,000.
#'
#' @return A data frame with aggregated counts and `taxa_mortalidade`.
#' @export
#'
#' @examples
#' dados <- data.frame(ano = c(2024, 2024), obitos = c(3, 7),
#'                     populacao = c(5000, 5000))
#' taxa_mortalidade(dados, "obitos", "populacao", grupo = "ano")
taxa_mortalidade <- function(dados, obitos, populacao, grupo = NULL,
                             multiplicador = 100000, confianca = NULL,
                             na_rm = FALSE,
                             zero_denominador = c("na", "erro")) {
  .indicator_wrapper(
    dados, obitos, populacao, grupo, "taxa", "taxa_mortalidade",
    multiplicador, confianca, na_rm, zero_denominador
  )
}

#' Calculate grouped incidence rates
#'
#' @inheritParams calcular_indicador
#' @param casos Name of the incident cases column.
#' @param populacao Name of the population or person-time denominator column.
#' @param multiplicador Positive rate scale, default 100,000.
#'
#' @return A data frame with aggregated counts and `taxa_incidencia`.
#' @export
#'
#' @examples
#' dados <- data.frame(ano = c(2024, 2024), casos = c(10, 20),
#'                     populacao = c(50000, 50000))
#' taxa_incidencia(dados, "casos", "populacao", grupo = "ano")
taxa_incidencia <- function(dados, casos, populacao, grupo = NULL,
                            multiplicador = 100000, confianca = NULL,
                            na_rm = FALSE,
                            zero_denominador = c("na", "erro")) {
  .indicator_wrapper(
    dados, casos, populacao, grupo, "taxa", "taxa_incidencia",
    multiplicador, confianca, na_rm, zero_denominador
  )
}

#' Calculate grouped case fatality
#'
#' @inheritParams calcular_indicador
#' @param obitos Name of the deaths among cases column.
#' @param casos Name of the cases column.
#' @param multiplicador Positive scale, default 100 for a percentage.
#'
#' @return A data frame with aggregated counts and `letalidade`.
#' @export
#'
#' @examples
#' dados <- data.frame(doenca = c("A", "A"), obitos = c(1, 2),
#'                     casos = c(20, 30))
#' letalidade(dados, "obitos", "casos", grupo = "doenca")
letalidade <- function(dados, obitos, casos, grupo = NULL,
                       multiplicador = 100, confianca = NULL,
                       na_rm = FALSE,
                       zero_denominador = c("na", "erro")) {
  .indicator_wrapper(
    dados, obitos, casos, grupo, "letalidade", "letalidade",
    multiplicador, confianca, na_rm, zero_denominador
  )
}

#' Calculate grouped proportions
#'
#' @inheritParams calcular_indicador
#' @param parte Name of the subset count column.
#' @param total Name of the total count column.
#' @param multiplicador Positive scale, default 100 for a percentage.
#'
#' @return A data frame with aggregated counts and `proporcao`.
#' @export
#'
#' @examples
#' dados <- data.frame(ano = c(2024, 2024), vacinados = c(40, 45),
#'                     elegiveis = c(50, 50))
#' proporcao(dados, "vacinados", "elegiveis", grupo = "ano")
proporcao <- function(dados, parte, total, grupo = NULL,
                      multiplicador = 100, confianca = NULL,
                      na_rm = FALSE,
                      zero_denominador = c("na", "erro")) {
  .indicator_wrapper(
    dados, parte, total, grupo, "proporcao", "proporcao",
    multiplicador, confianca, na_rm, zero_denominador
  )
}

.population_join_columns <- function(por) {
  if (!is.character(por) || !length(por) ||
      anyNA(por) || any(!nzchar(por))) {
    stop("'por' must contain one or more column names", call. = FALSE)
  }
  mapping <- names(por)
  unnamed <- is.null(mapping) || all(!nzchar(mapping))
  if (unnamed) {
    return(list(dados = unname(por), populacao = unname(por)))
  }
  if (any(!nzchar(mapping))) {
    stop(
      "When 'por' is named, every data key must have a population key",
      call. = FALSE
    )
  }
  list(dados = mapping, populacao = unname(por))
}

.population_join_key <- function(dados, colunas, argument) {
  missing <- setdiff(colunas, names(dados))
  if (length(missing)) {
    stop(
      "Join column(s) not found in '", argument, "': ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  values <- dados[colunas]
  if (anyNA(values)) {
    stop("Join columns in '", argument, "' must not contain missing values",
         call. = FALSE)
  }
  do.call(paste, c(lapply(values, as.character), sep = "\u001f"))
}

#' Join validated population denominators
#'
#' Performs a many-to-one join from observations to a population table,
#' preserving observation order and rejecting ambiguous denominator keys.
#' This is designed to connect outputs from [populacao_residente()] to event
#' tables before calculating population-based indicators.
#'
#' @param dados Observation data frame.
#' @param populacao Population data frame.
#' @param por Character vector of join columns. An unnamed vector uses the
#'   same names in both tables. In a named vector, names refer to columns in
#'   `dados` and values to columns in `populacao`.
#' @param coluna_populacao Name of the numeric denominator column in
#'   `populacao`.
#' @param nome Name assigned to the joined denominator column.
#' @param ausente How to handle observations without a denominator:
#'   `"erro"` stops and `"na"` keeps them with a missing value.
#' @param sobrescrever Whether an existing `nome` column in `dados` may be
#'   replaced.
#'
#' @return `dados` with one validated population denominator column appended.
#' @export
#'
#' @examples
#' eventos <- data.frame(codigo = c("A", "B"), ano = 2024, casos = c(2, 4))
#' denominadores <- data.frame(
#'   municipio = c("A", "B"), ano = 2024, habitantes = c(1000, 2000)
#' )
#' juntar_populacao(
#'   eventos, denominadores,
#'   por = c(codigo = "municipio", ano = "ano"),
#'   coluna_populacao = "habitantes"
#' )
juntar_populacao <- function(dados, populacao, por,
                             coluna_populacao = "populacao",
                             nome = "populacao",
                             ausente = c("erro", "na"),
                             sobrescrever = FALSE) {
  if (!is.data.frame(dados) || !is.data.frame(populacao)) {
    stop("'dados' and 'populacao' must be data frames", call. = FALSE)
  }
  coluna_populacao <- .indicator_column(
    populacao, coluna_populacao, "coluna_populacao"
  )
  if (!is.character(nome) || length(nome) != 1L ||
      is.na(nome) || !nzchar(nome)) {
    stop("'nome' must be one valid column name", call. = FALSE)
  }
  if (!is.logical(sobrescrever) || length(sobrescrever) != 1L ||
      is.na(sobrescrever)) {
    stop("'sobrescrever' must be TRUE or FALSE", call. = FALSE)
  }
  if (nome %in% names(dados) && !sobrescrever) {
    stop(
      "Column '", nome, "' already exists in 'dados'; set ",
      "'sobrescrever = TRUE' to replace it",
      call. = FALSE
    )
  }
  ausente <- match.arg(ausente)
  mapping <- .population_join_columns(por)
  data_key <- .population_join_key(dados, mapping$dados, "dados")
  population_key <- .population_join_key(
    populacao, mapping$populacao, "populacao"
  )
  if (anyDuplicated(population_key)) {
    duplicated_keys <- unique(population_key[duplicated(population_key)])
    stop(
      "Population join keys must be unique; found ",
      length(duplicated_keys), " duplicated key(s)",
      call. = FALSE
    )
  }
  denominator <- .analytics_numeric(
    populacao[[coluna_populacao]], coluna_populacao
  )
  matched <- match(data_key, population_key)
  joined <- denominator[matched]
  missing <- is.na(matched) | is.na(joined)
  if (any(missing) && identical(ausente, "erro")) {
    examples <- unique(data_key[missing])
    examples <- utils::head(examples, 5L)
    stop(
      "No population denominator was found for ",
      sum(missing), " observation(s). Example key(s): ",
      paste(examples, collapse = ", "),
      call. = FALSE
    )
  }
  result <- dados
  result[[nome]] <- joined
  provenance <- attr(dados, "datasus_proveniencia", exact = TRUE)
  if (!is.null(provenance)) {
    attr(result, "datasus_proveniencia") <- provenance
  }
  population_provenance <- attr(
    populacao, "datasus_proveniencia", exact = TRUE
  )
  if (!is.null(population_provenance)) {
    attr(result, "datasus_populacao_proveniencia") <- population_provenance
  }
  result
}

.standard_populations <- function() {
  list(
    oms_2000_2025 = c(
      "0-4" = 8.86, "5-9" = 8.69, "10-14" = 8.60,
      "15-19" = 8.47, "20-24" = 8.22, "25-29" = 7.93,
      "30-34" = 7.61, "35-39" = 7.15, "40-44" = 6.59,
      "45-49" = 6.04, "50-54" = 5.37, "55-59" = 4.55,
      "60-64" = 3.72, "65-69" = 2.96, "70-74" = 2.21,
      "75-79" = 1.52, "80-84" = 0.91, "85-89" = 0.44,
      "90-94" = 0.15, "95-99" = 0.04, "100+" = 0.005
    ),
    segi = c(
      "0-4" = 12, "5-9" = 10, "10-14" = 9, "15-19" = 9,
      "20-24" = 8, "25-29" = 8, "30-34" = 6, "35-39" = 6,
      "40-44" = 6, "45-49" = 6, "50-54" = 5, "55-59" = 4,
      "60-64" = 4, "65-69" = 3, "70-74" = 2, "75-79" = 1,
      "80-84" = 0.5, "85+" = 0.5
    ),
    escandinava = c(
      "0-4" = 8, "5-9" = 7, "10-14" = 7, "15-19" = 7,
      "20-24" = 7, "25-29" = 7, "30-34" = 7, "35-39" = 7,
      "40-44" = 7, "45-49" = 7, "50-54" = 7, "55-59" = 6,
      "60-64" = 5, "65-69" = 4, "70-74" = 3, "75-79" = 2,
      "80-84" = 1, "85+" = 1
    )
  )
}

#' Retrieve ready-made standard populations
#'
#' Returns age-group weights for the WHO 2000--2025 world standard, Segi's
#' world standard or the Scandinavian standard. By default weights are
#' normalized to sum to one and can be passed directly to
#' [padronizar_idade()].
#'
#' @param nome Standard population: `"oms_2000_2025"`, `"segi"` or
#'   `"escandinava"`. `"oms"` and `"who"` are accepted aliases.
#' @param formato Output as a named numeric `"vetor"` or a data frame
#'   (`"dados"`).
#' @param normalizar Whether weights should be normalized to sum to one.
#'
#' @return A named numeric vector or a data frame with `faixa_etaria`,
#'   `peso_publicado` and `peso`.
#' @references Ahmad OB et al. (2001). Age standardization of rates: a new
#'   WHO standard.
#'   \url{https://cdn.who.int/media/docs/default-source/gho-documents/global-health-estimates/gpe_discussion_paper_series_paper31_2001_age_standardization_rates.pdf}
#' @export
#'
#' @examples
#' populacao_padrao("oms")
#' populacao_padrao("segi", formato = "dados")
populacao_padrao <- function(nome = "oms_2000_2025",
                             formato = c("vetor", "dados"),
                             normalizar = TRUE) {
  if (!is.character(nome) || length(nome) != 1L || is.na(nome)) {
    stop("'nome' must identify one standard population", call. = FALSE)
  }
  aliases <- c(
    oms = "oms_2000_2025",
    who = "oms_2000_2025",
    oms_2000_2025 = "oms_2000_2025",
    segi = "segi",
    escandinava = "escandinava"
  )
  if (!nome %in% names(aliases)) {
    stop(
      "Unknown standard population. Use 'oms_2000_2025', 'segi' or ",
      "'escandinava'",
      call. = FALSE
    )
  }
  nome <- unname(aliases[[nome]])
  formato <- match.arg(formato)
  if (!is.logical(normalizar) || length(normalizar) != 1L ||
      is.na(normalizar)) {
    stop("'normalizar' must be TRUE or FALSE", call. = FALSE)
  }
  published <- .standard_populations()[[nome]]
  weights <- if (normalizar) published / sum(published) else published
  if (identical(formato, "dados")) {
    result <- data.frame(
      faixa_etaria = names(published),
      peso_publicado = unname(published),
      peso = unname(weights),
      stringsAsFactors = FALSE
    )
  } else {
    result <- weights
  }
  attr(result, "populacao_padrao") <- nome
  attr(result, "normalizada") <- normalizar
  attr(result, "fonte") <- paste(
    "Ahmad OB et al. (2001), WHO Age standardization of rates:",
    "a new WHO standard"
  )
  result
}
