# Epidemiological analysis helpers ---------------------------------------

.analytics_numeric <- function(value, argument, nonnegative = TRUE) {
  if (!is.numeric(value) || !length(value)) {
    stop("The '", argument, "' argument must be a non-empty numeric vector",
         call. = FALSE)
  }
  if (any(!is.finite(value) & !is.na(value))) {
    stop("The '", argument, "' argument must contain finite values",
         call. = FALSE)
  }
  if (nonnegative && any(value < 0, na.rm = TRUE)) {
    stop("The '", argument, "' argument must not contain negative values",
         call. = FALSE)
  }
  value
}

.analytics_recycle <- function(first, second, first_name, second_name) {
  lengths <- c(length(first), length(second))
  target <- max(lengths)
  valid <- lengths == target | lengths == 1L
  if (!all(valid)) {
    stop(
      "The '", first_name, "' and '", second_name,
      "' arguments must have equal lengths or one must be scalar",
      call. = FALSE
    )
  }
  list(
    rep(first, length.out = target),
    rep(second, length.out = target)
  )
}

.analytics_validate_multiplier <- function(multiplicador) {
  if (!is.numeric(multiplicador) || length(multiplicador) != 1L ||
      is.na(multiplicador) || !is.finite(multiplicador) ||
      multiplicador <= 0) {
    stop("The 'multiplicador' argument must be a positive finite number",
         call. = FALSE)
  }
  multiplicador
}

.analytics_zero_denominator <- function(eventos, populacao,
                                        zero_denominador) {
  zero_denominador <- match.arg(zero_denominador, c("na", "erro"))
  invalid <- !is.na(populacao) & populacao == 0
  if (any(invalid) && identical(zero_denominador, "erro")) {
    stop("Rates cannot be calculated with a zero denominator",
         call. = FALSE)
  }
  if (any(invalid & !is.na(eventos) & eventos > 0)) {
    warning(
      "Positive events with a zero denominator were converted to NA",
      call. = FALSE
    )
  }
  invalid
}

#' Calculate crude epidemiological rates
#'
#' Calculates `eventos / populacao * multiplicador` with explicit validation
#' of counts and denominators. Missing values propagate to the result.
#'
#' @param eventos Non-negative event counts.
#' @param populacao Non-negative population or person-time denominators.
#' @param multiplicador Scale of the rate, default 100,000.
#' @param zero_denominador How to handle zero denominators: `"na"` returns
#'   `NA`, while `"erro"` stops.
#'
#' @return A numeric vector with the calculated rates.
#' @export
#'
#' @examples
#' calcular_taxa(c(10, 25), c(10000, 20000))
#' calcular_taxa(25, 20000, multiplicador = 1000)
calcular_taxa <- function(eventos, populacao, multiplicador = 100000,
                          zero_denominador = c("na", "erro")) {
  eventos <- .analytics_numeric(eventos, "eventos")
  populacao <- .analytics_numeric(populacao, "populacao")
  values <- .analytics_recycle(
    eventos, populacao, "eventos", "populacao"
  )
  eventos <- values[[1L]]
  populacao <- values[[2L]]
  multiplicador <- .analytics_validate_multiplier(multiplicador)
  invalid <- .analytics_zero_denominator(
    eventos, populacao, zero_denominador
  )

  result <- eventos / populacao * multiplicador
  result[invalid] <- NA_real_
  names(result) <- names(eventos)
  attr(result, "multiplicador") <- multiplicador
  result
}

.analytics_validate_confidence <- function(confianca) {
  if (!is.numeric(confianca) || length(confianca) != 1L ||
      is.na(confianca) || confianca <= 0 || confianca >= 1) {
    stop("The 'confianca' argument must be a number between 0 and 1",
         call. = FALSE)
  }
  confianca
}

#' Calculate exact Poisson confidence intervals for rates
#'
#' Uses chi-squared quantiles for exact Poisson count limits and divides them
#' by the supplied denominator. Event counts must be whole numbers.
#'
#' @inheritParams calcular_taxa
#' @param confianca Confidence level between 0 and 1.
#'
#' @return A data frame with event counts, denominators, rates and lower and
#'   upper confidence limits.
#' @export
#'
#' @examples
#' intervalo_taxa(c(0, 10), c(10000, 10000))
intervalo_taxa <- function(eventos, populacao, multiplicador = 100000,
                           confianca = 0.95,
                           zero_denominador = c("na", "erro")) {
  eventos <- .analytics_numeric(eventos, "eventos")
  populacao <- .analytics_numeric(populacao, "populacao")
  values <- .analytics_recycle(
    eventos, populacao, "eventos", "populacao"
  )
  eventos <- values[[1L]]
  populacao <- values[[2L]]
  if (any(eventos != floor(eventos), na.rm = TRUE)) {
    stop("Exact Poisson intervals require whole event counts",
         call. = FALSE)
  }
  multiplicador <- .analytics_validate_multiplier(multiplicador)
  confianca <- .analytics_validate_confidence(confianca)
  invalid <- .analytics_zero_denominator(
    eventos, populacao, zero_denominador
  )

  alpha <- 1 - confianca
  lower_count <- rep(NA_real_, length(eventos))
  upper_count <- rep(NA_real_, length(eventos))
  observed <- !is.na(eventos)
  lower_count[observed & eventos == 0] <- 0
  positive <- observed & eventos > 0
  lower_count[positive] <- 0.5 * stats::qchisq(
    alpha / 2,
    df = 2 * eventos[positive]
  )
  upper_count[observed] <- 0.5 * stats::qchisq(
    1 - alpha / 2,
    df = 2 * (eventos[observed] + 1)
  )

  result <- data.frame(
    eventos = eventos,
    populacao = populacao,
    taxa = eventos / populacao * multiplicador,
    limite_inferior = lower_count / populacao * multiplicador,
    limite_superior = upper_count / populacao * multiplicador
  )
  result[invalid, c("taxa", "limite_inferior", "limite_superior")] <- NA_real_
  attr(result, "metodo") <- "Poisson exact"
  attr(result, "confianca") <- confianca
  attr(result, "multiplicador") <- multiplicador
  result
}

.epidemiological_year_start <- function(year) {
  january_fourth <- as.Date(paste0(year, "-01-04"))
  weekday <- as.POSIXlt(january_fourth, tz = "UTC")$wday
  january_fourth - weekday
}

.analytics_as_date <- function(data) {
  if (inherits(data, "Date")) {
    return(data)
  }
  if (inherits(data, "POSIXt")) {
    return(as.Date(data))
  }
  if (!is.character(data)) {
    stop("The 'data' argument must contain dates or ISO date strings",
         call. = FALSE)
  }
  result <- as.Date(data, format = "%Y-%m-%d")
  invalid <- is.na(result) & !is.na(data)
  if (any(invalid)) {
    stop(
      "The 'data' argument contains invalid ISO dates: ",
      paste(utils::head(unique(data[invalid]), 3L), collapse = ", "),
      call. = FALSE
    )
  }
  result
}

#' Convert dates to Brazilian epidemiological weeks
#'
#' Epidemiological weeks run from Sunday through Saturday. Week 1 is the
#' Sunday-to-Saturday week containing at least four days of the new year.
#' Dates near New Year can therefore belong to the previous or following
#' epidemiological year.
#'
#' @param data A `Date`, date-time, or vector of ISO `"YYYY-MM-DD"` strings.
#'
#' @return A data frame containing the original date, epidemiological year and
#'   week, week start and end, and a stable `"YYYY-Www"` code.
#' @references Brazilian Ministry of Health, SINAN Epidemiological Calendar:
#'   \url{https://portalsinan.saude.gov.br/calendario-epidemiologico}
#' @export
#'
#' @examples
#' semana_epidemiologica(as.Date(c("2025-01-01", "2026-01-01")))
semana_epidemiologica <- function(data) {
  data <- .analytics_as_date(data)
  result <- data.frame(
    data = data,
    ano_epidemiologico = rep(NA_integer_, length(data)),
    semana_epidemiologica = rep(NA_integer_, length(data)),
    inicio = as.Date(rep(NA_real_, length(data)), origin = "1970-01-01"),
    fim = as.Date(rep(NA_real_, length(data)), origin = "1970-01-01"),
    codigo = rep(NA_character_, length(data)),
    stringsAsFactors = FALSE
  )
  valid <- !is.na(data)
  if (!any(valid)) {
    return(result)
  }

  calendar_year <- as.integer(format(data[valid], "%Y"))
  year <- calendar_year
  start <- .epidemiological_year_start(year)
  before <- data[valid] < start
  year[before] <- year[before] - 1L
  next_start <- .epidemiological_year_start(calendar_year + 1L)
  after <- data[valid] >= next_start
  year[after] <- year[after] + 1L

  start <- .epidemiological_year_start(year)
  week <- as.integer(as.integer(data[valid] - start) %/% 7L) + 1L
  week_start <- start + (week - 1L) * 7L
  positions <- which(valid)
  result$ano_epidemiologico[positions] <- year
  result$semana_epidemiologica[positions] <- week
  result$inicio[positions] <- week_start
  result$fim[positions] <- week_start + 6L
  result$codigo[positions] <- sprintf("%04d-W%02d", year, week)
  result
}

#' Generate a Brazilian epidemiological calendar
#'
#' @param ano One epidemiological year.
#'
#' @return A data frame with the 52 or 53 epidemiological weeks in the year.
#' @export
#'
#' @examples
#' calendario_epidemiologico(2026)
calendario_epidemiologico <- function(ano) {
  if (!is.numeric(ano) || length(ano) != 1L || is.na(ano) ||
      ano != as.integer(ano) || ano < 1900L || ano > 9999L) {
    stop("The 'ano' argument must be one four-digit year",
         call. = FALSE)
  }
  ano <- as.integer(ano)
  start <- .epidemiological_year_start(ano)
  next_start <- .epidemiological_year_start(ano + 1L)
  count <- as.integer(as.integer(next_start - start) / 7L)
  week <- seq_len(count)
  beginning <- start + (week - 1L) * 7L
  data.frame(
    ano_epidemiologico = ano,
    semana_epidemiologica = week,
    inicio = beginning,
    fim = beginning + 6L,
    codigo = sprintf("%04d-W%02d", ano, week),
    stringsAsFactors = FALSE
  )
}

#' Calculate moving averages
#'
#' @param x Numeric vector.
#' @param janela Positive window size.
#' @param alinhamento Window alignment: `"direita"`, `"centro"` or
#'   `"esquerda"`.
#' @param parcial Whether edge windows may contain fewer than `janela`
#'   observations.
#' @param na_rm Whether missing values should be removed within each window.
#'
#' @return A numeric vector of the same length as `x`.
#' @export
#'
#' @examples
#' media_movel(1:7, janela = 3)
#' media_movel(c(1, NA, 3, 4), janela = 2, na_rm = TRUE)
media_movel <- function(x, janela = 7L,
                        alinhamento = c("direita", "centro", "esquerda"),
                        parcial = FALSE, na_rm = FALSE) {
  x <- .analytics_numeric(x, "x", nonnegative = FALSE)
  if (!is.numeric(janela) || length(janela) != 1L ||
      is.na(janela) || janela != as.integer(janela) || janela < 1L) {
    stop("The 'janela' argument must be a positive integer",
         call. = FALSE)
  }
  janela <- as.integer(janela)
  alinhamento <- match.arg(alinhamento)
  for (value in list(parcial, na_rm)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("'parcial' and 'na_rm' must be TRUE or FALSE",
           call. = FALSE)
    }
  }

  size <- length(x)
  result <- rep(NA_real_, size)
  names(result) <- names(x)
  left_center <- floor((janela - 1L) / 2L)
  right_center <- janela - 1L - left_center

  for (index in seq_len(size)) {
    bounds <- switch(
      alinhamento,
      direita = c(index - janela + 1L, index),
      esquerda = c(index, index + janela - 1L),
      centro = c(index - left_center, index + right_center)
    )
    complete <- bounds[[1L]] >= 1L && bounds[[2L]] <= size
    if (!complete && !parcial) {
      next
    }
    bounds <- pmax(1L, pmin(size, bounds))
    values <- x[seq.int(bounds[[1L]], bounds[[2L]])]
    if (na_rm) {
      values <- values[!is.na(values)]
    }
    if (!length(values) || (!na_rm && anyNA(values))) {
      next
    }
    result[[index]] <- mean(values)
  }
  attr(result, "janela") <- janela
  attr(result, "alinhamento") <- alinhamento
  result
}

.age_standard_population <- function(idade, populacao_padrao) {
  if (!is.numeric(populacao_padrao) || !length(populacao_padrao) ||
      any(populacao_padrao < 0, na.rm = TRUE)) {
    stop(
      "The 'populacao_padrao' argument must contain non-negative weights",
      call. = FALSE
    )
  }
  standard_names <- names(populacao_padrao)
  has_names <- !is.null(standard_names) &&
    any(!is.na(standard_names) & nzchar(standard_names))
  if (has_names &&
      !all(!is.na(standard_names) & nzchar(standard_names))) {
    stop(
      "The names of 'populacao_padrao' must be either complete or absent",
      call. = FALSE
    )
  }
  if (has_names && anyDuplicated(standard_names)) {
    stop("The names of 'populacao_padrao' must be unique",
         call. = FALSE)
  }
  if (has_names) {
    matched <- match(as.character(idade), names(populacao_padrao))
    if (anyNA(matched) && any(!is.na(idade))) {
      missing <- unique(as.character(idade[is.na(matched) & !is.na(idade)]))
      stop(
        "No standard population weight was supplied for age group(s): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    return(unname(populacao_padrao[matched]))
  }
  if (length(populacao_padrao) != length(idade)) {
    stop(
      "An unnamed 'populacao_padrao' must have the same length as 'idade'",
      call. = FALSE
    )
  }
  populacao_padrao
}

.age_standardize_group <- function(index, eventos, populacao, idade,
                                   standard, multiplicador,
                                   confianca, na_rm) {
  events <- eventos[index]
  population <- populacao[index]
  age <- idade[index]
  weights <- standard[index]
  complete <- !is.na(events) & !is.na(population) &
    !is.na(age) & !is.na(weights)

  if (!all(complete) && !na_rm) {
    return(data.frame(
      taxa_padronizada = NA_real_,
      erro_padrao = NA_real_,
      limite_inferior = NA_real_,
      limite_superior = NA_real_,
      estratos = sum(complete),
      cobertura_padrao = NA_real_
    ))
  }
  events <- events[complete]
  population <- population[complete]
  age <- age[complete]
  weights <- weights[complete]
  if (!length(events)) {
    stop("No complete age strata are available for standardization",
         call. = FALSE)
  }
  if (anyDuplicated(as.character(age))) {
    stop(
      "Age groups must be unique within each analysis group; ",
      "aggregate events and population first",
      call. = FALSE
    )
  }
  if (any(population <= 0 & weights > 0)) {
    stop(
      "Population denominators must be positive in weighted age strata",
      call. = FALSE
    )
  }
  original_weight <- sum(standard[index], na.rm = TRUE)
  observed_weight <- sum(weights)
  if (!is.finite(observed_weight) || observed_weight <= 0) {
    stop("The standard population weights must sum to a positive value",
         call. = FALSE)
  }
  normalized <- weights / observed_weight
  rates <- rep(0, length(events))
  contributing <- weights > 0
  rates[contributing] <- events[contributing] / population[contributing]
  standardized <- sum(normalized * rates) * multiplicador
  variance <- sum(
    normalized[contributing]^2 *
      events[contributing] / population[contributing]^2
  ) * multiplicador^2
  standard_error <- sqrt(variance)

  lower <- upper <- NA_real_
  if (!is.null(confianca)) {
    z <- stats::qnorm(1 - (1 - confianca) / 2)
    lower <- max(0, standardized - z * standard_error)
    upper <- standardized + z * standard_error
  }
  coverage <- if (original_weight > 0) {
    observed_weight / original_weight
  } else {
    NA_real_
  }
  data.frame(
    taxa_padronizada = standardized,
    erro_padrao = standard_error,
    limite_inferior = lower,
    limite_superior = upper,
    estratos = length(events),
    cobertura_padrao = coverage
  )
}

#' Directly standardize rates by age
#'
#' Calculates a weighted average of age-specific rates using a supplied
#' standard population. Independent Poisson counts are assumed for the
#' standard error. Optional confidence limits use a normal approximation.
#'
#' @param eventos Non-negative event counts by age stratum.
#' @param populacao Positive population or person-time by age stratum.
#' @param idade Age-stratum labels.
#' @param populacao_padrao Standard population weights. Supply either one
#'   value per input row or a named vector whose names match `idade`.
#' @param grupo Optional analysis group, such as year or municipality. Each age
#'   stratum must occur at most once within a group.
#' @param multiplicador Scale of the standardized rate, default 100,000.
#' @param confianca Optional confidence level. `NULL` omits confidence limits.
#' @param na_rm Whether incomplete age strata should be removed and the
#'   remaining standard weights renormalized.
#'
#' @return A data frame with standardized rate, standard error, confidence
#'   limits, number of strata and proportion of standard weight represented.
#' @references Ahmad OB et al. (2001). Age standardization of rates: a new
#'   WHO standard. World Health Organization.
#' @export
#'
#' @examples
#' padronizar_idade(
#'   eventos = c(10, 40),
#'   populacao = c(1000, 1000),
#'   idade = c("0-49", "50+"),
#'   populacao_padrao = c("0-49" = 800, "50+" = 200)
#' )
padronizar_idade <- function(eventos, populacao, idade,
                             populacao_padrao, grupo = NULL,
                             multiplicador = 100000,
                             confianca = NULL, na_rm = FALSE) {
  eventos <- .analytics_numeric(eventos, "eventos")
  populacao <- .analytics_numeric(populacao, "populacao")
  if (length(eventos) != length(populacao) ||
      length(eventos) != length(idade)) {
    stop(
      "'eventos', 'populacao' and 'idade' must have equal lengths",
      call. = FALSE
    )
  }
  if (!is.atomic(idade) || !length(idade)) {
    stop("The 'idade' argument must contain age-stratum labels",
         call. = FALSE)
  }
  multiplicador <- .analytics_validate_multiplier(multiplicador)
  if (!is.null(confianca)) {
    confianca <- .analytics_validate_confidence(confianca)
  }
  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("The 'na_rm' argument must be TRUE or FALSE", call. = FALSE)
  }
  standard <- .age_standard_population(idade, populacao_padrao)

  supplied_group <- !is.null(grupo)
  if (is.null(grupo)) {
    grupo <- rep("Total", length(eventos))
  }
  if (length(grupo) != length(eventos) || anyNA(grupo)) {
    stop(
      "The 'grupo' argument must have one non-missing value per input row",
      call. = FALSE
    )
  }
  group_labels <- unique(as.character(grupo))
  values <- lapply(group_labels, function(label) {
    index <- which(as.character(grupo) == label)
    .age_standardize_group(
      index, eventos, populacao, idade, standard,
      multiplicador, confianca, na_rm
    )
  })
  result <- do.call(rbind, values)
  row.names(result) <- NULL
  if (supplied_group) {
    result <- data.frame(
      grupo = group_labels,
      result,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  if (is.null(confianca)) {
    result$limite_inferior <- NULL
    result$limite_superior <- NULL
  }
  attr(result, "metodo") <- "Direct age standardization"
  attr(result, "multiplicador") <- multiplicador
  attr(result, "confianca") <- confianca
  result
}
