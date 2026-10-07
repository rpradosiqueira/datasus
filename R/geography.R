# Brazilian territorial reference -----------------------------------------

.territory_normalized_text <- function(x) {
  x <- stringi::stri_trans_general(enc2utf8(as.character(x)), "Latin-ASCII")
  x <- tolower(x)
  trimws(gsub("[^a-z0-9]+", " ", x))
}

.territory_catalog <- function(nivel) {
  nivel <- match.arg(nivel, c("municipio", "uf", "regiao"))
  if (identical(nivel, "municipio")) {
    return(.datasus_municipios)
  }

  if (identical(nivel, "uf")) {
    columns <- c(
      "codigo_uf", "uf", "unidade_federacao",
      "codigo_regiao", "sigla_regiao", "regiao"
    )
    result <- unique(.datasus_municipios[columns])
    counts <- table(.datasus_municipios$codigo_uf)
    result$municipios <- as.integer(counts[result$codigo_uf])
    result <- result[order(result$codigo_uf), , drop = FALSE]
  } else {
    columns <- c("codigo_regiao", "sigla_regiao", "regiao")
    result <- unique(.datasus_municipios[columns])
    result$ufs <- vapply(
      result$codigo_regiao,
      function(code) {
        length(unique(
          .datasus_municipios$codigo_uf[
            .datasus_municipios$codigo_regiao == code
          ]
        ))
      },
      integer(1)
    )
    result$municipios <- as.integer(table(
      .datasus_municipios$codigo_regiao
    )[result$codigo_regiao])
    result <- result[order(result$codigo_regiao), , drop = FALSE]
  }

  rownames(result) <- NULL
  result
}

.territory_lookup <- function(codigo, nivel) {
  values <- trimws(as.character(codigo))
  values[is.na(codigo)] <- NA_character_
  catalog <- .territory_catalog(nivel)
  result <- rep(NA_integer_, length(values))

  if (identical(nivel, "municipio")) {
    six_digits <- !is.na(values) & grepl("^[0-9]{6}$", values)
    seven_digits <- !is.na(values) & grepl("^[0-9]{7}$", values)
    result[six_digits] <- match(
      values[six_digits],
      substr(catalog$codigo_municipio, 1L, 6L)
    )
    result[seven_digits] <- match(
      values[seven_digits],
      catalog$codigo_municipio
    )
    return(result)
  }

  if (identical(nivel, "uf")) {
    normalized <- .territory_normalized_text(values)
    result <- match(normalized, .territory_normalized_text(catalog$codigo_uf))
    missing <- is.na(result)
    result[missing] <- match(
      normalized[missing],
      .territory_normalized_text(catalog$uf)
    )
    missing <- is.na(result)
    result[missing] <- match(
      normalized[missing],
      .territory_normalized_text(catalog$unidade_federacao)
    )
    return(result)
  }

  normalized <- .territory_normalized_text(values)
  result <- match(normalized, .territory_normalized_text(catalog$codigo_regiao))
  missing <- is.na(result)
  result[missing] <- match(
    normalized[missing],
    .territory_normalized_text(catalog$sigla_regiao)
  )
  missing <- is.na(result)
  result[missing] <- match(
    normalized[missing],
    .territory_normalized_text(catalog$regiao)
  )
  result
}

.territory_unknown <- function(input, positions, desconhecido) {
  if (!length(positions) || !identical(desconhecido, "erro")) {
    return(invisible(NULL))
  }
  examples <- unique(as.character(input[positions]))
  examples <- utils::head(examples, 5L)
  stop(
    "Unknown or invalid territorial value(s): ",
    paste(examples, collapse = ", "),
    call. = FALSE
  )
}

#' List Brazilian territories
#'
#' Returns the package's offline territorial reference. The municipality
#' table is generated from the official IBGE Localities API and contains
#' current municipality, state, macroregion, immediate/intermediate region
#' and legacy micro/mesoregion codes and names.
#'
#' @param nivel Territorial level: `"municipio"`, `"uf"` or `"regiao"`.
#' @param uf Optional state selector. It accepts a two-digit IBGE code, state
#'   abbreviation or full state name. It is available for municipality and
#'   state results.
#'
#' @return A data frame. Source and extraction metadata are available through
#'   [datasus_proveniencia()].
#' @export
#'
#' @examples
#' datasus_territorios("regiao")
#' datasus_territorios("uf")
#' head(datasus_territorios("municipio", uf = "MS"))
datasus_territorios <- function(nivel = c("municipio", "uf", "regiao"),
                                uf = NULL) {
  nivel <- match.arg(nivel)
  result <- .territory_catalog(nivel)

  if (!is.null(uf)) {
    if (identical(nivel, "regiao")) {
      stop("'uf' is not available when 'nivel = \"regiao\"'",
           call. = FALSE)
    }
    selected <- normalizar_codigo_ibge(
      uf,
      nivel = "uf",
      formato = "ibge"
    )
    result <- result[result$codigo_uf %in% selected, , drop = FALSE]
  }

  rownames(result) <- NULL
  provenance <- .datasus_territorios_meta
  provenance$nivel <- nivel
  provenance$filtro_uf <- if (is.null(uf)) NULL else selected
  attr(result, "datasus_proveniencia") <- provenance
  result
}

#' Normalize IBGE territorial codes
#'
#' Validates current territorial identifiers against the bundled IBGE
#' reference. Municipality inputs may use the full seven-digit IBGE code or
#' the six-digit form commonly returned by DATASUS. State and region inputs
#' may also use abbreviations or full names.
#'
#' @param codigo Vector of territorial identifiers.
#' @param nivel Territorial level: `"municipio"`, `"uf"` or `"regiao"`.
#' @param formato Output format. Municipalities support `"ibge"` (seven
#'   digits) and `"datasus"` (six digits). States and regions additionally
#'   support `"sigla"` and `"nome"`.
#' @param desconhecido How to handle unknown values: `"erro"`, `"na"` or
#'   `"manter"`.
#'
#' @return A character vector with the same length as `codigo`.
#' @export
#'
#' @examples
#' normalizar_codigo_ibge(c("500270", "5003702"))
#' normalizar_codigo_ibge(c("MS", "Mato Grosso do Sul"), "uf", "ibge")
normalizar_codigo_ibge <- function(
    codigo,
    nivel = c("municipio", "uf", "regiao"),
    formato = NULL,
    desconhecido = c("erro", "na", "manter")) {
  nivel <- match.arg(nivel)
  desconhecido <- match.arg(desconhecido)
  if (is.null(formato)) {
    formato <- "ibge"
  }

  allowed <- switch(
    nivel,
    municipio = c("ibge", "datasus"),
    uf = c("ibge", "datasus", "sigla", "nome"),
    regiao = c("ibge", "datasus", "sigla", "nome")
  )
  formato <- match.arg(formato, allowed)

  if (!is.atomic(codigo) || is.list(codigo)) {
    stop("'codigo' must be an atomic vector", call. = FALSE)
  }
  input <- codigo
  index <- .territory_lookup(input, nivel)
  invalid <- which(is.na(index) & !is.na(input))
  .territory_unknown(input, invalid, desconhecido)

  catalog <- .territory_catalog(nivel)
  field <- switch(
    nivel,
    municipio = if (identical(formato, "datasus")) {
      "codigo_municipio"
    } else {
      "codigo_municipio"
    },
    uf = switch(
      formato,
      ibge = "codigo_uf",
      datasus = "codigo_uf",
      sigla = "uf",
      nome = "unidade_federacao"
    ),
    regiao = switch(
      formato,
      ibge = "codigo_regiao",
      datasus = "codigo_regiao",
      sigla = "sigla_regiao",
      nome = "regiao"
    )
  )
  result <- rep(NA_character_, length(input))
  known <- !is.na(index)
  result[known] <- catalog[[field]][index[known]]
  if (identical(nivel, "municipio") &&
      identical(formato, "datasus")) {
    result[known] <- substr(result[known], 1L, 6L)
  }
  if (identical(desconhecido, "manter")) {
    result[invalid] <- as.character(input[invalid])
  }
  result
}

#' Validate IBGE territorial codes
#'
#' @inheritParams normalizar_codigo_ibge
#'
#' @return A logical vector. Missing and unknown values return `FALSE`.
#' @export
#'
#' @examples
#' validar_codigo_ibge(c("500270", "999999"))
#' validar_codigo_ibge(c("MS", "XX"), "uf")
validar_codigo_ibge <- function(
    codigo,
    nivel = c("municipio", "uf", "regiao")) {
  nivel <- match.arg(nivel)
  !is.na(normalizar_codigo_ibge(
    codigo,
    nivel = nivel,
    desconhecido = "na"
  ))
}

#' Extract IBGE codes from DATASUS labels
#'
#' Extracts the first territorial code embedded in labels such as
#' `"500270 Campo Grande"` and then validates and converts it with
#' [normalizar_codigo_ibge()].
#'
#' @param x Character vector containing codes or labeled values.
#' @inheritParams normalizar_codigo_ibge
#'
#' @return A normalized character vector.
#' @export
#'
#' @examples
#' extrair_codigo_ibge(c("500270 Campo Grande", "500370 Dourados"))
extrair_codigo_ibge <- function(
    x,
    nivel = c("municipio", "uf", "regiao"),
    formato = NULL,
    desconhecido = c("erro", "na", "manter")) {
  nivel <- match.arg(nivel)
  desconhecido <- match.arg(desconhecido)
  if (!is.atomic(x) || is.list(x)) {
    stop("'x' must be an atomic vector", call. = FALSE)
  }

  digits <- switch(nivel, municipio = "{6,7}", uf = "{2}", regiao = "{1}")
  pattern <- paste0("(?<![0-9])([0-9]", digits, ")(?![0-9])")
  text <- as.character(x)
  match <- regexpr(pattern, text, perl = TRUE)
  extracted <- rep(NA_character_, length(text))
  found <- !is.na(match) & match > 0L
  extracted[found] <- regmatches(text, match)

  if (identical(desconhecido, "manter")) {
    result <- normalizar_codigo_ibge(
      extracted,
      nivel = nivel,
      formato = formato,
      desconhecido = "na"
    )
    result[is.na(result) & !is.na(x)] <- as.character(x[
      is.na(result) & !is.na(x)
    ])
    return(result)
  }

  if (identical(desconhecido, "erro")) {
    invalid <- which(is.na(extracted) & !is.na(x))
    .territory_unknown(x, invalid, "erro")
  }
  normalizar_codigo_ibge(
    extracted,
    nivel = nivel,
    formato = formato,
    desconhecido = desconhecido
  )
}

#' Add territorial attributes to a data frame
#'
#' Validates a territorial code column and appends the corresponding current
#' IBGE names and hierarchy. Six-digit DATASUS municipality codes are
#' converted to seven-digit IBGE codes. The function does not redistribute
#' historical observations across boundary changes.
#'
#' @param dados A data frame.
#' @param codigo Name of the territorial code column.
#' @inheritParams normalizar_codigo_ibge
#' @param sobrescrever Whether existing columns with catalog names may be
#'   replaced.
#'
#' @return `dados` with territorial columns appended.
#' @export
#'
#' @examples
#' dados <- data.frame(codmun = c("500270", "500370"), casos = c(10, 5))
#' adicionar_territorio(dados, "codmun")
adicionar_territorio <- function(
    dados,
    codigo,
    nivel = c("municipio", "uf", "regiao"),
    desconhecido = c("erro", "na"),
    sobrescrever = FALSE) {
  nivel <- match.arg(nivel)
  desconhecido <- match.arg(desconhecido)
  if (!is.data.frame(dados)) {
    stop("'dados' must be a data frame", call. = FALSE)
  }
  if (!is.character(codigo) || length(codigo) != 1L ||
      is.na(codigo) || !codigo %in% names(dados)) {
    stop("'codigo' must name one column in 'dados'", call. = FALSE)
  }
  if (!is.logical(sobrescrever) || length(sobrescrever) != 1L ||
      is.na(sobrescrever)) {
    stop("'sobrescrever' must be TRUE or FALSE", call. = FALSE)
  }

  normalized <- normalizar_codigo_ibge(
    dados[[codigo]],
    nivel = nivel,
    formato = "ibge",
    desconhecido = desconhecido
  )
  catalog <- .territory_catalog(nivel)
  key <- switch(
    nivel,
    municipio = "codigo_municipio",
    uf = "codigo_uf",
    regiao = "codigo_regiao"
  )
  selected <- catalog[match(normalized, catalog[[key]]), , drop = FALSE]
  selected[[key]] <- normalized
  rownames(selected) <- NULL

  result <- dados
  for (column in names(selected)) {
    if (column %in% names(result) && !sobrescrever) {
      next
    }
    result[[column]] <- selected[[column]]
  }

  provenance <- attr(dados, "datasus_proveniencia", exact = TRUE)
  if (is.null(provenance)) {
    provenance <- list()
  }
  provenance$territorios <- .datasus_territorios_meta
  attr(result, "datasus_proveniencia") <- provenance
  result
}

.territory_validate_columns <- function(dados, columns, argument) {
  if (is.null(columns)) {
    return(character())
  }
  if (!is.character(columns) || !length(columns) || anyNA(columns) ||
      any(!nzchar(columns)) || anyDuplicated(columns)) {
    stop("'", argument, "' must contain unique column names",
         call. = FALSE)
  }
  unknown <- setdiff(columns, names(dados))
  if (length(unknown)) {
    stop(
      "Unknown column(s) in '", argument, "': ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  columns
}

#' Complete territorial and temporal combinations
#'
#' Adds absent territory-period rows while protecting existing observations.
#' By default the territorial universe is the set observed in `dados`.
#' Supply `uf` to complete all current territories in one or more states, or
#' `territorios` for an explicit universe. If `periodos` is omitted, the
#' distinct periods observed in the data are used.
#'
#' @param dados A data frame.
#' @param codigo Name of the territorial code column.
#' @param periodo Optional name of the period column.
#' @param grupo Optional names of grouping columns whose observed
#'   combinations must be completed separately.
#' @inheritParams normalizar_codigo_ibge
#' @param territorios Optional vector defining the territorial universe.
#' @param uf Optional state selector used to define the current territorial
#'   universe. For municipality data it includes every current municipality
#'   in the selected states.
#' @param periodos Optional vector defining the period universe.
#' @param preencher Named list of scalar values used only for rows created by
#'   the function, for example `list(casos = 0)`.
#' @param adicionar Whether to append current territorial attributes with
#'   [adicionar_territorio()].
#'
#' @return A data frame ordered by groups, territory and period.
#' @export
#'
#' @examples
#' dados <- data.frame(
#'   codmun = c("500270", "500270", "500370"),
#'   ano = c(2023, 2025, 2023),
#'   casos = c(10, 12, 5)
#' )
#' completar_territorios(
#'   dados, "codmun", "ano",
#'   periodos = 2023:2025,
#'   preencher = list(casos = 0)
#' )
completar_territorios <- function(
    dados,
    codigo,
    periodo = NULL,
    grupo = NULL,
    nivel = c("municipio", "uf", "regiao"),
    territorios = NULL,
    uf = NULL,
    periodos = NULL,
    preencher = list(),
    adicionar = TRUE) {
  nivel <- match.arg(nivel)
  if (!is.data.frame(dados)) {
    stop("'dados' must be a data frame", call. = FALSE)
  }
  codigo <- .territory_validate_columns(dados, codigo, "codigo")
  if (length(codigo) != 1L) {
    stop("'codigo' must name exactly one column", call. = FALSE)
  }
  periodo <- .territory_validate_columns(dados, periodo, "periodo")
  if (length(periodo) > 1L) {
    stop("'periodo' must name at most one column", call. = FALSE)
  }
  grupo <- .territory_validate_columns(dados, grupo, "grupo")
  if (codigo %in% c(periodo, grupo) ||
      length(intersect(periodo, grupo))) {
    stop("Key columns must be distinct", call. = FALSE)
  }
  if (!is.list(preencher) ||
      (length(preencher) &&
       (is.null(names(preencher)) || any(!nzchar(names(preencher))))) ||
      anyDuplicated(names(preencher))) {
    stop("'preencher' must be a named list", call. = FALSE)
  }
  if (length(preencher) &&
      (any(lengths(preencher) != 1L) ||
       length(setdiff(names(preencher), names(dados))))) {
    stop(
      "Every value in 'preencher' must be scalar and name a data column",
      call. = FALSE
    )
  }
  if (!is.logical(adicionar) || length(adicionar) != 1L ||
      is.na(adicionar)) {
    stop("'adicionar' must be TRUE or FALSE", call. = FALSE)
  }

  normalized <- normalizar_codigo_ibge(dados[[codigo]], nivel = nivel)
  data <- dados
  data[[codigo]] <- normalized

  if (!is.null(territorios) && !is.null(uf)) {
    stop("Use only one of 'territorios' and 'uf'", call. = FALSE)
  }
  if (!is.null(territorios)) {
    universe <- unique(normalizar_codigo_ibge(territorios, nivel = nivel))
  } else if (!is.null(uf)) {
    if (identical(nivel, "regiao")) {
      stop("'uf' cannot define a region-level universe", call. = FALSE)
    }
    if (identical(nivel, "municipio")) {
      universe <- datasus_territorios("municipio", uf)$codigo_municipio
    } else {
      universe <- normalizar_codigo_ibge(uf, "uf", "ibge")
    }
  } else {
    universe <- unique(normalized)
  }
  universe <- sort(unique(universe[!is.na(universe)]))
  if (!length(universe)) {
    stop("The territorial universe is empty", call. = FALSE)
  }

  if (length(periodo)) {
    if (anyNA(data[[periodo]])) {
      stop("The period column must not contain missing values",
           call. = FALSE)
    }
    if (is.null(periodos)) {
      period_values <- sort(unique(data[[periodo]]))
    } else {
      period_values <- unique(periodos)
    }
    if (!length(period_values) || anyNA(period_values)) {
      stop("'periodos' must not be empty or missing", call. = FALSE)
    }
  } else {
    if (!is.null(periodos)) {
      stop("'periodos' requires a 'periodo' column", call. = FALSE)
    }
    period_values <- NULL
  }

  base <- data.frame(
    .territory = universe,
    stringsAsFactors = FALSE
  )
  if (length(periodo)) {
    base <- base[rep(seq_len(nrow(base)), each = length(period_values)),
                 , drop = FALSE]
    base[[periodo]] <- rep(period_values, times = length(universe))
  }
  names(base)[names(base) == ".territory"] <- codigo

  if (length(grupo)) {
    groups <- unique(data[grupo])
    grid <- groups[
      rep(seq_len(nrow(groups)), each = nrow(base)),
      ,
      drop = FALSE
    ]
    base_repeated <- base[rep(seq_len(nrow(base)), times = nrow(groups)),
                          , drop = FALSE]
    grid <- cbind(grid, base_repeated)
  } else {
    grid <- base
  }
  key <- c(grupo, codigo, periodo)

  key_string <- function(x) {
    do.call(paste, c(lapply(x[key], as.character), sep = "\r"))
  }
  data_key <- key_string(data)
  if (anyDuplicated(data_key)) {
    stop(
      "The data contain duplicate rows for the selected key: ",
      paste(key, collapse = ", "),
      call. = FALSE
    )
  }
  index <- match(key_string(grid), data_key)
  added <- is.na(index)
  result <- data[index, , drop = FALSE]
  for (column in key) {
    result[[column]] <- grid[[column]]
  }
  for (column in names(preencher)) {
    value <- preencher[[column]]
    if (is.factor(result[[column]]) &&
        !as.character(value) %in% levels(result[[column]])) {
      result[[column]] <- as.character(result[[column]])
    }
    result[[column]][added] <- value
  }
  rownames(result) <- NULL

  if (adicionar) {
    result <- adicionar_territorio(
      result,
      codigo = codigo,
      nivel = nivel,
      desconhecido = "erro",
      sobrescrever = FALSE
    )
  }
  result
}
