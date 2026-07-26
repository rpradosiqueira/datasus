# Internal TABNET client ----------------------------------------------------

.tabnet_default_timeout <- 30
.tabnet_default_tries <- 2

.tabnet_user_agent <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("datasus")),
    error = function(...) "development"
  )

  paste0(
    "datasus/", version,
    " (https://github.com/rpradosiqueira/datasus)"
  )
}

.tabnet_request_raw <- function(url, body = NULL) {
  timeout <- getOption("datasus.timeout", .tabnet_default_timeout)
  tries <- getOption("datasus.max_tries", .tabnet_default_tries)

  if (!is.numeric(timeout) || length(timeout) != 1L ||
      is.na(timeout) || timeout <= 0) {
    stop("Option 'datasus.timeout' must be a positive number", call. = FALSE)
  }

  if (!is.numeric(tries) || length(tries) != 1L ||
      is.na(tries) || tries < 1) {
    stop("Option 'datasus.max_tries' must be a positive number", call. = FALSE)
  }

  request <- httr2::request(url)
  request <- httr2::req_user_agent(request, .tabnet_user_agent())
  request <- httr2::req_timeout(request, seconds = timeout)
  request <- httr2::req_retry(
    request,
    max_tries = as.integer(tries),
    retry_on_failure = TRUE
  )

  if (!is.null(body)) {
    request <- httr2::req_body_raw(
      request,
      charToRaw(body),
      type = "application/x-www-form-urlencoded"
    )
  }

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(error) {
      stop(
        "TABNET request failed: ", conditionMessage(error),
        call. = FALSE
      )
    }
  )

  httr2::resp_check_status(
    response,
    info = "TABNET returned an unsuccessful HTTP status"
  )
  httr2::resp_body_raw(response)
}

.tabnet_parse_html <- function(content) {
  if (!is.raw(content)) {
    stop("TABNET content must be supplied as a raw vector", call. = FALSE)
  }

  # TABNET pages are served using a Windows-1252/Latin-1-compatible encoding.
  # Parsing the raw bytes explicitly avoids invalid UTF-8 strings such as the
  # ordinal indicator used in "Apgar 1º minuto".
  xml2::read_html(content, encoding = "ISO-8859-1")
}

.tabnet_read_html <- function(url) {
  .tabnet_parse_html(.tabnet_request_raw(url))
}

.tabnet_post <- function(url, body) {
  .tabnet_parse_html(.tabnet_request_raw(url, body = body))
}

.tabnet_url <- function(system, dataset, level, uf = NULL,
                        action = c("form", "query")) {
  action <- match.arg(action)
  level <- match.arg(level, c("state", "municipality", "national"))

  if (!is.character(system) || length(system) != 1L ||
      !grepl("^[a-z0-9]+$", system)) {
    stop("TABNET system names must contain only lowercase letters and numbers",
         call. = FALSE)
  }
  if (!is.character(dataset) || length(dataset) != 1L ||
      !grepl("^[A-Za-z0-9_]+$", dataset)) {
    stop("TABNET dataset names must contain only letters, numbers and underscores",
         call. = FALSE)
  }

  suffix <- switch(
    level,
    state = .tabnet_validate_uf(uf),
    municipality = "br",
    national = "uf"
  )
  program <- switch(
    action,
    form = "deftohtm.exe",
    query = "tabcgi.exe"
  )

  paste0(
    "https://tabnet.datasus.gov.br/cgi/", program, "?",
    system, "/cnv/", dataset, suffix, ".def"
  )
}

.tabnet_options <- function(page, selector, numeric = FALSE) {
  options <- rvest::html_elements(page, selector)
  ids <- trimws(rvest::html_text2(options))
  values <- rvest::html_attr(options, "value")

  if (numeric) {
    ids <- as.numeric(suppressWarnings(readr::parse_number(ids)))
  } else {
    ids <- as.character(ids)
  }

  structure(
    list(id = ids, value = as.character(values)),
    class = "data.frame",
    row.names = seq_along(ids)
  )
}

.tabnet_filter <- function(options, selected) {
  matches <- options$id %in% selected | options$value %in% selected
  list(value = options$value[matches])
}

.tabnet_indexed_options <- function(page, selector) {
  values <- rvest::html_attr(
    rvest::html_elements(page, selector),
    "value"
  )

  structure(
    list(id = seq_along(values) - 1L, value = as.character(values)),
    class = "data.frame",
    row.names = seq_along(values)
  )
}

.tabnet_parse_table <- function(page) {
  cells <- page |>
    rvest::html_elements(".tabdados tbody td") |>
    rvest::html_text2() |>
    trimws()

  headers <- page |>
    rvest::html_elements("th") |>
    rvest::html_text2() |>
    trimws()

  if (!length(headers) || !length(cells)) {
    message <- page |>
      rvest::html_element("body") |>
      rvest::html_text2() |>
      trimws()
    message <- substr(message, 1L, 300L)

    stop(
      "TABNET returned no tabular data",
      if (nzchar(message)) paste0(": ", message) else "",
      call. = FALSE
    )
  }

  if (length(cells) %% length(headers) != 0L) {
    stop(
      "TABNET returned an unexpected table shape (",
      length(cells), " cells for ", length(headers), " columns)",
      call. = FALSE
    )
  }

  values <- matrix(
    data = cells,
    nrow = length(cells) / length(headers),
    ncol = length(headers),
    byrow = TRUE
  )
  result <- structure(
    lapply(seq_len(ncol(values)), function(column) values[, column]),
    names = headers,
    class = "data.frame",
    row.names = seq_len(nrow(values))
  )

  as_number <- function(value) {
    suppressWarnings(
      readr::parse_number(
        value,
        na = c("", "-", "..."),
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = "."
        )
      )
    )
  }
  result[-1L] <- lapply(result[-1L], as_number)
  result
}

.tabnet_parse_webtabx <- function(page) {
  scripts <- rvest::html_elements(page, "script") |>
    rvest::html_text2()
  scripts <- scripts[grepl("data\\.addRows\\(\\[", scripts)]

  if (!length(scripts)) {
    stop("TABNET webtabx returned no tabular data", call. = FALSE)
  }
  script <- paste(scripts, collapse = "\n")

  column_pattern <- paste0(
    "data\\.addColumn\\(",
    "'[^']*','[^']*'",
    "\\);"
  )
  column_definitions <- regmatches(
    script,
    gregexpr(column_pattern, script, perl = TRUE)
  )[[1L]]
  if (!length(column_definitions)) {
    stop("TABNET webtabx returned no column definitions", call. = FALSE)
  }

  types <- sub(
    "^data\\.addColumn\\('([^']*)'.*$",
    "\\1",
    column_definitions
  )
  headers <- sub(
    "^data\\.addColumn\\('[^']*','([^']*)'\\);$",
    "\\1",
    column_definitions
  )

  rows_block <- sub(
    "^[\\s\\S]*data\\.addRows\\(\\[",
    "",
    script,
    perl = TRUE
  )
  rows_block <- sub(
    "\\]\\);[\\s\\S]*$",
    "",
    rows_block,
    perl = TRUE
  )
  rows <- strsplit(rows_block, "\\r\\n|\\r|\\n", perl = TRUE)[[1L]]
  rows <- trimws(rows)
  rows <- sub("^,\\s*", "", rows, perl = TRUE)
  rows <- rows[grepl("^\\[", rows)]

  token_pattern <- paste0(
    "\"(?:\\\\.|[^\"\\\\])*\"",
    "|\\{v:\\s*[^,}]+(?:,[^}]*)?\\}",
    "|null"
  )
  parse_token <- function(token, type) {
    if (identical(type, "string")) {
      if (identical(token, "null")) {
        return(NA_character_)
      }
      value <- substr(token, 2L, nchar(token) - 1L)
      value <- gsub("\\\"", "\"", value, fixed = TRUE)
      value <- gsub("\\'", "'", value, fixed = TRUE)
      return(gsub("\\\\", "\\", value, fixed = TRUE))
    }

    if (identical(token, "null")) {
      return(NA_real_)
    }
    value <- sub("^\\{v:\\s*([^,}]+).*$", "\\1", token, perl = TRUE)
    suppressWarnings(as.numeric(trimws(value)))
  }

  parsed <- lapply(rows, function(row) {
    tokens <- regmatches(
      row,
      gregexpr(token_pattern, row, perl = TRUE)
    )[[1L]]
    if (length(tokens) != length(headers)) {
      stop(
        "TABNET webtabx returned an unexpected row shape (",
        length(tokens), " cells for ", length(headers), " columns): ",
        substr(row, 1L, 200L),
        call. = FALSE
      )
    }
    Map(parse_token, tokens, types)
  })

  result <- lapply(seq_along(headers), function(column) {
    values <- lapply(parsed, `[[`, column)
    if (identical(types[column], "number")) {
      return(as.numeric(unlist(values)))
    }
    as.character(unlist(values))
  })
  result <- structure(
    result,
    names = headers,
    class = "data.frame",
    row.names = seq_along(parsed)
  )

  total <- tolower(trimws(result[[1L]])) == "total"
  result[[1L]][total] <- "TOTAL"
  result
}

.tabnet_validate_period <- function(period, available) {
  if (identical(period, "last")) {
    return(utils::head(available, 1L))
  }

  if (!length(period) || anyNA(period)) {
    stop("The 'periodo' argument must contain at least one valid year",
         call. = FALSE)
  }

  if (is.character(period)) {
    converted <- suppressWarnings(as.numeric(period))
    if (anyNA(converted)) {
      stop("The 'periodo' argument must contain years or 'last'",
           call. = FALSE)
    }
    period <- converted
  }

  if (!is.numeric(period) || !all(period %in% available)) {
    stop("The 'periodo' argument is misspecified", call. = FALSE)
  }

  period
}

.tabnet_validate_uf <- function(uf) {
  if (!is.atomic(uf) || length(uf) != 1L || is.na(uf)) {
    stop("The 'uf' argument must identify a single state",
         call. = FALSE)
  }
  result <- tryCatch(
    normalizar_codigo_ibge(
      uf,
      nivel = "uf",
      formato = "sigla",
      desconhecido = "erro"
    ),
    error = function(...) NA_character_
  )
  if (is.na(result)) {
    stop(
      "The 'uf' argument must be a valid Brazilian state abbreviation, ",
      "IBGE code or state name",
      call. = FALSE
    )
  }
  tolower(result)
}

.tabnet_slug <- function(value) {
  value <- stringi::stri_trans_general(value, "Latin-ASCII")
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", "_", value)
  gsub("^_+|_+$", "", value)
}

.tabnet_form_options <- function(page) {
  primary_nodes <- rvest::html_elements(page, "select#L, select#C, select#I, select#A")
  primary_ids <- rvest::html_attr(primary_nodes, "id")
  primary_fields <- rvest::html_attr(primary_nodes, "name")
  field_names <- stats::setNames(primary_fields, primary_ids)
  field_names <- c(
    linha = unname(field_names[["L"]]),
    coluna = unname(field_names[["C"]]),
    conteudo = unname(field_names[["I"]]),
    periodo = unname(field_names[["A"]])
  )
  defaults <- c(
    linha = "Linha",
    coluna = "Coluna",
    conteudo = "Incremento",
    periodo = "Arquivos"
  )
  missing <- is.na(field_names) | !nzchar(field_names)
  field_names[missing] <- defaults[missing]

  filter_nodes <- rvest::html_elements(page, "select[id^='S']")
  filter_ids <- rvest::html_attr(filter_nodes, "id")
  filter_fields <- rvest::html_attr(filter_nodes, "name")
  filter_labels <- sub("^S", "", filter_fields)
  filter_names <- make.unique(.tabnet_slug(filter_labels), sep = "_")

  filters <- lapply(filter_ids, function(id) {
    result <- .tabnet_options(page, paste0("#", id, " option"))
    attr(result, "field") <- filter_fields[filter_ids == id][1L]
    result
  })
  names(filters) <- filter_names

  structure(
    list(
      linha = .tabnet_options(page, "#L option"),
      coluna = .tabnet_options(page, "#C option"),
      conteudo = .tabnet_options(page, "#I option"),
      periodo = .tabnet_options(page, "#A option"),
      filtros = filters
    ),
    filter_fields = stats::setNames(filter_fields, filter_names),
    field_names = field_names
  )
}

.tabnet_resolve_option <- function(options, selected, argument,
                                   default = 1L, index = FALSE,
                                   multiple = TRUE) {
  if (is.null(selected)) {
    selected <- default
    index <- TRUE
  }

  if (!length(selected) || anyNA(selected)) {
    stop("The '", argument, "' argument must not be empty or missing",
         call. = FALSE)
  }
  if (!multiple && length(selected) != 1L) {
    stop("The '", argument, "' argument must select exactly one option",
         call. = FALSE)
  }

  if (index && is.numeric(selected)) {
    if (any(selected != as.integer(selected)) ||
        any(selected < 1L | selected > nrow(options))) {
      stop("The '", argument, "' option index is out of range",
           call. = FALSE)
    }
    return(options$value[as.integer(selected)])
  }

  selected <- as.character(selected)
  if (identical(selected, "all")) {
    return(options$value[1L])
  }

  resolved <- vapply(selected, function(value) {
    match <- which(options$id == value | options$value == value)
    if (!length(match)) {
      stop(
        "Unknown value '", value, "' for '", argument,
        "'. Use datasus_opcoes() to inspect valid options.",
        call. = FALSE
      )
    }
    options$value[match[1L]]
  }, character(1))

  unique(unname(resolved))
}

.tabnet_resolve_period <- function(options, selected) {
  if (identical(selected, "last")) {
    return(options$value[1L])
  }
  if (!length(selected) || anyNA(selected)) {
    stop("The 'periodo' argument must not be empty or missing",
         call. = FALSE)
  }

  selected <- as.character(selected)
  resolved <- lapply(selected, function(value) {
    exact <- which(options$id == value | options$value == value)
    if (length(exact)) {
      return(options$value[exact])
    }

    if (grepl("^[0-9]{4}$", value)) {
      yearly <- which(
        options$id == value |
          grepl(paste0("/", value, "$"), options$id)
      )
      if (length(yearly)) {
        return(options$value[yearly])
      }
    }

    stop(
      "Unknown period '", value,
      "'. Use datasus_opcoes() to inspect available periods.",
      call. = FALSE
    )
  })

  unique(unlist(resolved, use.names = FALSE))
}

.tabnet_encode_component <- function(value) {
  value <- as.character(value)
  vapply(value, function(item) {
    if (is.na(item)) {
      return(NA_character_)
    }
    latin1 <- iconv(
      enc2utf8(item),
      from = "UTF-8",
      to = "ISO-8859-1",
      sub = "?"
    )
    bytes <- as.integer(charToRaw(latin1))
    unreserved <- bytes %in% c(
      45L, 46L, 95L, 126L, 33L, 42L, 39L, 40L, 41L
    ) |
      bytes >= 48L & bytes <= 57L |
      bytes >= 65L & bytes <= 90L |
      bytes >= 97L & bytes <= 122L
    encoded <- vapply(seq_along(bytes), function(index) {
      if (unreserved[[index]]) {
        rawToChar(as.raw(bytes[[index]]))
      } else {
        sprintf("%%%02X", bytes[[index]])
      }
    }, character(1))
    paste0(encoded, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

.tabnet_encode_fields <- function(fields) {
  pairs <- unlist(
    Map(function(name, value) {
      paste0(
        .tabnet_encode_component(name),
        "=",
        .tabnet_encode_component(value)
      )
    }, names(fields), fields),
    use.names = FALSE
  )

  paste(pairs, collapse = "&")
}

.tabnet_build_fields <- function(options, linha, coluna, conteudo,
                                 periodo, filtros) {
  if (!is.list(filtros)) {
    stop("The 'filtros' argument must be a named list", call. = FALSE)
  }
  if (length(filtros) &&
      (is.null(names(filtros)) || any(!nzchar(names(filtros))))) {
    stop("Every element of 'filtros' must have a name", call. = FALSE)
  }
  if (anyDuplicated(names(filtros))) {
    stop("Filter names must be unique", call. = FALSE)
  }

  filter_fields <- attr(options, "filter_fields")
  field_names <- attr(options, "field_names")
  if (is.null(field_names)) {
    field_names <- c(
      linha = "Linha",
      coluna = "Coluna",
      conteudo = "Incremento",
      periodo = "Arquivos"
    )
  }
  unknown <- setdiff(names(filtros), names(options$filtros))
  if (length(unknown)) {
    stop(
      "Unknown filter(s): ", paste(unknown, collapse = ", "),
      ". Use datasus_opcoes() to inspect valid filter names.",
      call. = FALSE
    )
  }

  values <- list(
    .tabnet_resolve_option(
      options$linha, linha, "linha", multiple = FALSE
    ),
    .tabnet_resolve_option(
      options$coluna, coluna, "coluna", multiple = FALSE
    ),
    .tabnet_resolve_option(
      options$conteudo, conteudo, "conteudo", index = is.numeric(conteudo)
    ),
    .tabnet_resolve_period(options$periodo, periodo)
  )
  names(values) <- unname(field_names[
    c("linha", "coluna", "conteudo", "periodo")
  ])
  fields <- values

  for (filter in names(options$filtros)) {
    selected <- if (filter %in% names(filtros)) filtros[[filter]] else "all"
    fields[[filter_fields[[filter]]]] <- .tabnet_resolve_option(
      options$filtros[[filter]],
      selected,
      paste0("filtros$", filter)
    )
  }

  fields$formato <- "table"
  fields$mostre <- "Mostra"
  fields
}
