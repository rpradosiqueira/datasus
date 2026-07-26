# OpenDataSUS client -------------------------------------------------------

.opendatasus_base_url <- "https://dadosabertos.saude.gov.br"

.opendatasus_request_raw <- function(url, destination = NULL) {
  request_timeout <- getOption(
    "datasus.timeout",
    .tabnet_default_timeout
  )
  timeout_name <- if (is.null(destination)) {
    "datasus.timeout"
  } else {
    "datasus.download_timeout"
  }
  timeout <- if (is.null(destination)) {
    request_timeout
  } else {
    getOption(
      "datasus.download_timeout",
      max(300, request_timeout)
    )
  }
  tries <- getOption("datasus.max_tries", .tabnet_default_tries)

  if (!is.numeric(timeout) || length(timeout) != 1L ||
      is.na(timeout) || timeout <= 0) {
    stop("Option '", timeout_name, "' must be a positive number",
         call. = FALSE)
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

  response <- tryCatch(
    if (is.null(destination)) {
      httr2::req_perform(request)
    } else {
      httr2::req_perform(request, path = destination)
    },
    error = function(error) {
      stop(
        "OpenDataSUS request failed: ", conditionMessage(error),
        call. = FALSE
      )
    }
  )

  httr2::resp_check_status(
    response,
    info = "OpenDataSUS returned an unsuccessful HTTP status"
  )
  if (is.null(destination)) {
    return(httr2::resp_body_raw(response))
  }
  invisible(destination)
}

.datasus_cache_root <- function() {
  directory <- getOption(
    "datasus.cache_dir",
    tools::R_user_dir("datasus", "cache")
  )
  if (!is.character(directory) || length(directory) != 1L ||
      is.na(directory) || !nzchar(directory)) {
    stop("Option 'datasus.cache_dir' must be a directory path",
         call. = FALSE)
  }
  path.expand(directory)
}

.opendatasus_cache_dir <- function() {
  file.path(.datasus_cache_root(), "opendatasus")
}

.opendatasus_cache_ttl <- function() {
  ttl <- getOption("datasus.cache_ttl", 3600)
  if (!is.numeric(ttl) || length(ttl) != 1L || is.na(ttl) || ttl < 0) {
    stop("Option 'datasus.cache_ttl' must be a non-negative number",
         call. = FALSE)
  }
  ttl
}

.opendatasus_key <- function(value) {
  value <- enc2utf8(paste(value, collapse = "_"))
  slug <- gsub("[^A-Za-z0-9._-]+", "_", value)
  slug <- substr(gsub("^_+|_+$", "", slug), 1L, 80L)
  if (!nzchar(slug)) {
    slug <- "request"
  }

  integers <- utf8ToInt(value)
  hash <- 0
  for (integer in integers) {
    hash <- (hash * 31 + integer) %% 2147483647
  }
  paste0(slug, "-", sprintf("%08x", as.integer(hash)))
}

.opendatasus_metadata_path <- function(key) {
  file.path(.opendatasus_cache_dir(), "metadata", paste0(key, ".rds"))
}

.opendatasus_cache_read <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  age <- as.numeric(
    difftime(Sys.time(), file.info(path)$mtime, units = "secs")
  )
  if (is.na(age) || age > .opendatasus_cache_ttl()) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(...) NULL)
}

.opendatasus_cache_write <- function(value, path) {
  directory <- dirname(path)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the OpenDataSUS cache directory: ", directory,
         call. = FALSE)
  }
  saveRDS(value, path)
  invisible(value)
}

.opendatasus_parse_page <- function(content) {
  if (!is.raw(content)) {
    stop("OpenDataSUS content must be supplied as a raw vector",
         call. = FALSE)
  }

  page <- xml2::read_html(content, encoding = "UTF-8")
  node <- rvest::html_element(page, "script#__NEXT_DATA__")
  if (inherits(node, "xml_missing")) {
    stop("OpenDataSUS returned a page without embedded metadata",
         call. = FALSE)
  }

  payload <- rvest::html_text(node)
  parsed <- tryCatch(
    jsonlite::fromJSON(payload, simplifyVector = FALSE),
    error = function(error) {
      stop(
        "Could not parse the metadata returned by OpenDataSUS: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  result <- parsed$props$pageProps
  if (is.null(result)) {
    stop("OpenDataSUS returned an unexpected metadata structure",
         call. = FALSE)
  }
  result
}

.opendatasus_page_data <- function(url, cache = TRUE, atualizar = FALSE) {
  if (!is.logical(cache) || length(cache) != 1L || is.na(cache)) {
    stop("The 'cache' argument must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(atualizar) || length(atualizar) != 1L ||
      is.na(atualizar)) {
    stop("The 'atualizar' argument must be TRUE or FALSE", call. = FALSE)
  }

  path <- .opendatasus_metadata_path(.opendatasus_key(url))
  if (cache && !atualizar) {
    cached <- .opendatasus_cache_read(path)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  result <- .opendatasus_parse_page(.opendatasus_request_raw(url))
  if (cache) {
    .opendatasus_cache_write(result, path)
  }
  result
}

.opendatasus_scalar <- function(value, name) {
  result <- value[[name]]
  if (is.null(result) || !length(result)) {
    return(NA_character_)
  }
  as.character(result[[1L]])
}

.opendatasus_collapse <- function(value, field = NULL) {
  if (is.null(value) || !length(value)) {
    return(NA_character_)
  }
  if (!is.null(field)) {
    value <- vapply(
      value,
      function(item) .opendatasus_scalar(item, field),
      character(1)
    )
  }
  value <- unique(as.character(unlist(value, use.names = FALSE)))
  value <- value[!is.na(value) & nzchar(value)]
  if (!length(value)) NA_character_ else paste(value, collapse = "; ")
}

.opendatasus_validate_positive_integer <- function(value, argument) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      value != as.integer(value) || value < 1L) {
    stop("The '", argument, "' argument must be a positive integer",
         call. = FALSE)
  }
  as.integer(value)
}

.opendatasus_url <- function(path, query = list()) {
  url <- httr2::url_parse(paste0(.opendatasus_base_url, path))
  query <- query[!vapply(query, is.null, logical(1))]
  url$query <- lapply(query, as.character)
  httr2::url_build(url)
}

#' Search the OpenDataSUS catalog
#'
#' Searches the current OpenDataSUS portal and returns lightweight metadata
#' for matching datasets. Use [opendatasus_recursos()] to inspect the files
#' and API links published for one dataset.
#'
#' @param busca Optional free-text search.
#' @param grupo Optional OpenDataSUS group slug.
#' @param formato Optional resource format, such as `"CSV"`, `"JSON"` or
#'   `"PDF"`.
#' @param pagina Positive page number.
#' @param limite Number of results requested per page.
#' @param cache Whether to cache portal metadata for the duration controlled by
#'   option `datasus.cache_ttl`.
#' @param atualizar Whether to ignore cached metadata and query the portal
#'   again.
#'
#' @return A data frame with dataset names, titles, descriptions, formats and
#'   groups. Attribute `"datasus_proveniencia"` records the source and
#'   consultation time.
#' @export
#'
#' @examples
#' \dontrun{
#' opendatasus_catalogo("dengue")
#' opendatasus_catalogo(grupo = "arboviroses", formato = "CSV")
#' }
opendatasus_catalogo <- function(busca = NULL, grupo = NULL, formato = NULL,
                                 pagina = 1L, limite = 20L, cache = TRUE,
                                 atualizar = FALSE) {
  pagina <- .opendatasus_validate_positive_integer(pagina, "pagina")
  limite <- .opendatasus_validate_positive_integer(limite, "limite")
  scalar_or_null <- function(value, argument) {
    if (is.null(value)) {
      return(NULL)
    }
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      stop("The '", argument, "' argument must be a single string",
           call. = FALSE)
    }
    value
  }
  busca <- scalar_or_null(busca, "busca")
  grupo <- scalar_or_null(grupo, "grupo")
  formato <- scalar_or_null(formato, "formato")

  url <- .opendatasus_url(
    "/dataset",
    list(
      q = busca,
      groups = grupo,
      res_format = formato,
      page = pagina,
      rows = limite
    )
  )
  payload <- .opendatasus_page_data(url, cache, atualizar)
  packages <- payload$packages

  if (is.null(packages) || !length(packages)) {
    result <- data.frame(
      conjunto = character(),
      titulo = character(),
      descricao = character(),
      formatos = character(),
      grupos = character(),
      stringsAsFactors = FALSE
    )
  } else {
    result <- do.call(
      rbind,
      lapply(packages, function(package) {
        data.frame(
          conjunto = .opendatasus_scalar(package, "name"),
          titulo = .opendatasus_scalar(package, "title"),
          descricao = .opendatasus_scalar(package, "notes"),
          formatos = .opendatasus_collapse(package$formats),
          grupos = .opendatasus_collapse(package$groups, "name"),
          stringsAsFactors = FALSE
        )
      })
    )
    row.names(result) <- NULL
  }

  total <- payload$numberOfPackages
  if (is.null(total) || !length(total)) {
    total <- nrow(result)
  }
  attr(result, "total") <- as.integer(total)
  attr(result, "datasus_proveniencia") <- list(
    fonte = url,
    consultado_em = Sys.time(),
    pagina = pagina,
    limite = limite
  )
  result
}

.opendatasus_validate_dataset <- function(conjunto) {
  if (!is.character(conjunto) || length(conjunto) != 1L ||
      is.na(conjunto) || !grepl("^[a-z0-9][a-z0-9-]*$", conjunto)) {
    stop(
      "The 'conjunto' argument must be a valid OpenDataSUS dataset slug",
      call. = FALSE
    )
  }
  conjunto
}

.opendatasus_dataset <- function(conjunto, cache = TRUE,
                                 atualizar = FALSE) {
  conjunto <- .opendatasus_validate_dataset(conjunto)
  url <- .opendatasus_url(paste0("/dataset/", conjunto))
  payload <- .opendatasus_page_data(url, cache, atualizar)
  if (is.null(payload$name) || is.null(payload$resources)) {
    stop("OpenDataSUS dataset '", conjunto, "' was not found",
         call. = FALSE)
  }
  payload
}

.opendatasus_resource_year <- function(name) {
  if (!length(name) || is.na(name) || !nzchar(name)) {
    return(NA_integer_)
  }
  years <- regmatches(
    as.character(name),
    gregexpr("(?<![0-9])(19|20)[0-9]{2}(?![0-9])", name, perl = TRUE)
  )[[1L]]
  if (!length(years) || identical(years, character(0))) {
    return(NA_integer_)
  }
  as.integer(years[[1L]])
}

.opendatasus_resource_format <- function(resource) {
  format <- toupper(.opendatasus_scalar(resource, "format"))
  if (!is.na(format) && nzchar(format)) {
    return(format)
  }
  text <- paste(
    .opendatasus_scalar(resource, "name"),
    .opendatasus_scalar(resource, "url")
  )
  matches <- regmatches(
    toupper(text),
    regexpr("PARQUET|CSV|JSON|XML|PDF", toupper(text))
  )
  if (!length(matches) || !nzchar(matches)) NA_character_ else matches
}

.opendatasus_extract_urls <- function(url, description = NULL) {
  values <- character()
  if (length(url) && !is.na(url) && nzchar(trimws(url))) {
    values <- trimws(url)
  }
  if (length(description) && !is.na(description) &&
      nzchar(description)) {
    matches <- regmatches(
      description,
      gregexpr("https?://[^)[:space:]]+", description, perl = TRUE)
    )[[1L]]
    if (length(matches) && !identical(matches, character(0))) {
      matches <- sub("[>.,;]+$", "", matches)
      values <- c(values, matches)
    }
  }
  unique(values[nzchar(values)])
}

#' List resources from an OpenDataSUS dataset
#'
#' @param conjunto Dataset slug returned by [opendatasus_catalogo()].
#' @inheritParams opendatasus_catalogo
#'
#' @return A data frame with one row per published resource, including its
#'   stable identifier, name, format, inferred year, URL and update date.
#' @export
#'
#' @examples
#' \dontrun{
#' opendatasus_recursos("srag-2019-a-2026")
#' opendatasus_recursos("arboviroses-dengue")
#' }
opendatasus_recursos <- function(conjunto, cache = TRUE,
                                 atualizar = FALSE) {
  package <- .opendatasus_dataset(conjunto, cache, atualizar)
  resources <- package$resources

  result <- do.call(
    rbind,
    lapply(resources, function(resource) {
      name <- .opendatasus_scalar(resource, "name")
      size <- suppressWarnings(
        as.numeric(.opendatasus_scalar(resource, "size"))
      )
      position <- suppressWarnings(
        as.integer(.opendatasus_scalar(resource, "position"))
      )
      data.frame(
        conjunto = .opendatasus_scalar(package, "name"),
        id = .opendatasus_scalar(resource, "id"),
        nome = name,
        descricao = .opendatasus_scalar(resource, "description"),
        formato = .opendatasus_resource_format(resource),
        ano = .opendatasus_resource_year(name),
        url = .opendatasus_scalar(resource, "url"),
        tamanho = size,
        criado = .opendatasus_scalar(resource, "created"),
        modificado = .opendatasus_scalar(resource, "last_modified"),
        posicao = position,
        stringsAsFactors = FALSE
      )
    })
  )
  row.names(result) <- NULL
  attr(result, "datasus_proveniencia") <- list(
    fonte = .opendatasus_url(paste0("/dataset/", conjunto)),
    consultado_em = Sys.time(),
    conjunto = conjunto,
    titulo = .opendatasus_scalar(package, "title"),
    metadados_modificados = .opendatasus_scalar(
      package, "metadata_modified"
    )
  )
  result
}

#' Expand downloadable files from OpenDataSUS resources
#'
#' Some OpenDataSUS resources, notably state-level influenza-like illness
#' datasets, publish several file parts as links inside the resource
#' description instead of the primary URL field. This function expands both
#' representations to one row per downloadable file.
#'
#' @param conjunto Dataset slug returned by [opendatasus_catalogo()].
#' @param recurso Optional exact resource name or identifier.
#' @param formato Optional resource format.
#' @inheritParams opendatasus_catalogo
#'
#' @return A data frame with one row per downloadable file and its parent
#'   resource identifier, part number and URL.
#' @export
#'
#' @examples
#' \dontrun{
#' opendatasus_arquivos(
#'   "notificacoes-de-sindrome-gripal-leve-2024",
#'   recurso = "Dados MS - 20/12",
#'   formato = "CSV"
#' )
#' }
opendatasus_arquivos <- function(conjunto, recurso = NULL, formato = NULL,
                                 cache = TRUE, atualizar = FALSE) {
  resources <- opendatasus_recursos(
    conjunto,
    cache = cache,
    atualizar = atualizar
  )
  selected <- resources
  if (!is.null(recurso)) {
    if (!is.character(recurso) || length(recurso) != 1L ||
        is.na(recurso) || !nzchar(recurso)) {
      stop("'recurso' must be one resource name or identifier",
           call. = FALSE)
    }
    selected <- selected[
      (!is.na(selected$id) & selected$id == recurso) |
        (!is.na(selected$nome) & selected$nome == recurso),
      ,
      drop = FALSE
    ]
  }
  if (!is.null(formato)) {
    if (!is.character(formato) || length(formato) != 1L ||
        is.na(formato) || !nzchar(formato)) {
      stop("'formato' must identify one resource format",
           call. = FALSE)
    }
    selected <- selected[
      !is.na(selected$formato) &
        toupper(selected$formato) == toupper(formato),
      ,
      drop = FALSE
    ]
  }
  if (!nrow(selected)) {
    stop(
      "No OpenDataSUS resource matches the requested file selection",
      call. = FALSE
    )
  }

  rows <- lapply(seq_len(nrow(selected)), function(index) {
    resource <- selected[index, , drop = FALSE]
    urls <- .opendatasus_extract_urls(
      resource$url[[1L]],
      resource$descricao[[1L]]
    )
    if (!length(urls)) {
      return(NULL)
    }
    year <- resource$ano[[1L]]
    if (is.na(year)) {
      year <- .opendatasus_resource_year(resource$conjunto[[1L]])
    }
    data.frame(
      conjunto = resource$conjunto[[1L]],
      recurso_id = resource$id[[1L]],
      recurso = resource$nome[[1L]],
      formato = resource$formato[[1L]],
      ano = year,
      parte = seq_along(urls),
      url = urls,
      modificado = resource$modificado[[1L]],
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    result <- data.frame(
      conjunto = character(),
      recurso_id = character(),
      recurso = character(),
      formato = character(),
      ano = integer(),
      parte = integer(),
      url = character(),
      modificado = character(),
      stringsAsFactors = FALSE
    )
  } else {
    result <- do.call(rbind, rows)
    row.names(result) <- NULL
  }
  attr(result, "datasus_proveniencia") <- list(
    fonte = .opendatasus_url(paste0("/dataset/", conjunto)),
    consultado_em = Sys.time(),
    conjunto = conjunto,
    recursos = unique(result$recurso_id)
  )
  result
}

.opendatasus_select_resource <- function(resources, recurso = NULL,
                                          ano = "last", formato = NULL) {
  selected <- resources

  if (!is.null(recurso)) {
    if (!is.character(recurso) || length(recurso) != 1L ||
        is.na(recurso) || !nzchar(recurso)) {
      stop("The 'recurso' argument must be a resource name or identifier",
           call. = FALSE)
    }
    selected <- selected[
      (!is.na(selected$id) & selected$id == recurso) |
        (!is.na(selected$nome) & selected$nome == recurso),
      ,
      drop = FALSE
    ]
  } else {
    if (is.null(formato)) {
      formato <- "CSV"
    }
    if (!is.null(ano)) {
      if (identical(ano, "last")) {
        available <- selected$ano[!is.na(selected$ano)]
        if (!length(available)) {
          stop("No dated resource is available for this dataset",
               call. = FALSE)
        }
        ano <- max(available)
      }
      if (!is.numeric(ano) || length(ano) != 1L || is.na(ano) ||
          ano != as.integer(ano)) {
        stop("The 'ano' argument must be a year, 'last', or NULL",
             call. = FALSE)
      }
      selected <- selected[
        !is.na(selected$ano) & selected$ano == as.integer(ano),
        ,
        drop = FALSE
      ]
    }
  }

  if (!is.null(formato)) {
    if (!is.character(formato) || length(formato) != 1L ||
        is.na(formato) || !nzchar(formato)) {
      stop("The 'formato' argument must be a single resource format",
           call. = FALSE)
    }
    selected <- selected[
      !is.na(selected$formato) &
        toupper(selected$formato) == toupper(formato),
      ,
      drop = FALSE
    ]
  }

  if (!nrow(selected)) {
    stop(
      "No OpenDataSUS resource matches the requested selection. ",
      "Use opendatasus_recursos() to inspect available resources.",
      call. = FALSE
    )
  }
  if (nrow(selected) > 1L) {
    choices <- paste0(selected$nome, " [", selected$formato, "]")
    stop(
      "The OpenDataSUS resource selection is ambiguous: ",
      paste(utils::head(choices, 5L), collapse = "; "),
      ". Supply 'recurso', 'ano' or 'formato' more precisely.",
      call. = FALSE
    )
  }
  selected
}

.opendatasus_download_name <- function(resource) {
  parsed <- httr2::url_parse(resource$url)
  name <- basename(parsed$path)
  if (!nzchar(name) || identical(name, "/")) {
    name <- paste0(resource$id, ".", tolower(resource$formato))
  }
  name
}

.opendatasus_download_provenance <- function(resource, path, cached) {
  list(
    fonte = resource$url,
    conjunto = resource$conjunto,
    recurso_id = resource$id,
    recurso = resource$nome,
    formato = resource$formato,
    ano = resource$ano,
    recurso_modificado = resource$modificado,
    baixado_em = Sys.time(),
    arquivo = normalizePath(path, winslash = "/", mustWork = FALSE),
    md5 = unname(tools::md5sum(path)),
    cache = isTRUE(cached)
  )
}

#' Download a resource from OpenDataSUS
#'
#' Resolves a resource from the live portal metadata, downloads it atomically
#' and records its origin and MD5 checksum. By default, files are reused from
#' the package cache. A cached file is refreshed automatically when the
#' resource update timestamp changes.
#'
#' @param conjunto Dataset slug returned by [opendatasus_catalogo()].
#' @param recurso Optional exact resource name or stable resource identifier.
#' @param ano Resource year, `"last"` for the latest dated resource, or
#'   `NULL`. Ignored when `recurso` is supplied.
#' @param formato Resource format. When both `recurso` and `formato` are
#'   `NULL`, CSV is preferred.
#' @param destino Optional destination file or existing directory. `NULL`
#'   uses the package cache, or a temporary file when `cache = FALSE`.
#' @inheritParams opendatasus_catalogo
#'
#' @return The normalized local file path, with classes `"datasus_arquivo"`
#'   and `"character"`. Use [datasus_proveniencia()] to inspect its origin.
#' @export
#'
#' @examples
#' \dontrun{
#' file <- opendatasus_baixar(
#'   "arboviroses-dengue",
#'   ano = 2025,
#'   formato = "CSV"
#' )
#' datasus_proveniencia(file)
#' }
opendatasus_baixar <- function(conjunto, recurso = NULL, ano = "last",
                               formato = NULL, destino = NULL, cache = TRUE,
                               atualizar = FALSE) {
  resources <- opendatasus_recursos(
    conjunto,
    cache = cache,
    atualizar = atualizar
  )
  resource <- .opendatasus_select_resource(
    resources, recurso, ano, formato
  )

  if (!is.logical(cache) || length(cache) != 1L || is.na(cache)) {
    stop("The 'cache' argument must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(atualizar) || length(atualizar) != 1L ||
      is.na(atualizar)) {
    stop("The 'atualizar' argument must be TRUE or FALSE", call. = FALSE)
  }

  name <- .opendatasus_download_name(resource)
  if (is.null(destino)) {
    if (cache) {
      destination <- file.path(
        .opendatasus_cache_dir(), "files", resource$id, name
      )
    } else {
      destination <- tempfile(fileext = paste0("-", name))
    }
  } else {
    if (!is.character(destino) || length(destino) != 1L ||
        is.na(destino) || !nzchar(destino)) {
      stop("The 'destino' argument must be a file or directory path",
           call. = FALSE)
    }
    destination <- path.expand(destino)
    if (dir.exists(destination)) {
      destination <- file.path(destination, name)
    }
  }

  directory <- dirname(destination)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the download directory: ", directory,
         call. = FALSE)
  }
  sidecar <- paste0(destination, ".datasus.rds")
  previous <- if (file.exists(sidecar)) {
    tryCatch(readRDS(sidecar), error = function(...) NULL)
  } else {
    NULL
  }
  current_version <- resource$modificado
  same_version <- !is.null(previous) &&
    identical(previous$recurso_modificado, current_version)
  reuse <- file.exists(destination) && !atualizar &&
    (is.null(previous) || same_version)

  if (reuse) {
    provenance <- if (is.null(previous)) {
      .opendatasus_download_provenance(resource, destination, TRUE)
    } else {
      previous$cache <- TRUE
      previous
    }
  } else {
    temporary <- tempfile(
      pattern = paste0(".", basename(destination), "-"),
      tmpdir = directory
    )
    on.exit(unlink(temporary), add = TRUE)
    .opendatasus_request_raw(resource$url, destination = temporary)
    if (!file.copy(temporary, destination, overwrite = TRUE)) {
      stop("Could not move the downloaded OpenDataSUS resource to ",
           destination, call. = FALSE)
    }
    provenance <- .opendatasus_download_provenance(
      resource, destination, FALSE
    )
    saveRDS(provenance, sidecar)
  }

  result <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  attr(result, "datasus_proveniencia") <- provenance
  class(result) <- c("datasus_arquivo", "character")
  result
}

.opendatasus_open_file <- function(path, format) {
  if (!grepl("[.]zip$", path, ignore.case = TRUE)) {
    return(path)
  }

  listing <- utils::unzip(path, list = TRUE)
  members <- listing$Name
  extension <- paste0("[.]", tolower(format), "$")
  selected_index <- which(grepl(extension, tolower(members)))
  if (length(selected_index) != 1L) {
    stop(
      "The downloaded ZIP archive must contain exactly one ",
      format, " file",
      call. = FALSE
    )
  }
  if (!is.null(listing$Length) &&
      listing$Length[[selected_index]] <= 0) {
    stop(
      "The downloaded ZIP archive contains an empty ",
      format, " file. The source resource is currently unavailable.",
      call. = FALSE
    )
  }
  selected <- members[[selected_index]]
  unz(path, selected, open = "rb")
}

.opendatasus_read_file <- function(path, format, n_max = Inf,
                                    colunas = NULL, ...) {
  format <- toupper(format)
  if (!is.null(colunas) &&
      (!is.character(colunas) || !length(colunas) ||
       anyNA(colunas) || any(!nzchar(colunas)))) {
    stop("'colunas' must contain valid column names", call. = FALSE)
  }
  connection <- .opendatasus_open_file(path, format)
  if (inherits(connection, "connection")) {
    on.exit(close(connection), add = TRUE)
  }

  if (identical(format, "CSV")) {
    arguments <- list(
      file = connection,
      delim = NULL,
      n_max = n_max,
      show_col_types = FALSE
    )
    if (!is.null(colunas)) {
      arguments$col_select <- colunas
    }
    extra_arguments <- list(...)
    if (length(extra_arguments)) {
      extra_names <- names(extra_arguments)
      if (is.null(extra_names) || any(!nzchar(extra_names))) {
        stop("Additional CSV reader arguments must be named",
             call. = FALSE)
      }
      arguments[extra_names] <- extra_arguments
    }
    return(do.call(readr::read_delim, arguments))
  }
  if (identical(format, "JSON")) {
    result <- jsonlite::fromJSON(connection, ...)
    if (is.finite(n_max) && is.data.frame(result)) {
      result <- utils::head(result, n_max)
    }
    if (!is.null(colunas) && is.data.frame(result)) {
      missing <- setdiff(colunas, names(result))
      if (length(missing)) {
        stop(
          "Requested column(s) not found in JSON resource: ",
          paste(missing, collapse = ", "),
          call. = FALSE
        )
      }
      result <- result[colunas]
    }
    return(result)
  }

  stop(
    "Reading format '", format,
    "' is not supported. Use opendatasus_baixar() to download it.",
    call. = FALSE
  )
}

#' Download and read tabular OpenDataSUS data
#'
#' Selects and downloads one CSV or JSON resource, then reads it into R.
#' CSV delimiters are detected automatically. For very large datasets, use
#' `n_max` while exploring the columns, or call [opendatasus_baixar()] and
#' use a streaming database engine.
#'
#' @inheritParams opendatasus_baixar
#' @param n_max Maximum number of CSV rows to read. For JSON data, the limit is
#'   applied after parsing when the result is a data frame.
#' @param colunas Optional character vector selecting columns while CSV files
#'   are parsed. JSON columns are selected after parsing.
#' @param ... Additional arguments passed to [readr::read_delim()] or
#'   [jsonlite::fromJSON()].
#'
#' @return The parsed resource. Attribute `"datasus_proveniencia"` records
#'   the selected resource, download time, local file and checksum.
#' @export
#'
#' @examples
#' \dontrun{
#' dengue <- opendatasus_ler(
#'   "arboviroses-dengue",
#'   ano = 2025,
#'   formato = "CSV",
#'   n_max = 1000
#' )
#' datasus_proveniencia(dengue)
#' }
opendatasus_ler <- function(conjunto, recurso = NULL, ano = "last",
                            formato = NULL, destino = NULL, cache = TRUE,
                            atualizar = FALSE, n_max = Inf,
                            colunas = NULL, ...) {
  if (!is.numeric(n_max) || length(n_max) != 1L || is.na(n_max) ||
      n_max < 0) {
    stop("The 'n_max' argument must be a non-negative number",
         call. = FALSE)
  }
  file <- opendatasus_baixar(
    conjunto = conjunto,
    recurso = recurso,
    ano = ano,
    formato = formato,
    destino = destino,
    cache = cache,
    atualizar = atualizar
  )
  provenance <- datasus_proveniencia(file)
  result <- .opendatasus_read_file(
    as.character(file),
    provenance$formato,
    n_max = n_max,
    colunas = colunas,
    ...
  )
  attr(result, "datasus_proveniencia") <- provenance
  result
}

#' Retrieve DATASUS provenance metadata
#'
#' @param x An object returned by [opendatasus_catalogo()],
#'   [opendatasus_recursos()], [opendatasus_baixar()] or
#'   [opendatasus_ler()].
#'
#' @return A named list, or `NULL` when no provenance metadata is attached.
#' @export
datasus_proveniencia <- function(x) {
  attr(x, "datasus_proveniencia", exact = TRUE)
}

#' Read SIVEP-Gripe severe acute respiratory syndrome microdata
#'
#' @param ano Dataset year or `"last"`.
#' @param formato `"CSV"` or `"JSON"`.
#' @param destino Optional destination file or existing directory.
#' @param cache Whether to reuse the local cache.
#' @param atualizar Whether to force a new metadata query and download.
#' @param n_max Maximum number of CSV rows to read.
#' @param ... Additional arguments passed to the underlying reader.
#'
#' @return The parsed OpenDataSUS resource.
#' @export
#'
#' @examples
#' \dontrun{
#' srag <- sivep_gripe(ano = 2025, n_max = 1000)
#' }
sivep_gripe <- function(ano = "last", formato = "CSV", destino = NULL,
                         cache = TRUE, atualizar = FALSE, n_max = Inf, ...) {
  opendatasus_ler(
    "srag-2019-a-2026",
    ano = ano,
    formato = formato,
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    n_max = n_max,
    ...
  )
}

#' Read current SINAN dengue microdata
#'
#' Downloads the annual OpenDataSUS resources. This complements the aggregated
#' legacy TABNET interface available from `sinan("dengue")`.
#'
#' @inheritParams sivep_gripe
#'
#' @return The parsed OpenDataSUS resource.
#' @export
#'
#' @examples
#' \dontrun{
#' dengue <- sinan_dengue(ano = 2025, n_max = 1000)
#' }
sinan_dengue <- function(ano = "last", formato = "CSV", destino = NULL,
                          cache = TRUE, atualizar = FALSE, n_max = Inf, ...) {
  opendatasus_ler(
    "arboviroses-dengue",
    ano = ano,
    formato = formato,
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    n_max = n_max,
    ...
  )
}

#' Read current e-SUS SINAN Mpox microdata
#'
#' @inheritParams sivep_gripe
#'
#' @return The parsed OpenDataSUS resource.
#' @export
#'
#' @examples
#' \dontrun{
#' cases <- sinan_mpox(ano = 2025, n_max = 1000)
#' }
sinan_mpox <- function(ano = "last", formato = "CSV", destino = NULL,
                        cache = TRUE, atualizar = FALSE, n_max = Inf, ...) {
  opendatasus_ler(
    "mpox",
    ano = ano,
    formato = formato,
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    n_max = n_max,
    ...
  )
}
