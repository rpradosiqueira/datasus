# Efficient OpenDataSUS reading ------------------------------------------

.opendatasus_file_name <- function(file) {
  parsed <- httr2::url_parse(file$url[[1L]])
  name <- basename(parsed$path)
  if (!nzchar(name) || identical(name, "/")) {
    name <- paste0(
      file$recurso_id[[1L]], "-parte-", file$parte[[1L]], ".",
      tolower(file$formato[[1L]])
    )
  }
  name
}

.opendatasus_file_provenance <- function(file, path, cached) {
  list(
    fonte = file$url[[1L]],
    conjunto = file$conjunto[[1L]],
    recurso_id = file$recurso_id[[1L]],
    recurso = file$recurso[[1L]],
    formato = file$formato[[1L]],
    ano = file$ano[[1L]],
    parte = file$parte[[1L]],
    recurso_modificado = file$modificado[[1L]],
    baixado_em = Sys.time(),
    arquivo = normalizePath(path, winslash = "/", mustWork = FALSE),
    md5 = unname(tools::md5sum(path)),
    cache = isTRUE(cached)
  )
}

.opendatasus_download_file <- function(file, destino = NULL, cache = TRUE,
                                       atualizar = FALSE) {
  if (!is.data.frame(file) || nrow(file) != 1L ||
      is.na(file$url[[1L]]) || !nzchar(file$url[[1L]])) {
    stop("One downloadable OpenDataSUS file is required", call. = FALSE)
  }
  if (!is.logical(cache) || length(cache) != 1L || is.na(cache) ||
      !is.logical(atualizar) || length(atualizar) != 1L ||
      is.na(atualizar)) {
    stop("'cache' and 'atualizar' must be TRUE or FALSE",
         call. = FALSE)
  }
  name <- .opendatasus_file_name(file)
  if (is.null(destino)) {
    if (cache) {
      destination <- file.path(
        .opendatasus_cache_dir(),
        "files",
        file$recurso_id[[1L]],
        .opendatasus_key(file$url[[1L]]),
        name
      )
    } else {
      destination <- tempfile(fileext = paste0("-", name))
    }
  } else {
    if (!is.character(destino) || length(destino) != 1L ||
        is.na(destino) || !nzchar(destino)) {
      stop("'destino' must be a file or directory path",
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
  version <- paste(file$modificado[[1L]], file$url[[1L]])
  reuse <- file.exists(destination) && !atualizar &&
    (is.null(previous) || identical(previous$versao_arquivo, version))

  if (reuse) {
    provenance <- if (is.null(previous)) {
      .opendatasus_file_provenance(file, destination, TRUE)
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
    .opendatasus_request_raw(file$url[[1L]], destination = temporary)
    if (!file.copy(temporary, destination, overwrite = TRUE)) {
      stop("Could not move the downloaded OpenDataSUS file to ",
           destination, call. = FALSE)
    }
    provenance <- .opendatasus_file_provenance(
      file, destination, FALSE
    )
    provenance$versao_arquivo <- version
    saveRDS(provenance, sidecar)
  }
  result <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  attr(result, "datasus_proveniencia") <- provenance
  class(result) <- c("datasus_arquivo", "character")
  result
}

.opendatasus_validate_columns <- function(colunas) {
  if (is.null(colunas)) {
    return(NULL)
  }
  if (!is.character(colunas) || !length(colunas) ||
      anyNA(colunas) || any(!nzchar(colunas)) ||
      anyDuplicated(colunas)) {
    stop("'colunas' must contain unique valid column names",
         call. = FALSE)
  }
  colunas
}

.opendatasus_validate_normalize <- function(normalizar) {
  if (!is.logical(normalizar) || length(normalizar) != 1L ||
      is.na(normalizar)) {
    stop("'normalizar' must be TRUE or FALSE", call. = FALSE)
  }
  normalizar
}

.opendatasus_bind_rows <- function(values) {
  if (!length(values)) {
    return(data.frame())
  }
  columns <- unique(unlist(lapply(values, names), use.names = FALSE))
  values <- lapply(values, function(value) {
    missing <- setdiff(columns, names(value))
    for (column in missing) {
      value[[column]] <- NA
    }
    value[columns]
  })
  result <- do.call(rbind, values)
  row.names(result) <- NULL
  result
}

.opendatasus_detect_delimiter <- function(path) {
  connection <- .opendatasus_open_file(path, "CSV")
  close_connection <- inherits(connection, "connection")
  header <- tryCatch(
    readLines(connection, n = 1L, warn = FALSE, encoding = "UTF-8"),
    finally = {
      if (close_connection && isOpen(connection)) {
        close(connection)
      }
    }
  )
  if (!length(header)) {
    stop("The CSV resource is empty", call. = FALSE)
  }
  candidates <- c(",", ";", "\t", "|")
  counts <- vapply(candidates, function(delimiter) {
    lengths(regmatches(
      header,
      gregexpr(delimiter, header, fixed = TRUE)
    ))
  }, integer(1))
  if (!any(counts > 0L)) {
    stop("Could not detect the CSV delimiter", call. = FALSE)
  }
  candidates[[which.max(counts)]]
}

.contemporary_read_files <- function(files, destino, cache, atualizar,
                                     n_max, colunas, sistema,
                                     normalizar, ...) {
  if (!nrow(files)) {
    stop("No downloadable file was found for this resource",
         call. = FALSE)
  }
  if (nrow(files) > 1L && !is.null(destino) && !dir.exists(destino)) {
    stop(
      "'destino' must be an existing directory when a resource has ",
      "multiple file parts",
      call. = FALSE
    )
  }
  colunas <- .opendatasus_validate_columns(colunas)
  normalizar <- .opendatasus_validate_normalize(normalizar)
  if (!is.numeric(n_max) || length(n_max) != 1L || is.na(n_max) ||
      n_max < 0) {
    stop("'n_max' must be a non-negative number", call. = FALSE)
  }
  remaining <- n_max
  values <- list()
  provenance <- list()
  for (index in seq_len(nrow(files))) {
    if (is.finite(remaining) && remaining <= 0) {
      break
    }
    path <- .opendatasus_download_file(
      files[index, , drop = FALSE],
      destino = destino,
      cache = cache,
      atualizar = atualizar
    )
    part <- .opendatasus_read_file(
      as.character(path),
      files$formato[[index]],
      n_max = remaining,
      colunas = colunas,
      ...
    )
    if (normalizar) {
      part <- datasus_padronizar(part, sistema)
    }
    values[[length(values) + 1L]] <- part
    provenance[[length(provenance) + 1L]] <- datasus_proveniencia(path)
    if (is.finite(remaining)) {
      remaining <- remaining - nrow(part)
    }
  }
  result <- .opendatasus_bind_rows(values)
  attr(result, "datasus_proveniencia") <- list(
    conjunto = unique(files$conjunto),
    recursos = provenance,
    arquivos_lidos = length(provenance),
    linhas = nrow(result)
  )
  if (normalizar) {
    attr(result, "datasus_dicionario") <- datasus_dicionario(sistema)
  }
  result
}

#' Process large OpenDataSUS CSV resources in chunks
#'
#' Downloads the selected resource using the regular atomic cache, expands
#' multipart resources, and invokes a callback for bounded-size chunks.
#' Returning small summaries or `NULL` from the callback keeps memory use
#' independent of the complete dataset size.
#'
#' @inheritParams opendatasus_ler
#' @param FUN Function called as `FUN(dados, posicao, arquivo)`, where
#'   `posicao` is the first row position within a file and `arquivo` is the
#'   one-based file-part index.
#' @param tamanho_bloco Positive number of rows parsed per chunk.
#' @param sistema Optional dictionary system accepted by
#'   [datasus_padronizar()]. When supplied, each chunk is standardized before
#'   `FUN` is called.
#'
#' @return An object of class `"datasus_processamento"` containing callback
#'   results, row, chunk and file counts, and provenance for every file.
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- opendatasus_processar(
#'   "doses-aplicadas-pelo-programa-de-nacional-de-imunizacoes-pni-2026",
#'   recurso = "Vacinação - Julho 2026",
#'   ano = NULL,
#'   formato = "CSV",
#'   FUN = function(dados, posicao, arquivo) nrow(dados),
#'   tamanho_bloco = 50000
#' )
#' }
opendatasus_processar <- function(conjunto, FUN, recurso = NULL,
                                  ano = "last", formato = "CSV",
                                  destino = NULL, cache = TRUE,
                                  atualizar = FALSE, colunas = NULL,
                                  tamanho_bloco = 100000L,
                                  sistema = NULL, ...) {
  if (!is.function(FUN)) {
    stop("'FUN' must be a function", call. = FALSE)
  }
  if (!is.numeric(tamanho_bloco) || length(tamanho_bloco) != 1L ||
      is.na(tamanho_bloco) || tamanho_bloco != as.integer(tamanho_bloco) ||
      tamanho_bloco < 1L) {
    stop("'tamanho_bloco' must be a positive integer",
         call. = FALSE)
  }
  formato <- toupper(formato)
  if (!identical(formato, "CSV")) {
    stop("Chunk processing is currently available for CSV resources",
         call. = FALSE)
  }
  colunas <- .opendatasus_validate_columns(colunas)
  if (!is.null(sistema)) {
    datasus_dicionario(sistema)
  }
  resources <- opendatasus_recursos(
    conjunto,
    cache = cache,
    atualizar = atualizar
  )
  selected <- .opendatasus_select_resource(
    resources,
    recurso = recurso,
    ano = ano,
    formato = formato
  )
  files <- opendatasus_arquivos(
    conjunto,
    recurso = selected$id[[1L]],
    formato = formato,
    cache = cache,
    atualizar = atualizar
  )
  if (!nrow(files)) {
    stop("The selected resource has no downloadable files",
         call. = FALSE)
  }
  if (nrow(files) > 1L && !is.null(destino) && !dir.exists(destino)) {
    stop(
      "'destino' must be an existing directory for multipart resources",
      call. = FALSE
    )
  }

  outputs <- list()
  provenance <- list()
  rows <- 0
  chunks <- 0L
  for (file_index in seq_len(nrow(files))) {
    path <- .opendatasus_download_file(
      files[file_index, , drop = FALSE],
      destino = destino,
      cache = cache,
      atualizar = atualizar
    )
    provenance[[file_index]] <- datasus_proveniencia(path)
    delimiter <- .opendatasus_detect_delimiter(as.character(path))
    connection <- .opendatasus_open_file(as.character(path), "CSV")
    callback <- readr::SideEffectChunkCallback$new(
      function(data, position) {
        if (!is.null(colunas)) {
          missing <- setdiff(colunas, names(data))
          if (length(missing)) {
            stop(
              "Requested column(s) not found: ",
              paste(missing, collapse = ", "),
              call. = FALSE
            )
          }
          data <- data[colunas]
        }
        if (!is.null(sistema)) {
          data <- datasus_padronizar(data, sistema)
        }
        chunks <<- chunks + 1L
        rows <<- rows + nrow(data)
        outputs[chunks] <<- list(FUN(data, position, file_index))
        invisible(NULL)
      }
    )
    tryCatch(
      readr::read_delim_chunked(
        connection,
        callback = callback,
        delim = delimiter,
        chunk_size = as.integer(tamanho_bloco),
        show_col_types = FALSE,
        ...
      ),
      finally = {
        if (inherits(connection, "connection") && isOpen(connection)) {
          close(connection)
        }
      }
    )
  }
  result <- list(
    resultados = outputs,
    linhas = rows,
    blocos = chunks,
    arquivos = length(provenance),
    proveniencia = provenance
  )
  class(result) <- "datasus_processamento"
  result
}
