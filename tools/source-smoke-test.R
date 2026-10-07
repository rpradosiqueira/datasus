options(
  datasus.timeout = 20,
  datasus.download_timeout = 30,
  datasus.max_tries = 1,
  datasus.cache_ttl = 0,
  datasus.cache_dir = file.path(tempdir(), "datasus-source-smoke-cache")
)

suppressPackageStartupMessages(library(datasus))

report_path <- "source-smoke-report.json"
checked_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

fingerprint <- function(values) {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  values <- sort(unique(enc2utf8(as.character(values))))
  writeLines(values, path, useBytes = TRUE)
  unname(tools::md5sum(path))
}

check <- function(name, code) {
  started <- Sys.time()
  tryCatch(
    {
      value <- force(code)
      list(
        source = name,
        status = "ok",
        elapsed_seconds = unname(round(
          as.numeric(difftime(Sys.time(), started, units = "secs")),
          3
        )),
        details = value
      )
    },
    error = function(error) {
      list(
        source = name,
        status = "error",
        elapsed_seconds = unname(round(
          as.numeric(difftime(Sys.time(), started, units = "secs")),
          3
        )),
        message = conditionMessage(error)
      )
    }
  )
}

tabnet <- check("DATASUS TABNET", {
  local_catalog <- datasus_catalogo("sim")
  if (!"obitos" %in% local_catalog$conjunto) {
    stop("The local SIM catalog no longer contains the 'obitos' dataset")
  }

  current <- datasus_opcoes(
    "sim",
    "obitos",
    abrangencia = "uf"
  )
  required <- c("linha", "coluna", "conteudo", "periodo", "filtros")
  missing <- setdiff(required, names(current))
  if (length(missing)) {
    stop("TABNET options are missing components: ", paste(missing, collapse = ", "))
  }
  counts <- vapply(current[required[1:4]], nrow, integer(1))
  if (any(counts < 1L)) {
    stop("TABNET returned an empty core option table")
  }

  schema <- c(
    paste0("component:", names(current)),
    paste0("filter:", sort(names(current$filtros)))
  )
  list(
    endpoint = "https://tabnet.datasus.gov.br/",
    system = "sim",
    dataset = "obitos",
    scope = "uf",
    option_counts = as.list(counts),
    filter_count = length(current$filtros),
    schema_fingerprint = fingerprint(schema)
  )
})

opendatasus <- check("OpenDataSUS", {
  current <- opendatasus_catalogo(
    "dengue",
    limite = 10,
    cache = FALSE,
    atualizar = TRUE
  )
  required_catalog <- c(
    "conjunto", "titulo", "descricao", "formatos", "grupos"
  )
  missing <- setdiff(required_catalog, names(current))
  if (length(missing)) {
    stop("OpenDataSUS catalog is missing columns: ", paste(missing, collapse = ", "))
  }
  if (!"arboviroses-dengue" %in% current$conjunto) {
    stop("The OpenDataSUS dengue search did not return 'arboviroses-dengue'")
  }

  resources <- opendatasus_recursos(
    "arboviroses-dengue",
    cache = FALSE,
    atualizar = TRUE
  )
  required_resources <- c(
    "conjunto", "id", "nome", "descricao", "formato", "ano", "url",
    "tamanho", "criado", "modificado", "posicao"
  )
  missing <- setdiff(required_resources, names(resources))
  if (length(missing)) {
    stop("OpenDataSUS resources are missing columns: ", paste(missing, collapse = ", "))
  }
  if (!nrow(resources)) {
    stop("OpenDataSUS returned no resources for 'arboviroses-dengue'")
  }
  downloadable <- !is.na(resources$url) & grepl("^https://", resources$url)
  if (!any(downloadable)) {
    stop("OpenDataSUS returned no HTTPS resource URL for 'arboviroses-dengue'")
  }

  schema <- c(
    paste0("catalog:", names(current)),
    paste0("resource:", names(resources)),
    paste0("format:", sort(unique(resources$formato)))
  )
  list(
    endpoint = "https://dadosabertos.saude.gov.br/",
    query = "dengue",
    dataset = "arboviroses-dengue",
    catalog_rows = nrow(current),
    resource_rows = nrow(resources),
    https_resources = sum(downloadable),
    formats = sort(unique(resources$formato)),
    metadata_modified = datasus_proveniencia(resources)$metadados_modificados,
    schema_fingerprint = fingerprint(schema)
  )
})

report <- list(
  checked_at_utc = checked_at,
  package = "datasus",
  package_version = as.character(utils::packageVersion("datasus")),
  checks = list(tabnet, opendatasus)
)

json <- jsonlite::toJSON(
  report,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null",
  na = "null"
)
writeLines(json, report_path, useBytes = TRUE)

summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
summary_lines <- c(
  "## datasus source smoke test",
  "",
  paste0("Checked at: `", checked_at, "`"),
  "",
  "| Source | Status | Elapsed (s) | Fingerprint / message |",
  "|---|---:|---:|---|"
)
for (item in report$checks) {
  final <- if (identical(item$status, "ok")) {
    item$details$schema_fingerprint
  } else {
    item$message
  }
  summary_lines <- c(
    summary_lines,
    paste0(
      "| ", item$source, " | ", item$status, " | ",
      item$elapsed_seconds, " | ", final, " |"
    )
  )
}
cat(paste(summary_lines, collapse = "\n"), "\n")
if (nzchar(summary_path)) {
  cat(paste(summary_lines, collapse = "\n"), "\n", file = summary_path)
}

failed <- vapply(report$checks, function(item) {
  !identical(item$status, "ok")
}, logical(1))
if (any(failed)) {
  stop(
    "Source smoke test failed: ",
    paste(vapply(report$checks[failed], `[[`, character(1), "source"),
          collapse = ", "),
    call. = FALSE
  )
}
