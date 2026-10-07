test_that("OpenDataSUS page metadata is parsed without evaluating scripts", {
  payload <- list(
    props = list(
      pageProps = list(
        numberOfPackages = 1L,
        packages = list(
          list(
            name = "arboviroses-dengue",
            title = "Sinan/Dengue",
            notes = "Notifications",
            formats = list("CSV", "JSON"),
            groups = list(list(name = "arboviroses"))
          )
        )
      )
    )
  )
  html <- paste0(
    "<html><body><script id='unrelated'>alert('ignored')</script>",
    "<script id='__NEXT_DATA__' type='application/json'>",
    jsonlite::toJSON(payload, auto_unbox = TRUE),
    "</script></body></html>"
  )

  result <- datasus:::.opendatasus_parse_page(charToRaw(html))

  expect_identical(result$numberOfPackages, 1L)
  expect_identical(
    result$packages[[1L]]$name,
    "arboviroses-dengue"
  )
  expect_error(
    datasus:::.opendatasus_parse_page(
      charToRaw("<html><body>No metadata</body></html>")
    ),
    "without embedded metadata"
  )
})

test_that("large-file downloads validate their separate timeout option", {
  previous <- options(datasus.download_timeout = 0)
  on.exit(options(previous), add = TRUE)
  expect_error(
    datasus:::.opendatasus_request_raw(
      "https://example.test/data.csv",
      destination = tempfile()
    ),
    "datasus.download_timeout"
  )
})

test_that("resource years and missing format labels are normalized", {
  expect_identical(
    datasus:::.opendatasus_resource_year(
      "2026- Banco vivo 20/07/2026 - CSV"
    ),
    2026L
  )
  expect_true(is.na(
    datasus:::.opendatasus_resource_year("Dicionario de dados")
  ))

  resource <- list(
    name = "2026 - Banco vivo - PARQUET",
    format = "",
    url = "https://example.test/data.parquet"
  )
  expect_identical(
    datasus:::.opendatasus_resource_format(resource),
    "PARQUET"
  )
})

test_that("resource selection is explicit and predictable", {
  resources <- data.frame(
    conjunto = rep("example", 5L),
    id = paste0("id-", 1:5),
    nome = c(
      "Dictionary", "Cases 2025", "Cases 2025",
      "Cases 2026", "Cases 2026"
    ),
    formato = c("PDF", "CSV", "JSON", "CSV", "JSON"),
    ano = c(NA, 2025L, 2025L, 2026L, 2026L),
    url = paste0("https://example.test/", 1:5),
    tamanho = NA_real_,
    criado = NA_character_,
    modificado = NA_character_,
    posicao = 0:4,
    stringsAsFactors = FALSE
  )

  latest <- datasus:::.opendatasus_select_resource(resources)
  expect_identical(latest$id, "id-4")

  json <- datasus:::.opendatasus_select_resource(
    resources,
    ano = 2025,
    formato = "json"
  )
  expect_identical(json$id, "id-3")

  dictionary <- datasus:::.opendatasus_select_resource(
    resources,
    recurso = "id-1",
    ano = NULL,
    formato = NULL
  )
  expect_identical(dictionary$formato, "PDF")

  expect_error(
    datasus:::.opendatasus_select_resource(
      resources,
      recurso = "Cases 2026",
      formato = NULL
    ),
    "ambiguous"
  )
  expect_error(
    datasus:::.opendatasus_select_resource(
      resources,
      ano = 1999,
      formato = "CSV"
    ),
    "No OpenDataSUS resource"
  )
})

test_that("OpenDataSUS CSV files are read with automatic delimiters", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    c("municipio;casos", "Campo Grande;10", "Dourados;5"),
    path,
    useBytes = TRUE
  )

  result <- datasus:::.opendatasus_read_file(
    path, "CSV", n_max = 1L
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("municipio", "casos"))
  expect_equal(nrow(result), 1L)
  expect_identical(result$casos, 10)
  selected <- datasus:::.opendatasus_read_file(
    path, "CSV", colunas = "casos", show_col_types = FALSE
  )
  expect_named(selected, "casos")
  expect_equal(nrow(selected), 2L)
  expect_error(
    datasus:::.opendatasus_read_file(
      path, "CSV", Inf, NULL, FALSE
    ),
    "must be named"
  )
  expect_error(
    datasus:::.opendatasus_read_file(path, "XML"),
    "not supported"
  )
})

test_that("empty files inside ZIP resources are reported explicitly", {
  directory <- tempfile()
  dir.create(directory)
  archive <- tempfile(fileext = ".zip")
  previous <- setwd(directory)
  on.exit({
    setwd(previous)
    unlink(directory, recursive = TRUE)
    unlink(archive)
  }, add = TRUE)
  file.create("empty.csv")
  status <- suppressWarnings(
    utils::zip(archive, "empty.csv", flags = "-q")
  )
  skip_if_not(identical(status, 0L), "ZIP utility is unavailable")

  expect_error(
    datasus:::.opendatasus_open_file(archive, "CSV"),
    "empty CSV file"
  )
})

test_that("downloadable links are expanded from resource descriptions", {
  expect_identical(
    datasus:::.opendatasus_extract_urls(
      "",
      paste(
        "[Lote 1](https://example.test/part-1.csv)",
        "[Lote 2](https://example.test/part-2.csv)"
      )
    ),
    c(
      "https://example.test/part-1.csv",
      "https://example.test/part-2.csv"
    )
  )

  resources <- data.frame(
    conjunto = "example",
    id = "resource-1",
    nome = "Dados MS",
    descricao = paste(
      "[Lote 1](https://example.test/part-1.csv)",
      "[Lote 2](https://example.test/part-2.csv)"
    ),
    formato = "CSV",
    ano = 2024L,
    url = "",
    tamanho = NA_real_,
    criado = NA_character_,
    modificado = "2025-01-01",
    posicao = 1L,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    opendatasus_recursos = function(...) resources,
    .package = "datasus"
  )

  files <- opendatasus_arquivos(
    "example",
    recurso = "resource-1",
    formato = "CSV"
  )

  expect_equal(nrow(files), 2L)
  expect_identical(files$parte, 1:2)
  expect_identical(files$recurso_id, rep("resource-1", 2))
  expect_identical(files$ano, rep(2024L, 2))
})

test_that("cache keys are stable and cache options are validated", {
  expect_identical(
    datasus:::.opendatasus_key("https://example.test/a?q=1"),
    datasus:::.opendatasus_key("https://example.test/a?q=1")
  )
  expect_false(identical(
    datasus:::.opendatasus_key("https://example.test/a?q=1"),
    datasus:::.opendatasus_key("https://example.test/a?q=2")
  ))

  old <- options(datasus.cache_ttl = -1)
  on.exit(options(old), add = TRUE)
  expect_error(
    datasus:::.opendatasus_cache_ttl(),
    "non-negative"
  )
})

test_that("provenance is available without changing the data shape", {
  data <- data.frame(cases = 1:2)
  provenance <- list(
    fonte = "https://example.test/data.csv",
    md5 = "example"
  )
  attr(data, "datasus_proveniencia") <- provenance

  expect_identical(datasus_proveniencia(data), provenance)
  expect_null(datasus_proveniencia(data.frame()))
  expect_equal(nrow(data), 2L)
})
