resource_table <- function(conjunto, names, formats, ids = NULL) {
  if (is.null(ids)) {
    ids <- paste0("id-", seq_along(names))
  }
  data.frame(
    conjunto = conjunto,
    id = ids,
    nome = names,
    formato = formats,
    ano = vapply(
      names,
      datasus:::.opendatasus_resource_year,
      integer(1)
    ),
    url = paste0("https://example.test/", ids),
    tamanho = NA_real_,
    criado = NA_character_,
    modificado = NA_character_,
    posicao = seq_along(names) - 1L,
    stringsAsFactors = FALSE
  )
}

test_that("contemporary dataset years and formats are validated", {
  expect_identical(datasus:::.contemporary_year("last"), "last")
  expect_identical(datasus:::.contemporary_year(2024), 2024L)
  expect_error(datasus:::.contemporary_year(2019), "from 2020")
  expect_error(datasus:::.contemporary_year(c(2023, 2024)), "one four-digit")

  expect_identical(
    datasus:::.contemporary_format("csv", c("CSV", "JSON")),
    "CSV"
  )
  expect_error(
    datasus:::.contemporary_format("XML", c("CSV", "JSON")),
    "must be one of"
  )
})

test_that("latest annual dataset resolution ignores unrelated results", {
  catalog <- data.frame(
    conjunto = c(
      "notificacoes-de-sindrome-gripal-leve-2023",
      "notificacoes-de-sindrome-gripal-api-opensearch",
      "notificacoes-de-sindrome-gripal-leve-2024"
    ),
    titulo = c(
      "Notificacoes 2023", "API", "Notificacoes 2024"
    ),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    opendatasus_catalogo = function(...) catalog,
    .package = "datasus"
  )

  result <- datasus:::.contemporary_latest_dataset(
    "sindrome gripal",
    "^notificacoes-de-sindrome-gripal-leve-[0-9]{4}$",
    cache = TRUE,
    atualizar = FALSE
  )
  expect_identical(
    result,
    "notificacoes-de-sindrome-gripal-leve-2024"
  )
})

test_that("PNI months support numbers, names and resource labels", {
  expect_identical(datasus:::.pni_month("last"), "last")
  expect_identical(datasus:::.pni_month("março"), 3L)
  expect_identical(datasus:::.pni_month(12), 12L)
  expect_identical(
    datasus:::.pni_resource_month("Vacinação - Setembro 2025"),
    9L
  )
  expect_true(is.na(
    datasus:::.pni_resource_month("Dicionario de dados")
  ))
  expect_error(datasus:::.pni_month(13), "1 to 12")
})

test_that("PNI annual slugs preserve the exceptional 2022 identifier", {
  expect_identical(
    datasus:::.pni_dataset(2022L, TRUE, FALSE),
    paste0(
      "dataset-doses-aplicadas-pelo-programa-de-nacional-",
      "de-imunizacoes-pni_2022"
    )
  )
  expect_identical(
    datasus:::.pni_dataset(2025L, TRUE, FALSE),
    paste0(
      "doses-aplicadas-pelo-programa-de-nacional-",
      "de-imunizacoes-pni-2025"
    )
  )
})

test_that("resource selection rejects missing and ambiguous partitions", {
  resources <- resource_table(
    "example",
    c("Dados MS - 20/12", "Dados SP - 20/12", "Dados SP - 20/12"),
    c("CSV", "CSV", "CSV")
  )
  expect_identical(
    datasus:::.contemporary_resource(
      resources, "CSV",
      keep = c(TRUE, FALSE, FALSE),
      description = "MS"
    )$id,
    "id-1"
  )
  expect_error(
    datasus:::.contemporary_resource(
      resources, "CSV",
      keep = c(FALSE, TRUE, TRUE),
      description = "SP"
    ),
    "ambiguous"
  )
  expect_error(
    datasus:::.contemporary_resource(
      resources, "JSON",
      description = "JSON"
    ),
    "currently published"
  )
})

test_that("ESAVI and occupancy wrappers delegate safe selections", {
  calls <- list()
  testthat::local_mocked_bindings(
    opendatasus_ler = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      data.frame(value = 1)
    },
    .package = "datasus"
  )

  esavi_result <- esavi(formato = "json", n_max = 10)
  occupancy_result <- ocupacao_hospitalar(
    ano = 2022, formato = "csv", n_max = 5
  )

  expect_s3_class(esavi_result, "data.frame")
  expect_s3_class(occupancy_result, "data.frame")
  expect_identical(calls[[1L]]$conjunto, "esavi")
  expect_null(calls[[1L]]$ano)
  expect_identical(calls[[1L]]$formato, "JSON")
  expect_identical(
    calls[[2L]]$conjunto,
    "registro-de-ocupacao-hospitalar-covid-19"
  )
  expect_identical(calls[[2L]]$ano, 2022L)
})

test_that("syndrome-gripal selects one state resource", {
  resources <- resource_table(
    "notificacoes-de-sindrome-gripal-leve-2024",
    c(
      "Dicionário de Dados",
      "Dados MS - 20/12",
      "Dados SP - 20/12"
    ),
    c("PDF", "CSV", "CSV")
  )
  calls <- list()
  testthat::local_mocked_bindings(
    opendatasus_recursos = function(...) resources,
    opendatasus_arquivos = function(...) {
      resource <- resources[resources$id == "id-2", , drop = FALSE]
      data.frame(
        conjunto = resource$conjunto,
        recurso_id = resource$id,
        recurso = resource$nome,
        formato = resource$formato,
        ano = 2024L,
        parte = 1L,
        url = resource$url,
        modificado = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    .contemporary_read_files = function(...) {
      calls <<- list(...)
      data.frame(casos = 1)
    },
    .package = "datasus"
  )

  result <- esus_sindrome_gripal(
    uf = "Mato Grosso do Sul",
    ano = 2024,
    n_max = 100
  )

  expect_s3_class(result, "data.frame")
  expect_identical(calls$files$recurso_id, "id-2")
  expect_identical(
    calls$files$conjunto,
    "notificacoes-de-sindrome-gripal-leve-2024"
  )
  expect_identical(calls$n_max, 100)
})

test_that("PNI dose wrapper selects requested and latest month", {
  resources <- resource_table(
    "doses-aplicadas-pelo-programa-de-nacional-de-imunizacoes-pni-2025",
    c(
      "Vacinação - Janeiro 2025",
      "Vacinação - Fevereiro 2025",
      "Vacinação - Fevereiro 2025"
    ),
    c("CSV", "CSV", "JSON")
  )
  calls <- list()
  testthat::local_mocked_bindings(
    opendatasus_recursos = function(...) resources,
    opendatasus_ler = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      data.frame(dose = 1)
    },
    .package = "datasus"
  )

  january <- pni_doses(ano = 2025, mes = "janeiro")
  latest_json <- pni_doses(
    ano = 2025, mes = "last", formato = "JSON"
  )

  expect_s3_class(january, "data.frame")
  expect_s3_class(latest_json, "data.frame")
  expect_identical(calls[[1L]]$recurso, "id-1")
  expect_identical(calls[[1L]]$formato, "CSV")
  expect_identical(calls[[2L]]$recurso, "id-3")
  expect_identical(calls[[2L]]$formato, "JSON")
})
