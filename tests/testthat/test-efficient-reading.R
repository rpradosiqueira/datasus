test_that("multipart reading respects a global row limit and schema", {
  first <- tempfile(fileext = ".csv")
  second <- tempfile(fileext = ".csv")
  on.exit(unlink(c(first, second)), add = TRUE)
  writeLines(
    c(
      "dataNotificacao,municipioIBGE,extra",
      "10/01/2024,5002704,a",
      "11/01/2024,5003702,b"
    ),
    first,
    useBytes = TRUE
  )
  writeLines(
    c(
      "dataNotificacao,municipioIBGE,extra",
      "12/01/2024,5002704,c",
      "13/01/2024,5003702,d"
    ),
    second,
    useBytes = TRUE
  )
  files <- data.frame(
    conjunto = "sg",
    recurso_id = "resource",
    recurso = "Dados MS",
    formato = "CSV",
    ano = 2024L,
    parte = 1:2,
    url = c("https://example.test/1", "https://example.test/2"),
    modificado = NA_character_,
    stringsAsFactors = FALSE
  )
  paths <- c(first, second)
  testthat::local_mocked_bindings(
    .opendatasus_download_file = function(file, ...) {
      result <- paths[[file$parte[[1L]]]]
      attr(result, "datasus_proveniencia") <- list(
        fonte = file$url[[1L]]
      )
      class(result) <- c("datasus_arquivo", "character")
      result
    },
    .package = "datasus"
  )

  result <- datasus:::.contemporary_read_files(
    files,
    destino = NULL,
    cache = TRUE,
    atualizar = FALSE,
    n_max = 3,
    colunas = c("dataNotificacao", "municipioIBGE"),
    sistema = "sindrome_gripal",
    normalizar = TRUE
  )

  expect_equal(nrow(result), 3L)
  expect_named(
    result,
    c("data_notificacao", "codigo_municipio_residencia")
  )
  expect_s3_class(result$data_notificacao, "Date")
  expect_identical(
    result$codigo_municipio_residencia,
    c("5002704", "5003702", "5002704")
  )
  expect_identical(
    datasus_proveniencia(result)$arquivos_lidos,
    2L
  )
})

test_that("chunk processing bounds memory and returns callback summaries", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(
    c("grupo,valor", "A,1", "A,2", "B,3", "B,4", "C,5"),
    path,
    useBytes = TRUE
  )
  resources <- data.frame(
    conjunto = "example",
    id = "resource-1",
    nome = "Cases 2025",
    descricao = "",
    formato = "CSV",
    ano = 2025L,
    url = "https://example.test/data.csv",
    tamanho = NA_real_,
    criado = NA_character_,
    modificado = NA_character_,
    posicao = 1L,
    stringsAsFactors = FALSE
  )
  files <- data.frame(
    conjunto = "example",
    recurso_id = "resource-1",
    recurso = "Cases 2025",
    formato = "CSV",
    ano = 2025L,
    parte = 1L,
    url = "https://example.test/data.csv",
    modificado = NA_character_,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    opendatasus_recursos = function(...) resources,
    opendatasus_arquivos = function(...) files,
    .opendatasus_download_file = function(...) {
      result <- path
      attr(result, "datasus_proveniencia") <- list(fonte = files$url)
      class(result) <- c("datasus_arquivo", "character")
      result
    },
    .package = "datasus"
  )

  result <- opendatasus_processar(
    "example",
    recurso = "resource-1",
    ano = NULL,
    FUN = function(dados, posicao, arquivo) {
      c(
        soma = sum(dados$valor),
        posicao = posicao,
        arquivo = arquivo
      )
    },
    colunas = c("grupo", "valor"),
    tamanho_bloco = 2L
  )

  expect_s3_class(result, "datasus_processamento")
  expect_identical(result$linhas, 5)
  expect_identical(result$blocos, 3L)
  expect_identical(result$arquivos, 1L)
  expect_equal(
    vapply(result$resultados, `[[`, numeric(1), "soma"),
    c(3, 7, 5)
  )
})
