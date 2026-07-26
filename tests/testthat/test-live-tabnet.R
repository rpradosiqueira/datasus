test_that("live TABNET integration returns a table", {
  skip_if_not(
    identical(Sys.getenv("DATASUS_RUN_INTEGRATION"), "true"),
    "Set DATASUS_RUN_INTEGRATION=true to run live TABNET tests"
  )

  result <- sinasc(
    uf = "ms",
    periodo = "last",
    coluna = "Ano do nascimento"
  )
  legacy <- expect_warning(
    sinasc_nv_uf(
      uf = "ms",
      periodo = "last",
      coluna = "Ano do nascimento"
    ),
    "deprecated"
  )
  mortality <- sim(abrangencia = "uf", periodo = "last")

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 1L)
  expect_gt(ncol(result), 1L)
  expect_identical(result[[1L]][[1L]], "TOTAL")
  expect_equal(legacy, result, ignore_attr = TRUE)
  expect_s3_class(mortality, "data.frame")
  expect_gt(nrow(mortality), 1L)
  expect_true(is.list(datasus_proveniencia(mortality)))
})

test_that("live health-services integrations return tables", {
  skip_if_not(
    identical(Sys.getenv("DATASUS_RUN_INTEGRATION"), "true"),
    "Set DATASUS_RUN_INTEGRATION=true to run live TABNET tests"
  )

  results <- list(
    sih = sih_producao(uf = "ms"),
    sia = sia_producao(uf = "ms"),
    cnes = cnes(uf = "ms")
  )

  for (result in results) {
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 1L)
    expect_gt(ncol(result), 1L)
    expect_identical(result[[1L]][[1L]], "TOTAL")
  }
})

test_that("live population, morbidity and SINAN integrations return tables", {
  skip_if_not(
    identical(Sys.getenv("DATASUS_RUN_INTEGRATION"), "true"),
    "Set DATASUS_RUN_INTEGRATION=true to run live TABNET tests"
  )

  results <- list(
    population = populacao_residente(uf = "ms"),
    morbidity = sih_morbidade(uf = "ms"),
    sinan = sinan("dengue", uf = "ms")
  )

  for (result in results) {
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 1L)
    expect_gt(ncol(result), 1L)
    expect_identical(result[[1L]][[1L]], "TOTAL")
  }
})

test_that("live PNI, SISCAN, SISVAN and financing integrations return tables", {
  skip_if_not(
    identical(Sys.getenv("DATASUS_RUN_INTEGRATION"), "true"),
    "Set DATASUS_RUN_INTEGRATION=true to run live TABNET tests"
  )

  results <- list(
    pni = pni_imunizacoes(uf = "ms"),
    siscan = siscan(uf = "ms"),
    sisvan = sisvan(uf = "ms"),
    financing = financiamento_sus(uf = "ms")
  )

  for (result in results) {
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 1L)
    expect_gt(ncol(result), 1L)
    expect_identical(result[[1L]][[1L]], "TOTAL")
  }
})

test_that("live OpenDataSUS catalog exposes current surveillance resources", {
  skip_if_not(
    identical(Sys.getenv("DATASUS_RUN_INTEGRATION"), "true"),
    "Set DATASUS_RUN_INTEGRATION=true to run live OpenDataSUS tests"
  )

  catalog <- opendatasus_catalogo("dengue", atualizar = TRUE)
  expect_true("arboviroses-dengue" %in% catalog$conjunto)

  expected <- list(
    srag = c("srag-2019-a-2026", "CSV"),
    dengue = c("arboviroses-dengue", "CSV"),
    mpox = c("mpox", "CSV"),
    esavi = c("esavi", "CSV"),
    occupancy = c(
      "registro-de-ocupacao-hospitalar-covid-19", "CSV"
    )
  )
  for (item in expected) {
    resources <- opendatasus_recursos(item[[1L]], atualizar = TRUE)
    expect_gt(nrow(resources), 1L)
    expect_true(item[[2L]] %in% resources$formato)
    expect_true(any(!is.na(resources$ano)))
  }

  syndrome <- opendatasus_recursos(
    "notificacoes-de-sindrome-gripal-leve-2024",
    atualizar = TRUE
  )
  expect_true(any(grepl("^Dados MS", syndrome$nome)))
  historical <- opendatasus_recursos(
    "notificacoes-de-sindrome-gripal-leve-2020",
    atualizar = TRUE
  )
  historical_ms <- historical$id[
    historical$formato == "CSV" &
      grepl("^Dados MS", historical$nome)
  ][[1L]]
  historical_files <- opendatasus_arquivos(
    "notificacoes-de-sindrome-gripal-leve-2020",
    recurso = historical_ms,
    formato = "CSV",
    atualizar = TRUE
  )
  expect_gt(nrow(historical_files), 1L)
  expect_true(all(grepl("^https://", historical_files$url)))

  pni <- opendatasus_catalogo(
    "doses aplicadas PNI",
    limite = 50,
    atualizar = TRUE
  )
  expect_true(any(grepl(
    "doses-aplicadas-pelo-programa-de-nacional-de-imunizacoes-pni",
    pni$conjunto
  )))
})

test_that("live raw DATASUS microdata can be discovered and decoded", {
  skip_if_not(
    identical(Sys.getenv("DATASUS_RUN_INTEGRATION"), "true"),
    "Set DATASUS_RUN_INTEGRATION=true to run live DBC tests"
  )

  files <- list(
    sim = microdados_arquivos("sim", ano = 2023, uf = "RR"),
    sinasc = microdados_arquivos("sinasc", ano = 2023, uf = "RR"),
    sih = microdados_arquivos(
      "sih", ano = 2024, mes = 1, uf = "AC"
    )
  )
  for (result in files) {
    expect_equal(nrow(result), 1L)
    expect_true(result$existe)
    expect_match(result$arquivo, "[.]dbc$")
  }

  admissions <- sih_microdados(
    ano = 2024,
    mes = 1,
    uf = "AC",
    colunas = c("MUNIC_RES", "DT_INTER", "DIAG_PRINC", "VAL_TOT"),
    n_max = 5,
    cache = FALSE,
    normalizar = TRUE
  )
  expect_equal(nrow(admissions), 5L)
  expect_named(
    admissions,
    c(
      "codigo_municipio_residencia",
      "data_internacao",
      "diagnostico_principal_cid10",
      "valor_total"
    )
  )
  expect_length(datasus_proveniencia(admissions)$arquivos, 1L)
})
