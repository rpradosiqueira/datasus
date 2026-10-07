test_that("raw microdata catalog is local, complete and filterable", {
  catalog <- microdados_catalogo()

  expect_s3_class(catalog, "data.frame")
  expect_named(
    catalog,
    c("sistema", "fonte", "tipo", "descricao", "escopo", "frequencia")
  )
  expect_equal(nrow(catalog), 11L)
  expect_equal(sum(catalog$sistema == "sim"), 5L)
  expect_equal(sum(catalog$sistema == "sinasc"), 2L)
  expect_equal(sum(catalog$sistema == "sih"), 4L)
  expect_equal(nrow(microdados_catalogo("sih")), 4L)
  expect_error(microdados_catalogo("unknown"), "Unknown microdata")
})

test_that("microdata selections are validated before network access", {
  expect_identical(
    datasus:::.microdata_resolve("SIM")$tipo,
    "DO"
  )
  expect_identical(
    datasus:::.microdata_resolve("sih", "sp")$tipo,
    "SP"
  )
  expect_error(
    datasus:::.microdata_resolve("sih", "unknown"),
    "Unknown file type"
  )
  expect_identical(
    datasus:::.microdata_validate_year(c(2023, 2024)),
    c(2023L, 2024L)
  )
  expect_error(
    datasus:::.microdata_validate_year(19),
    "four-digit years"
  )
  expect_identical(
    datasus:::.microdata_validate_month(c(1, 12), TRUE),
    c(1L, 12L)
  )
  expect_error(
    datasus:::.microdata_validate_month(NULL, TRUE),
    "required"
  )
  expect_identical(
    datasus:::.microdata_validate_ufs(c("ac", "MS"), TRUE),
    c("AC", "MS")
  )
  expect_identical(
    datasus:::.microdata_validate_ufs(c(12, 50), TRUE),
    c("AC", "MS")
  )
})

test_that("curated dictionaries cover common event and location fields", {
  sim <- datasus_dicionario("sim")
  sinasc <- datasus_dicionario("sinasc", "DNEX")
  sih <- datasus_dicionario("sih")

  expect_true(all(
    c("data_obito", "codigo_municipio_residencia", "causa_basica_cid10") %in%
      sim$campo_padronizado
  ))
  expect_true("peso_gramas" %in% sinasc$campo_padronizado)
  expect_true(all(
    c("data_internacao", "data_saida", "valor_total") %in%
      sih$campo_padronizado
  ))
  expect_equal(nrow(datasus_dicionario("sih", "SP")), 0L)
})

test_that("field standardization is conservative and preserves provenance", {
  data <- data.frame(
    MUNIC_RES = 120040,
    DT_INTER = c("20240110", ""),
    DT_SAIDA = c("20240112", "20240201"),
    DIAG_PRINC = c("J18", "O80"),
    VAL_TOT = c("123.45", "200"),
    CampoOriginal = c("A", "B"),
    check.names = FALSE
  )
  provenance <- list(fonte = "fixture")
  attr(data, "datasus_proveniencia") <- provenance

  result <- datasus_padronizar(data, "sih")

  expect_named(
    result,
    c(
      "codigo_municipio_residencia",
      "data_internacao",
      "data_saida",
      "diagnostico_principal_cid10",
      "valor_total",
      "CampoOriginal"
    )
  )
  expect_s3_class(result$data_internacao, "Date")
  expect_identical(
    result$data_internacao,
    as.Date(c("2024-01-10", NA))
  )
  expect_identical(
    result$codigo_municipio_residencia,
    c("120040", "120040")
  )
  expect_identical(result$valor_total, c(123.45, 200))
  expect_identical(datasus_proveniencia(result), provenance)
  expect_gt(nrow(attr(result, "datasus_dicionario")), 0L)
})

test_that("standardization protects existing target fields", {
  data <- data.frame(
    DT_INTER = "20240110",
    data_internacao = "existing",
    check.names = FALSE
  )
  expect_error(
    datasus_padronizar(data, "sih"),
    "target field"
  )
  expect_error(
    datasus_padronizar(list(), "sih"),
    "must be a data frame"
  )
})

test_that("contemporary dictionaries type analysis-critical fields", {
  pni <- datasus_dicionario("pni_doses")
  esavi_dictionary <- datasus_dicionario("esavi")
  syndrome <- datasus_dicionario("sindrome_gripal")
  occupancy <- datasus_dicionario("ocupacao_hospitalar")

  expect_equal(nrow(esavi_dictionary), 73L)
  expect_equal(nrow(syndrome), 63L)
  expect_equal(nrow(pni), 61L)
  expect_equal(nrow(occupancy), 25L)
  expect_true(all(
    c("data_vacinacao", "cnes", "codigo_municipio_residencia") %in%
      pni$campo_padronizado
  ))
  expect_true(all(
    c("id_notificacao", "data_notificacao", "causalidade") %in%
      esavi_dictionary$campo_padronizado
  ))
  expect_true(all(
    c("datetime", "logical", "numeric") %in% occupancy$classe
  ))
  expect_true(all(
    c(
      "codigo_estrategia_vacinacao",
      "natureza_estabelecimento",
      "estado_estabelecimento",
      "excluido_rnds_em"
    ) %in% pni$campo_padronizado
  ))
  expect_true(all(
    c(
      "codigo_tipo_teste_4",
      "classificacao_final",
      "profissional_saude"
    ) %in% syndrome$campo_padronizado
  ))
  expect_true(all(
    c(
      "relacao_medicamento_evento",
      "duracao_evento_minutos",
      "data_admissao"
    ) %in% esavi_dictionary$campo_padronizado
  ))
  for (dictionary in list(
      pni, esavi_dictionary, syndrome, occupancy
  )) {
    expect_false(anyDuplicated(tolower(dictionary$campo)) > 0L)
    expect_false(anyDuplicated(
      tolower(dictionary$campo_padronizado)
    ) > 0L)
  }
  expect_error(datasus_dicionario("unknown"), "Unknown dictionary")
  expect_error(
    datasus_dicionario("pni_doses", "RD"),
    "not used"
  )
})

test_that("contemporary standardization parses dates and logical values", {
  raw <- data.frame(
    `_id` = c("a", "b"),
    dataNotificacao = c(
      "2022-01-17T03:00:00.000Z",
      "2022-02-01T12:30:00.000Z"
    ),
    cnes = c(1234567, 7654321),
    ocupacaoSuspeitoCli = c("2", "3"),
    excluido = c("False", "True"),
    check.names = FALSE
  )

  result <- datasus_padronizar(raw, "ocupacao_hospitalar")

  expect_identical(result$id_registro, c("a", "b"))
  expect_s3_class(result$data_notificacao, "POSIXct")
  expect_false(anyNA(result$data_notificacao))
  expect_identical(result$cnes, c("1234567", "7654321"))
  expect_identical(result$ocupacao_suspeito_clinico, c(2, 3))
  expect_identical(result$excluido, c(FALSE, TRUE))
})

test_that("expanded contemporary schemas convert documented field types", {
  esavi_data <- datasus_padronizar(
    data.frame(
      st_comunidade_tradicional = c("Sim", "Nao"),
      nu_mes_gestante = c("4", "7"),
      dt_admissao_atendimento = c("10/01/2026", "2026-01-11")
    ),
    "esavi"
  )
  expect_identical(esavi_data$comunidade_tradicional, c(TRUE, FALSE))
  expect_identical(esavi_data$mes_gestacao, c(4, 7))
  expect_s3_class(esavi_data$data_admissao, "Date")

  syndrome_data <- datasus_padronizar(
    data.frame(
      profissionalSaude = c("Sim", "Nao"),
      totalTestesRealizados = c("2", "4"),
      codigoTipoTeste4 = c("1", "3")
    ),
    "sindrome_gripal"
  )
  expect_identical(syndrome_data$profissional_saude, c(TRUE, FALSE))
  expect_identical(syndrome_data$total_testes_realizados, c(2, 4))
  expect_identical(syndrome_data$codigo_tipo_teste_4, c("1", "3"))

  pni_data <- datasus_padronizar(
    data.frame(
      nu_cep_paciente = c(79000000, 69300000),
      co_estrategia_vacinacao = c(1, 2),
      no_uf_estabelecimento = c("Mato Grosso do Sul", "Roraima")
    ),
    "pni_doses"
  )
  expect_identical(pni_data$cep_residencia, c("79000000", "69300000"))
  expect_identical(
    pni_data$codigo_estrategia_vacinacao,
    c("1", "2")
  )
  expect_identical(
    pni_data$estado_estabelecimento,
    c("Mato Grosso do Sul", "Roraima")
  )
})

test_that("curated schema validation reports drift explicitly", {
  raw <- data.frame(
    dt_vacina = "2025-01-10",
    co_municipio_paciente = "5002704",
    campo_novo = "value"
  )
  raw_validation <- datasus_validar_esquema(
    raw,
    "pni_doses",
    campos = c("dt_vacina", "co_municipio_paciente")
  )
  expect_true(all(raw_validation$presente))
  expect_true(all(raw_validation$status == "presente_bruto"))
  expect_identical(
    attr(raw_validation, "campos_extras"),
    "campo_novo"
  )

  standardized <- datasus_padronizar(raw, "pni_doses")
  validation <- datasus_validar_esquema(
    standardized,
    "pni_doses",
    campos = c("data_vacinacao", "codigo_municipio_residencia"),
    estrito = TRUE
  )
  expect_true(all(validation$status == "padronizado"))
  expect_error(
    datasus_validar_esquema(
      data.frame(other = 1),
      "pni_doses",
      campos = "dt_vacina",
      estrito = TRUE
    ),
    "validation failed"
  )
})

test_that("the native reader handles local DBF files and row limits", {
  skip_if_not_installed("foreign")
  path <- tempfile(fileext = ".dbf")
  on.exit(unlink(path), add = TRUE)
  foreign::write.dbf(
    data.frame(
      MUNIC_RES = c("120040", "120060", "120040"),
      VAL_TOT = c(100, 200, 300),
      DT_INTER = c("20240110", "20240111", "20240112"),
      DIAG_PRINC = c("J18", "O80", "A09")
    ),
    path
  )

  result <- datasus:::.microdata_read_paths(
    path,
    sistema = "sih",
    tipo = "RD",
    colunas = c(
      "MUNIC_RES", "DT_INTER", "DIAG_PRINC", "VAL_TOT"
    ),
    n_max = 2L,
    normalizar = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_named(
    result,
    c(
      "codigo_municipio_residencia",
      "data_internacao",
      "diagnostico_principal_cid10",
      "valor_total"
    )
  )
})

test_that("multiple microdata tables are combined by name", {
  first <- data.frame(a = 1L, b = "x")
  second <- data.frame(b = "y", c = 2L)

  result <- datasus:::.microdata_bind_rows(list(first, second))

  expect_named(result, c("a", "b", "c"))
  expect_equal(nrow(result), 2L)
  expect_true(is.na(result$a[[2L]]))
  expect_true(is.na(result$c[[1L]]))
})
