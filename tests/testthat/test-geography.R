test_that("offline IBGE reference has complete current territorial levels", {
  municipalities <- datasus_territorios("municipio")
  states <- datasus_territorios("uf")
  regions <- datasus_territorios("regiao")

  expect_s3_class(municipalities, "data.frame")
  expect_equal(nrow(municipalities), 5571L)
  expect_equal(nrow(states), 27L)
  expect_equal(nrow(regions), 5L)
  expect_equal(anyDuplicated(municipalities$codigo_municipio), 0L)
  expect_true("5101837" %in% municipalities$codigo_municipio)
  expect_identical(
    municipalities$municipio[
      municipalities$codigo_municipio == "5101837"
    ],
    "Boa Esperança do Norte"
  )
  expect_equal(nrow(datasus_territorios("municipio", "MS")), 79L)
  expect_equal(nrow(datasus_territorios("municipio", "51")), 142L)
  expect_equal(sum(regions$municipios), 5571L)
  expect_equal(sum(regions$ufs), 27L)

  provenance <- datasus_proveniencia(municipalities)
  expect_identical(provenance$fonte, "IBGE API de Localidades")
  expect_identical(provenance$nivel, "municipio")
  expect_s3_class(provenance$atualizado_em, "Date")
})

test_that("IBGE and DATASUS code formats are normalized vectorially", {
  expect_identical(
    normalizar_codigo_ibge(c("500270", "5003702")),
    c("5002704", "5003702")
  )
  expect_identical(
    normalizar_codigo_ibge(c(500270, 5003702), formato = "datasus"),
    c("500270", "500370")
  )
  expect_identical(
    normalizar_codigo_ibge(
      c("MS", "Mato Grosso do Sul", "50"),
      nivel = "uf",
      formato = "ibge"
    ),
    rep("50", 3L)
  )
  expect_identical(
    normalizar_codigo_ibge(
      c("5", "CO", "Centro Oeste"),
      nivel = "regiao",
      formato = "nome"
    ),
    rep("Centro-Oeste", 3L)
  )
  expect_identical(
    normalizar_codigo_ibge(
      c("MS", "XX"),
      nivel = "uf",
      formato = "sigla",
      desconhecido = "na"
    ),
    c("MS", NA_character_)
  )
  expect_identical(
    normalizar_codigo_ibge(
      c("500270", "999999"),
      desconhecido = "manter"
    ),
    c("5002704", "999999")
  )
  expect_error(
    normalizar_codigo_ibge("999999"),
    "Unknown or invalid"
  )
  expect_identical(
    validar_codigo_ibge(c("500270", "5003702", "999999", NA)),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("codes are extracted safely from TABNET labels", {
  expect_identical(
    extrair_codigo_ibge(
      c("500270 Campo Grande", "Município 5003702 - Dourados")
    ),
    c("5002704", "5003702")
  )
  expect_identical(
    extrair_codigo_ibge(
      c("UF 50 Mato Grosso do Sul", "sem código", NA),
      nivel = "uf",
      formato = "sigla",
      desconhecido = "na"
    ),
    c("MS", NA_character_, NA_character_)
  )
  expect_error(
    extrair_codigo_ibge("TOTAL"),
    "Unknown or invalid"
  )
})

test_that("territorial hierarchy can be added without losing observations", {
  data <- data.frame(
    codmun = c("500270", "5003702"),
    casos = c(10, 5)
  )
  result <- adicionar_territorio(data, "codmun")

  expect_identical(result$codmun, data$codmun)
  expect_identical(
    result$codigo_municipio,
    c("5002704", "5003702")
  )
  expect_identical(result$municipio, c("Campo Grande", "Dourados"))
  expect_identical(result$uf, c("MS", "MS"))
  expect_identical(result$regiao, c("Centro-Oeste", "Centro-Oeste"))
  expect_true(is.list(datasus_proveniencia(result)$territorios))

  unknown <- adicionar_territorio(
    data.frame(codmun = c("500270", "999999")),
    "codmun",
    desconhecido = "na"
  )
  expect_identical(unknown$municipio, c("Campo Grande", NA_character_))

  protected <- data.frame(
    codmun = "500270",
    municipio = "nome preservado"
  )
  expect_identical(
    adicionar_territorio(protected, "codmun")$municipio,
    "nome preservado"
  )
  expect_identical(
    adicionar_territorio(
      protected, "codmun", sobrescrever = TRUE
    )$municipio,
    "Campo Grande"
  )
})

test_that("territorial time series are completed without changing observed rows", {
  data <- data.frame(
    codmun = c("500270", "500270", "500370"),
    ano = c(2023, 2025, 2023),
    casos = c(10, NA, 5)
  )
  result <- completar_territorios(
    data,
    codigo = "codmun",
    periodo = "ano",
    periodos = 2023:2025,
    preencher = list(casos = 0)
  )

  expect_equal(nrow(result), 6L)
  expect_identical(unique(result$codmun), c("5002704", "5003702"))
  expect_identical(result$ano, rep(2023:2025, 2L))
  expect_identical(
    result$casos,
    c(10, 0, NA, 5, 0, 0)
  )
  expect_identical(
    unique(result$municipio),
    c("Campo Grande", "Dourados")
  )
})

test_that("completion supports observed groups and explicit universes", {
  data <- data.frame(
    codmun = c("500270", "500370", "500270"),
    ano = c(2024, 2024, 2024),
    sexo = c("F", "F", "M"),
    casos = c(5, 2, 4)
  )
  result <- completar_territorios(
    data,
    codigo = "codmun",
    periodo = "ano",
    grupo = "sexo",
    territorios = c("500270", "500370"),
    periodos = 2024:2025,
    preencher = list(casos = 0),
    adicionar = FALSE
  )

  expect_equal(nrow(result), 8L)
  expect_equal(sum(result$casos), 11)
  expect_identical(unique(result$sexo), c("F", "M"))
  expect_false("municipio" %in% names(result))

  state_universe <- completar_territorios(
    data.frame(codmun = "500270", casos = 1),
    codigo = "codmun",
    uf = "MS",
    preencher = list(casos = 0),
    adicionar = FALSE
  )
  expect_equal(nrow(state_universe), 79L)
  expect_equal(sum(state_universe$casos), 1)
})

test_that("completion rejects ambiguous keys and universes", {
  duplicate <- data.frame(
    codmun = c("500270", "500270"),
    ano = c(2024, 2024),
    casos = c(1, 2)
  )
  expect_error(
    completar_territorios(duplicate, "codmun", "ano"),
    "duplicate rows"
  )
  expect_error(
    completar_territorios(
      duplicate[1, ],
      "codmun",
      "ano",
      territorios = "500270",
      uf = "MS"
    ),
    "only one"
  )
  expect_error(
    completar_territorios(
      duplicate[1, ],
      "codmun",
      preencher = list(inexistente = 0)
    ),
    "must be scalar"
  )
})
