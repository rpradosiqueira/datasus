test_that("generic indicators aggregate groups before calculation", {
  dados <- data.frame(
    ano = c(2024, 2024, 2025),
    eventos = c(10, 20, 15),
    populacao = c(50000, 50000, 100000)
  )
  result <- calcular_indicador(
    dados,
    numerador = "eventos",
    denominador = "populacao",
    grupo = "ano",
    tipo = "taxa"
  )

  expect_identical(result$ano, c(2024, 2025))
  expect_equal(result$numerador, c(30, 15))
  expect_equal(result$denominador, c(100000, 100000))
  expect_equal(result$indicador, c(30, 15))
  expect_identical(attr(result, "tipo"), "taxa")
  expect_identical(attr(result, "multiplicador"), 100000)
})

test_that("generic indicators use exact confidence intervals", {
  dados <- data.frame(eventos = 10, populacao = 10000)
  rate <- calcular_indicador(
    dados, "eventos", "populacao",
    tipo = "taxa", confianca = 0.95
  )
  expected <- intervalo_taxa(10, 10000)
  expect_equal(rate$limite_inferior, expected$limite_inferior)
  expect_equal(rate$limite_superior, expected$limite_superior)

  binomial <- calcular_indicador(
    data.frame(parte = 10, total = 20),
    "parte", "total",
    tipo = "proporcao", confianca = 0.95
  )
  expected_binomial <- stats::binom.test(10, 20)$conf.int * 100
  expect_equal(
    as.numeric(c(
      binomial$limite_inferior,
      binomial$limite_superior
    )),
    as.numeric(expected_binomial)
  )
  expect_error(
    calcular_indicador(
      data.frame(a = 1, b = 2), "a", "b",
      tipo = "razao", confianca = 0.95
    ),
    "not available for ratios"
  )
})

test_that("indicator shortcuts expose domain-specific result names", {
  dados <- data.frame(
    grupo = c("A", "A"),
    obitos = c(1, 2),
    casos = c(20, 30),
    populacao = c(5000, 5000)
  )

  mortality <- taxa_mortalidade(
    dados, "obitos", "populacao", grupo = "grupo"
  )
  incidence <- taxa_incidencia(
    dados, "casos", "populacao", grupo = "grupo"
  )
  fatality <- letalidade(dados, "obitos", "casos", grupo = "grupo")
  share <- proporcao(dados, "obitos", "casos", grupo = "grupo")

  expect_equal(mortality$taxa_mortalidade, 30)
  expect_equal(incidence$taxa_incidencia, 500)
  expect_equal(fatality$letalidade, 6)
  expect_equal(share$proporcao, 6)
  expect_false("indicador" %in% names(mortality))
})

test_that("indicator validation rejects invalid epidemiological inputs", {
  expect_error(
    calcular_indicador(
      data.frame(parte = 11, total = 10),
      "parte", "total", tipo = "proporcao"
    ),
    "cannot exceed"
  )
  expect_error(
    calcular_indicador(
      data.frame(eventos = 1.5, populacao = 100),
      "eventos", "populacao", tipo = "taxa", confianca = 0.95
    ),
    "whole counts"
  )
  expect_warning(
    result <- calcular_indicador(
      data.frame(eventos = 1, populacao = 0),
      "eventos", "populacao"
    ),
    "zero denominator"
  )
  expect_true(is.na(result$indicador))
  expect_error(
    calcular_indicador(
      data.frame(grupo = NA, a = 1, b = 2),
      "a", "b", grupo = "grupo"
    ),
    "must not contain missing"
  )
})

test_that("missing values have explicit aggregation semantics", {
  dados <- data.frame(
    grupo = c("A", "A", "B"),
    a = c(1, NA, NA),
    b = c(10, 10, 10)
  )
  propagated <- calcular_indicador(
    dados, "a", "b", grupo = "grupo", tipo = "razao"
  )
  removed <- calcular_indicador(
    dados, "a", "b", grupo = "grupo", tipo = "razao", na_rm = TRUE
  )

  expect_true(all(is.na(propagated$indicador)))
  expect_equal(removed$indicador[[1L]], 1 / 10)
  expect_true(is.na(removed$indicador[[2L]]))
})

test_that("Poisson intervals accept non-integer person-time denominators", {
  result <- calcular_indicador(
    data.frame(eventos = 3, pessoa_tempo = 12.5),
    "eventos", "pessoa_tempo",
    tipo = "taxa", multiplicador = 1000, confianca = 0.95
  )

  expect_equal(result$indicador, 240)
  expect_true(is.finite(result$limite_superior))
})

test_that("population joins are validated many-to-one operations", {
  eventos <- data.frame(
    codigo = c("A", "A", "B"),
    ano = c(2024, 2024, 2024),
    casos = c(1, 2, 3)
  )
  populacao <- data.frame(
    municipio = c("B", "A"),
    periodo = c(2024, 2024),
    habitantes = c(2000, 1000)
  )
  result <- juntar_populacao(
    eventos,
    populacao,
    por = c(codigo = "municipio", ano = "periodo"),
    coluna_populacao = "habitantes"
  )

  expect_identical(result$casos, eventos$casos)
  expect_equal(result$populacao, c(1000, 1000, 2000))
  expect_error(
    juntar_populacao(
      eventos,
      rbind(populacao, populacao[1, ]),
      por = c(codigo = "municipio", ano = "periodo"),
      coluna_populacao = "habitantes"
    ),
    "must be unique"
  )
  expect_error(
    juntar_populacao(
      rbind(eventos, data.frame(codigo = "C", ano = 2024, casos = 4)),
      populacao,
      por = c(codigo = "municipio", ano = "periodo"),
      coluna_populacao = "habitantes"
    ),
    "No population denominator"
  )
})

test_that("population joins preserve provenance and support missing keys", {
  eventos <- data.frame(codigo = c("A", "C"))
  attr(eventos, "datasus_proveniencia") <- list(source = "events")
  populacao <- data.frame(codigo = "A", habitantes = 1000)
  attr(populacao, "datasus_proveniencia") <- list(source = "population")

  result <- juntar_populacao(
    eventos, populacao,
    por = "codigo",
    coluna_populacao = "habitantes",
    ausente = "na"
  )

  expect_equal(result$populacao, c(1000, NA))
  expect_identical(
    datasus_proveniencia(result),
    list(source = "events")
  )
  expect_identical(
    attr(result, "datasus_populacao_proveniencia"),
    list(source = "population")
  )
})

test_that("ready-made standard populations are normalized and reusable", {
  who <- populacao_padrao()
  segi <- populacao_padrao("segi")
  scandinavian <- populacao_padrao("escandinava")

  expect_equal(sum(who), 1)
  expect_equal(sum(segi), 1)
  expect_equal(sum(scandinavian), 1)
  expect_length(who, 21L)
  expect_length(segi, 18L)
  expect_identical(names(who)[c(1, 21)], c("0-4", "100+"))
  expect_identical(names(segi)[18], "85+")
  expect_identical(
    attr(populacao_padrao("oms"), "populacao_padrao"),
    "oms_2000_2025"
  )

  result <- padronizar_idade(
    eventos = rep(1, length(who)),
    populacao = rep(100, length(who)),
    idade = names(who),
    populacao_padrao = who
  )
  expect_equal(result$taxa_padronizada, 1000)
})

test_that("published standard-population weights can be inspected", {
  data <- populacao_padrao("oms", formato = "dados")
  raw <- populacao_padrao("oms", normalizar = FALSE)

  expect_named(data, c("faixa_etaria", "peso_publicado", "peso"))
  expect_equal(data$peso_publicado, as.numeric(raw))
  expect_equal(sum(data$peso), 1)
  expect_equal(data$peso_publicado[data$faixa_etaria == "100+"], 0.005)
  expect_error(populacao_padrao("unknown"), "Unknown")
})
