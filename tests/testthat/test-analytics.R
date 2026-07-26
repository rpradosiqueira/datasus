test_that("crude rates validate and scale vectorized inputs", {
  result <- calcular_taxa(
    eventos = c(a = 10, b = 25),
    populacao = c(10000, 20000)
  )

  expect_equal(as.numeric(result), c(100, 125))
  expect_identical(names(result), c("a", "b"))
  expect_identical(attr(result, "multiplicador"), 100000)
  expect_equal(
    as.numeric(calcular_taxa(25, 20000, multiplicador = 1000)),
    1.25
  )
  expect_error(
    calcular_taxa(c(1, 2), c(10, 20, 30)),
    "equal lengths"
  )
  expect_error(calcular_taxa(-1, 100), "negative")
  expect_error(calcular_taxa(1, Inf), "finite")
})

test_that("zero denominators are explicit", {
  expect_warning(
    result <- calcular_taxa(c(1, 0), c(0, 0)),
    "Positive events"
  )
  expect_true(all(is.na(result)))
  expect_error(
    calcular_taxa(0, 0, zero_denominador = "erro"),
    "zero denominator"
  )
})

test_that("exact Poisson intervals match chi-squared limits", {
  result <- intervalo_taxa(c(0, 10), c(10000, 10000))

  expected_lower <- 0.5 * stats::qchisq(0.025, 20) / 10000 * 100000
  expected_upper <- 0.5 * stats::qchisq(0.975, 22) / 10000 * 100000
  expect_equal(result$taxa, c(0, 100))
  expect_identical(result$limite_inferior[[1L]], 0)
  expect_equal(result$limite_inferior[[2L]], expected_lower)
  expect_equal(result$limite_superior[[2L]], expected_upper)
  expect_identical(attr(result, "metodo"), "Poisson exact")
  expect_error(
    intervalo_taxa(1.5, 1000),
    "whole event counts"
  )
  expect_error(
    intervalo_taxa(1, 1000, confianca = 1),
    "between 0 and 1"
  )
})

test_that("Brazilian epidemiological weeks handle year boundaries", {
  dates <- as.Date(c(
    "2024-12-29",
    "2025-01-04",
    "2026-01-01",
    "2026-01-04",
    "2026-07-26",
    NA
  ))
  result <- semana_epidemiologica(dates)

  expect_identical(
    result$ano_epidemiologico,
    c(2025L, 2025L, 2025L, 2026L, 2026L, NA_integer_)
  )
  expect_identical(
    result$semana_epidemiologica,
    c(1L, 1L, 53L, 1L, 30L, NA_integer_)
  )
  expect_identical(
    result$codigo,
    c(
      "2025-W01", "2025-W01", "2025-W53",
      "2026-W01", "2026-W30", NA_character_
    )
  )
  expect_identical(result$inicio[[4L]], as.Date("2026-01-04"))
  expect_identical(result$fim[[4L]], as.Date("2026-01-10"))
  expect_error(
    semana_epidemiologica("not-a-date"),
    "invalid ISO dates"
  )
})

test_that("epidemiological calendars contain 52 or 53 complete weeks", {
  calendar_2025 <- calendario_epidemiologico(2025)
  calendar_2026 <- calendario_epidemiologico(2026)

  expect_equal(nrow(calendar_2025), 53L)
  expect_equal(nrow(calendar_2026), 52L)
  expect_identical(calendar_2026$inicio[[1L]], as.Date("2026-01-04"))
  expect_identical(calendar_2026$fim[[52L]], as.Date("2027-01-02"))
  expect_true(all(calendar_2026$fim - calendar_2026$inicio == 6))
  expect_error(calendario_epidemiologico(c(2025, 2026)), "one four-digit")
})

test_that("moving averages support alignment and partial windows", {
  expect_equal(
    as.numeric(media_movel(1:5, 3, "direita")),
    c(NA, NA, 2, 3, 4)
  )
  expect_equal(
    as.numeric(media_movel(1:5, 3, "centro")),
    c(NA, 2, 3, 4, NA)
  )
  expect_equal(
    as.numeric(media_movel(1:5, 3, "esquerda")),
    c(2, 3, 4, NA, NA)
  )
  expect_equal(
    as.numeric(media_movel(1:5, 3, "direita", parcial = TRUE)),
    c(1, 1.5, 2, 3, 4)
  )
  expect_equal(
    as.numeric(media_movel(c(1, NA, 3), 2, na_rm = TRUE)),
    c(NA, 1, 3)
  )
  expect_error(media_movel(1:3, janela = 0), "positive integer")
})

test_that("direct age standardization matches a known weighted rate", {
  result <- padronizar_idade(
    eventos = c(10, 40),
    populacao = c(1000, 1000),
    idade = c("0-49", "50+"),
    populacao_padrao = c("0-49" = 800, "50+" = 200),
    confianca = 0.95
  )

  expect_equal(result$taxa_padronizada, 1600)
  expect_equal(result$erro_padrao, sqrt(80000))
  expect_equal(result$cobertura_padrao, 1)
  expect_equal(result$estratos, 2L)
  expect_gte(result$limite_inferior, 0)
  expect_gt(result$limite_superior, result$taxa_padronizada)
  expect_identical(attr(result, "metodo"), "Direct age standardization")
})

test_that("age standardization supports analysis groups", {
  result <- padronizar_idade(
    eventos = c(10, 40, 20, 80),
    populacao = rep(1000, 4),
    idade = rep(c("0-49", "50+"), 2),
    populacao_padrao = c("0-49" = 800, "50+" = 200),
    grupo = c("A", "A", "B", "B")
  )

  expect_identical(result$grupo, c("A", "B"))
  expect_equal(result$taxa_padronizada, c(1600, 3200))
  expect_false("limite_inferior" %in% names(result))
})

test_that("incomplete standardization reports standard-weight coverage", {
  incomplete <- padronizar_idade(
    eventos = c(10, NA),
    populacao = c(1000, 1000),
    idade = c("0-49", "50+"),
    populacao_padrao = c("0-49" = 800, "50+" = 200)
  )
  expect_true(is.na(incomplete$taxa_padronizada))

  complete <- padronizar_idade(
    eventos = c(10, NA),
    populacao = c(1000, 1000),
    idade = c("0-49", "50+"),
    populacao_padrao = c("0-49" = 800, "50+" = 200),
    na_rm = TRUE
  )
  expect_equal(complete$taxa_padronizada, 1000)
  expect_equal(complete$cobertura_padrao, 0.8)
  expect_equal(complete$estratos, 1L)
})

test_that("age standardization rejects ambiguous or invalid strata", {
  expect_error(
    padronizar_idade(
      eventos = c(1, 2),
      populacao = c(100, 100),
      idade = c("0-49", "0-49"),
      populacao_padrao = c(80, 20)
    ),
    "must be unique"
  )
  expect_error(
    padronizar_idade(
      eventos = c(1, 2),
      populacao = c(100, 100),
      idade = c("0-49", "50+"),
      populacao_padrao = c("0-49" = 80)
    ),
    "No standard population weight"
  )
  expect_error(
    padronizar_idade(
      eventos = c(1, 2),
      populacao = c(100, 100),
      idade = c("0-49", "50+"),
      populacao_padrao = c("0-49" = 80, "0-49" = 20)
    ),
    "must be unique"
  )
  expect_error(
    padronizar_idade(
      eventos = 1,
      populacao = 0,
      idade = "all",
      populacao_padrao = c(all = 1)
    ),
    "must be positive"
  )
})
