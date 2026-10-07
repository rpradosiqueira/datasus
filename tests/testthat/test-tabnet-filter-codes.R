test_that("numeric filter prefixes resolve to internal TABNET values", {
  options <- data.frame(
    id = c("Todas", "500270 CAMPO GRANDE", "500370 DOURADOS", "00123 LOCAL"),
    value = c("all", "5147", "5150", "other")
  )
  resolve <- function(selected) {
    datasus:::.tabnet_resolve_option(
      options, selected, "filtros$municipio", numeric_prefix = TRUE
    )
  }

  expect_identical(resolve(500270), "5147")
  expect_identical(resolve("500270"), "5147")
  expect_identical(resolve(c(500370, 500270, 500370)), c("5150", "5147"))
  expect_identical(resolve(c("500270", "500370")), c("5147", "5150"))
  expect_identical(resolve("500270 CAMPO GRANDE"), "5147")
  expect_identical(resolve("5147"), "5147")
  expect_identical(resolve("all"), "all")
  expect_identical(resolve("00123"), "other")
  expect_error(resolve(123), "Unknown value")
  expect_error(resolve("50027"), "Unknown value")
  expect_error(resolve("999999"), "Unknown value")
  expect_error(resolve(NA_character_), "empty or missing")

  seven_digits <- data.frame(id = "5002704 CAMPO GRANDE", value = "5147")
  expect_identical(
    datasus:::.tabnet_resolve_option(
      seven_digits, "5002704", "filtros$municipio", numeric_prefix = TRUE
    ),
    "5147"
  )
  expect_identical(
    datasus:::.tabnet_resolve_option(
      seven_digits, 5002704, "filtros$municipio", numeric_prefix = TRUE
    ),
    "5147"
  )
})

test_that("numeric filter shortcuts reject ambiguity and keep exact matches", {
  options <- data.frame(
    id = c("500270 FIRST", "500270 SECOND", "Internal code takes priority"),
    value = c("first", "second", "500270")
  )
  expect_identical(
    datasus:::.tabnet_resolve_option(
      options, "500270", "filtros$municipio", numeric_prefix = TRUE
    ),
    "500270"
  )
  expect_identical(
    datasus:::.tabnet_resolve_option(
      options, "500270 SECOND", "filtros$municipio", numeric_prefix = TRUE
    ),
    "second"
  )
  expect_error(
    datasus:::.tabnet_resolve_option(
      options[1:2, ], "500270", "filtros$municipio", numeric_prefix = TRUE
    ),
    "Ambiguous numeric code.*filtros\\$municipio.*datasus_opcoes"
  )

  non_codes <- data.frame(
    id = c("Group 500270", "5002700 OTHER", "500270A CATEGORY", "1.5 YEARS"),
    value = c("a", "b", "c", "d")
  )
  expect_error(
    datasus:::.tabnet_resolve_option(
      non_codes, "500270", "filtros$municipio", numeric_prefix = TRUE
    ),
    "Unknown value"
  )
  expect_error(
    datasus:::.tabnet_resolve_option(
      non_codes, "1", "filtros$age", numeric_prefix = TRUE
    ),
    "Unknown value"
  )
})

test_that("numeric prefixes are opt-in and do not change dimension indices", {
  options <- data.frame(
    id = c("10 FIRST", "20 SECOND"), value = c("a", "b")
  )
  expect_error(
    datasus:::.tabnet_resolve_option(options, "10", "linha"),
    "Unknown value"
  )
  expect_identical(
    datasus:::.tabnet_resolve_option(options, 2, "conteudo", index = TRUE),
    "b"
  )
  expect_error(
    datasus:::.tabnet_resolve_option(options, 10, "conteudo", index = TRUE),
    "out of range"
  )
})

test_that("modern and legacy vital queries send translated municipality codes", {
  calls <- list()
  page <- NULL
  result_page <- xml2::read_html(paste0(
    "<table class='tabdados'><thead><tr><th>Municipio</th><th>Total</th></tr>",
    "</thead><tbody><tr><td>CAMPO GRANDE</td><td>10</td></tr></tbody></table>"
  ))
  local_mocked_bindings(
    .tabnet_read_html = function(...) page,
    .tabnet_post = function(url, body) {
      calls[[length(calls) + 1L]] <<- list(url = url, body = body)
      result_page
    },
    .package = "datasus"
  )

  form <- function(filter_names) {
    filters <- vapply(seq_along(filter_names), function(i) {
      name <- filter_names[[i]]
      field <- if (name == "municipio") "Munic\u00edpio" else name
      choices <- if (name == "municipio") {
        paste0(
          "<option value='5147'>500270 CAMPO GRANDE</option>",
          "<option value='5150'>500370 DOURADOS</option>"
        )
      } else {
        ""
      }
      paste0(
        "<select id='S", i, "' name='S", field, "' multiple>",
        "<option value='all'>Todas</option>", choices, "</select>"
      )
    }, character(1))
    xml2::read_html(paste0(
      "<form><select id='L' name='Linha'>",
      "<option value='Municipio'>Munic\u00edpio</option></select>",
      "<select id='C' name='Coluna'>",
      "<option value='inactive'>N\u00e3o ativa</option></select>",
      "<select id='I' name='Incremento'>",
      "<option value='Total'>Total</option></select>",
      "<select id='A' name='Arquivos'>",
      "<option value='2024'>2024</option></select>",
      paste(filters, collapse = ""), "</form>"
    ))
  }

  page <- form("municipio")
  for (query in list(sim, sinasc)) {
    for (code in list(500270, "500270")) {
      result <- query(filtros = list(municipio = code))
      expect_s3_class(result, "data.frame")
      expect_match(tail(calls, 1L)[[1L]]$body, "SMunic%EDpio=5147", fixed = TRUE)
    }
    query(filtros = list(municipio = c("500270", "500370")))
    body <- tail(calls, 1L)[[1L]]$body
    expect_match(body, "SMunic%EDpio=5147&SMunic%EDpio=5150", fixed = TRUE)
    expect_false(grepl("500270|500370", body))
  }

  requests_before <- length(calls)
  expect_error(
    sim(filtros = list(municipio = "999999")),
    "Unknown value"
  )
  xml2::xml_set_text(
    xml2::xml_find_first(page, "//select[@id='S1']/option[@value='5150']"),
    "500270 OTHER MUNICIPALITY"
  )
  expect_error(
    sim(filtros = list(municipio = "500270")),
    "Ambiguous numeric code"
  )
  expect_length(calls, requests_before)

  wrappers <- c(
    "sim_obt10_mun", "sim_obt10_uf", "sim_inf10_mun", "sim_inf10_uf",
    "sim_evita10_mun", "sim_evita10_uf", "sim_evitb10_mun", "sim_evitb10_uf",
    "sinasc_nv_mun", "sinasc_nv_uf"
  )
  for (name in wrappers) {
    wrapper <- get(name, envir = asNamespace("datasus"))
    filter_names <- setdiff(
      names(formals(wrapper)), c("uf", "linha", "coluna", "conteudo", "periodo")
    )
    page <- form(filter_names)
    for (code in list(500270, "500270", c(500270, 500370))) {
      arguments <- list(municipio = code)
      if ("uf" %in% names(formals(wrapper))) arguments$uf <- "MS"
      expect_warning(result <- do.call(wrapper, arguments), "deprecated")
      expect_s3_class(result, "data.frame")
      body <- tail(calls, 1L)[[1L]]$body
      expect_match(body, "SMunic%EDpio=5147", fixed = TRUE)
      if (length(code) > 1L) {
        expect_match(body, "SMunic%EDpio=5147&SMunic%EDpio=5150", fixed = TRUE)
      }
      expect_false(grepl("500270|500370", body))
    }
  }
})
