test_that("Latin-1 TABNET pages are converted to valid UTF-8", {
  content <- c(
    charToRaw("<html><body><option>Apgar 1"),
    as.raw(0xba),
    charToRaw(" minuto</option></body></html>")
  )

  page <- datasus:::.tabnet_parse_html(content)
  value <- rvest::html_text2(rvest::html_element(page, "option"))

  expect_identical(
    value,
    paste0("Apgar 1", intToUtf8(186), " minuto")
  )
  expect_true(validEnc(value))
})

test_that("TABNET tables are parsed and numeric counts are normalized", {
  content <- charToRaw(paste0(
    "<html><body><table class='tabdados'>",
    "<thead><tr><th>Municipio</th><th>Nascimentos</th><th>Valor</th></tr></thead>",
    "<tbody>",
    "<tr><td>TOTAL</td><td>1.234</td><td>1.234,56</td></tr>",
    "<tr><td>Sem dados</td><td>-</td><td>-</td></tr>",
    "</tbody></table></body></html>"
  ))

  result <- datasus:::.tabnet_parse_table(
    datasus:::.tabnet_parse_html(content)
  )

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("Municipio", "Nascimentos", "Valor"))
  expect_identical(result$Nascimentos, c(1234, NA_real_))
  expect_identical(result$Valor, c(1234.56, NA_real_))
})

test_that("period vectors and the latest period are resolved consistently", {
  available <- c(2024, 2023, 2022)

  expect_identical(
    datasus:::.tabnet_validate_period("last", available),
    2024
  )
  expect_identical(
    datasus:::.tabnet_validate_period(c("2022", "2023"), available),
    c(2022, 2023)
  )
  expect_error(
    datasus:::.tabnet_validate_period("not-a-year", available),
    "years or 'last'"
  )
})

test_that("state abbreviations and endpoint routes are validated centrally", {
  expect_identical(datasus:::.tabnet_validate_uf(" MS "), "ms")
  expect_identical(datasus:::.tabnet_validate_uf("50"), "ms")
  expect_identical(
    datasus:::.tabnet_validate_uf("Mato Grosso do Sul"),
    "ms"
  )
  expect_error(datasus:::.tabnet_validate_uf("xx"), "valid Brazilian state")

  expect_identical(
    datasus:::.tabnet_url(
      "sim", "evitb10", "municipality", action = "query"
    ),
    paste0(
      "https://tabnet.datasus.gov.br/cgi/tabcgi.exe?",
      "sim/cnv/evitb10br.def"
    )
  )
  expect_identical(
    datasus:::.tabnet_url(
      "sinasc", "nv", "state", uf = "MS", action = "form"
    ),
    paste0(
      "https://tabnet.datasus.gov.br/cgi/deftohtm.exe?",
      "sinasc/cnv/nvms.def"
    )
  )
})

test_that("form options are extracted once with stable encodings and types", {
  content <- c(
    charToRaw("<select id='S1'><option value='all'>Todos</option>"),
    charToRaw("<option value='2024'>Apgar 1"),
    as.raw(0xba),
    charToRaw(" minuto 2024</option></select>")
  )
  page <- datasus:::.tabnet_parse_html(content)

  text <- datasus:::.tabnet_options(page, "#S1 option")
  number <- datasus:::.tabnet_options(page, "#S1 option", numeric = TRUE)

  expect_identical(
    text$id,
    c("Todos", paste0("Apgar 1", intToUtf8(186), " minuto 2024"))
  )
  expect_identical(number$id, c(NA_real_, 1))
  expect_identical(text$value, c("all", "2024"))
  expect_identical(
    datasus:::.tabnet_filter(text, "Todos")$value,
    "all"
  )
  expect_identical(
    datasus:::.tabnet_filter(text, "2024")$value,
    "2024"
  )
  expect_identical(
    datasus:::.tabnet_indexed_options(page, "#S1 option")$id,
    0:1
  )
})

test_that("empty and malformed tables fail with useful errors", {
  empty <- datasus:::.tabnet_parse_html(
    charToRaw("<html><body>Consulta sem resultado</body></html>")
  )
  expect_error(
    datasus:::.tabnet_parse_table(empty),
    "returned no tabular data"
  )

  malformed <- datasus:::.tabnet_parse_html(charToRaw(paste0(
    "<table class='tabdados'>",
    "<tr><th>A</th><th>B</th></tr>",
    "<tbody><tr><td>1</td><td>2</td><td>3</td></tr></tbody>",
    "</table>"
  )))
  expect_error(
    datasus:::.tabnet_parse_table(malformed),
    "unexpected table shape"
  )
})

test_that("webtabx JavaScript tables are parsed without evaluating code", {
  script <- paste0(
    "var data = new google.visualization.DataTable();\r",
    "data.addColumn('string','Municipio');\r",
    "data.addColumn('number','Doses');\r",
    "data.addRows([\r",
    "[\" Total\" ,{v: 1234.0 ,f: '1.234'} ]\r",
    ", [\"500001 D'OESTE\" ,{v: 12.0 ,f: '12'} ]\r",
    "]);"
  )
  page <- xml2::read_html(paste0(
    "<html><body><script>",
    script,
    "</script></body></html>"
  ))

  result <- datasus:::.tabnet_parse_webtabx(page)

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("Municipio", "Doses"))
  expect_identical(result$Municipio, c("TOTAL", "500001 D'OESTE"))
  expect_identical(result$Doses, c(1234, 12))

  compact_page <- xml2::read_html(paste0(
    "<html><body><script>",
    gsub("\r", " ", script, fixed = TRUE),
    "</script></body></html>"
  ))
  compact_result <- datasus:::.tabnet_parse_webtabx(compact_page)

  expect_identical(compact_result, result)
})
