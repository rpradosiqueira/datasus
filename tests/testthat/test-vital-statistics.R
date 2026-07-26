test_that("SIM and SINASC datasets are part of the declarative catalog", {
  sim_catalog <- datasus_catalogo("sim")
  sinasc_catalog <- datasus_catalogo("sinasc")

  expect_identical(
    sim_catalog$conjunto,
    c(
      "obitos",
      "mortalidade_infantil",
      "causas_evitaveis_0_4",
      "causas_evitaveis_5_74"
    )
  )
  expect_identical(sinasc_catalog$conjunto, "nascidos_vivos")
})

test_that("vital-statistics routes cover all legacy geographic forms", {
  sim_metadata <- datasus:::.datasus_resolve_dataset("sim", "obitos")
  sinasc_metadata <- datasus:::.datasus_resolve_dataset(
    "sinasc", "nascidos_vivos"
  )

  expect_match(
    datasus:::.datasus_dataset_urls(
      sim_metadata, abrangencia = "uf"
    )$form,
    "sim/cnv/obt10uf[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(
      sim_metadata, abrangencia = "municipio"
    )$form,
    "sim/cnv/obt10br[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(
      sim_metadata, "SP", abrangencia = "municipio"
    )$query,
    "sim/cnv/obt10sp[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(
      sinasc_metadata, abrangencia = "uf"
    )$form,
    "sinasc/cnv/nvuf[.]def$"
  )
  expect_error(
    datasus:::.datasus_dataset_urls(
      sim_metadata, "SP", abrangencia = "uf"
    ),
    "uf = NULL"
  )
})

test_that("unified vital-statistics functions forward a stable contract", {
  calls <- list()
  local_mocked_bindings(
    .datasus_query = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      data.frame(resultado = 1)
    },
    .package = "datasus"
  )

  sim_result <- sim(
    "mortalidade_infantil",
    abrangencia = "uf",
    periodo = 2024,
    filtros = list(sexo = "Masculino")
  )
  sinasc_result <- sinasc(
    uf = "SP",
    periodo = c(2023, 2024),
    filtros = list(tipo_de_parto = "Cesário")
  )

  expect_s3_class(sim_result, "data.frame")
  expect_s3_class(sinasc_result, "data.frame")
  expect_identical(calls[[1L]][[1L]], "sim")
  expect_identical(calls[[1L]][[2L]], "mortalidade_infantil")
  expect_identical(calls[[1L]]$abrangencia, "uf")
  expect_identical(calls[[2L]][[1L]], "sinasc")
  expect_identical(calls[[2L]][[3L]], "SP")
  expect_identical(calls[[2L]]$abrangencia, "municipio")
})

test_that("legacy functions are thin positional compatibility wrappers", {
  calls <- list()
  local_mocked_bindings(
    .datasus_query = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      data.frame(resultado = 1)
    },
    .package = "datasus"
  )

  expect_warning(
    sim_obt10_uf("SP", sexo = "Masculino"),
    "deprecated"
  )
  expect_warning(
    sinasc_nv_bruf(tipo_de_parto = "Cesário"),
    "deprecated"
  )

  expect_identical(calls[[1L]]$sistema, "sim")
  expect_identical(calls[[1L]]$conjunto, "obitos")
  expect_identical(calls[[1L]]$uf, "SP")
  expect_identical(calls[[1L]]$abrangencia, "municipio")
  expect_true(calls[[1L]]$filtros_posicionais)
  expect_null(names(calls[[1L]]$filtros))
  expect_identical(calls[[1L]]$filtros[[16L]], "Masculino")

  expect_identical(calls[[2L]]$sistema, "sinasc")
  expect_identical(calls[[2L]]$conjunto, "nascidos_vivos")
  expect_null(calls[[2L]]$uf)
  expect_identical(calls[[2L]]$abrangencia, "uf")
  expect_identical(calls[[2L]]$filtros[[9L]], "Cesário")
})

test_that("TABNET query results carry reproducibility metadata", {
  page <- xml2::read_html(paste0(
    "<form>",
    "<select id='L' name='Linha'>",
    "<option value='Regiao'>Região</option></select>",
    "<select id='C' name='Coluna'>",
    "<option value='--Nao-Ativa--'>Não ativa</option></select>",
    "<select id='I' name='Incremento'>",
    "<option value='Obitos'>Óbitos</option></select>",
    "<select id='A' name='Arquivos'>",
    "<option value='2024'>2024</option></select>",
    "</form>"
  ))
  result_page <- xml2::read_html(paste0(
    "<table class='tabdados'>",
    "<thead><tr><th>Região</th><th>Óbitos</th></tr></thead>",
    "<tbody><tr><td>Brasil</td><td>10</td></tr></tbody>",
    "</table>"
  ))

  local_mocked_bindings(
    .tabnet_read_html = function(...) page,
    .tabnet_post = function(...) result_page,
    .package = "datasus"
  )

  result <- sim(abrangencia = "uf", periodo = 2024)
  provenance <- datasus_proveniencia(result)

  expect_s3_class(result, "data.frame")
  expect_identical(provenance$fonte, "DATASUS TABNET")
  expect_identical(provenance$sistema, "sim")
  expect_identical(provenance$conjunto, "obitos")
  expect_identical(provenance$abrangencia, "uf")
  expect_match(provenance$url, "sim/cnv/obt10uf[.]def$")
  expect_s3_class(provenance$consultado_em, "POSIXct")
})
