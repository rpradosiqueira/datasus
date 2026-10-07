test_that("health-services catalog is complete and can be filtered", {
  catalog <- datasus_catalogo()

  expect_s3_class(catalog, "data.frame")
  expect_named(
    catalog,
    c("sistema", "conjunto", "categoria", "descricao", "escopo")
  )
  expect_equal(sum(catalog$sistema == "sih"), 16L)
  expect_equal(sum(catalog$sistema == "sia"), 4L)
  expect_equal(sum(catalog$sistema == "cnes"), 24L)
  expect_equal(sum(catalog$sistema == "populacao"), 6L)
  expect_equal(sum(catalog$sistema == "sinan"), 46L)
  expect_equal(sum(catalog$sistema == "pni"), 2L)
  expect_equal(sum(catalog$sistema == "siscan"), 15L)
  expect_equal(sum(catalog$sistema == "sisvan"), 2L)
  expect_equal(sum(catalog$sistema == "financiamento"), 3L)
  expect_equal(sum(catalog$sistema == "sim"), 4L)
  expect_equal(sum(catalog$sistema == "sinasc"), 1L)
  expect_equal(nrow(catalog), 123L)
  expect_equal(nrow(datasus_catalogo("cnes")), 24L)
  expect_equal(nrow(datasus_catalogo("sim")), 4L)
})

test_that("health-services routes use Brazil or state forms", {
  metadata <- datasus:::.datasus_resolve_dataset(
    "sih", "aih_rd_internacao"
  )

  expect_match(
    datasus:::.datasus_dataset_urls(metadata)$form,
    "sih/cnv/qibr[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(metadata, "MS")$query,
    "sih/cnv/qims[.]def$"
  )
  expect_error(
    datasus:::.datasus_resolve_dataset("sih", "unknown"),
    "Unknown SIH dataset"
  )
})

test_that("fixed geographic forms use their published suffixes", {
  population <- datasus:::.datasus_resolve_dataset(
    "populacao", "estimativa_municipal"
  )
  projection <- datasus:::.datasus_resolve_dataset(
    "populacao", "projecao_uf"
  )
  intoxication <- datasus:::.datasus_resolve_dataset(
    "sinan", "intoxicacao_exogena"
  )

  expect_match(
    datasus:::.datasus_dataset_urls(population, "MS")$form,
    "ibge/cnv/popsvsbr[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(projection)$form,
    "ibge/cnv/projpopuf[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(intoxication, "MS")$form,
    "sinannet/cnv/Intoxms[.]def$"
  )

  pni <- datasus:::.datasus_resolve_dataset("pni", "doses_aplicadas")
  siscan <- datasus:::.datasus_resolve_dataset(
    "siscan", "citologia_colo_residencia"
  )
  sisvan <- datasus:::.datasus_resolve_dataset(
    "sisvan", "atencao_basica"
  )
  financing <- datasus:::.datasus_resolve_dataset(
    "financiamento", "recursos_federais"
  )

  expect_match(
    datasus:::.datasus_dataset_urls(pni, "MS")$form,
    "dhdat[.]exe[?]bd_pni/dpnibr[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(pni, "MS")$query,
    "webtabx[.]exe[?]bd_pni/dpnibr[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(siscan, "MS")$form,
    "dhdat[.]exe[?]SISCAN/cito_colo_residms[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(sisvan, "MS")$form,
    "sisvan/cnv/acom_ms[.]def$"
  )
  expect_match(
    datasus:::.datasus_dataset_urls(financing, "MS")$form,
    "recsus/cnv/rsbr[.]def$"
  )
})

test_that("TABNET forms become stable public option names", {
  page <- xml2::read_html(paste0(
    "<form>",
    "<select id='L' name='Linha'>",
    "<option value='Municipio'>Municipio</option></select>",
    "<select id='C' name='Coluna'>",
    "<option value='--Nao-Ativa--'>Nao ativa</option></select>",
    "<select id='I' name='Incremento' multiple>",
    "<option value='Quantidade'>Quantidade</option></select>",
    "<select id='A' name='Arquivos' multiple>",
    "<option value='arquivo2605.dbf'>Mai/2026</option>",
    "<option value='arquivo2505.dbf'>Mai/2025</option>",
    "<option value='arquivo2504.dbf'>Abr/2025</option></select>",
    "<select id='S1' name='SCarater_atendimento' multiple>",
    "<option value='TODAS_AS_CATEGORIAS__'>Todas as categorias</option>",
    "<option value='2'>Urgencia</option></select>",
    "</form>"
  ))

  options <- datasus:::.tabnet_form_options(page)
  expect_named(
    options,
    c("linha", "coluna", "conteudo", "periodo", "filtros")
  )
  expect_named(options$filtros, "carater_atendimento")
  expect_identical(
    unname(attr(options, "field_names")),
    c("Linha", "Coluna", "Incremento", "Arquivos")
  )

  fields <- datasus:::.tabnet_build_fields(
    options,
    linha = NULL,
    coluna = NULL,
    conteudo = 1,
    periodo = 2025,
    filtros = list(carater_atendimento = "Urgencia")
  )
  expect_identical(fields$Arquivos, c("arquivo2505.dbf", "arquivo2504.dbf"))
  expect_identical(fields$SCarater_atendimento, "2")
  expect_match(
    datasus:::.tabnet_encode_fields(fields),
    "Arquivos=arquivo2505.dbf&Arquivos=arquivo2504.dbf"
  )

  population <- data.frame(
    conjunto = "estimativa_municipal",
    escopo = "br"
  )
  names(options$filtros) <- "unidade_da_federacao"
  selected <- datasus:::.datasus_add_fixed_scope_uf(
    population,
    options,
    list(),
    "MS"
  )
  expect_identical(
    selected$unidade_da_federacao,
    "Mato Grosso do Sul"
  )
  expect_error(
    datasus:::.datasus_add_fixed_scope_uf(
      population,
      options,
      list(unidade_da_federacao = "Acre"),
      "MS"
    ),
    "Do not combine"
  )
})

test_that("webtabx period field names are read from the form", {
  page <- xml2::read_html(paste0(
    "<select id='L' name='Linha'><option value='x'>X</option></select>",
    "<select id='C' name='Coluna'><option value='y'>Y</option></select>",
    "<select id='I' name='Incremento'><option value='z'>Z</option></select>",
    "<select id='A' name='PAno'><option value='2022|2022|4'>2022</option>",
    "</select>"
  ))
  options <- datasus:::.tabnet_form_options(page)
  fields <- datasus:::.tabnet_build_fields(
    options,
    linha = NULL,
    coluna = NULL,
    conteudo = 1,
    periodo = "last",
    filtros = list()
  )

  expect_named(
    fields,
    c("Linha", "Coluna", "Incremento", "PAno", "formato", "mostre")
  )
  expect_identical(fields$PAno, "2022|2022|4")
})

test_that("query selections fail early with actionable messages", {
  options <- data.frame(
    id = c("One", "Two"),
    value = c("1", "2")
  )

  expect_identical(
    datasus:::.tabnet_resolve_option(
      options, "Two", "example", multiple = FALSE
    ),
    "2"
  )
  expect_error(
    datasus:::.tabnet_resolve_option(
      options, "Three", "example", multiple = FALSE
    ),
    "datasus_opcoes"
  )
  expect_identical(
    datasus:::.tabnet_encode_component("Região"),
    "Regi%E3o"
  )
})
