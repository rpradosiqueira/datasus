test_that("public TABNET planning works with an offline form fixture", {
  page <- xml2::read_html(paste0(
    "<form>",
    "<select id='L' name='Linha'><option value='Regiao'>Regiao</option></select>",
    "<select id='C' name='Coluna'><option value='--Nao-Ativa--'>Nao ativa</option></select>",
    "<select id='I' name='Incremento'><option value='Obitos'>Obitos</option></select>",
    "<select id='A' name='Arquivos'><option value='obitos24.dbf'>2024</option></select>",
    "<select id='S1' name='SCapitulo'><option value='all'>Todos</option></select>",
    "</form>"
  ))
  testthat::local_mocked_bindings(
    .tabnet_read_html = function(...) page,
    .package = "datasus"
  )

  result <- datasus_opcoes("sim", "obitos", abrangencia = "uf")

  expect_s3_class(result, "datasus_opcoes")
  expect_named(result, c("linha", "coluna", "conteudo", "periodo", "filtros"))
  expect_identical(result$linha$value, "Regiao")
  expect_identical(result$periodo$id, "2024")
  expect_named(result$filtros, "capitulo")
})

test_that("public OpenDataSUS discovery contracts are offline and stable", {
  catalog_payload <- list(
    packages = list(list(
      name = "arboviroses-dengue",
      title = "Arboviroses - Dengue",
      notes = "Anonymous notifications",
      formats = list("CSV"),
      groups = list(list(name = "vigilancia"))
    )),
    numberOfPackages = 1L
  )
  testthat::local_mocked_bindings(
    .opendatasus_page_data = function(...) catalog_payload,
    .package = "datasus"
  )

  catalog <- opendatasus_catalogo("dengue", cache = FALSE)

  expect_named(
    catalog,
    c("conjunto", "titulo", "descricao", "formatos", "grupos")
  )
  expect_identical(catalog$conjunto, "arboviroses-dengue")
  expect_identical(attr(catalog, "total"), 1L)
  expect_match(datasus_proveniencia(catalog)$fonte, "[?]q=dengue")

  dataset <- list(
    name = "arboviroses-dengue",
    title = "Arboviroses - Dengue",
    metadata_modified = "2026-08-01T00:00:00",
    resources = list(list(
      id = "resource-2026",
      name = "Dengue 2026",
      description = "Current anonymous records",
      format = "CSV",
      url = "https://example.test/dengue-2026.csv",
      size = "123",
      created = "2026-01-01",
      last_modified = "2026-08-01",
      position = "0"
    ))
  )
  testthat::local_mocked_bindings(
    .opendatasus_dataset = function(...) dataset,
    .package = "datasus"
  )

  resources <- opendatasus_recursos("arboviroses-dengue", cache = FALSE)

  expect_named(
    resources,
    c(
      "conjunto", "id", "nome", "descricao", "formato", "ano", "url",
      "tamanho", "criado", "modificado", "posicao"
    )
  )
  expect_identical(resources$ano, 2026L)
  expect_identical(resources$formato, "CSV")
  expect_identical(
    datasus_proveniencia(resources)$metadados_modificados,
    "2026-08-01T00:00:00"
  )
})
