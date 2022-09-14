#' Scrapes SIA's SUS ambulatorial production by service location
#'
#' This function allows the user to retrieve data from
#' SIA-qabr's much in the same way that is done
#' by the online portal. The argument options refer to
#' ambulatorial procedures produced by service location or provider type
#' and quantity (presented and approved) or value types (value of increment,
#' of local policymaker complement, of federal government complement)
#' data focused on brazilian
#' cities and age ranging between 5-74 years old.
#'
#' @usage qabr_sia_mun(linha = "Município", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", municipio = "all", capital = "all",
#'   cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all",
#'   ride = "all", territorio_da_cidadania = "all", mesorregiao_pndr = "all",
#'   amazonia_legal = "all", semiarido = "all", faixa_de_fronteira = "all",
#'   zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
#'   procedimento = "all", grupo_procedimento = "all", subgrupo_procedimento = "all",
#'   forma_organizacao = "all", complexidade = "all", financiamento = "all",
#'   subtipo_financiamento = "all",regra_contratual = "all", carater_atendimento = "all",
#'   gestao = "all", documento_registro = "all", esfera_administrativa = "all",
#'   tipo_prestador = "all", natureza_juridica = "all", esfera_juridica = "all",
#'   aprovacao_producao = "all", profissional_cbo = "all" )
#' @param linha A character describing which element will be displayed in the rows of the data.frame. Defaults to "Município".
#' @param coluna A character describing which element will be displayed in the columns of the data.frame. Defaults to "Não ativa".
#' @param conteudo A character of length = 1 with the state's acronym of interest.
#' @param periodo A character vector describing the period of data. Defaults to the last available.
#' @param municipio "all" or a numeric vector with the IBGE's city codes codes to filter the data. Defaults to "all".
#' @param capital "all" or a numeric vector with the IBGE's cities codes to filter the data. Defaults to "all".
#' @param cir "all" or a numeric vector with the CIR's codes to filter the data. Defaults to "all".
#' @param macrorregiao_de_saude "all" or a numeric vector with the Health macro-region's codes to filter the data. Defaults to "all".
#' @param microrregiao_ibge "all" or a numeric vector with the IBGE's micro-region codes to filter the data. Defaults to "all".
#' @param ride "all" or a numeric vector with the IBGE's metropolitan-region codes to filter the data. Defaults to "all".
#' @param territorio_da_cidadania "all" or a numeric vector with the territory of citizenship codes to filter the data. Defaults to "all".
#' @param mesorregiao_pndr "all" or a numeric vector with the PNDR's mesoregion codes to filter the data. Defaults to "all".
#' @param amazonia_legal "all" or a character ("Sim" or "Não") indicating if only the Legal Amazon region must be included. Defaults to "all".
#' @param semiarido "all" or a character ("Sim" or "Não") indicating if only the semiarid region must be included. Defaults to "all".
#' @param faixa_de_fronteira "all" or a character ("Sim" or "Não") indicating if only the border area must be included. Defaults to "all".
#' @param zona_de_fronteira "all" or a character ("Sim" or "Não") indicating if only the border strip must be included. Defaults to "all".
#' @param municipio_de_extrema_pobreza "all" or a character ("Sim" or "Não") indicating if only the municipalities of extreme poverty must be included. Defaults to "all".
#' @param procedimento "all" or a character vector with the procedure or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param regra_contratual "all" or a character vector with the contractual rule (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param grupo_procedimento "all" or a numeric vector with the procedure group to filter the data. Defaults to "all".
#' @param subgrupo_procedimento "all" or a character vector with the procedure group to filter the data. Defaults to "all".
#' @param forma_organizacao "all" or a character vector with the organization_type or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param complexidade "all" or a character vector with procedure complexity or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param financiamento "all" or a character vector with financing form (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param subtipo_financiamento "all" or a character vector with the financing form subtype (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param regra_contratual "all" or a character vector with the contractual rule (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param carater_atendimento "all" or a character vector with the service character (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param gestao "all" or a character vector with the management (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param documento_registro "all" or a character vector with the registry document (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param esfera_administrativa "all" or a character vector with the administrative sphere (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_prestador "all" or a character vector with the service provider type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param natureza_juridica "all" or a character vector with the legal nature (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param esfera_juridica "all" or a character vector with the legal sphere (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param aprovacao_producao "all" or a character vector with the approval status (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param profissional_cbo "all" or a character vector with the professional occupation code according to Brazilian Occupations' Classifications (CBO, written in the same way as presented in the site) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Rodrigo Borges based on excellent work from Renato Prado Siqueira \email{<rodrigo@@borges.net.br>}
#' @seealso \code{\link{sim_evita10_mun}}
#' @examples
#' \dontrun{
#' ## Requesting data from the city of Campo Grande/MS
#' cnv_sih_mun(municipio = 500270)
#' }
#'
#' @keywords SIA datasus produção ambulatorial
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export

qabr_sia_mun <- function(linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", municipio = "all",
                         capital = "all", cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all", ride = "all",
                         territorio_da_cidadania = "all", mesorregiao_pndr = "all", amazonia_legal = "all", semiarido = "all",
                         faixa_de_fronteira = "all", zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
                         procedimento = "all", grupo_procedimento = "all", subgrupo_procedimento = "all", forma_organizacao = "all",
                         complexidade = "all", financiamento = "all", subtipo_financiamento = "all", regra_contratual = "all",
                         carater_atendimento = "all",gestao = "all",documento_registro = "all",esfera_administrativa = "all",
                         tipo_prestador = "all",natureza_juridica = "all",esfera_juridica = "all",aprovacao_producao = "all",profissional_cbo = "all") {


  page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sia/cnv/qabr.def", encoding = "ISO-8859-1")

  #### DF ####
  linha.df <- data.frame(id = page %>% rvest::html_nodes("#L option") %>% rvest::html_text() %>% trimws(),
                         value = page %>% rvest::html_nodes("#L option") %>% rvest::html_attr("value"))
  linha.df[] <- lapply(linha.df, as.character)

  coluna.df <- data.frame(id = page %>% rvest::html_nodes("#C option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#C option") %>% rvest::html_attr("value"))
  coluna.df[] <- lapply(coluna.df, as.character)

  conteudo.df <- data.frame(id1 = c(1:8),
                            id2 = page %>% rvest::html_nodes("#I option") %>% rvest::html_text() %>% trimws(),
                            value = page %>% rvest::html_nodes("#I option") %>% rvest::html_attr("value")
                            )

  periodos.df <- data.frame(id = page %>% rvest::html_nodes("#A option") %>% rvest::html_text() %>% trimws(),
                            value = page %>% rvest::html_nodes("#A option") %>% rvest::html_attr("value"))
  print(class(periodos.df$id))

  municipios.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_text() %>% readr::parse_number(),
                                               value = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_attr("value")))

  capital.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_text() %>% readr::parse_number(),
                                            value = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_attr("value")))

  cir.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_text() %>% readr::parse_number(),
                                        value = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_attr("value")))

  macrorregiao_de_saude.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                          value = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_attr("value")))

  microrregiao_ibge.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                      value = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_attr("value")))

  ride.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_text() %>% readr::parse_number(),
                                         value = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_attr("value")))

  territorio_da_cidadania.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                            value = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_attr("value")))

  mesorregiao_pndr.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                     value = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_attr("value")))

  amazonia_legal.df <- data.frame(id = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_attr("value"))
  amazonia_legal.df[] <- lapply(amazonia_legal.df, as.character)

  semiarido.df <- data.frame(id = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_text() %>% trimws(),
                             value = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_attr("value"))
  semiarido.df[] <- lapply(semiarido.df, as.character)

  faixa_de_fronteira.df <- data.frame(id = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_text() %>% trimws(),
                                      value = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_attr("value"))
  faixa_de_fronteira.df[] <- lapply(faixa_de_fronteira.df, as.character)

  zona_de_fronteira.df <- data.frame(id = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_text() %>% trimws(),
                                     value = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_attr("value"))
  zona_de_fronteira.df[] <- lapply(zona_de_fronteira.df, as.character)

  municipio_de_extrema_pobreza.df <- data.frame(id = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_text() %>% trimws(),
                                                value = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_attr("value"))
  municipio_de_extrema_pobreza.df[] <- lapply(municipio_de_extrema_pobreza.df, as.character)

  procedimento.df <- data.frame(id = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_attr("value"))
  procedimento.df[] <- lapply(procedimento.df, as.character)
  procedimento.df$id <- gsub(" .*$", "", procedimento.df$id)
  procedimento.df$id <- gsub("\\.", " ", procedimento.df$id) %>% trimws()
  procedimento.df$id <- gsub(" ", ".", procedimento.df$id)



  grupo_procedimento.df <- data.frame(id = 0:8,
                                    value = page %>% rvest::html_nodes("#S15 option") %>% rvest::html_attr("value"))
  grupo_procedimento.df[] <- lapply(grupo_procedimento.df, as.character)

  subgrupo_procedimento.df <- data.frame(id = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_attr("value"))
  subgrupo_procedimento.df[] <- lapply(subgrupo_procedimento.df, as.character)
  subgrupo_procedimento.df$id <- gsub(" .*$", "", subgrupo_procedimento.df$id)

  forma_organizacao.df <- data.frame(id = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_attr("value"))
  forma_organizacao.df[] <- lapply(forma_organizacao.df, as.character)

  complexidade.df <- data.frame(id = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_text() %>% trimws(),
                                          value = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_attr("value"))
  complexidade.df[] <- lapply(complexidade.df, as.character)

  financiamento.df <- data.frame(id = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_text() %>% trimws(),
                        value = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_attr("value"))
  financiamento.df[] <- lapply(financiamento.df, as.character)

  subtipo_financiamento.df <- data.frame(id = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_text() %>% trimws(),
                            value = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_attr("value"))

  subtipo_financiamento.df[] <- lapply(subtipo_financiamento.df, as.character)

  regra_contratual.df <- data.frame( id = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_text() %>% trimws(),
                                     value = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_attr("value") )

  regra_contratual.df[] <- lapply(regra_contratual.df, as.character)
  regra_contratual.df$id <- gsub(" .*$", "", regra_contratual.df$id)
  regra_contratual.df$id <- gsub("\\.", " ", regra_contratual.df$id) %>% trimws()
  regra_contratual.df$id <- gsub(" ", ".", regra_contratual.df$id)

  carater_atendimento.df <- data.frame(id = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_text() %>% trimws(),
                                       value = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_attr("value"))
  carater_atendimento.df[] <- lapply(carater_atendimento.df, as.character)
  carater_atendimento.df$id <- gsub(" .*$", "", carater_atendimento.df$id)
  carater_atendimento.df$id <- gsub("\\.", " ", carater_atendimento.df$id) %>% trimws()
  carater_atendimento.df$id <- gsub(" ", ".", carater_atendimento.df$id)

  gestao.df <- data.frame(id = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_text() %>% trimws(),
                                         value = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_attr("value"))

  gestao.df[] <- lapply(gestao.df, as.character)

  documento_registro.df <- data.frame(id = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_attr("value"))

  documento_registro.df[] <- lapply(documento_registro.df, as.character)

  esfera_administrativa.df <- data.frame(id = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_attr("value"))

  esfera_administrativa.df[] <- lapply(esfera_administrativa.df, as.character)

  tipo_prestador.df <- data.frame(id = page %>% rvest::html_nodes("#S26 option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#S26 option") %>% rvest::html_attr("value"))

  tipo_prestador.df[] <- lapply(tipo_prestador.df, as.character)

  natureza_juridica.df <- data.frame(id = page %>% rvest::html_nodes("#S27 option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#S27 option") %>% rvest::html_attr("value"))

  natureza_juridica.df[] <- lapply(natureza_juridica.df, as.character)

  esfera_juridica.df <- data.frame(id = page %>% rvest::html_nodes("#S28 option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#S28 option") %>% rvest::html_attr("value"))

  esfera_juridica.df[] <- lapply(esfera_juridica.df, as.character)

  aprovacao_producao.df <- data.frame(id = page %>% rvest::html_nodes("#S29 option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#S29 option") %>% rvest::html_attr("value"))

  aprovacao_producao.df[] <- lapply(aprovacao_producao.df, as.character)

  profissional_cbo.df <- data.frame(id = page %>% rvest::html_nodes("#S30 option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#S30 option") %>% rvest::html_attr("value"))

  profissional_cbo.df[] <- lapply(profissional_cbo.df, as.character)

  municipios.df$id[1] <- capital.df$id[1] <- cir.df$id[1] <- macrorregiao_de_saude.df$id[1] <- microrregiao_ibge.df$id[1] <- "all"
  territorio_da_cidadania.df$id[1] <- mesorregiao_pndr.df$id[1] <- amazonia_legal.df$id[1] <- semiarido.df$id[1] <- "all"
  faixa_de_fronteira.df$id[1] <- zona_de_fronteira.df$id[1] <- municipio_de_extrema_pobreza.df$id[1] <- "all"
  ride.df$id[1] <- grupo_procedimento.df$id[1] <- subgrupo_procedimento.df$id[1] <- "all"
  forma_organizacao.df$id[1] <- complexidade.df$id[1] <- financiamento.df$id[1] <- subtipo_financiamento.df$id[1] <- "all"
  procedimento.df$id[1] <- regra_contratual.df$id[1] <- carater_atendimento.df$id[1] <- gestao.df$id[1] <- documento_registro.df$id[1] <-
  esfera_administrativa.df$id[1] <- tipo_prestador.df$id[1] <- natureza_juridica.df$id[1] <- esfera_juridica.df$id[1] <-
    aprovacao_producao.df$id[1] <- profissional_cbo.df$id[1] <- "all"

  #### ERROR HANDLING ####
  if (linha != "Munic\u00edpio") {

    if (!is.character(linha)) stop("The 'linha' argument must be a character element")

    if(length(linha) != 1) stop("The 'linha' argument must have only one element")

    if (!(all(linha %in% linha.df$id))) {

      if (!(all(linha %in% linha.df$value))) {

        stop("The 'linha' argument is misspecified")

      }

    }

  }

  if (coluna != "N\u00e3o ativa") {

    if (!is.character(coluna)) stop("The 'coluna' argument must be a character element")

    if(length(coluna) != 1) stop("The 'coluna' argument must have only one element")

    if (!(all(coluna %in% coluna.df$id))) {

      if (!(all(coluna %in% coluna.df$value))) {

        stop("The 'coluna' argument is misspecified")

      }

    }

  }

  if (conteudo != 1 & conteudo > 15) {

    if (is.numeric(conteudo)) stop("The only numeric elements allowed are 1 to 15")

    if(length(conteudo) != 1) stop("The 'coluna' argument must have only one element")

    if (!(all(conteudo %in% conteudo.df$id2))) {

      if (!(all(conteudo %in% conteudo.df$value))) {

        stop("The 'conteudo' argument is misspecified")

      }

    }

  }

  if (periodo[1] != "last") {

    #if (is.character(periodo)) {
    #  periodo <- as.numeric(periodo)
    #}

    if (!(all(periodo %in% periodos.df$id))) stop("The 'periodo' argument is misspecified")

  }

  if (any(municipio != "all")) {

    municipio <- as.character(municipio)

    if (!(all(municipio %in% municipios.df$id))) stop("Some element in 'municipio' argument is wrong")

  }

  if (any(capital != "all")) {

    capital <- as.character(capital)

    if (!(all(capital %in% capital.df$id))) stop("Some element in 'capital' argument is wrong")

  }

  if (any(cir != "all")) {

    cir <- as.character(cir)

    if (!(all(cir %in% cir.df$id))) stop("Some element in 'cir' argument is wrong")

  }

  if (any(macrorregiao_de_saude != "all")) {

    macrorregiao_de_saude <- as.character(macrorregiao_de_saude)

    if (!(all(macrorregiao_de_saude %in% macrorregiao_de_saude.df$id))) stop("Some element in 'macrorregiao_de_saude' argument is wrong")

  }

  if (any(microrregiao_ibge != "all")) {

    microrregiao_ibge <- as.character(microrregiao_ibge)

    if (!(all(microrregiao_ibge %in% microrregiao_ibge.df$id))) stop("Some element in 'microrregiao_ibge' argument is wrong")

  }

  if (any(ride != "all")) {

    ride <- as.character(ride)

    if (!(all(ride %in% ride.df$id))) stop("Some element in 'ride' argument is wrong")

  }

  if (any(territorio_da_cidadania != "all")) {

    territorio_da_cidadania <- as.character(as.numeric(territorio_da_cidadania))

    if (!(all(territorio_da_cidadania %in% territorio_da_cidadania.df$id))) stop("Some element in 'territorio_da_cidadania' argument is wrong")

  }

  if (any(mesorregiao_pndr != "all")) {

    mesorregiao_pndr <- as.character(as.numeric(mesorregiao_pndr))

    if (!(all(mesorregiao_pndr %in% mesorregiao_pndr.df$id))) stop("Some element in 'mesorregiao_pndr' argument is wrong")

  }

  if (any(amazonia_legal != "all")) {

    amazonia_legal <- as.character(amazonia_legal)

    if (!(all(amazonia_legal %in% amazonia_legal.df$id))) stop("The element in 'amazonia_legal' argument is wrong")

  }

  if (any(semiarido != "all")) {

    semiarido <- as.character(semiarido)

    if (!(all(semiarido %in% semiarido.df$id))) stop("The element in 'semiarido' argument is wrong")

  }

  if (any(faixa_de_fronteira != "all")) {

    faixa_de_fronteira <- as.character(faixa_de_fronteira)

    if (!(all(faixa_de_fronteira %in% faixa_de_fronteira.df$id))) stop("The element in 'faixa_de_fronteira' argument is wrong")

  }

  if (any(zona_de_fronteira != "all")) {

    zona_de_fronteira <- as.character(zona_de_fronteira)

    if (!(all(zona_de_fronteira %in% zona_de_fronteira.df$id))) stop("The element in 'zona_de_fronteira' argument is wrong")

  }

  if (any(municipio_de_extrema_pobreza != "all")) {

    municipio_de_extrema_pobreza <- as.character(municipio_de_extrema_pobreza)

    if (!(all(municipio_de_extrema_pobreza %in% municipio_de_extrema_pobreza.df$id))) stop("The element in 'municipio_de_extrema_pobreza' argument is wrong")

  }

  if (any(procedimento != "all")) {

    procedimento <- as.character(procedimento)

    if (!(all(procedimento %in% procedimento.df$id))) {

      procedimento <- as.character(procedimento)

      if (!(all(procedimento %in% procedimento.df$value))) {

        stop("Some element in 'procedimento' argument is wrong")

      }

    }

  }

      if (any(regra_contratual != "all")) {

      regra_contratual <- as.character(regra_contratual)

      if (!(all(regra_contratual %in% regra_contratual.df$id))) {

        regra_contratual <- as.character(regra_contratual)

        if (!(all(regra_contratual %in% regra_contratual.df$value))) {

          stop("Some element in 'regra_contratual' argument is wrong")

        }

      }

    }

  if (any(grupo_procedimento != "all")) {

    grupo_procedimento <- as.character(grupo_procedimento)

    if (!(all(grupo_procedimento %in% grupo_procedimento.df$id))) stop("Some element in 'grupo_procedimento' argument is wrong")

  }

  if (any(subgrupo_procedimento != "all")) {

    subgrupo_procedimento <- as.character(subgrupo_procedimento)

    if (!(all(subgrupo_procedimento %in% subgrupo_procedimento.df$id))) stop("Some element in 'subgrupo_procedimento' argument is wrong")

  }

  if (any(forma_organizacao != "all")) {

    if (!(all(forma_organizacao %in% forma_organizacao.df$id))) {

      forma_organizacao <- as.character(forma_organizacao)

      if (!(all(forma_organizacao %in% forma_organizacao.df$value))) {

        stop("Some element in 'forma_organizacao' argument is wrong")

      }

    }

  }

  if (any(complexidade != "all")) {

    if (!(all(complexidade %in% complexidade.df$id))) {

      complexidade <- as.character(complexidade)

      if (!(all(complexidade %in% complexidade.df$value))) {

        stop("Some element in 'complexidade' argument is wrong")

      }

    }

  }

  if (any(financiamento != "all")) {

    if (!(all(financiamento %in% financiamento.df$id))) {

      financiamento <- as.character(financiamento)

      if (!(all(financiamento %in% financiamento.df$value))) {

        stop("Some element in 'financiamento' argument is wrong")

      }

    }

  }

  if (any(subtipo_financiamento != "all")) {

    if (!(all(subtipo_financiamento %in% subtipo_financiamento.df$id))) {

      subtipo_financiamento <- as.character(subtipo_financiamento)

      if (!(all(subtipo_financiamento %in% subtipo_financiamento.df$value))) {

        stop("Some element in 'subtipo_financiamento' argument is wrong")

      }

    }

  }



  #### FILTERS APPLICATIONS ####

  #linha
  if (linha %in% linha.df$id) {
    linha <- dplyr::filter(linha.df, linha.df$id %in% linha)
    linha <- linha$value
  }

  if (!stringi::stri_enc_isascii(linha)) {
    form_linha <- paste0("Linha=", stringi::stri_escape_unicode(linha))
  } else {
    form_linha <- paste0("Linha=", linha)
  }

  #coluna
  if (coluna %in% coluna.df$id) {
    coluna <- dplyr::filter(coluna.df, coluna.df$id %in% coluna)
    coluna <- coluna$value
  }

  if (!stringi::stri_enc_isascii(coluna)) {
    form_coluna <- paste0("Coluna=", stringi::stri_escape_unicode(coluna))
  } else {
    form_coluna <- paste0("Coluna=", coluna)
  }

  #conteudo
  form_conteudo <- conteudo.df$value[conteudo]
  if (!stringi::stri_enc_isascii(form_conteudo)) {
    form_conteudo <- paste0("Incremento=", stringi::stri_escape_unicode(form_conteudo))
  } else {
    form_conteudo <- paste0("Incremento=", form_conteudo)
  }

  #periodo
  suppressWarnings( if (periodo == "last") {periodo <- utils::head(periodos.df$id, 1)} )
  form_periodo <- dplyr::filter(periodos.df, periodos.df$id %in% periodo)

  form_periodo <- paste0("Arquivos=", form_periodo$value, collapse = "&")

  #municipio
  form_pesqmes1 <- "pesqmes1=Digite+o+texto+e+ache+f%E1cil"

  #municipio
  form_municipio <- dplyr::filter(municipios.df, municipios.df$id %in% municipio)
  form_municipio <- paste0("SMunic%EDpio=", form_municipio$value, collapse = "&")

  #capital
  form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"

  #capital
  form_capital <- dplyr::filter(capital.df, capital.df$id %in% capital)
  form_capital <- paste0("SCapital=", form_capital$value, collapse = "&")

  form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"

  #cir
  form_cir <- dplyr::filter(cir.df, cir.df$id %in% cir)
  form_cir <- paste0("SRegi%E3o_de_Sa%FAde_%28CIR%29=", form_cir$value, collapse = "&")

  form_pesqmes4 <- "pesqmes4=Digite+o+texto+e+ache+f%E1cil"

  #macrorregiao_de_saude
  form_macrorregiao_de_saude <- dplyr::filter(macrorregiao_de_saude.df, macrorregiao_de_saude.df$id %in% macrorregiao_de_saude)
  form_macrorregiao_de_saude <- paste0("SMacrorregi%E3o_de_Sa%FAde=", form_macrorregiao_de_saude$value, collapse = "&")

  form_pesqmes5 <- "pesqmes5=Digite+o+texto+e+ache+f%E1cil"

  #microrregiao_ibge
  form_microrregiao_ibge <- dplyr::filter(microrregiao_ibge.df, microrregiao_ibge.df$id %in% microrregiao_ibge)
  form_microrregiao_ibge <- paste0("SMicrorregi%E3o_IBGE=", form_microrregiao_ibge$value, collapse = "&")

  form_pesqmes6 <- "pesqmes6=Digite+o+texto+e+ache+f%E1cil"

  #ride
  form_ride <- dplyr::filter(ride.df, ride.df$id %in% ride)
  form_ride <- paste0("SRegi%E3o_Metropolitana_-_RIDE=", form_ride$value, collapse = "&")

  form_pesqmes7 <- "pesqmes7=Digite+o+texto+e+ache+f%E1cil"

  #territorio_da_cidadania
  form_territorio_da_cidadania <- dplyr::filter(territorio_da_cidadania.df, territorio_da_cidadania.df$id %in% territorio_da_cidadania)
  form_territorio_da_cidadania <- paste0("STerrit%F3rio_da_Cidadania=", form_territorio_da_cidadania$value, collapse = "&")

  form_pesqmes8 <- "pesqmes8=Digite+o+texto+e+ache+f%E1cil"

  #mesorregiao_pndr
  form_mesorregiao_pndr <- dplyr::filter(mesorregiao_pndr.df, mesorregiao_pndr.df$id %in% mesorregiao_pndr)
  form_mesorregiao_pndr <- paste0("SMesorregi%E3o_PNDR=", form_mesorregiao_pndr$value, collapse = "&")

  #amazonia_legal
  form_amazonia_legal <- dplyr::filter(amazonia_legal.df, amazonia_legal.df$id %in% amazonia_legal)
  form_amazonia_legal <- paste0("SAmaz%F4nia_Legal=", form_amazonia_legal$value, collapse = "&")

  #semiarido
  form_semiarido <- dplyr::filter(semiarido.df, semiarido.df$id %in% semiarido)
  form_semiarido <- paste0("SSemi%E1rido=", form_semiarido$value, collapse = "&")

  #faixa_de_fronteira
  form_faixa_de_fronteira <- dplyr::filter(faixa_de_fronteira.df, faixa_de_fronteira.df$id %in% faixa_de_fronteira)
  form_faixa_de_fronteira <- paste0("SFaixa_de_Fronteira=", form_faixa_de_fronteira$value, collapse = "&")

  #zona_de_fronteira
  form_zona_de_fronteira <- dplyr::filter(zona_de_fronteira.df, zona_de_fronteira.df$id %in% zona_de_fronteira)
  form_zona_de_fronteira <- paste0("SZona_de_Fronteira=", form_zona_de_fronteira$value, collapse = "&")

  #municipio_de_extrema_pobreza
  form_municipio_de_extrema_pobreza <- dplyr::filter(municipio_de_extrema_pobreza.df, municipio_de_extrema_pobreza.df$id %in% municipio_de_extrema_pobreza)
  form_municipio_de_extrema_pobreza <- paste0("SMunic%EDpio_de_extrema_pobreza=", form_municipio_de_extrema_pobreza$value, collapse = "&")

  #procedimento
  form_procedimento <- dplyr::filter(procedimento.df, procedimento.df$id %in% procedimento)
  form_procedimento <- paste0("SProcedimento=", form_procedimento$value, collapse = "&")

  form_pesqmes14 <- "pesqmes14=Digite+o+texto+e+ache+f%E1cil"

  #grupo_procedimento
  form_grupo_procedimento <- dplyr::filter(grupo_procedimento.df, grupo_procedimento.df$id %in% grupo_procedimento)
  form_grupo_procedimento <- paste0("SGrupo_procedimento=", form_grupo_procedimento$value, collapse = "&")



  #subgrupo_procedimento
  form_subgrupo_procedimento <- dplyr::filter(subgrupo_procedimento.df, subgrupo_procedimento.df$id %in% subgrupo_procedimento)
  form_subgrupo_procedimento <- paste0("SSubgrupo_proced.=", form_subgrupo_procedimento$value, collapse = "&")

  form_pesqmes16 <- "pesqmes16=Digite+o+texto+e+ache+f%E1cil"

  #forma_organizacao
  form_forma_organizacao <- dplyr::filter(forma_organizacao.df, forma_organizacao.df$id %in% forma_organizacao)
  form_forma_organizacao <- paste0("SForma_organiza%E7%E3o=", form_forma_organizacao$value, collapse = "&")

  form_pesqmes17 <- "pesqmes17=Digite+o+texto+e+ache+f%E1cil"

  #complexidade
  form_complexidade <- dplyr::filter(complexidade.df, complexidade.df$id %in% complexidade)
  form_complexidade <- paste0("SComplexidade=", form_complexidade$value, collapse = "&")

  #financiamento
  form_financiamento <- dplyr::filter(financiamento.df, financiamento.df$id %in% financiamento)
  form_financiamento <- paste0("SFinanciamento=", form_financiamento$value, collapse = "&")

  #subtipo_financiamento
  form_subtipo_financiamento <- dplyr::filter(subtipo_financiamento.df, subtipo_financiamento.df$id %in% subtipo_financiamento)
  form_subtipo_financiamento <- paste0("SSubtp_Financiament=", form_subtipo_financiamento$value, collapse = "&")

  form_pesqmes20 <- "pesqmes20=Digite+o+texto+e+ache+f%E1cil"

  #regra_contratual
  form_regra_contratual <- dplyr::filter(regra_contratual.df,regra_contratual.df$id %in% regra_contratual)
  form_regra_contratual <- paste0("SRegra_contratual=", form_regra_contratual$value, collapse = "&")

  form_pesqmes21 <- "pesqmes21=Digite+o+texto+e+ache+f%E1cil"

  #carater_atendimento
  form_carater_atendimento <- dplyr::filter(carater_atendimento.df,carater_atendimento.df$id %in% carater_atendimento)
  form_carater_atendimento <- paste0("SCar%E1ter_Atendiment=", form_carater_atendimento$value, collapse = "&")



  #gestao
  form_gestao <- dplyr::filter(gestao.df,gestao.df$id %in% gestao)
  form_gestao <- paste0("SGest%E3o=", form_gestao$value, collapse = "&")


  #documento_registro
  form_documento_registro <- dplyr::filter(documento_registro.df,documento_registro.df$id %in% documento_registro)
  form_documento_registro <- paste0("SDocumento_registro=", form_documento_registro$value, collapse = "&")


  #esfera_administrativa
  form_esfera_administrativa <- dplyr::filter(esfera_administrativa.df,esfera_administrativa.df$id %in% esfera_administrativa)
  form_esfera_administrativa <- paste0("SEsfera_administrat=", form_esfera_administrativa$value, collapse = "&")


  #tipo_prestador
  form_tipo_prestador <- dplyr::filter(tipo_prestador.df,tipo_prestador.df$id %in% tipo_prestador)
  form_tipo_prestador <- paste0("STipo_de_prestador=", form_tipo_prestador$value, collapse = "&")

  #natureza_juridica
  form_natureza_juridica <- dplyr::filter(natureza_juridica.df,natureza_juridica.df$id %in% natureza_juridica)
  form_natureza_juridica <- paste0("SNatureza_Jur%EDdica=", form_natureza_juridica$value, collapse = "&")

  form_pesqmes27 <- "pesqmes27=Digite+o+texto+e+ache+f%E1cil"
  #esfera_juridica
  form_esfera_juridica <- dplyr::filter(esfera_juridica.df,esfera_juridica.df$id %in% esfera_juridica)
  form_esfera_juridica <- paste0("SEsfera_Jur%EDdica=", form_esfera_juridica$value, collapse = "&")

  form_pesqmes28 <- "pesqmes28=Digite+o+texto+e+ache+f%E1cil"
  #aprovacao_producao
  form_aprovacao_producao <- dplyr::filter(aprovacao_producao.df,aprovacao_producao.df$id %in% aprovacao_producao)
  form_aprovacao_producao <- paste0("SAprova%E7%E3o_produ%E7%E3o=", form_aprovacao_producao$value, collapse = "&")

    #profissional_cbo

  form_profissional_cbo <- dplyr::filter(profissional_cbo.df,profissional_cbo.df$id %in% profissional_cbo)
  form_profissional_cbo <- paste0("SProfissional_-_CBO=", form_profissional_cbo$value, collapse = "&")

  form_pesqmes30 <- "pesqmes30=Digite+o+texto+e+ache+f%E1cil"

  form_data <- paste(form_linha, form_coluna, form_conteudo, form_periodo, form_pesqmes1, form_municipio,
                     form_pesqmes2, form_capital, form_pesqmes3, form_cir, form_pesqmes4, form_macrorregiao_de_saude,
                     form_pesqmes5, form_microrregiao_ibge, form_pesqmes6, form_ride, form_pesqmes7,
                     form_territorio_da_cidadania, form_pesqmes8, form_mesorregiao_pndr, form_amazonia_legal,
                     form_semiarido, form_faixa_de_fronteira, form_zona_de_fronteira, form_municipio_de_extrema_pobreza,
                     form_procedimento, form_pesqmes14, form_grupo_procedimento, form_pesqmes16, form_subgrupo_procedimento,
                     form_pesqmes17,form_forma_organizacao, form_complexidade, form_financiamento, form_pesqmes20,form_subtipo_financiamento,
                     form_pesqmes21,form_regra_contratual,form_carater_atendimento,form_gestao,form_documento_registro,form_esfera_administrativa,
                     form_tipo_prestador,form_pesqmes27,form_natureza_juridica,form_pesqmes28,form_esfera_juridica,form_aprovacao_producao,
                     form_pesqmes30,form_profissional_cbo,
                     "formato=table&mostre=Mostra", sep = "&")

  form_data <- gsub("\\\\u00", "%", form_data)

  ##### REQUEST FORM AND DATA WRANGLING ####
  site <- httr::POST(url = "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sia/cnv/qabr.def",
                     body = form_data)

  tabdados <- httr::content(site, encoding = "Latin1") %>%
    rvest::html_nodes(".tabdados tbody td") %>%
    rvest::html_text() %>%
    trimws()

  col_tabdados <- httr::content(site, encoding = "Latin1") %>%
    rvest::html_nodes("th") %>%
    rvest::html_text() %>%
    trimws()


  f1 <- function(x) x <- gsub("\\.", "", x)
  f2 <- function(x) x <- as.numeric(as.character(x))

  tabela_final <- as.data.frame(matrix(data = tabdados, nrow = length(tabdados)/length(col_tabdados),
                                       ncol = length(col_tabdados), byrow = TRUE))

  names(tabela_final) <- col_tabdados

  tabela_final[-1] <- lapply(tabela_final[-1], f1)
  tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1], f2))

  tabela_final

}

