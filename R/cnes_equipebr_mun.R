#' Scrapes CNES's Health Teams database at the city/region/state level
#'
#' This function allows the retrieval of data from CNES's
#' Health Teams database much in the same way done at the online portal.
#' The argument options refer to Health Teams characteristics and attributes,
#' allowing info at the municipality / city level.
#' @usage
#' cnes_equipebr_mun(linha = "Município", coluna = "Não ativa", conteudo = 1,
#'                   periodo = "last", regiao = "all", unidade_da_federacao = "all",
#'                   municipio = "all", municipio_gestor = "all", capital = "all",
#'                   cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all",
#'                   ride = "all", territorio_da_cidadania = "all", mesorregiao_pndr = "all",
#'                   amazonia_legal = "all", semiarido = "all", faixa_de_fronteira = "all",
#'                   zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
#'                   ensino_pesquisa = "all", natureza_juridica = "all",
#'                   esfera_juridica = "all", esfera_administrativa = "all",
#'                   natureza = "all", tipo_de_estabelecimento = "all",
#'                   tipo_de_gestao = "all", tipo_de_prestador = "all",
#'                   tipo_da_equipe = "all")
#' @param linha A character describing which element will be displayed in the rows of the data.frame. Defaults to "Município".
#' @param coluna A character describing which element will be displayed in the columns of the data.frame. Defaults to "Não ativa".
#' @param conteudo fixed to 1 since the only variable is quantity.
#' @param periodo A character vector describing the period of data. Defaults to the last available.
#' @param regiao Region
#' @param unidade_da_federacao State or Federative Unit
#' @param municipio "all" or a numeric vector with the IBGE's city codes codes to filter the data. Defaults to "all".
#' @param municipio_gestor "all" or a numeric vector with the IBGE's city codes codes to filter the data. Defaults to "all".
#' @param capital "all" or a numeric vector with the IBGE's state capital (city) codes codes to filter the data. Defaults to "all".
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
#' @param ensino_pesquisa "all" or a character vector with types of Research or Teaching activities from the institution to which the team belongs.
#' @param natureza_juridica "all" or a character vector with a selection of juridical nature of institutions to be filtered.
#' @param esfera_juridica "all" or a character vector with a selection os juridical spheres - Federal, corporate, etc. Refer to portal classifications.
#' @param esfera_administrativa "all" or a character vector, similar to esfera_juridica, old and simplified classification, data up to oct/2015
#' @param natureza "all" or a character vector, similar to natureza_juridica, old and simplified classification, data up to oct/2015
#' @param tipo_de_estabelecimento "all" or a character vector selecting types of facilities to filter by.
#' @param tipo_de_gestao "all" or a character vector with types of management such as "Dupla", "Estadual", "Municipal", "Sem gestão", "Não informado" to filter by.
#' @param tipo_de_prestador "all" or a character vector specifying types of supplier such as "Público", "Filantrópico", "Privado", "Sindicato", "Não informada" to filter by.
#' @param tipo_da_equipe "all" or a numeric vector with the types of health team to filter by.
#'
#' @return The function returns a data frame printed/filtered by parameters input.
#' @author Rodrigo E. S. Borges \email{<rodrigoesborges@@gmail.com>}
#' @seealso \code{\link{sim_evita10_mun}}
#' @examples
#' \dontrun{
#' ## Requesting data from the city of Teresina/PI
#' cnes_equipebr_mun(municipio = 221100)
#' }
#'
#' @keywords CNES datasus health_team
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export

cnes_equipebr_mun <- function(linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1,
                              periodo = "last", regiao = "all", unidade_da_federacao = "all",
                              municipio = "all", municipio_gestor = "all", capital = "all",
                              cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all",
                              ride = "all", territorio_da_cidadania = "all", mesorregiao_pndr = "all",
                              amazonia_legal = "all", semiarido = "all", faixa_de_fronteira = "all",
                              zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
                              ensino_pesquisa = "all", natureza_juridica = "all",
                              esfera_juridica = "all", esfera_administrativa = "all",
                              natureza = "all", tipo_de_estabelecimento = "all",
                              tipo_de_gestao = "all", tipo_de_prestador = "all",
                              tipo_da_equipe = "all") {

  page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?cnes/cnv/equipebr.def")

  #### DF ####
  linha.df <- data.frame(id = page %>% rvest::html_nodes("#L option") %>% rvest::html_text() %>% trimws(),
                         value = page %>% rvest::html_nodes("#L option") %>% rvest::html_attr("value"))
  linha.df[] <- lapply(linha.df, as.character)

  coluna.df <- data.frame(id = page %>% rvest::html_nodes("#C option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#C option") %>% rvest::html_attr("value"))
  coluna.df[] <- lapply(coluna.df, as.character)

  conteudo.df <- data.frame(id1 = 1,
                            id2 = "Quantidade",
                            value = "Quantidade")

  periodos.df <- data.frame(id = page %>% rvest::html_nodes("#A option") %>% rvest::html_text(),
                            value = page %>% rvest::html_nodes("#A option") %>% rvest::html_attr("value"))

  regiao.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_text()%>%trimws(),
                                               value = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_attr("value")))


  unidade_da_federacao.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_text()%>%trimws(),
                                                         value = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_attr("value")))



  municipios.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_text() %>% readr::parse_number(),
                                               name = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_text() %>% trimws()%>%
                                                 gsub(pattern="^\\d+ ",replacement=""),
                                               value = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_attr("value")))

  municipios.df[is.na(municipios.df$id),]$id <- municipios.df[is.na(municipios.df$id),]$value

  municipios_gestores.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_text() %>% readr::parse_number(),
                                               name = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_text() %>% trimws()%>%gsub(pattern="^\\d+ ",replacement=""),
                                               value = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_attr("value")))

  municipios_gestores.df[is.na(municipios_gestores.df$id),]$id <-
    municipios_gestores.df[is.na(municipios_gestores.df$id),]$value



  capital.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_text() %>% readr::parse_number(),
                                            value = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_attr("value")))


  cir.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_text() %>% readr::parse_number(),
                                        value = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_attr("value")))

  macrorregiao_de_saude.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                          value = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_attr("value")))

  microrregiao_ibge.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_text() ,
                                                      value = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_attr("value")))

  ride.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_text() %>% readr::parse_number(),
                                         value = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_attr("value")))

  territorio_da_cidadania.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                            value = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_attr("value")))

  mesorregiao_pndr.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                     value = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_attr("value")))

  amazonia_legal.df <- data.frame(id = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_attr("value"))
  amazonia_legal.df[] <- lapply(amazonia_legal.df, as.character)

  semiarido.df <- data.frame(id = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_text() %>% trimws(),
                             value = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_attr("value"))
  semiarido.df[] <- lapply(semiarido.df, as.character)

  faixa_de_fronteira.df <- data.frame(id = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_text() %>% trimws(),
                                      value = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_attr("value"))
  faixa_de_fronteira.df[] <- lapply(faixa_de_fronteira.df, as.character)

  zona_de_fronteira.df <- data.frame(id = page %>% rvest::html_nodes("#S15 option") %>% rvest::html_text() %>% trimws(),
                                     value = page %>% rvest::html_nodes("#S15 option") %>% rvest::html_attr("value"))
  zona_de_fronteira.df[] <- lapply(zona_de_fronteira.df, as.character)

  municipio_de_extrema_pobreza.df <- data.frame(id = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_text() %>% trimws(),
                                                value = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_attr("value"))
  municipio_de_extrema_pobreza.df[] <- lapply(municipio_de_extrema_pobreza.df, as.character)

  ensino_pesquisa.df <- data.frame(id = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_text() %>% trimws(),
                                   value = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_attr("value"))


  natureza_juridica.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_text() %>% trimws()%>%
                                       sub(pattern="-",replacement=".")%>%readr::parse_number()%>%
                                       sprintf(fmt="%.1f")%>%
                                       gsub(pattern="\\.",replacement = "-"),
                                     name = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_text() %>% trimws()%>%
                                       gsub(pattern="^\\d+-\\d+ ",replacement=""),
             value = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_attr("value"))
  )

  natureza_juridica.df[1,]$id=0
  natureza_juridica.df[natureza_juridica.df$id=="NA",]$id <-
    natureza_juridica.df[natureza_juridica.df$id=="NA",]$value

  esfera_juridica.df <- data.frame(id = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_text() %>% trimws(),
                                     value = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_attr("value"))

  esfera_administrativa.df <- data.frame(id = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_text() %>% trimws(),
                                   value = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_attr("value"))

  natureza.df <- data.frame(id = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_text() %>% trimws(),
                                         value = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_attr("value"))



  tipo_de_estabelecimento.df <- data.frame(
      id = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_text() %>% trimws(),
      value = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_attr("value"))

  tipo_de_gestao.df <- data.frame(
      id = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_text() %>% trimws(),
      value = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_attr("value"))

  tipo_de_prestador.df <- data.frame(
      id = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_text() %>% trimws(),
      value = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_attr("value"))

  tipo_da_equipe.df <- data.frame(
      id = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_text() %>% trimws(),
      value = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_attr("value"))


  regiao.df$id[1] <- unidade_da_federacao.df$id[1] <- municipios.df$id[1] <-
    municipios_gestores.df$id[1] <- capital.df$id[1] <- cir.df$id[1] <- macrorregiao_de_saude.df$id[1] <- microrregiao_ibge.df$id[1] <- "all"
  ride.df$id[1] <- territorio_da_cidadania.df$id[1] <- mesorregiao_pndr.df$id[1] <- amazonia_legal.df$id[1] <- semiarido.df$id[1] <- "all"
  faixa_de_fronteira.df$id[1] <- zona_de_fronteira.df$id[1] <- municipio_de_extrema_pobreza.df$id[1] <- "all"
  ensino_pesquisa.df$id[1]  <- natureza_juridica.df$id[1]  <- esfera_juridica.df$id[1]  <- esfera_administrativa.df$id[1]  <- "all"
  natureza.df$id[1]  <- tipo_de_estabelecimento.df$id[1]  <- tipo_de_gestao.df$id[1]  <-  tipo_de_prestador.df$id[1]  <-  "all"
  tipo_da_equipe.df$id[1]  <-  "all"

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

  if (conteudo != 1 ) {

    if (is.numeric(conteudo)) stop("The only numeric elements allowed is 1")

    if(length(conteudo) != 1) stop("The 'coluna' argument must have only one element")

    if (!(all(conteudo %in% conteudo.df$id2))) {

      if (!(all(conteudo %in% conteudo.df$value))) {

        stop("The 'conteudo' argument is misspecified")

      }

    }

  }

  if (periodo[1] != "last" & periodo[1] != "all") {

    # if (is.character(periodo)) {
    #   periodo <- as.numeric(periodo)
    # }

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
  suppressWarnings( if (periodo == "all") {periodo <- periodos.df$id} )
  form_periodo <- dplyr::filter(periodos.df, periodos.df$id %in% periodo)
  form_periodo <- paste0("Arquivos=", form_periodo$value, collapse = "&")


  #regiao
  form_regiao <- dplyr::filter(regiao.df, regiao.df$id %in% regiao)
  form_regiao <- paste0("SRegi%E3o=", form_regiao$value, collapse = "&")

  #unidade_da_federacao
  form_unidade_da_federacao <- dplyr::filter(unidade_da_federacao.df, unidade_da_federacao.df$id %in% unidade_da_federacao)
  form_unidade_da_federacao <- paste0("SUnidade_da_Federa%E7%E3o=", form_unidade_da_federacao$value, collapse = "&")

  form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"
  #municipio
  form_municipio <- dplyr::filter(municipios.df, municipios.df$id %in% municipio)
  form_municipio <- paste0("SMunic%EDpio=", form_municipio$value, collapse = "&")

  form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"

  #municipio
  form_municipio_gestor <- dplyr::filter(municipios_gestores.df, municipios_gestores.df$id %in% municipio_gestor)
  form_municipio_gestor <- paste0("SMunic%EDpio_gestor=", form_municipio_gestor$value, collapse = "&")

  form_pesqmes4 <- "pesqmes4=Digite+o+texto+e+ache+f%E1cil"

  #capital
  form_capital <- dplyr::filter(capital.df, capital.df$id %in% capital)
  form_capital <- paste0("SCapital=", form_capital$value, collapse = "&")

  form_pesqmes5 <- "pesqmes5=Digite+o+texto+e+ache+f%E1cil"

  #cir
  form_cir <- dplyr::filter(cir.df, cir.df$id %in% cir)
  form_cir <- paste0("SRegi%E3o_de_Sa%FAde_%28CIR%29=", form_cir$value, collapse = "&")

  form_pesqmes6 <- "pesqmes6=Digite+o+texto+e+ache+f%E1cil"

  #macrorregiao_de_saude
  form_macrorregiao_de_saude <- dplyr::filter(macrorregiao_de_saude.df, macrorregiao_de_saude.df$id %in% macrorregiao_de_saude)
  form_macrorregiao_de_saude <- paste0("SMacrorregi%E3o_de_Sa%FAde=", form_macrorregiao_de_saude$value, collapse = "&")

  form_pesqmes7 <- "pesqmes7=Digite+o+texto+e+ache+f%E1cil"

  #microrregiao_ibge
  form_microrregiao_ibge <- dplyr::filter(microrregiao_ibge.df, microrregiao_ibge.df$id %in% microrregiao_ibge)
  form_microrregiao_ibge <- paste0("SMicrorregi%E3o_IBGE=", form_microrregiao_ibge$value, collapse = "&")

  form_pesqmes8 <- "pesqmes8=Digite+o+texto+e+ache+f%E1cil"

  #ride
  form_ride <- dplyr::filter(ride.df, ride.df$id %in% ride)
  form_ride <- paste0("SRegi%E3o_Metropolitana_-_RIDE=", form_ride$value, collapse = "&")

  form_pesqmes9 <- "pesqmes9=Digite+o+texto+e+ache+f%E1cil"

  #territorio_da_cidadania
  form_territorio_da_cidadania <- dplyr::filter(territorio_da_cidadania.df, territorio_da_cidadania.df$id %in% territorio_da_cidadania)
  form_territorio_da_cidadania <- paste0("STerrit%F3rio_da_Cidadania=", form_territorio_da_cidadania$value, collapse = "&")

  form_pesqmes10 <- "pesqmes10=Digite+o+texto+e+ache+f%E1cil"

  #mesorregiao_pndr
  form_mesorregiao_pndr <- dplyr::filter(mesorregiao_pndr.df, mesorregiao_pndr.df$id %in% mesorregiao_pndr)
  form_mesorregiao_pndr <- paste0("SMesorregi%E3o_PNDR=", form_mesorregiao_pndr$value, collapse = "&")

  form_pesqmes11 <- "pesqmes11=Digite+o+texto+e+ache+f%E1cil"
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

  #ensino_pesquisa
  form_ensino_pesquisa <- dplyr::filter(
    ensino_pesquisa.df,ensino_pesquisa.df$id %in%
      ensino_pesquisa)
  form_ensino_pesquisa <- paste0("SEnsino/Pesquisa=",form_ensino_pesquisa$value,collapse="&")

  #natureza_juridica
  form_natureza_juridica <- dplyr::filter(natureza_juridica.df,natureza_juridica.df$id %in% natureza_juridica)
  form_natureza_juridica <- paste0("SNatureza_Jur%EDdica=", form_natureza_juridica$value, collapse = "&")

  form_pesqmes18 <- "pesqmes18=Digite+o+texto+e+ache+f%E1cil"

  #esfera_juridica
  form_esfera_juridica <- dplyr::filter(esfera_juridica.df,esfera_juridica.df$id %in% esfera_juridica)
  form_esfera_juridica <- paste0("SEsfera_Jur%EDdica=", form_esfera_juridica$value, collapse = "&")

  form_pesqmes19 <- "pesqmes19=Digite+o+texto+e+ache+f%E1cil"

  #esfera_administrativa
  form_esfera_administrativa <- dplyr::filter(esfera_administrativa.df,esfera_administrativa.df$id %in% esfera_administrativa)
  form_esfera_administrativa <- paste0("SEsfera_Administrativa=", form_esfera_administrativa$value, collapse = "&")

  #natureza
  form_natureza <- dplyr::filter(natureza.df,natureza.df$id %in% natureza)
  form_natureza <- paste0("SNatureza=", form_natureza$value, collapse = "&")

  form_pesqmes21 <- "pesqmes21=Digite+o+texto+e+ache+f%E1cil"

  #tipo_de_estabelecimento
  form_tipo_de_estabelecimento <- dplyr::filter(tipo_de_estabelecimento.df,
                                                tipo_de_estabelecimento.df$id %in%
                                                tipo_de_estabelecimento)
  form_tipo_de_estabelecimento <- paste0("STipo_de_Estabelecimento=",
                                         form_tipo_de_estabelecimento$value,
                                         collapse="&")
  form_pesqmes22 <- "pesqmes22=Digite+o+texto+e+ache+f%E1cil"

  #tipo_de_gestao
  form_tipo_de_gestao <- dplyr::filter(tipo_de_gestao.df,
                                                tipo_de_gestao.df$id %in%
                                                  tipo_de_gestao)
  form_tipo_de_gestao <- paste0("STipo_de_Gest%E3o=",
                                         form_tipo_de_gestao$value,
                                         collapse="&")


  #tipo_de_prestador
  form_tipo_de_prestador <- dplyr::filter(tipo_de_prestador.df,
                                                tipo_de_prestador.df$id %in%
                                                  tipo_de_prestador)
  form_tipo_de_prestador <- paste0("STipo_de_Prestador=",
                                         form_tipo_de_prestador$value,
                                         collapse="&")


  #tipo_da_equipe
  form_tipo_da_equipe <- dplyr::filter(tipo_da_equipe.df,
                                                tipo_da_equipe.df$id %in%
                                                  tipo_da_equipe)
  form_tipo_da_equipe <- paste0("STipo_da_Equipe=",
                                         form_tipo_da_equipe$value,
                                         collapse="&")
  form_pesqmes25 <- "pesqmes25=Digite+o+texto+e+ache+f%E1cil"


  form_data <- paste(
    form_linha, form_coluna, form_conteudo, form_periodo, form_regiao, form_unidade_da_federacao,
    form_pesqmes2, form_municipio, form_pesqmes3,form_municipio_gestor,
    form_pesqmes4,form_capital,form_pesqmes5,
    form_cir, form_pesqmes6, form_macrorregiao_de_saude, form_pesqmes7,
    form_microrregiao_ibge, form_pesqmes8, form_ride, form_pesqmes9,
    form_territorio_da_cidadania,
    form_pesqmes10, form_mesorregiao_pndr, form_pesqmes11, form_amazonia_legal,
    form_semiarido, form_faixa_de_fronteira,
    form_zona_de_fronteira, form_municipio_de_extrema_pobreza,
    form_ensino_pesquisa,form_natureza_juridica,
    form_pesqmes18,form_esfera_juridica,form_pesqmes19,
    form_esfera_administrativa,form_natureza,form_pesqmes21,
    form_tipo_de_estabelecimento,form_pesqmes22,
    form_tipo_de_gestao,form_tipo_de_prestador,
    form_tipo_da_equipe,form_pesqmes25,
     "formato=table&mostre=Mostra", sep = "&")

  form_data <- gsub("\\\\u00", "%", form_data)

  ##### REQUEST FORM AND DATA WRANGLING ####
  site <- httr::POST(url = "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/equipebr.def",
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
