#' Scrapes Demographic projections of Resident Population from RIPSA
#'
#' This function allows the user to retrieve data from
#' RIPSA database / IBGE Population Projections
#' much in the same way that is done
#' by the online portal. The argument options refer to
#' regions and population tipes
#'
#' @usage ibge_projpop_bruf(linha = "Unidade da Federação", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", regiao = "all", unidade_da_federacao = "all",
#'   sexo = "all", faixa_etaria_1 = "all", faixa_etaria_2 = "all")
#' @param linha A character describing which element will be displayed in the rows of the data.frame. Defaults to "Município".
#' @param coluna A character describing which element will be displayed in the columns of the data.frame. Defaults to "Não ativa".
#' @param conteudo A character of length = 1 with the state's acronym of interest.
#' @param periodo A character vector describing the period of data. Defaults to the last available.
#' @param regiao "all" or a numeric vector with the IBGE's region codes to filter the data. Defaults to "all".
#' @param unidade_da_federacao "all" or a numeric vector with the IBGE's state codes to filter the data. Defaults to "all".
#' @param sexo "all" or a character vector with the gender (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_1 "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_2 "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Rodrigo Borges based on work by Renato Prado Siqueira  \email{<rodrigo@@borges.net.br>}
#' @seealso \code{\link{novapop_popbr_mun}}
#' @examples
#' \dontrun{
#' ## Requesting data from the State of ES
#' ibge_projpop_bruf(unidade_da_federacao = 32)
#' }
#'
#' @keywords IBGE/RIPSA ProjPop datasus estimativas de população
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export

ibge_projpop_bruf <- function(linha = "Unidade da Federa\u00e7\u00e3o", coluna = "Faixa Et\u00e1ria 2",
                              conteudo = 1, periodo = "last", regiao = "all", unidade_da_federacao = "all",
                              sexo = "all", faixa_etaria_1 = "all", faixa_etaria_2 = "all") {

#ajuste do link para tabela de população
  page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?ibge/cnv/projpopuf.def")

  #### DF ####
  linha.df <- data.frame(id = page %>% rvest::html_nodes("#L option") %>% rvest::html_text() %>% trimws(),
                         value = page %>% rvest::html_nodes("#L option") %>% rvest::html_attr("value"))
  linha.df[] <- lapply(linha.df, as.character)

  coluna.df <- data.frame(id = page %>% rvest::html_nodes("#C option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#C option") %>% rvest::html_attr("value"))
  coluna.df[] <- lapply(coluna.df, as.character)

#ajuste do conteúdo da tabela
  conteudo.df <- data.frame(id1 = c(1,2,3),
                            id2 = c("Popula\u00e7\u00e3o_residente",
                                    "Popula\u00e7\u00e3o_residente \u0028\u0025 na linha\u0029",
                                    "Popula\u00e7\u00e3o_residente \u0028\u0025 na coluna\u0029"),
                            value = c("Popula\u00e7\u00e3o_residente",
                                      "Popula\u00e7\u00e3o_residente \u0028\u0025 na linha\u0029",
                                      "Popula\u00e7\u00e3o_residente \u0028\u0025 na coluna\u0029"))

  periodos.df <- data.frame(id = page %>% rvest::html_nodes("#A option") %>% rvest::html_text() %>% as.numeric(),
                            value = page %>% rvest::html_nodes("#A option") %>% rvest::html_attr("value"))
#adiciona regiao e uf cf tab e ref nac
  regiao.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_text() %>% readr::parse_number(),
                                           value = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_attr("value")))

  unidade_da_federacao.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_text() %>% trimws(),
                                                         value = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_attr("value")))
  unidade_da_federacao.df[] <- lapply(unidade_da_federacao.df, as.character)

  sexo.df <- data.frame(id = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_text() %>% trimws(),
                        value = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_attr("value"))
  sexo.df[] <- lapply(sexo.df, as.character)

#remoção de variáveis que não aparecem, adição de faixa_etaria_1 e 2

  faixa_etaria_1.df <- data.frame(id = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_attr("value"))
  faixa_etaria_1.df[] <- lapply(faixa_etaria_1.df, as.character)

  faixa_etaria_2.df <- data.frame(id = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_attr("value"))
  faixa_etaria_2.df[] <- lapply(faixa_etaria_2.df, as.character)


  regiao.df$id[1] <- unidade_da_federacao.df$id[1] <- "all"
  sexo.df$id[1] <- faixa_etaria_1.df$id[1]  <- faixa_etaria_2.df$id[1] <- "all"

  #### ERROR HANDLING ####
  if (linha != "Unidade da Federa\u00e7\u00e3o") {

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

  if (conteudo != 1 & conteudo != 2 & conteudo != 3) {

    if (is.numeric(conteudo)) stop("The only numeric elements allowed are 1 or 2")

    if(length(conteudo) != 1) stop("The 'coluna' argument must have only one element")

    if (!(all(conteudo %in% conteudo.df$id2))) {

      if (!(all(conteudo %in% conteudo.df$value))) {

        stop("The 'conteudo' argument is misspecified")

      }

    }

  }

  if (periodo[1] != "last") {

    if (is.character(periodo)) {
      periodo <- as.numeric(periodo)
    }

    if (!(all(periodo %in% periodos.df$id))) stop("The 'periodo' argument is misspecified")

  }
  #adicionada região e UF
  if (any(regiao != "all")) {

    regiao <- as.character(regiao)

    if (!(all(regiao %in% regiao.df$id))) stop("Some element in 'regiao' argument is wrong")

  }

  if (any(unidade_da_federacao != "all")) {

    unidade_da_federacao <- as.character(unidade_da_federacao)

    if (!(all(unidade_da_federacao %in% unidade_da_federacao.df$id))) stop("Some element in 'unidade_da_federacao' argument is wrong")

  }


#Trocado para faixa_etaria_1 e 2
  if (any(faixa_etaria_1 != "all")) {

    if (!(all(faixa_etaria_1 %in% faixa_etaria_1.df$id))) {

      faixa_etaria_1 <- as.character(faixa_etaria_1)

      if (!(all(faixa_etaria_1 %in% faixa_etaria_1.df$value))) {

        stop("Some element in 'faixa_etaria_1' argument is wrong")

      }

    }

  }

  if (any(faixa_etaria_2 != "all")) {

    if (!(all(faixa_etaria_2 %in% faixa_etaria_2.df$id))) {

      faixa_etaria_2 <- as.character(faixa_etaria_2)

      if (!(all(faixa_etaria_2 %in% faixa_etaria_2.df$value))) {

        stop("Some element in 'faixa_etaria_2' argument is wrong")

      }

    }

  }

  if (any(sexo != "all")) {

    if (!(all(sexo %in% sexo.df$id))) {

      sexo <- as.character(sexo)

      if (!(all(sexo %in% sexo.df$value))) {

        stop("Some element in 'sexo' argument is wrong")

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

  form_pesqmes1 <- "pesqmes1=Digite+o+texto+e+ache+f%E1cil"

  #regiao
  form_regiao <- dplyr::filter(regiao.df, regiao.df$id %in% regiao)
  form_regiao <- paste0("SRegi%E3o=", form_regiao$value, collapse = "&")

  form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"

  #unidade_da_federacao
  form_unidade_da_federacao <- dplyr::filter(unidade_da_federacao.df, unidade_da_federacao.df$id %in% unidade_da_federacao)
  form_unidade_da_federacao <- paste0("SUnidade_da_Federa%E7%E3o=", form_unidade_da_federacao$value, collapse = "&")

  form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"

  #faixa_etaria_1
  form_faixa_etaria_1 <- dplyr::filter(faixa_etaria_1.df, faixa_etaria_1.df$id %in% faixa_etaria_1)
  form_faixa_etaria_1 <- paste0("SFaixa_Et%E1ria_1=", form_faixa_etaria_1$value, collapse = "&")

  #faixa_etaria_2
  form_faixa_etaria_2 <- dplyr::filter(faixa_etaria_2.df, faixa_etaria_2.df$id %in% faixa_etaria_2)
  form_faixa_etaria_2 <- paste0("SFaixa_Et%E1ria_2=", form_faixa_etaria_2$value, collapse = "&")

  #sexo
  form_sexo <- dplyr::filter(sexo.df, sexo.df$id %in% sexo)
  form_sexo <- paste0("SSexo=", form_sexo$value, collapse = "&")

  form_data <- paste(form_linha, form_coluna, form_conteudo, form_periodo,
                     form_pesqmes1, form_regiao, form_pesqmes2,
                     form_unidade_da_federacao, form_pesqmes3, form_sexo,
                     form_faixa_etaria_1, form_faixa_etaria_2,
                     "formato=table&mostre=Mostra", sep = "&")

  form_data <- gsub("\\\\u00", "%", form_data)

  ##### REQUEST FORM AND DATA WRANGLING ####
  site <- httr::POST(url = "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/projpopuf.def",
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

