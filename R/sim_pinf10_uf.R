#' Scrapes SIM's ICD-10 child mortality data from ufs
#'
#' This function allows the user to retrive data from
#' SIM's ICD-10 database much in the same way that is done
#' by the online portal. The argument options refer to
#' child mortality data from the states unities.
#'
#' @usage sim_pinf10_uf(uf, linha = "Município", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", municipio = "all", cir = "all",
#'   macrorregiao_de_saude = "all", divisao_admnist_estadual = "all",
#'   microrregiao_ibge = "all", ride = "all", capitulo_cid10 = "all",
#'   categoria_cid10 = "all", lista_mort_cid10 = "all",
#'   causa_mal_definidas = "all", faixa_etaria_1 = "all",
#'   faixa_etaria_2 = "all", faixa_etaria_3 = "all", faixa_etaria_4 = "all",
#'   faixa_etaria_5 = "all", faixa_etaria_detalhada = "all",
#'   sexo = "all", cor_raca = "all", local_ocorrencia = "all",
#'   idade_mae = "all", escolaridade_mae = "all",
#'   duracao_gestacao = "all", tipo_gravidez = "all", tipo_parto = "all",
#'   peso_ao_nascer = "all", obito_relacao_parto = "all",
#'   obito_investigado = "all")
#' @param uf A character of length = 1 with the state"all".
#' @param linha A character describing which element will be displayed in the rows of the data.frame. Defaults to "Município".
#' @param coluna A character describing which element will be displayed in the columns of the data.frame. Defaults to "Não ativa".
#' @param conteudo A character of length = 1 with the state's acronym of interest.
#' @param periodo A character vector describing the period of data. Defaults to the last available.
#' @param municipio "all" or a numeric vector with the IBGE's city codes codes to filter the data. Defaults to "all".
#' @param cir "all" or a numeric vector with the CIR's codes to filter the data. Defaults to "all".
#' @param macrorregiao_de_saude "all" or a numeric vector with the Health macro-region's codes to filter the data. Defaults to "all".
#' @param divisao_admnist_estadual "all" or a numeric vector with the State administrative division's codes to filter the data. Defaults to "all".
#' @param microrregiao_ibge "all" or a numeric vector with the IBGE's micro-region codes to filter the data. Defaults to "all".
#' @param ride "all" or a numeric vector with the IBGE's metropolitan-region codes to filter the data. Defaults to "all".
#' @param capitulo_cid10 "all" or a numeric vector with the ICD-10 chapter to filter the data. Defaults to "all".
#' @param categoria_cid10 "all" or a character vector with the ICD-10 category codes (capital letter and two numbers) to filter the data. Defaults to "all".
#' @param lista_mort_cid10 "all" or a character vector with the mortality type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param causa_mal_definidas "all" or a character vector with the ill-defined causes (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_1 "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_2 "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_3 "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_4 "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_5 "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_detalhada "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param sexo "all" or a character vector with the gender (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param cor_raca "all" or a character vector with the color/race (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param local_ocorrencia "all" or a character vector with the place of ocurrence to filter the data. Defaults to "all".
#' @param idade_mae "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param escolaridade_mae "all" or a character vector with the mother's instruction (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param duracao_gestacao "all" or a character vector with the marital status of the mother (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_gravidez "all" or a character vector with the type of pregnancy (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_parto "all" or a character vector with the Parturition type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param peso_ao_nascer "all" or a character vector with the birth weight (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param obito_relacao_parto "all" or a character vector with period of child mortality in relation to childbirth (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param obito_investigado "all" or a character vector indicating if the death was investigated (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{sim_evita10_uf}}
#' @examples
#' \dontrun{
#' ## Requesting data from the state of Mato Grosso do Sul
#' sim_pinf10_uf(uf = "ms")
#' }
#'
#' @keywords SIM datasus child mortality
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export

sim_pinf10_uf <- function(uf, linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", municipio = "all", cir = "all",
                         macrorregiao_de_saude = "all", divisao_admnist_estadual = "all", microrregiao_ibge = "all", ride = "all",
                         capitulo_cid10 = "all", categoria_cid10 = "all", lista_mort_cid10 = "all", causa_mal_definidas = "all", faixa_etaria_1 = "all",
                         faixa_etaria_2 = "all", faixa_etaria_3 = "all", faixa_etaria_4 = "all", faixa_etaria_5 = "all",faixa_etaria_detalhada = "all",
                         sexo = "all", cor_raca = "all", local_ocorrencia = "all", idade_mae = "all", escolaridade_mae = "all",
                         duracao_gestacao = "all", tipo_gravidez = "all", tipo_parto = "all", peso_ao_nascer = "all", obito_relacao_parto = "all",
                         obito_investigado = "all") {


  page <- xml2::read_html(paste0("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/pinf10",uf,".def"))

  #### DF ####
  linha.df <- data.frame(id = page %>% rvest::html_nodes("#L option") %>% rvest::html_text() %>% trimws(),
                         value = page %>% rvest::html_nodes("#L option") %>% rvest::html_attr("value"))
  linha.df[] <- lapply(linha.df, as.character)

  coluna.df <- data.frame(id = page %>% rvest::html_nodes("#C option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#C option") %>% rvest::html_attr("value"))
  coluna.df[] <- lapply(coluna.df, as.character)

  conteudo.df <- data.frame(id1 = c(1, 2),
                            id2 = c("\u00d3bitos_p/Resid\u00eanc", "\u00d3bitos_p/Ocorr\u00eanc"),
                            value = c("\u00d3bitos_p/Resid\u00eanc", "\u00d3bitos_p/Ocorr\u00eanc"))

  periodos.df <- data.frame(id = page %>% rvest::html_nodes("#A option") %>% rvest::html_text() %>% as.numeric(),
                            value = page %>% rvest::html_nodes("#A option") %>% rvest::html_attr("value"))

  municipios.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_text() %>% readr::parse_number(),
                                               value = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_attr("value")))

  cir.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_text() %>% readr::parse_number(),
                                        value = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_attr("value")))

  macrorregiao_de_saude.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                          value = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_attr("value")))

  divisao_admnist_estadual.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                             value = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_attr("value")))

  microrregiao_ibge.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                      value = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_attr("value")))

  ride.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_text() %>% readr::parse_number(),
                                         value = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_attr("value")))

  capitulo_cid10.df <- data.frame(id = 0:22,
                                    value = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_attr("value"))
  capitulo_cid10.df[] <- lapply(capitulo_cid10.df, as.character)

  categoria_cid10.df <- data.frame(id = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_attr("value"))
  categoria_cid10.df[] <- lapply(categoria_cid10.df, as.character)
  categoria_cid10.df$id <- gsub(" .*$", "", categoria_cid10.df$id)

  lista_mort_cid10.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_text() %>% trimws(),
                                                     value = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_attr("value")))
  lista_mort_cid10.df[] <- lapply(lista_mort_cid10.df, as.character)

  causa_mal_definidas.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_text() %>% trimws(),
                                                        value = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_attr("value")))
  causa_mal_definidas.df[] <- lapply(causa_mal_definidas.df, as.character)

  faixa_etaria_1.df <- data.frame(id = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_attr("value"))
  faixa_etaria_1.df[] <- lapply(faixa_etaria_1.df, as.character)

  faixa_etaria_2.df <- data.frame(id = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_attr("value"))
  faixa_etaria_2.df[] <- lapply(faixa_etaria_2.df, as.character)

  faixa_etaria_3.df <- data.frame(id = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_attr("value"))
  faixa_etaria_3.df[] <- lapply(faixa_etaria_3.df, as.character)

  faixa_etaria_4.df <- data.frame(id = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_attr("value"))
  faixa_etaria_4.df[] <- lapply(faixa_etaria_4.df, as.character)

  faixa_etaria_5.df <- data.frame(id = page %>% rvest::html_nodes("#S15 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S15 option") %>% rvest::html_attr("value"))
  faixa_etaria_5.df[] <- lapply(faixa_etaria_5.df, as.character)

  faixa_etaria_detalhada.df <- data.frame(id = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_text() %>% trimws(),
                                          value = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_attr("value"))
  faixa_etaria_detalhada.df[] <- lapply(faixa_etaria_detalhada.df, as.character)

  sexo.df <- data.frame(id = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_text() %>% trimws(),
                        value = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_attr("value"))
  sexo.df[] <- lapply(sexo.df, as.character)

  cor_raca.df <- data.frame(id = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_text() %>% trimws(),
                            value = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_attr("value"))
  cor_raca.df[] <- lapply(cor_raca.df, as.character)

  local_ocorrencia.df <- data.frame(id = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_attr("value"))
  local_ocorrencia.df[] <- lapply(local_ocorrencia.df, as.character)

  idade_mae.df <- data.frame(id = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_attr("value"))
  idade_mae.df[] <- lapply(idade_mae.df, as.character)

  escolaridade_mae.df <- data.frame(id = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_text() %>% trimws(),
                                       value = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_attr("value"))
  escolaridade_mae.df[] <- lapply(escolaridade_mae.df, as.character)

  duracao_gestacao.df <- data.frame(id = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_text() %>% trimws(),
                                       value = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_attr("value"))
  duracao_gestacao.df[] <- lapply(duracao_gestacao.df, as.character)

  tipo_gravidez.df <- data.frame(id = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_attr("value"))
  tipo_gravidez.df[] <- lapply(tipo_gravidez.df, as.character)

  tipo_parto.df <- data.frame(id = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_attr("value"))
  tipo_parto.df[] <- lapply(tipo_parto.df, as.character)

  peso_ao_nascer.df <- data.frame(id = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_text() %>% trimws(),
                                 value = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_attr("value"))
  peso_ao_nascer.df[] <- lapply(peso_ao_nascer.df, as.character)

  obito_relacao_parto.df <- data.frame(id = page %>% rvest::html_nodes("#S26 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S26 option") %>% rvest::html_attr("value"))
  obito_relacao_parto.df[] <- lapply(obito_relacao_parto.df, as.character)

  obito_investigado.df <- data.frame(id = page %>% rvest::html_nodes("#S27 option") %>% rvest::html_text() %>% trimws(),
                                       value = page %>% rvest::html_nodes("#S27 option") %>% rvest::html_attr("value"))
  obito_investigado.df[] <- lapply(obito_investigado.df, as.character)

  municipios.df$id[1] <- cir.df$id[1] <- macrorregiao_de_saude.df$id[1] <- divisao_admnist_estadual.df$id[1] <- microrregiao_ibge.df$id[1] <- "all"
  ride.df$id[1] <- local_ocorrencia.df$id[1]<- capitulo_cid10.df$id[1] <- categoria_cid10.df$id[1] <- lista_mort_cid10.df$id[1] <- "all"
  causa_mal_definidas.df$id[1] <- faixa_etaria_1.df$id[1] <- faixa_etaria_2.df$id[1] <- faixa_etaria_3.df$id[1] <- faixa_etaria_4.df$id[1] <- "all"
  faixa_etaria_5.df$id[1] <- faixa_etaria_detalhada.df$id[1] <- sexo.df$id[1] <- cor_raca.df$id[1] <- escolaridade_mae.df$id[1] <- "all"
  idade_mae.df$id[1] <- duracao_gestacao.df$id[1] <- tipo_gravidez.df$id[1] <- tipo_parto.df$id[1] <- peso_ao_nascer.df$id[1] <- "all"
  obito_relacao_parto.df$id[1] <- obito_investigado.df$id[1] <- "all"

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

  if (conteudo != 1 & conteudo != 2) {

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

  if (any(municipio != "all")) {

    municipio <- as.character(municipio)

    if (!(all(municipio %in% municipios.df$id))) stop("Some element in 'municipio' argument is wrong")

  }

  if (any(cir != "all")) {

    cir <- as.character(cir)

    if (!(all(cir %in% cir.df$id))) stop("Some element in 'cir' argument is wrong")

  }

  if (any(macrorregiao_de_saude != "all")) {

    macrorregiao_de_saude <- as.character(macrorregiao_de_saude)

    if (!(all(macrorregiao_de_saude %in% macrorregiao_de_saude.df$id))) stop("Some element in 'macrorregiao_de_saude' argument is wrong")

  }

  if (any(divisao_admnist_estadual != "all")) {

    divisao_admnist_estadual <- as.character(divisao_admnist_estadual)

    if (!(all(divisao_admnist_estadual %in% divisao_admnist_estadual.df$id))) stop("Some element in 'divisao_admnist_estadual' argument is wrong")

  }

  if (any(microrregiao_ibge != "all")) {

    microrregiao_ibge <- as.character(microrregiao_ibge)

    if (!(all(microrregiao_ibge %in% microrregiao_ibge.df$id))) stop("Some element in 'microrregiao_ibge' argument is wrong")

  }

  if (any(ride != "all")) {

    ride <- as.character(ride)

    if (!(all(ride %in% ride.df$id))) stop("Some element in 'ride' argument is wrong")

  }

  if (any(capitulo_cid10 != "all")) {

    capitulo_cid10 <- as.character(capitulo_cid10)

    if (!(all(capitulo_cid10 %in% capitulo_cid10.df$id))) stop("Some element in 'capitulo_cid10' argument is wrong")

  }

  if (any(categoria_cid10 != "all")) {

    categoria_cid10 <- as.character(categoria_cid10)

    if (!(all(categoria_cid10 %in% categoria_cid10.df$id))) stop("Some element in 'categoria_cid10' argument is wrong")

  }

  if (any(lista_mort_cid10 != "all")) {

    lista_mort_cid10 <- as.character(lista_mort_cid10)

    if (!(all(lista_mort_cid10 %in% lista_mort_cid10.df$id))) {

      lista_mort_cid10 <- as.character(lista_mort_cid10)

      if (!(all(lista_mort_cid10 %in% lista_mort_cid10.df$value))) {

        stop("Some element in 'lista_mort_cid10' argument is wrong")

      }

    }

  }

  if (any(causa_mal_definidas != "all")) {

    causa_mal_definidas <- as.character(causa_mal_definidas)

    if (!(all(causa_mal_definidas %in% causa_mal_definidas.df$id))) {

      causa_mal_definidas <- as.character(causa_mal_definidas)

      if (!(all(causa_mal_definidas %in% causa_mal_definidas.df$value))) {

        stop("Some element in 'causa_mal_definidas' argument is wrong")

      }

    }

  }

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

  if (any(faixa_etaria_3 != "all")) {

    if (!(all(faixa_etaria_3 %in% faixa_etaria_3.df$id))) {

      faixa_etaria_3 <- as.character(faixa_etaria_3)

      if (!(all(faixa_etaria_3 %in% faixa_etaria_3.df$value))) {

        stop("Some element in 'faixa_etaria_3' argument is wrong")

      }

    }

  }

  if (any(faixa_etaria_4 != "all")) {

    if (!(all(faixa_etaria_4 %in% faixa_etaria_4.df$id))) {

      faixa_etaria_4 <- as.character(faixa_etaria_4)

      if (!(all(faixa_etaria_4 %in% faixa_etaria_4.df$value))) {

        stop("Some element in 'faixa_etaria_4' argument is wrong")

      }

    }

  }

  if (any(faixa_etaria_5 != "all")) {

    if (!(all(faixa_etaria_5 %in% faixa_etaria_5.df$id))) {

      faixa_etaria_5 <- as.character(faixa_etaria_5)

      if (!(all(faixa_etaria_5 %in% faixa_etaria_5.df$value))) {

        stop("Some element in 'faixa_etaria_5' argument is wrong")

      }

    }

  }

  if (any(faixa_etaria_detalhada != "all")) {

    if (!(all(faixa_etaria_detalhada %in% faixa_etaria_detalhada.df$id))) {

      faixa_etaria_detalhada <- as.character(faixa_etaria_detalhada)

      if (!(all(faixa_etaria_detalhada %in% faixa_etaria_detalhada.df$value))) {

        stop("Some element in 'faixa_etaria_detalhada' argument is wrong")

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

  if (any(cor_raca != "all")) {

    if (!(all(cor_raca %in% cor_raca.df$id))) {

      cor_raca <- as.character(cor_raca)

      if (!(all(cor_raca %in% cor_raca.df$value))) {

        stop("Some element in 'cor_raca' argument is wrong")

      }

    }

  }

  if (any(local_ocorrencia != "all")) {

    if (!(all(local_ocorrencia %in% local_ocorrencia.df$id))) {

      local_ocorrencia <- as.character(local_ocorrencia)

      if (!(all(local_ocorrencia %in% local_ocorrencia.df$value))) {

        stop("Some element in 'local_ocorrencia' argument is wrong")

      }

    }

  }

  if (any(idade_mae != "all")) {

    if (!(all(idade_mae %in% idade_mae.df$id))) {

      idade_mae <- as.character(idade_mae)

      if (!(all(idade_mae %in% idade_mae.df$value))) {

        stop("Some element in 'idade_mae' argument is wrong")

      }

    }

  }

  if (any(escolaridade_mae != "all")) {

    if (!(all(escolaridade_mae %in% escolaridade_mae.df$id))) {

      escolaridade_mae <- as.character(escolaridade_mae)

      if (!(all(escolaridade_mae %in% escolaridade_mae.df$value))) {

        stop("Some element in 'escolaridade_mae' argument is wrong")

      }

    }

  }

  if (any(duracao_gestacao != "all")) {

    if (!(all(duracao_gestacao %in% duracao_gestacao.df$id))) {

      duracao_gestacao <- as.character(duracao_gestacao)

      if (!(all(duracao_gestacao %in% duracao_gestacao.df$value))) {

        stop("Some element in 'duracao_gestacao' argument is wrong")

      }

    }

  }

  if (any(tipo_gravidez != "all")) {

    if (!(all(tipo_gravidez %in% tipo_gravidez.df$id))) {

      tipo_gravidez <- as.character(tipo_gravidez)

      if (!(all(tipo_gravidez %in% tipo_gravidez.df$value))) {

        stop("Some element in 'tipo_gravidez' argument is wrong")

      }

    }

  }

  if (any(tipo_parto != "all")) {

    if (!(all(tipo_parto %in% tipo_parto.df$id))) {

      tipo_parto <- as.character(tipo_parto)

      if (!(all(tipo_parto %in% tipo_parto.df$value))) {

        stop("Some element in 'tipo_parto' argument is wrong")

      }

    }

  }

  if (any(peso_ao_nascer != "all")) {

    if (!(all(peso_ao_nascer %in% peso_ao_nascer.df$id))) {

      peso_ao_nascer <- as.character(peso_ao_nascer)

      if (!(all(peso_ao_nascer %in% peso_ao_nascer.df$value))) {

        stop("Some element in 'peso_ao_nascer' argument is wrong")

      }

    }

  }

  if (any(obito_relacao_parto != "all")) {

    if (!(all(obito_relacao_parto %in% obito_relacao_parto.df$id))) {

      obito_relacao_parto <- as.character(obito_relacao_parto)

      if (!(all(obito_relacao_parto %in% obito_relacao_parto.df$value))) {

        stop("Some element in 'obito_relacao_parto' argument is wrong")

      }

    }

  }

  if (any(obito_investigado != "all")) {

    if (!(all(obito_investigado %in% obito_investigado.df$id))) {

      obito_investigado <- as.character(obito_investigado)

      if (!(all(obito_investigado %in% obito_investigado.df$value))) {

        stop("Some element in 'obito_investigado' argument is wrong")

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

  #municipio
  form_municipio <- dplyr::filter(municipios.df, municipios.df$id %in% municipio)
  form_municipio <- paste0("SMunic%EDpio=", form_municipio$value, collapse = "&")

  #cir
  form_cir <- dplyr::filter(cir.df, cir.df$id %in% cir)
  form_cir <- paste0("SRegi%E3o_de_Sa%FAde_%28CIR%29=", form_cir$value, collapse = "&")

  #macrorregiao_de_saude
  form_macrorregiao_de_saude <- dplyr::filter(macrorregiao_de_saude.df, macrorregiao_de_saude.df$id %in% macrorregiao_de_saude)
  form_macrorregiao_de_saude <- paste0("SMacrorregi%E3o_de_Sa%FAde=", form_macrorregiao_de_saude$value, collapse = "&")

  form_pesqmes4 <- "pesqmes4=Digite+o+texto+e+ache+f%E1cil"

  #divisao_admnist_estadual
  form_divisao_admnist_estadual <- dplyr::filter(divisao_admnist_estadual.df, divisao_admnist_estadual.df$id %in% divisao_admnist_estadual)
  form_divisao_admnist_estadual <- paste0("SDivis%E3o_administ_estadual=", form_divisao_admnist_estadual$value, collapse = "&")

  form_pesqmes5 <- "pesqmes5=Digite+o+texto+e+ache+f%E1cil"

  #microrregiao_ibge
  form_microrregiao_ibge <- dplyr::filter(microrregiao_ibge.df, microrregiao_ibge.df$id %in% microrregiao_ibge)
  form_microrregiao_ibge <- paste0("SMicrorregi%E3o_IBGE=", form_microrregiao_ibge$value, collapse = "&")

  #ride
  form_ride <- dplyr::filter(ride.df, ride.df$id %in% ride)
  form_ride <- paste0("SRegi%E3o_Metropolitana_-_RIDE=", form_ride$value, collapse = "&")

  form_pesqmes7 <- "pesqmes7=Digite+o+texto+e+ache+f%E1cil"

  #capitulo_cid10
  form_capitulo_cid10 <- dplyr::filter(capitulo_cid10.df, capitulo_cid10.df$id %in% capitulo_cid10)
  form_capitulo_cid10 <- paste0("SCap%EDtulo_CID-10=", form_capitulo_cid10$value, collapse = "&")

  form_pesqmes8 <- "pesqmes8=Digite+o+texto+e+ache+f%E1cil"

  #categoria_cid10
  form_categoria_cid10 <- dplyr::filter(categoria_cid10.df, categoria_cid10.df$id %in% categoria_cid10)
  form_categoria_cid10 <- paste0("SCategoria_CID-10=", form_categoria_cid10$value, collapse = "&")

  form_pesqmes9 <- "pesqmes9=Digite+o+texto+e+ache+f%E1cil"

  #lista_mort_cid10
  form_lista_mort_cid10 <- dplyr::filter(lista_mort_cid10.df, lista_mort_cid10.df$id %in% lista_mort_cid10)
  form_lista_mort_cid10 <- paste0("SLista_Mort_CID-10=", form_lista_mort_cid10$value, collapse = "&")

  #causa_mal_definidas
  form_causa_mal_definidas <- dplyr::filter(causa_mal_definidas.df, causa_mal_definidas.df$id %in% causa_mal_definidas)
  form_causa_mal_definidas <- paste0("SCausa_mal_definidas=", form_causa_mal_definidas$value, collapse = "&")

  #faixa_etaria_1
  form_faixa_etaria_1 <- dplyr::filter(faixa_etaria_1.df, faixa_etaria_1.df$id %in% faixa_etaria_1)
  form_faixa_etaria_1 <- paste0("SFaixa_Et%E1ria_1=", form_faixa_etaria_1$value, collapse = "&")

  form_pesqmes12 <- "pesqmes12=Digite+o+texto+e+ache+f%E1cil"

  #faixa_etaria_2
  form_faixa_etaria_2 <- dplyr::filter(faixa_etaria_2.df, faixa_etaria_2.df$id %in% faixa_etaria_2)
  form_faixa_etaria_2 <- paste0("SFaixa_Et%E2ria_2=", form_faixa_etaria_2$value, collapse = "&")

  #faixa_etaria_3
  form_faixa_etaria_3 <- dplyr::filter(faixa_etaria_3.df, faixa_etaria_3.df$id %in% faixa_etaria_3)
  form_faixa_etaria_3 <- paste0("SFaixa_Et%E3ria_3=", form_faixa_etaria_3$value, collapse = "&")

  #faixa_etaria_4
  form_faixa_etaria_4 <- dplyr::filter(faixa_etaria_4.df, faixa_etaria_4.df$id %in% faixa_etaria_4)
  form_faixa_etaria_4 <- paste0("SFaixa_Et%E4ria_4=", form_faixa_etaria_4$value, collapse = "&")

  #faixa_etaria_5
  form_faixa_etaria_5 <- dplyr::filter(faixa_etaria_5.df, faixa_etaria_5.df$id %in% faixa_etaria_5)
  form_faixa_etaria_5 <- paste0("SFaixa_Et%E5ria_5=", form_faixa_etaria_5$value, collapse = "&")

  form_pesqmes16 <- "pesqmes16=Digite+o+texto+e+ache+f%E1cil"

  #faixa_etaria_detalhada
  form_faixa_etaria_detalhada <- dplyr::filter(faixa_etaria_detalhada.df, faixa_etaria_detalhada.df$id %in% faixa_etaria_detalhada)
  form_faixa_etaria_detalhada <- paste0("SFaixa_Et%E1ria_detalhada=", form_faixa_etaria_detalhada$value, collapse = "&")

  #sexo
  form_sexo <- dplyr::filter(sexo.df, sexo.df$id %in% sexo)
  form_sexo <- paste0("SSexo=", form_sexo$value, collapse = "&")

  #cor_raca
  form_cor_raca <- dplyr::filter(cor_raca.df, cor_raca.df$id %in% cor_raca)
  form_cor_raca <- paste0("SCor%2Fra%E7a=", form_cor_raca$value, collapse = "&")

  #local_ocorrencia
  form_local_ocorrencia <- dplyr::filter(local_ocorrencia.df, local_ocorrencia.df$id %in% local_ocorrencia)
  form_local_ocorrencia <- paste0("SLocal_ocorr%EAncia=", form_local_ocorrencia$value, collapse = "&")

  form_pesqmes20 <- "pesqmes20=Digite+o+texto+e+ache+f%E1cil"

  #idade_mae
  form_idade_mae <- dplyr::filter(idade_mae.df, idade_mae.df$id %in% idade_mae)
  form_idade_mae <- paste0("SIdade_m%E3e=", form_idade_mae$value, collapse = "&")

  #escolaridade_mae
  form_escolaridade_mae <- dplyr::filter(escolaridade_mae.df, escolaridade_mae.df$id %in% escolaridade_mae)
  form_escolaridade_mae <- paste0("SEscolaridade_m%E3e=", form_escolaridade_mae$value, collapse = "&")

  form_pesqmes22 <- "pesqmes22=Digite+o+texto+e+ache+f%E1cil"

  #duracao_gestacao
  form_duracao_gestacao <- dplyr::filter(duracao_gestacao.df, duracao_gestacao.df$id %in% duracao_gestacao)
  form_duracao_gestacao <- paste0("SDura%E7%E3o_gesta%E7%E3o=", form_duracao_gestacao$value, collapse = "&")

  #tipo_gravidez
  form_tipo_gravidez <- dplyr::filter(tipo_gravidez.df, tipo_gravidez.df$id %in% tipo_gravidez)
  form_tipo_gravidez <- paste0("STipo_gravidez=", form_tipo_gravidez$value, collapse = "&")

  #tipo_parto
  form_tipo_parto <- dplyr::filter(tipo_parto.df, tipo_parto.df$id %in% tipo_parto)
  form_tipo_parto <- paste0("STipo_parto=", form_tipo_parto$value, collapse = "&")

  #peso_ao_nascer
  form_peso_ao_nascer <- dplyr::filter(peso_ao_nascer.df, peso_ao_nascer.df$id %in% peso_ao_nascer)
  form_peso_ao_nascer <- paste0("SPeso_ao_nascer=", form_peso_ao_nascer$value, collapse = "&")

  #obito_relacao_parto
  form_obito_relacao_parto <- dplyr::filter(obito_relacao_parto.df, obito_relacao_parto.df$id %in% obito_relacao_parto)
  form_obito_relacao_parto <- paste0("S%D3bito_rela%E7%E3o_parto=", form_obito_relacao_parto$value, collapse = "&")

  #obito_investigado
  form_obito_investigado <- dplyr::filter(obito_investigado.df, obito_investigado.df$id %in% obito_investigado)
  form_obito_investigado <- paste0("S%D3bito_investigado=", form_obito_investigado$value, collapse = "&")


  form_data <- paste(
    form_linha, form_coluna, form_conteudo, form_periodo, form_pesqmes1, form_municipio, form_cir, form_macrorregiao_de_saude,
    form_pesqmes4, form_divisao_admnist_estadual, form_pesqmes5, form_microrregiao_ibge, form_ride, form_pesqmes7,
    form_capitulo_cid10, form_pesqmes8, form_categoria_cid10, form_pesqmes9, form_lista_mort_cid10, form_causa_mal_definidas,
    form_faixa_etaria_1, form_pesqmes12, form_faixa_etaria_3, form_faixa_etaria_4, form_faixa_etaria_5, form_pesqmes16,
    form_faixa_etaria_detalhada, form_sexo, form_cor_raca, form_local_ocorrencia, form_pesqmes20, form_idade_mae,
    form_escolaridade_mae, form_pesqmes22, form_duracao_gestacao, form_tipo_gravidez, form_tipo_parto, form_peso_ao_nascer,
    form_obito_relacao_parto, form_obito_investigado, "formato=table&mostre=Mostra", sep = "&")

  form_data <- gsub("\\\\u00", "%", form_data)

  ##### REQUEST FORM AND DATA WRANGLING ####
  site <- httr::POST(url = paste0("http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/pinf10", uf,".def"),
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

