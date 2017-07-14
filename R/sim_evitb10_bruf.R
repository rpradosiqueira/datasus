#' Scrapes SIM's ICD-10 evitale causes data from regions
#'
#' This function allows the user to retrieve data from
#' SIM's ICD-10 database much in the same way that is done
#' by the online portal. The argument options refer to
#' evitable causes data focused on the regions and states 
#' and age ranging between 5-74 years old.
#'
#' @usage sim_evitb10_bruf(linha = "Região", coluna = "Não ativa", 
#'   conteudo = 1, periodo = "last", regiao = "all", unidade_da_federacao = "all",
#'   causas_evitaveis = "all", capitulo_cid10 = "all", categoria_cid10 = "all", 
#'   faixa_etaria = "all", faixa_etaria_detalhada = "all", sexo = "all", 
#'   cor_raca = "all", escolaridade = "all", estado_civil = "all", 
#'   local_ocorrencia = "all")
#' @param linha A character describing which element will be displayed in the rows of the data.frame. Defaults to "Região".
#' @param coluna A character describing which element will be displayed in the columns of the data.frame. Defaults to "Não ativa".
#' @param conteudo A character of length = 1 with the state's acronym of interest.
#' @param periodo A character vector describing the period of data. Defaults to the last available.
#' @param regiao "all" or a numeric vector with the IBGE's region codes to filter the data. Defaults to "all".
#' @param unidade_da_federacao "all" or a numeric vector with the IBGE's state codes to filter the data. Defaults to "all".
#' @param causas_evitaveis "all" or a character vector with the evitable cause code (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param capitulo_cid10 "all" or a numeric vector with the ICD-10 chapter to filter the data. Defaults to "all".
#' @param categoria_cid10 "all" or a character vector with the ICD-10 category codes (capital letter and two numbers) to filter the data. Defaults to "all".
#' @param faixa_etaria "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_detalhada "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param sexo "all" or a character vector with the gender (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param cor_raca "all" or a character vector with the color/race (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param escolaridade "all" or a character vector with the instruction (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param estado_civil "all" or a character vector with the marital status (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param local_ocorrencia "all" or a character vector with the place of ocurrence to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{sim_evita10_bruf}}
#' @examples
#' \dontrun{
#' ## Requesting data from Midwest region
#' sim_evitb10_bruf(region = 5)
#' }
#'
#' @keywords SIM datasus causas evitáveis
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export

sim_evitb10_bruf <- function(linha = "Regi\u00e3o", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", regiao = "all", 
                             unidade_da_federacao = "all", causas_evitaveis = "all", capitulo_cid10 = "all", 
                             categoria_cid10 = "all", faixa_etaria = "all", faixa_etaria_detalhada = "all",
                             sexo = "all", cor_raca = "all", escolaridade = "all", estado_civil = "all", 
                             local_ocorrencia = "all") {
  
  
  page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/evitb10uf.def") 
  
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
  
  regiao.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_text() %>% readr::parse_number(),
                                           value = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_attr("value")))
  
  unidade_da_federacao.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_text() %>% trimws(),
                                                         value = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_attr("value")))
  unidade_da_federacao.df[] <- lapply(unidade_da_federacao.df, as.character)
  
  causas_evitaveis.df <- data.frame(id = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_attr("value"))
  causas_evitaveis.df[] <- lapply(causas_evitaveis.df, as.character)
  causas_evitaveis.df$id <- gsub(" .*$", "", causas_evitaveis.df$id)
  causas_evitaveis.df$id <- gsub("\\.", " ", causas_evitaveis.df$id) %>% trimws()
  causas_evitaveis.df$id <- gsub(" ", ".", causas_evitaveis.df$id)
  
  capitulo_cid10.df <- data.frame(id = 0:(length(page %>% rvest::html_nodes("#S4 option") %>% rvest::html_attr("value")) %>% as.numeric() - 1),
                                  value = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_attr("value"))
  capitulo_cid10.df[] <- lapply(capitulo_cid10.df, as.character)
  
  categoria_cid10.df <- data.frame(id = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_text() %>% trimws(),
                                   value = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_attr("value"))
  categoria_cid10.df[] <- lapply(categoria_cid10.df, as.character)
  categoria_cid10.df$id <- gsub(" .*$", "", categoria_cid10.df$id)
  
  faixa_etaria.df <- data.frame(id = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_attr("value"))
  faixa_etaria.df[] <- lapply(faixa_etaria.df, as.character)
  
  faixa_etaria_detalhada.df <- data.frame(id = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_text() %>% trimws(),
                                          value = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_attr("value"))
  faixa_etaria_detalhada.df[] <- lapply(faixa_etaria_detalhada.df, as.character)

  sexo.df <- data.frame(id = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_text() %>% trimws(),
                        value = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_attr("value"))
  sexo.df[] <- lapply(sexo.df, as.character)
  
  cor_raca.df <- data.frame(id = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_text() %>% trimws(),
                            value = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_attr("value"))
  cor_raca.df[] <- lapply(cor_raca.df, as.character)
  
  escolaridade.df <- data.frame(id = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_attr("value"))
  escolaridade.df[] <- lapply(escolaridade.df, as.character)
  
  estado_civil.df <- data.frame(id = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_attr("value"))
  estado_civil.df[] <- lapply(estado_civil.df, as.character)
  
  local_ocorrencia.df <- data.frame(id = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_attr("value"))
  local_ocorrencia.df[] <- lapply(local_ocorrencia.df, as.character)
  
  regiao.df$id[1] <- unidade_da_federacao.df$id[1] <- capitulo_cid10.df$id[1] <- "all"
  causas_evitaveis.df$id[1] <- local_ocorrencia.df$id[1] <- categoria_cid10.df$id[1] <- "all"
  faixa_etaria.df$id[1] <- faixa_etaria_detalhada.df$id[1] <- estado_civil.df$id[1] <- "all"
  sexo.df$id[1] <- cor_raca.df$id[1] <- escolaridade.df$id[1] <- "all"

  #### ERROR HANDLING ####
  if (linha != "Regi\u00e3o") {
    
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
  
  if (any(regiao != "all")) {
    
    regiao <- as.character(regiao)
    
    if (!(all(regiao %in% regiao.df$id))) stop("Some element in 'regiao' argument is wrong")
    
  }  
  
  if (any(unidade_da_federacao != "all")) {
    
    unidade_da_federacao <- as.character(unidade_da_federacao)
    
    if (!(all(unidade_da_federacao %in% unidade_da_federacao.df$id))) stop("Some element in 'unidade_da_federacao' argument is wrong")
    
  } 
  
  if (any(causas_evitaveis != "all")) {
    
    causas_evitaveis <- as.character(causas_evitaveis)
    
    if (!(all(causas_evitaveis %in% causas_evitaveis.df$id))) {
      
      causas_evitaveis <- as.character(causas_evitaveis)
      
      if (!(all(causas_evitaveis %in% causas_evitaveis.df$value))) {
        
        stop("Some element in 'causas_evitaveis' argument is wrong")
        
      }
      
    } 
    
  }  
  
  if (any(capitulo_cid10 != "all")) {
    
    capitulo_cid10 <- as.character(capitulo_cid10)
    
    if (!(all(capitulo_cid10 %in% capitulo_cid10.df$id))) stop("Some element in 'capitulo_cid10' argument is wrong")
    
  }  
  
  if (any(categoria_cid10 != "all")) {
    
    categoria_cid10 <- as.character(categoria_cid10)
    
    if (!(all(categoria_cid10 %in% categoria_cid10.df$id))) stop("Some element in 'categoria_cid10' argument is wrong")
    
  }
  
  if (any(faixa_etaria != "all")) {
    
    if (!(all(faixa_etaria %in% faixa_etaria.df$id))) {
      
      faixa_etaria <- as.character(faixa_etaria)
      
      if (!(all(faixa_etaria %in% faixa_etaria.df$value))) {
        
        stop("Some element in 'faixa_etaria' argument is wrong")
        
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
  
  if (any(escolaridade != "all")) {
    
    if (!(all(escolaridade %in% escolaridade.df$id))) {
      
      escolaridade <- as.character(escolaridade)
      
      if (!(all(escolaridade %in% escolaridade.df$value))) {
        
        stop("Some element in 'escolaridade' argument is wrong")
        
      }
      
    } 
    
  }  
  
  if (any(estado_civil != "all")) {
    
    if (!(all(estado_civil %in% estado_civil.df$id))) {
      
      estado_civil <- as.character(estado_civil)
      
      if (!(all(estado_civil %in% estado_civil.df$value))) {
        
        stop("Some element in 'estado_civil' argument is wrong")
        
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
  form_periodo <- dplyr::filter(periodos.df, id %in% periodo)
  form_periodo <- paste0("Arquivos=", form_periodo$value, collapse = "&")
  
  #regiao
  form_regiao <- dplyr::filter(regiao.df, id %in% regiao)
  form_regiao <- paste0("SRegi%E3o=", form_regiao$value, collapse = "&")
  
  form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"
  
  #unidade_da_federacao
  form_unidade_da_federacao <- dplyr::filter(unidade_da_federacao.df, id %in% unidade_da_federacao)
  form_unidade_da_federacao <- paste0("SUnidade_da_Federa%E7%E3o=", form_unidade_da_federacao$value, collapse = "&")
  
  form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"
  
  #causas_evitaveis
  form_causas_evitaveis <- dplyr::filter(causas_evitaveis.df, id %in% causas_evitaveis)
  form_causas_evitaveis <- paste0("SCausas_evit%E1veis=", form_causas_evitaveis$value, collapse = "&")
  
  form_pesqmes4 <- "pesqmes4=Digite+o+texto+e+ache+f%E1cil"
  
  #capitulo_cid10
  form_capitulo_cid10 <- dplyr::filter(capitulo_cid10.df, id %in% capitulo_cid10)
  form_capitulo_cid10 <- paste0("SCap%EDtulo_CID-10=", form_capitulo_cid10$value, collapse = "&")
  
  form_pesqmes5 <- "pesqmes5=Digite+o+texto+e+ache+f%E1cil"
  
  #categoria_cid10
  form_categoria_cid10 <- dplyr::filter(categoria_cid10.df, id %in% categoria_cid10)
  form_categoria_cid10 <- paste0("SCategoria_CID-10=", form_categoria_cid10$value, collapse = "&")

  #faixa_etaria
  form_faixa_etaria <- dplyr::filter(faixa_etaria.df, id %in% faixa_etaria)
  form_faixa_etaria <- paste0("SFaixa_Et%E1ria=", form_faixa_etaria$value, collapse = "&")

  form_pesqmes7 <- "pesqmes7=Digite+o+texto+e+ache+f%E1cil"
  
  #faixa_etaria_detalhada
  form_faixa_etaria_detalhada <- dplyr::filter(faixa_etaria_detalhada.df, id %in% faixa_etaria_detalhada)
  form_faixa_etaria_detalhada <- paste0("SFaixa_Et%E1ria_detalhada=", form_faixa_etaria_detalhada$value, collapse = "&")
  
  #sexo
  form_sexo <- dplyr::filter(sexo.df, id %in% sexo)
  form_sexo <- paste0("SSexo=", form_sexo$value, collapse = "&")
  
  #cor_raca
  form_cor_raca <- dplyr::filter(cor_raca.df, id %in% cor_raca)
  form_cor_raca <- paste0("SCor%2Fra%E7a=", form_cor_raca$value, collapse = "&")
  
  #estado_civil
  form_estado_civil <- dplyr::filter(estado_civil.df, id %in% estado_civil)
  form_estado_civil <- paste0("SEstado_civil=", form_estado_civil$value, collapse = "&")
  
  #local_ocorrencia
  form_local_ocorrencia <- dplyr::filter(local_ocorrencia.df, id %in% local_ocorrencia)
  form_local_ocorrencia <- paste0("SLocal_ocorr%EAncia=", form_local_ocorrencia$value, collapse = "&")
  
    form_data <- paste(form_linha, form_coluna, form_conteudo, form_periodo, form_regiao, 
                     form_pesqmes2, form_unidade_da_federacao, form_pesqmes3, form_causas_evitaveis, 
                     form_pesqmes4, form_capitulo_cid10, form_pesqmes5, form_categoria_cid10, 
                     form_faixa_etaria, form_pesqmes7, form_faixa_etaria_detalhada, form_sexo,
                     form_cor_raca, form_estado_civil, form_local_ocorrencia,
                     "formato=table&mostre=Mostra", sep = "&")
  
  form_data <- gsub("\\\\u00", "%", form_data)
  
  ##### REQUEST FORM AND DATA WRANGLING ####
  site <- httr::POST(url = "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/evita10uf.def",
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

