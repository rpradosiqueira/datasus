

##datasusmeta


metabaseurl <- "https://datasus.saude.gov.br/informacoes-de-saude-tabnet/"

page <- xml2::read_html(metabaseurl)


grupos <- page|>
  rvest::html_elements(xpath="//a[@class = 'elementor-toggle-title']")|>
  rvest::html_text()

grupos <- data.frame(
  id = page|>
    rvest::html_elements(
      xpath="//div[@class = 'elementor-toggle-item']/div[2]/@data-tab")|>
    rvest::html_text()|>as.numeric(),
  grupo=grupos)


pegassunto <- \(x) {
  infodet <- page|>
    rvest::html_elements(
      xpath=paste0(
        "//div[@data-tab=",
        x,"][2]//a"))
  data.frame(grupo_id = x,
             assunto = infodet|>
               rvest::html_text2(),
             url = infodet|>
               rvest::html_attr("href")
  )
}

assuntos <- data.table::rbindlist(lapply(1:nrow(grupos),pegassunto))

assuntos$id <- 1:nrow(assuntos)


scrapeass <- \(assunto) {
  burl <- assuntos[assunto,]$url
  if(grepl(".def|://cnes|anstabnet",burl)){

    data.frame(
      assunto_id=assunto,
      taburl=burl,
      tabela=assuntos[assunto,]$assunto
    )

  } else {
    pinfo <- rvest::read_html_live(burl)
    tabelaurl <- pinfo|>
      rvest::html_elements(css="input[type=radio]")|>
      rvest::html_attr("value")|>trimws()

    tabelalabels <- (pinfo|>
                       rvest::html_elements(css="#infesq")|>rvest::html_text2()|>strsplit("\\n"))[[1]]

    if(length(tabelaurl)==0){
      tabelaurl <- "provisoriamente indisponibilizado"
    }

    if(length(tabelaurl)<length(tabelalabels)) {
      tabelalabels <-
        tabelalabels[tabelalabels!=""]
    }
    if(length(tabelaurl)<length(tabelalabels)) {
      tabelalabels <-
        tabelalabels[-1]
    }
    if(length(tabelaurl)<length(tabelalabels)) {
      tabelalabels <-
        tabelalabels[nchar(tabelalabels)<100]
    }

    ##clean tabelalabels
    tabelalabels <-
      gsub(assuntos[assunto,]$assunto,"",tabelalabels,
           fixed=T)

    data.frame(
      assunto_id=assunto,
      taburl=tabelaurl,
      tabela=tabelalabels
    )
  }
}

tabelas <-
  data.frame(
    assunto_id=numeric(),
    taburl=character(),
    tabela=character()
  )

#for (i in assuntos$id) {
for (i in 1:nrow(assuntos)) {
  tabelascrps <- scrapeass(i)
  tabelas <- rbind(tabelas,tabelascrps)
}

tabelas <- dplyr::distinct(tabelas,taburl,.keep_all = TRUE)

tabelas$id <- 1:nrow(tabelas)

metatabnet <- tabelas|>
  dplyr::left_join(assuntos|>dplyr::rename(assuntourl=url),
                   by=c("assunto_id"="id"))|>
  dplyr::left_join(grupos,
                   by=c("grupo_id"="id"))|>
  dplyr::select(grupo_id,grupo,assunto_id,assunto,assuntourl,id,tabela,taburl)

## code to prepare `metatabnet` dataset goes here

usethis::use_data(metatabnet, overwrite = TRUE)

