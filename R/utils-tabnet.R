.tabnet_get_html <- function(url, tries = 3) {
  for (i in seq_len(tries)) {
    r <- tryCatch(httr::GET(url, httr::timeout(60)), error = function(e) e)
    if (!inherits(r, "error") && httr::status_code(r) == 200) {
      page <- tryCatch(xml2::read_html(httr::content(r, as = "raw"), encoding = "latin1"),
                       error = function(e) e)
      if (!inherits(page, "error")) return(page)
    }
    Sys.sleep(2)
  }
  stop("Nao foi possivel baixar a pagina de definicao do TABNET: ", url)
}

.tabnet_post <- function(url, form_data, tries = 3) {
  for (i in seq_len(tries)) {
    r <- tryCatch(httr::POST(url = url, body = form_data, httr::timeout(120)),
                  error = function(e) e)
    if (!inherits(r, "error") && httr::status_code(r) == 200 &&
        length(httr::content(r, as = "raw")) > 200) {
      return(r)
    }
    Sys.sleep(2)
  }
  stop("Nao foi possivel consultar o TABNET: ", url)
}

.parse_tabnet_response <- function(site) {
  tabdados <- httr::content(site, encoding = "Latin1") %>%
    rvest::html_nodes(".tabdados tbody td") %>%
    rvest::html_text() %>%
    trimws()

  col_tabdados <- httr::content(site, encoding = "Latin1") %>%
    rvest::html_nodes("th") %>%
    rvest::html_text() %>%
    trimws()

  if (length(col_tabdados) == 0 || length(tabdados) == 0 ||
      length(tabdados) %% length(col_tabdados) != 0) {
    stop("Nao foi possivel interpretar a resposta do TABNET (resposta vazia ou em formato inesperado).")
  }

  f1 <- function(x) x <- gsub("\\.", "", x)
  f2 <- function(x) x <- as.numeric(as.character(x))

  tabela_final <- as.data.frame(matrix(data = tabdados, nrow = length(tabdados)/length(col_tabdados),
                                       ncol = length(col_tabdados), byrow = TRUE))

  names(tabela_final) <- col_tabdados

  tabela_final[-1] <- lapply(tabela_final[-1], f1)
  tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1], f2))

  tabela_final
}
