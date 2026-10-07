#' Metadata on groups, subjects and table urls from DATASUS tabnet
#'
#' Metadata on groups, subjects and table urls from DATASUS tabnet for searching
#'
#' @docType data
#' @usage data(metatabnet)
#'
#' @format A data frame with 298 rows and 8 variables:
#' \describe{
#'   \item{grupo_id}{Arbitrary group id from position on source url}
#'   \item{grupo}{Group Label}
#'   \item{assunto_id}{Arbitrary assunto id from position on scraped (sub)url}
#'   \item{assunto}{Subject Label}
#'   \item{assuntourl}{Subject URL}
#'   \item{id}{Arbitrary table id from position on scraped links}
#'   \item{tabela}{tabela}
#'   \item{taburl}{table tabnet url}
#' }
#' @keywords metadados tabnet datasus
#'
#' @source \href{https://datasus.saude.gov.br/informacoes-de-saude-tabnet/}{URL TABNET datasus}
#'
#' @examples
#' data(metatabnet)
#'
"metatabnet"
