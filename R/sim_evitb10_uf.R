#' Scrapes SIM's ICD-10 evitable causes data from ufs
#'
#' This function allows the user to retrive data from
#' SIM's ICD-10 database much in the same way that is done
#' by the online portal. The argument options refer to
#' evitable causes in 5-74 age group data from the states
#' unities.
#'
#' @usage sim_evitb10_uf(uf, linha = "Município", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", municipio = "all", cir = "all",
#'   macrorregiao_de_saude = "all", divisao_admnist_estadual = "all",
#'   microrregiao_ibge = "all", ride = "all", causas_evitaveis = "all",
#'   capitulo_cid10 = "all", categoria_cid10 = "all", faixa_etaria = "all",
#'   faixa_etaria_detalhada = "all", sexo = "all", cor_raca = "all",
#'   escolaridade = "all", estado_civil = "all", local_ocorrencia = "all")
#' @param uf A character of length = 1 with the state's acronym of interest.
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
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso \code{\link{sim_evita10_uf}}
#' @examples
#' \dontrun{
#' ## Requesting data from the state of Mato Grosso do Sul
#' sim_evitb10_uf(uf = "ms")
#' }
#'
#' @keywords SIM datasus causas evitáveis
#' @section Lifecycle:
#' This function is deprecated. Use `sim()` or `sinasc()` with a named
#' `filtros` list. The legacy signature is retained for compatibility.
#' @export

sim_evitb10_uf <- function(uf, linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", municipio = "all", cir = "all",
                           macrorregiao_de_saude = "all", divisao_admnist_estadual = "all", microrregiao_ibge = "all", ride = "all",
                           causas_evitaveis = "all", capitulo_cid10 = "all", categoria_cid10 = "all", faixa_etaria = "all", faixa_etaria_detalhada = "all",
                           sexo = "all", cor_raca = "all", escolaridade = "all", estado_civil = "all", local_ocorrencia = "all") {
  .datasus_legacy_vital(
    old = "sim_evitb10_uf", replacement = "sim",
    sistema = "sim", conjunto = "causas_evitaveis_5_74",
    abrangencia = "municipio"
  )
}

