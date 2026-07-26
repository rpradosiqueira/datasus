#' Scrapes SIM's ICD-10 data from UF's
#'
#' This function allows the user to retrive data from
#' SIM's ICD-10 database much in the same way that is done
#' by the online portal. The argument options refer to
#' data from the states unities.
#'
#' @usage sim_obt10_uf(uf, linha = "Município", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", municipio = "all", cir = "all",
#'   macrorregiao_de_saude = "all", divisao_admnist_estadual = "all",
#'   microrregiao_ibge = "all", ride = "all", capitulo_cid10 = "all",
#'   grupo_cid10 = "all", categoria_cid10 = "all", causa_br_cid10 = "all",
#'   causa_mal_definida = "all", faixa_etaria = "all", faixa_etaria_ops = "all",
#'   faixa_etaria_det = "all", faixa_etaria_menor1a = "all", sexo = "all",
#'   cor_raca = "all", escolaridade = "all", estado_civil = "all",
#'   local_ocorrencia = "all")
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
#' @param capitulo_cid10 "all" or a numeric vector with the ICD-10 chapter to filter the data. Defaults to "all".
#' @param grupo_cid10 "all" or a character vector with the ICD-10 group (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param categoria_cid10 "all" or a character vector with the ICD-10 category codes (capital letter and two numbers) to filter the data. Defaults to "all".
#' @param causa_br_cid10 "all" or a character vector with the ICD-10 cause codes to filter the data. Defaults to "all".
#' @param causa_mal_definida "all" or a character vector with the ill-defined causes (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_ops "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all". "ops" in the argument name stands for Pan American Health Organization.
#' @param faixa_etaria_det "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param faixa_etaria_menor1a "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param sexo "all" or a character vector with the gender (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param cor_raca "all" or a character vector with the color/race (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param escolaridade "all" or a character vector with the instruction (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param estado_civil "all" or a character vector with the marital status (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param local_ocorrencia "all" or a character vector with the place of ocurrence to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso \code{\link{sim_obt10_mun}}
#' @examples
#' \dontrun{
#' ## Requesting data from the state of Mato Grosso do Sul
#' sim_obt10_uf(uf = "ms")
#' }
#'
#' @keywords SIM datasus
#' @section Lifecycle:
#' This function is deprecated. Use `sim()` or `sinasc()` with a named
#' `filtros` list. The legacy signature is retained for compatibility.
#' @export

sim_obt10_uf <- function(uf, linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", municipio = "all", cir = "all",
                         macrorregiao_de_saude = "all", divisao_admnist_estadual = "all", microrregiao_ibge = "all", ride = "all",
                         capitulo_cid10 = "all", grupo_cid10 = "all", categoria_cid10 = "all", causa_br_cid10 = "all",
                         causa_mal_definida = "all", faixa_etaria = "all", faixa_etaria_ops = "all", faixa_etaria_det = "all",
                         faixa_etaria_menor1a = "all", sexo = "all", cor_raca = "all", escolaridade = "all", estado_civil = "all",
                         local_ocorrencia = "all") {
  .datasus_legacy_vital(
    old = "sim_obt10_uf", replacement = "sim",
    sistema = "sim", conjunto = "obitos",
    abrangencia = "municipio"
  )
}
