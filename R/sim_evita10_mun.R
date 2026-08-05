#' Scrapes SIM's ICD-10 evitable causes data from cities
#'
#' This function allows the user to retrieve data from
#' SIM's ICD-10 database much in the same way that is done
#' by the online portal. The argument options refer to
#' evitable causes data focused on brazilian cities.
#'
#' @usage sim_evita10_mun(linha = "Município", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", municipio = "all", capital = "all",
#'   cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all",
#'   ride = "all", territorio_da_cidadania = "all", mesorregiao_pndr = "all",
#'   amazonia_legal = "all", semiarido = "all", faixa_de_fronteira = "all",
#'   zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
#'   causas_evitaveis = "all", capitulo_cid10 = "all", categoria_cid10 = "all",
#'   faixa_etaria = "all", sexo = "all", cor_raca = "all", escolaridade = "all",
#'   estado_civil = "all", local_ocorrencia = "all")
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
#' @param causas_evitaveis "all" or a character vector with the evitable cause code (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param capitulo_cid10 "all" or a numeric vector with the ICD-10 chapter to filter the data. Defaults to "all".
#' @param categoria_cid10 "all" or a character vector with the ICD-10 category codes (capital letter and two numbers) to filter the data. Defaults to "all".
#' @param faixa_etaria "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param sexo "all" or a character vector with the gender (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param cor_raca "all" or a character vector with the color/race (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param escolaridade "all" or a character vector with the instruction (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param estado_civil "all" or a character vector with the marital status (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param local_ocorrencia "all" or a character vector with the place of ocurrence to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso \code{\link{sinasc_nv_uf}}
#' @examples
#' \donttest{
#' old_options <- options(
#'   datasus.timeout = 5,
#'   datasus.download_timeout = 15,
#'   datasus.max_tries = 1
#' )
#' try({
#' ## Requesting data from the city of Campo Grande/MS
#' sim_evita10_mun(municipio = 500270)
#' })
#' options(old_options)
#' }
#'
#' @keywords SIM datasus causas evitáveis
#' @section Lifecycle:
#' This function is deprecated. Use `sim()` or `sinasc()` with a named
#' `filtros` list. The legacy signature is retained for compatibility.
#' @export

sim_evita10_mun <- function(linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", municipio = "all",
                            capital = "all", cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all", ride = "all",
                            territorio_da_cidadania = "all", mesorregiao_pndr = "all", amazonia_legal = "all", semiarido = "all",
                            faixa_de_fronteira = "all", zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
                            causas_evitaveis = "all", capitulo_cid10 = "all", categoria_cid10 = "all", faixa_etaria = "all",
                            sexo = "all", cor_raca = "all", escolaridade = "all", estado_civil = "all", local_ocorrencia = "all") {
  .datasus_legacy_vital(
    old = "sim_evita10_mun", replacement = "sim",
    sistema = "sim", conjunto = "causas_evitaveis_0_4",
    abrangencia = "municipio"
  )
}

