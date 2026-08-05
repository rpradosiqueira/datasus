#' Scrapes SIM's ICD-10 child mortality data from regions
#'
#' This function allows the user to retrive data from
#' SIM's ICD-10 database much in the same way that is done
#' by the online portal. The argument options refer to
#' child mortality data focused on focused on the regions
#' and states.
#'
#' @usage sim_inf10_bruf(linha = "Região", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", regiao = "all",
#'   unidade_da_federacao = "all", capitulo_cid10 = "all",
#'   categoria_cid10 = "all", lista_mort_cid10 = "all",
#'   causa_mal_definidas = "all", causas_evitaveis_0a4anos = "all",
#'   faixa_etaria_1 = "all", faixa_etaria_2 = "all", faixa_etaria_3 = "all",
#'   faixa_etaria_4 = "all", faixa_etaria_5 = "all",
#'   faixa_etaria_detalhada = "all", sexo = "all", cor_raca = "all",
#'   local_ocorrencia = "all", idade_mae = "all", escolaridade_mae = "all",
#'   duracao_gestacao = "all", tipo_gravidez = "all", tipo_parto = "all",
#'   peso_ao_nascer = "all", obito_relacao_parto = "all",
#'   obito_investigado = "all")
#' @param linha A character describing which element will be displayed in the rows of the data.frame. Defaults to "Município".
#' @param coluna A character describing which element will be displayed in the columns of the data.frame. Defaults to "Não ativa".
#' @param conteudo A character of length = 1 with the state's acronym of interest.
#' @param periodo A character vector describing the period of data. Defaults to the last available.
#' @param regiao "all" or a numeric vector with the IBGE's region codes to filter the data. Defaults to "all".
#' @param unidade_da_federacao "all" or a numeric vector with the IBGE's state codes to filter the data. Defaults to "all".
#' @param capitulo_cid10 "all" or a numeric vector with the ICD-10 chapter to filter the data. Defaults to "all".
#' @param categoria_cid10 "all" or a character vector with the ICD-10 category codes (capital letter and two numbers) to filter the data. Defaults to "all".
#' @param lista_mort_cid10 "all" or a character vector with the mortality type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param causa_mal_definidas "all" or a character vector with the ill-defined causes (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param causas_evitaveis_0a4anos "all" or a character vector with the evitable cause code (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
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
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso \code{\link{sim_evita10_mun}}
#' @examples
#' \donttest{
#' old_options <- options(
#'   datasus.timeout = 5,
#'   datasus.download_timeout = 15,
#'   datasus.max_tries = 1
#' )
#' try({
#' ## Requesting data from Midwest region
#' sim_inf10_bruf(regiao = 5)
#' })
#' options(old_options)
#' }
#'
#' @keywords SIM datasus child mortality
#' @section Lifecycle:
#' This function is deprecated. Use `sim()` or `sinasc()` with a named
#' `filtros` list. The legacy signature is retained for compatibility.
#' @export

sim_inf10_bruf <- function(linha = "Regi\u00e3o", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", regiao = "all",
                           unidade_da_federacao = "all", capitulo_cid10 = "all", categoria_cid10 = "all", lista_mort_cid10 = "all",
                           causa_mal_definidas = "all", causas_evitaveis_0a4anos = "all", faixa_etaria_1 = "all", faixa_etaria_2 = "all",
                           faixa_etaria_3 = "all", faixa_etaria_4 = "all", faixa_etaria_5 = "all", faixa_etaria_detalhada = "all", sexo = "all",
                           cor_raca = "all", local_ocorrencia = "all", idade_mae = "all", escolaridade_mae = "all", duracao_gestacao = "all",
                           tipo_gravidez = "all", tipo_parto = "all", peso_ao_nascer = "all", obito_relacao_parto = "all",
                           obito_investigado = "all") {
  .datasus_legacy_vital(
    old = "sim_inf10_bruf", replacement = "sim",
    sistema = "sim", conjunto = "mortalidade_infantil",
    abrangencia = "uf"
  )
}

