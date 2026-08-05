#' Scrapes SINASC data from ufs
#'
#' This function allows the user to retrive data from
#' SINASC database much in the same way that is done
#' by the online portal. The argument options refer to
#' data from the states unities.
#'
#' @usage sinasc_nv_uf(uf, linha = "Município", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", municipio = "all", cir = "all",
#'   macrorregiao_de_saude = "all", divisao_admnist_estadual = "all",
#'   microrregiao_ibge = "all", ride = "all", local_ocorrencia = "all",
#'   idade_da_mae = "all", instrucao_da_mae = "all", estado_civil_mae = "all",
#'   duracao_gestacao = "all", tipo_de_gravidez = "all", tipo_de_parto = "all",
#'   consult_pre_natal = "all", sexo = "all", cor_raca = "all", apgar_1_minuto = "all",
#'    apgar_5_minuto = "all", peso_ao_nascer = "all", anomalia_congenita = "all",
#'    tipo_anomal_congen = "all")
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
#' @param local_ocorrencia "all" or a character vector with the place of ocurrence to filter the data. Defaults to "all".
#' @param idade_da_mae "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param instrucao_da_mae "all" or a character vector with the mother's instruction (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param estado_civil_mae "all" or a character vector with the marital status of the mother (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param duracao_gestacao "all" or a character vector with the marital status of the mother (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_de_gravidez "all" or a character vector with the type of pregnancy (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_de_parto "all" or a character vector with the Parturition type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param consult_pre_natal "all" or a character vector with the amount of prenatal consultation  (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param sexo "all" or a character vector with the gender (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param cor_raca "all" or a character vector with the color/race (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param apgar_1_minuto "all" or a character vector with the value of the apgar exam of the first minute (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param apgar_5_minuto "all" or a character vector with the value of the apgar exam of the fifth minute (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param peso_ao_nascer "all" or a character vector with the birth weight (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param anomalia_congenita "all" or a character vector with the indicative of congenital anomaly (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_anomal_congen "all" or a character vector with the congenital anomaly type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Renato Prado Siqueira \email{rpradosiqueira@@gmail.com}
#' @seealso \code{\link{sinasc_nv_mun}}
#' @examples
#' \donttest{
#' old_options <- options(
#'   datasus.timeout = 5,
#'   datasus.download_timeout = 15,
#'   datasus.max_tries = 1
#' )
#' try({
#' ## Requesting data from the state of Mato Grosso do Sul
#' sinasc_nv_uf(uf = "ms")
#' })
#' options(old_options)
#' }
#'
#' @keywords SINASC datasus
#' @section Lifecycle:
#' This function is deprecated. Use `sim()` or `sinasc()` with a named
#' `filtros` list. The legacy signature is retained for compatibility.
#' @export

sinasc_nv_uf <- function(uf, linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", municipio = "all", cir = "all",
                         macrorregiao_de_saude = "all", divisao_admnist_estadual = "all", microrregiao_ibge = "all", ride = "all",
                         local_ocorrencia = "all", idade_da_mae = "all", instrucao_da_mae = "all", estado_civil_mae = "all",
                         duracao_gestacao = "all", tipo_de_gravidez = "all", tipo_de_parto = "all", consult_pre_natal = "all",
                         sexo = "all", cor_raca = "all", apgar_1_minuto = "all", apgar_5_minuto = "all", peso_ao_nascer = "all",
                         anomalia_congenita = "all", tipo_anomal_congen = "all") {
  .datasus_legacy_vital(
    old = "sinasc_nv_uf", replacement = "sinasc",
    sistema = "sinasc", conjunto = "nascidos_vivos",
    abrangencia = "municipio"
  )
}

