# DATASUS DBC/DBF microdata -----------------------------------------------

.microdata_catalog <- local({
  rows <- list(
    c("sim", "SIM", "DO", "Declara\u00e7\u00f5es de \u00f3bito",
      "UF", "anual"),
    c("sim", "SIM", "DOEXT", "\u00d3bitos por causas externas",
      "Brasil", "anual"),
    c("sim", "SIM", "DOFET", "\u00d3bitos fetais", "Brasil", "anual"),
    c("sim", "SIM", "DOINF", "\u00d3bitos infantis", "Brasil", "anual"),
    c("sim", "SIM", "DOMAT", "\u00d3bitos maternos", "Brasil", "anual"),
    c("sinasc", "SINASC", "DN", "Declara\u00e7\u00f5es de nascidos vivos",
      "UF", "anual"),
    c("sinasc", "SINASC", "DNEX",
      "Nascidos vivos residentes no exterior", "Brasil", "anual"),
    c("sih", "SIHSUS", "RD", "AIH reduzida", "UF", "mensal"),
    c("sih", "SIHSUS", "SP", "Servi\u00e7os profissionais",
      "UF", "mensal"),
    c("sih", "SIHSUS", "ER",
      "AIH rejeitadas com c\u00f3digo de erro", "UF", "mensal"),
    c("sih", "SIHSUS", "RJ", "AIH rejeitadas", "UF", "mensal")
  )
  result <- as.data.frame(
    do.call(rbind, rows),
    stringsAsFactors = FALSE
  )
  names(result) <- c(
    "sistema", "fonte", "tipo", "descricao", "escopo", "frequencia"
  )
  result
})

.microdata_default_type <- c(sim = "DO", sinasc = "DN", sih = "RD")

.microdata_resolve <- function(sistema, tipo = NULL) {
  if (!is.character(sistema) || length(sistema) != 1L ||
      is.na(sistema)) {
    stop("The 'sistema' argument must be a single system name",
         call. = FALSE)
  }
  sistema <- tolower(trimws(sistema))
  if (!sistema %in% names(.microdata_default_type)) {
    stop(
      "Unknown microdata system '", sistema,
      "'. Use microdados_catalogo() to inspect supported systems.",
      call. = FALSE
    )
  }
  if (is.null(tipo)) {
    tipo <- unname(.microdata_default_type[[sistema]])
  }
  if (!is.character(tipo) || length(tipo) != 1L || is.na(tipo)) {
    stop("The 'tipo' argument must be a single file type",
         call. = FALSE)
  }
  tipo <- toupper(trimws(tipo))
  selected <- .microdata_catalog[
    .microdata_catalog$sistema == sistema &
      .microdata_catalog$tipo == tipo,
    ,
    drop = FALSE
  ]
  if (!nrow(selected)) {
    stop(
      "Unknown file type '", tipo, "' for ", toupper(sistema),
      ". Use microdados_catalogo('", sistema, "') to inspect valid types.",
      call. = FALSE
    )
  }
  selected
}

.microdata_validate_year <- function(ano) {
  if (!is.numeric(ano) || !length(ano) || anyNA(ano) ||
      any(ano != as.integer(ano)) ||
      any(ano < 1970L | ano > as.integer(format(Sys.Date(), "%Y")) + 1L)) {
    stop("The 'ano' argument must contain valid four-digit years",
         call. = FALSE)
  }
  as.integer(ano)
}

.microdata_validate_month <- function(mes, required) {
  if (is.null(mes)) {
    if (required) {
      stop("The 'mes' argument is required for monthly systems",
           call. = FALSE)
    }
    return(NULL)
  }
  if (!is.numeric(mes) || !length(mes) || anyNA(mes) ||
      any(mes != as.integer(mes)) || any(mes < 1L | mes > 12L)) {
    stop("The 'mes' argument must contain integers from 1 to 12",
         call. = FALSE)
  }
  as.integer(mes)
}

.microdata_validate_ufs <- function(uf, required) {
  if (is.null(uf)) {
    if (required) {
      stop("The 'uf' argument is required for this file type",
           call. = FALSE)
    }
    return(NULL)
  }
  if (!is.atomic(uf) || is.list(uf) || !length(uf) || anyNA(uf)) {
    stop("The 'uf' argument must contain state identifiers",
         call. = FALSE)
  }
  unname(toupper(vapply(uf, .tabnet_validate_uf, character(1))))
}

.microdata_cache_dir <- function() {
  file.path(.datasus_cache_root(), "microdados")
}

#' List supported DATASUS microdata files
#'
#' Returns the local catalog of raw DBC/DBF file families supported by the
#' package. These files provide record-level data, unlike aggregated TABNET
#' results.
#'
#' @param sistema Optional system: `"sim"`, `"sinasc"` or `"sih"`.
#'
#' @return A data frame with system, upstream source, file type, description,
#'   geographic scope and frequency.
#' @export
#'
#' @examples
#' microdados_catalogo()
#' microdados_catalogo("sih")
microdados_catalogo <- function(sistema = NULL) {
  result <- .microdata_catalog
  if (!is.null(sistema)) {
    if (!is.character(sistema) || length(sistema) != 1L ||
        is.na(sistema)) {
      stop("The 'sistema' argument must be a single system name",
           call. = FALSE)
    }
    sistema <- tolower(trimws(sistema))
    if (!sistema %in% names(.microdata_default_type)) {
      stop("Unknown microdata system '", sistema, "'", call. = FALSE)
    }
    result <- result[result$sistema == sistema, , drop = FALSE]
  }
  row.names(result) <- NULL
  result
}

#' Discover raw DATASUS microdata files
#'
#' Builds the official DATASUS transfer paths and optionally checks whether
#' each file currently exists. SIM and SINASC files are annual; SIH files are
#' monthly.
#'
#' @param sistema System: `"sim"`, `"sinasc"` or `"sih"`.
#' @param tipo File type returned by [microdados_catalogo()]. `NULL` selects
#'   the default type for the system.
#' @param ano One or more four-digit years.
#' @param mes One or more months from 1 to 12. Required for SIH.
#' @param uf One or more state abbreviations, two-digit IBGE codes or state
#'   names. Required for UF-scoped types.
#' @param preliminares Whether preliminary directories may be considered.
#' @param verificar Whether to verify file existence on the DATASUS server.
#' @param timeout Network timeout in seconds for file discovery.
#'
#' @return A data frame containing official file names, URLs and existence
#'   checks. Provenance records the selection and consultation time.
#' @export
#'
#' @examples
#' \dontrun{
#' microdados_arquivos("sim", ano = 2023, uf = "RR")
#' microdados_arquivos("sih", ano = 2024, mes = 1, uf = "AC")
#' }
microdados_arquivos <- function(sistema, tipo = NULL, ano, mes = NULL,
                                uf = NULL, preliminares = TRUE,
                                verificar = TRUE, timeout = 120) {
  metadata <- .microdata_resolve(sistema, tipo)
  ano <- .microdata_validate_year(ano)
  monthly <- identical(metadata$frequencia, "mensal")
  mes <- .microdata_validate_month(mes, monthly)
  if (!monthly && !is.null(mes)) {
    stop("The 'mes' argument is only available for monthly systems",
         call. = FALSE)
  }
  uf <- .microdata_validate_ufs(uf, identical(metadata$escopo, "UF"))
  if (identical(metadata$escopo, "Brasil") && !is.null(uf)) {
    stop("The 'uf' argument is not available for Brazil-scoped file types",
         call. = FALSE)
  }
  for (value in list(preliminares, verificar)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("'preliminares' and 'verificar' must be TRUE or FALSE",
           call. = FALSE)
    }
  }
  if (!is.numeric(timeout) || length(timeout) != 1L ||
      is.na(timeout) || timeout <= 0) {
    stop("The 'timeout' argument must be a positive number",
         call. = FALSE)
  }

  files <- datasusr::datasus_list_files(
    source = metadata$fonte,
    file_type = metadata$tipo,
    year = ano,
    month = mes,
    uf = uf,
    include_prelim = preliminares,
    check_exists = verificar,
    timeout = timeout,
    verbose = FALSE
  )
  files <- as.data.frame(files, stringsAsFactors = FALSE)
  result <- data.frame(
    sistema = metadata$sistema,
    tipo = files$file_type,
    descricao = files$description,
    escopo = files$scope,
    frequencia = files$frequency,
    periodo = files$period,
    arquivo = files$file_name,
    url = files$url,
    existe = files$exists,
    stringsAsFactors = FALSE
  )
  attr(result, "datasusr_files") <- files
  attr(result, "datasus_proveniencia") <- list(
    fonte = "ftp://ftp.datasus.gov.br/dissemin/publicos/",
    consultado_em = Sys.time(),
    sistema = metadata$sistema,
    tipo = metadata$tipo,
    ano = ano,
    mes = mes,
    uf = uf,
    preliminares = preliminares,
    existencia_verificada = verificar
  )
  result
}

.microdata_download_provenance <- function(downloaded, metadata) {
  paths <- downloaded$local_file
  checksums <- unname(tools::md5sum(paths))
  files <- lapply(seq_len(nrow(downloaded)), function(index) {
    list(
      fonte = downloaded$url[[index]],
      sistema = metadata$sistema,
      tipo = metadata$tipo,
      arquivo = downloaded$file_name[[index]],
      arquivo_local = normalizePath(
        paths[[index]], winslash = "/", mustWork = FALSE
      ),
      md5 = checksums[[index]],
      baixado = isTRUE(downloaded$downloaded[[index]])
    )
  })
  list(
    fonte = "DATASUS File Transfer",
    consultado_em = Sys.time(),
    sistema = metadata$sistema,
    tipo = metadata$tipo,
    arquivos = files
  )
}

#' Download raw DATASUS DBC files
#'
#' Discovers and downloads one or more official DBC files. Downloads use a
#' dedicated cache below option `datasus.cache_dir`; `atualizar = TRUE`
#' requests a fresh copy.
#'
#' @inheritParams microdados_arquivos
#' @param destino Optional destination directory. `NULL` uses the package
#'   cache, or a session temporary directory when `cache = FALSE`.
#' @param cache Whether to reuse the disk cache.
#' @param atualizar Whether to request a fresh copy of already cached files.
#'
#' @return A data frame describing the local files. Use
#'   [datasus_proveniencia()] for URLs and checksums.
#' @export
#'
#' @examples
#' \dontrun{
#' files <- microdados_baixar("sih", ano = 2024, mes = 1, uf = "AC")
#' datasus_proveniencia(files)
#' }
microdados_baixar <- function(sistema, tipo = NULL, ano, mes = NULL,
                              uf = NULL, destino = NULL, cache = TRUE,
                              atualizar = FALSE, preliminares = TRUE,
                              verificar = TRUE, timeout = 240) {
  metadata <- .microdata_resolve(sistema, tipo)
  if (!is.numeric(timeout) || length(timeout) != 1L ||
      is.na(timeout) || timeout <= 0) {
    stop("The 'timeout' argument must be a positive number",
         call. = FALSE)
  }
  choices <- microdados_arquivos(
    sistema = metadata$sistema,
    tipo = metadata$tipo,
    ano = ano,
    mes = mes,
    uf = uf,
    preliminares = preliminares,
    verificar = verificar,
    timeout = min(timeout, 120)
  )
  files <- attr(choices, "datasusr_files", exact = TRUE)

  if (verificar && any(!files$exists)) {
    missing <- files$file_name[!files$exists]
    stop(
      "The following DATASUS file(s) are unavailable: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  for (value in list(cache, atualizar)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("'cache' and 'atualizar' must be TRUE or FALSE",
           call. = FALSE)
    }
  }
  if (!is.null(destino)) {
    if (!is.character(destino) || length(destino) != 1L ||
        is.na(destino) || !nzchar(destino)) {
      stop("The 'destino' argument must be a directory path",
           call. = FALSE)
    }
    destino <- path.expand(destino)
    if (!dir.exists(destino) &&
        !dir.create(destino, recursive = TRUE, showWarnings = FALSE)) {
      stop("Could not create the download directory: ", destino,
           call. = FALSE)
    }
  } else if (!cache) {
    destino <- tempdir()
  }

  downloaded <- datasusr::datasus_download(
    files,
    dest_dir = destino,
    overwrite = atualizar,
    timeout = timeout,
    use_cache = cache,
    cache_dir = .microdata_cache_dir(),
    refresh = atualizar,
    verbose = FALSE
  )
  downloaded <- as.data.frame(downloaded, stringsAsFactors = FALSE)
  result <- data.frame(
    sistema = metadata$sistema,
    tipo = downloaded$file_type,
    arquivo = downloaded$file_name,
    url = downloaded$url,
    arquivo_local = normalizePath(
      downloaded$local_file, winslash = "/", mustWork = FALSE
    ),
    baixado = downloaded$downloaded,
    stringsAsFactors = FALSE
  )
  attr(result, "datasus_proveniencia") <-
    .microdata_download_provenance(downloaded, metadata)
  result
}

.microdata_bind_rows <- function(values) {
  if (length(values) == 1L) {
    return(values[[1L]])
  }
  columns <- unique(unlist(lapply(values, names), use.names = FALSE))
  values <- lapply(values, function(value) {
    missing <- setdiff(columns, names(value))
    for (name in missing) {
      value[[name]] <- NA
    }
    value[columns]
  })
  result <- do.call(rbind, values)
  row.names(result) <- NULL
  result
}

.microdata_read_paths <- function(paths, sistema, tipo, colunas = NULL,
                                  n_max = Inf, normalizar = FALSE,
                                  juntar = TRUE, ...) {
  values <- lapply(seq_along(paths), function(index) {
    value <- datasusr::read_datasus_dbc(
      paths[[index]],
      select = colunas,
      n_max = n_max,
      verbose = FALSE,
      ...
    )
    if (!is.null(colunas)) {
      selected <- match(tolower(colunas), tolower(names(value)))
      if (anyNA(selected)) {
        stop(
          "Requested microdata column(s) not found after decoding: ",
          paste(colunas[is.na(selected)], collapse = ", "),
          call. = FALSE
        )
      }
      value <- value[selected]
    }
    if (length(paths) > 1L) {
      value$arquivo_origem <- basename(paths[[index]])
    }
    if (normalizar) {
      value <- datasus_padronizar(value, sistema, tipo)
    }
    value
  })
  if (!juntar) {
    names(values) <- basename(paths)
    return(values)
  }
  .microdata_bind_rows(values)
}

#' Download and read raw DATASUS microdata
#'
#' Downloads the requested DBC files and reads them directly in memory using
#' the native `datasusr` decoder. Column selection and `n_max` are applied
#' during parsing, which is useful for large SIH files.
#'
#' @inheritParams microdados_baixar
#' @param colunas Optional vector of original or lower-case columns to read.
#' @param n_max Maximum number of records to read from each file.
#' @param normalizar Whether to rename and type common fields with
#'   [datasus_padronizar()].
#' @param juntar Whether to combine multiple files. If `FALSE`, returns a named
#'   list.
#' @param ... Additional arguments passed to
#'   [datasusr::read_datasus_dbc()].
#'
#' @return A data frame, or a named list when `juntar = FALSE`, with
#'   provenance metadata attached.
#' @export
#'
#' @examples
#' \dontrun{
#' admissions <- microdados_ler(
#'   "sih",
#'   ano = 2024,
#'   mes = 1,
#'   uf = "AC",
#'   colunas = c("MUNIC_RES", "DT_INTER", "DIAG_PRINC", "VAL_TOT"),
#'   n_max = 1000,
#'   normalizar = TRUE
#' )
#' }
microdados_ler <- function(sistema, tipo = NULL, ano, mes = NULL,
                           uf = NULL, destino = NULL, cache = TRUE,
                           atualizar = FALSE, preliminares = TRUE,
                           verificar = TRUE, timeout = 240,
                           colunas = NULL, n_max = Inf,
                           normalizar = FALSE, juntar = TRUE, ...) {
  metadata <- .microdata_resolve(sistema, tipo)
  for (value in list(normalizar, juntar)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("'normalizar' and 'juntar' must be TRUE or FALSE",
           call. = FALSE)
    }
  }
  if (!is.numeric(n_max) || length(n_max) != 1L ||
      is.na(n_max) || n_max < 0) {
    stop("The 'n_max' argument must be a non-negative number",
         call. = FALSE)
  }

  downloaded <- microdados_baixar(
    sistema = metadata$sistema,
    tipo = metadata$tipo,
    ano = ano,
    mes = mes,
    uf = uf,
    destino = destino,
    cache = cache,
    atualizar = atualizar,
    preliminares = preliminares,
    verificar = verificar,
    timeout = timeout
  )
  provenance <- datasus_proveniencia(downloaded)
  result <- .microdata_read_paths(
    downloaded$arquivo_local,
    metadata$sistema,
    metadata$tipo,
    colunas = colunas,
    n_max = n_max,
    normalizar = normalizar,
    juntar = juntar,
    ...
  )
  attr(result, "datasus_proveniencia") <- provenance
  result
}

#' Read SIM death microdata
#'
#' @param ano One or more years.
#' @param uf One or more state abbreviations.
#' @param tipo SIM file type, default `"DO"`.
#' @param ... Additional arguments passed to [microdados_ler()].
#'
#' @return Record-level SIM data.
#' @export
sim_microdados <- function(ano, uf = NULL, tipo = "DO", ...) {
  microdados_ler("sim", tipo = tipo, ano = ano, uf = uf, ...)
}

#' Read SINASC live birth microdata
#'
#' @inheritParams sim_microdados
#'
#' @return Record-level SINASC data.
#' @export
sinasc_microdados <- function(ano, uf = NULL, tipo = "DN", ...) {
  microdados_ler("sinasc", tipo = tipo, ano = ano, uf = uf, ...)
}

#' Read SIH hospitalization microdata
#'
#' @param ano One or more years.
#' @param mes One or more months.
#' @param uf One or more state abbreviations.
#' @param tipo SIH file type, default `"RD"`.
#' @param ... Additional arguments passed to [microdados_ler()].
#'
#' @return Record-level SIH data.
#' @export
sih_microdados <- function(ano, mes, uf, tipo = "RD", ...) {
  microdados_ler(
    "sih", tipo = tipo, ano = ano, mes = mes, uf = uf, ...
  )
}

# Curated common fields ----------------------------------------------------

.datasus_dictionary <- local({
  rows <- list(
    c("sim", "*", "dtobito", "data_obito", "Data do \u00f3bito",
      "date", "%d%m%Y"),
    c("sim", "*", "dtnasc", "data_nascimento", "Data de nascimento",
      "date", "%d%m%Y"),
    c("sim", "*", "codmunres", "codigo_municipio_residencia",
      "Munic\u00edpio de resid\u00eancia", "character", ""),
    c("sim", "*", "codmunocor", "codigo_municipio_ocorrencia",
      "Munic\u00edpio de ocorr\u00eancia", "character", ""),
    c("sim", "*", "sexo", "sexo", "Sexo", "character", ""),
    c("sim", "*", "idade", "idade_codificada",
      "Idade codificada pelo padr\u00e3o do SIM", "character", ""),
    c("sim", "*", "causabas", "causa_basica_cid10",
      "Causa b\u00e1sica CID-10", "character", ""),
    c("sim", "*", "racacor", "raca_cor", "Ra\u00e7a/cor",
      "character", ""),
    c("sinasc", "*", "dtnasc", "data_nascimento",
      "Data de nascimento", "date", "%d%m%Y"),
    c("sinasc", "*", "codmunres", "codigo_municipio_residencia",
      "Munic\u00edpio de resid\u00eancia", "character", ""),
    c("sinasc", "*", "codmunnasc", "codigo_municipio_nascimento",
      "Munic\u00edpio de nascimento", "character", ""),
    c("sinasc", "*", "sexo", "sexo", "Sexo", "character", ""),
    c("sinasc", "*", "idademae", "idade_mae",
      "Idade da m\u00e3e", "numeric", ""),
    c("sinasc", "*", "peso", "peso_gramas",
      "Peso ao nascer em gramas", "numeric", ""),
    c("sinasc", "*", "racacor", "raca_cor", "Ra\u00e7a/cor",
      "character", ""),
    c("sih", "RD", "n_aih", "aih", "N\u00famero da AIH",
      "character", ""),
    c("sih", "RD", "cnes", "cnes", "C\u00f3digo CNES",
      "character", ""),
    c("sih", "RD", "munic_res", "codigo_municipio_residencia",
      "Munic\u00edpio de resid\u00eancia", "character", ""),
    c("sih", "RD", "dt_inter", "data_internacao",
      "Data de interna\u00e7\u00e3o", "date", "%Y%m%d"),
    c("sih", "RD", "dt_saida", "data_saida",
      "Data de sa\u00edda", "date", "%Y%m%d"),
    c("sih", "RD", "diag_princ", "diagnostico_principal_cid10",
      "Diagn\u00f3stico principal CID-10", "character", ""),
    c("sih", "RD", "val_tot", "valor_total",
      "Valor total aprovado", "numeric", ""),
    c("sih", "RD", "sexo", "sexo", "Sexo", "character", ""),
    c("sih", "RD", "idade", "idade_anos", "Idade em anos",
      "numeric", ""),
    c("esavi", "*", "nu_notificacao", "id_notificacao",
      "Identificador da notificacao", "character", ""),
    c("esavi", "*", "ds_sexo", "sexo", "Sexo", "character", ""),
    c("esavi", "*", "nu_idade", "idade", "Idade no evento",
      "numeric", ""),
    c("esavi", "*", "st_comunidade_tradicional",
      "comunidade_tradicional", "Pertence a comunidade tradicional",
      "logical", ""),
    c("esavi", "*", "ds_not_mae_filho", "exposicao_materna",
      "Exposicao pela gestacao ou aleitamento", "logical", ""),
    c("esavi", "*", "nu_mes_gestante", "mes_gestacao",
      "Mes de gestacao na vacinacao", "numeric", ""),
    c("esavi", "*", "ds_versao_medra", "versao_meddra",
      "Versao da terminologia MedDRA", "character", ""),
    c("esavi", "*", "ds_mulher_amamentando", "amamentando",
      "Amamentando no momento da vacinacao", "logical", ""),
    c("esavi", "*", "ds_profissional_seguranca",
      "profissional_seguranca", "Profissional de seguranca",
      "logical", ""),
    c("esavi", "*", "ds_raca_cor", "raca_cor",
      "Raca ou cor", "character", ""),
    c("esavi", "*", "ds_raca_cor_mae", "raca_cor_mae",
      "Raca ou cor da mae", "character", ""),
    c("esavi", "*", "ds_gestante", "gestante",
      "Gestante no momento da vacinacao", "character", ""),
    c("esavi", "*", "ds_crianca_aleitamento",
      "crianca_em_aleitamento",
      "Crianca em aleitamento no momento da vacinacao", "logical", ""),
    c("esavi", "*", "ds_estrangeiro", "estrangeiro",
      "Pessoa estrangeira", "logical", ""),
    c("esavi", "*", "ds_profissional_saude",
      "profissional_saude", "Profissional de saude", "logical", ""),
    c("esavi", "*", "no_estado", "estado_residencia",
      "Estado de residencia", "character", ""),
    c("esavi", "*", "no_municipio", "municipio_residencia",
      "Municipio de residencia", "character", ""),
    c("esavi", "*", "no_estado_notificacao", "estado_notificacao",
      "Estado da notificacao", "character", ""),
    c("esavi", "*", "no_mun_notificacao", "municipio_notificacao",
      "Municipio da notificacao", "character", ""),
    c("esavi", "*", "no_estado_mae", "estado_residencia_mae",
      "Estado de residencia da mae", "character", ""),
    c("esavi", "*", "no_mun_mae", "municipio_residencia_mae",
      "Municipio de residencia da mae", "character", ""),
    c("esavi", "*", "dt_notificacao", "data_notificacao",
      "Data da notificacao", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "dt_recebimento_notificacao",
      "data_recebimento_notificacao", "Data de recebimento",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "dt_investigacao", "data_investigacao",
      "Data da investigacao", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "dt_desfecho", "data_desfecho",
      "Data do desfecho", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "dt_encerramento", "data_encerramento",
      "Data do encerramento", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "dt_inicio_ea", "data_inicio_evento",
      "Data de inicio do evento", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "dt_termino_ea", "data_termino_evento",
      "Data de termino do evento", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "dt_aplicacao_imuno", "data_vacinacao",
      "Data da aplicacao", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "ds_medicamento_uso", "medicamento_em_uso",
      "Medicamento anterior ou durante a vacinacao", "logical", ""),
    c("esavi", "*", "ds_atendimento_medico", "atendimento_medico",
      "Houve atendimento medico", "logical", ""),
    c("esavi", "*", "ds_mae_gestante", "mae_gestante",
      "Mae gestante no momento da vacinacao", "character", ""),
    c("esavi", "*", "nu_mes_gestacao_mae", "mes_gestacao_mae",
      "Mes de gestacao da mae", "numeric", ""),
    c("esavi", "*", "ds_mae_amamentando", "mae_amamentando",
      "Mae amamentando no momento da vacinacao", "logical", ""),
    c("esavi", "*", "ds_situacao_notificacao",
      "situacao_notificacao", "Situacao da notificacao", "character", ""),
    c("esavi", "*", "ds_tipo_atendimento", "tipo_atendimento",
      "Tipo de atendimento", "character", ""),
    c("esavi", "*", "no_estado_estab_atendimento",
      "estado_atendimento", "Estado do atendimento", "character", ""),
    c("esavi", "*", "dt_admissao_atendimento", "data_admissao",
      "Data da admissao", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "no_mun_estab_atendimento",
      "municipio_atendimento", "Municipio do atendimento",
      "character", ""),
    c("esavi", "*", "dt_alta_atendimento", "data_alta",
      "Data da alta", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("esavi", "*", "ds_evento_adverso", "evento_adverso",
      "Evento adverso no encerramento", "character", ""),
    c("esavi", "*", "ds_evolucao_caso", "evolucao",
      "Evolucao do caso", "character", ""),
    c("esavi", "*", "ds_encerramento_grave", "grave",
      "Classificacao de gravidade", "character", ""),
    c("esavi", "*", "ds_causalidade", "causalidade",
      "Avaliacao de causalidade", "character", ""),
    c("esavi", "*", "ds_conduta", "conduta",
      "Conduta adotada", "character", ""),
    c("esavi", "*", "ds_diagnostico", "diagnostico_cid10",
      "Diagnostico CID-10", "character", ""),
    c("esavi", "*", "ds_doencas_pre_existentes",
      "doencas_pre_existentes_cid10",
      "Doencas pre-existentes CID-10", "character", ""),
    c("esavi", "*", "ds_tipo_encerramento", "tipo_encerramento",
      "Esfera de encerramento", "character", ""),
    c("esavi", "*", "ds_class_gravidade_ea",
      "classificacao_gravidade", "Classificacao de gravidade",
      "character", ""),
    c("esavi", "*", "ds_hora_reacao_intervalo_admin_ea",
      "intervalo_reacao_horas",
      "Intervalo entre administracao e reacao em horas", "numeric", ""),
    c("esavi", "*", "hr_duracao_ea", "duracao_evento_horas",
      "Duracao do evento em horas", "numeric", ""),
    c("esavi", "*", "ds_gravidade_ea", "criterio_gravidade",
      "Criterio de gravidade", "character", ""),
    c("esavi", "*", "ds_dia_reacao_intervalo_admin_ea",
      "intervalo_reacao_dias",
      "Intervalo entre administracao e reacao em dias", "numeric", ""),
    c("esavi", "*", "ds_minuto_reacao_intervalo_admin_ea",
      "intervalo_reacao_minutos",
      "Intervalo entre administracao e reacao em minutos",
      "numeric", ""),
    c("esavi", "*", "ds_tipo_ea", "tipo_evento",
      "Tipo de evento", "character", ""),
    c("esavi", "*", "ds_reacao_ea", "reacao_evento",
      "Reacao ou evento adverso", "character", ""),
    c("esavi", "*", "co_reacao_ea", "codigo_reacao_meddra",
      "Codigo MedDRA da reacao", "character", ""),
    c("esavi", "*", "ds_hora_inicio_ea", "hora_inicio_evento",
      "Hora de inicio do evento", "character", ""),
    c("esavi", "*", "ds_hora_termino_ea", "hora_termino_evento",
      "Hora de termino do evento", "character", ""),
    c("esavi", "*", "ds_dia_duracao_ea", "duracao_evento_dias",
      "Duracao do evento em dias", "numeric", ""),
    c("esavi", "*", "ds_minuto_duracao_ea",
      "duracao_evento_minutos", "Duracao do evento em minutos",
      "numeric", ""),
    c("esavi", "*", "ds_estrategia_imuno",
      "estrategia_vacinacao", "Estrategia de vacinacao", "character", ""),
    c("esavi", "*", "ds_via_admin_imuno",
      "via_administracao", "Via de administracao", "character", ""),
    c("esavi", "*", "ds_local_aplica_imuno",
      "local_aplicacao", "Local de aplicacao", "character", ""),
    c("esavi", "*", "hr_aplica_imuno", "hora_vacinacao",
      "Hora da aplicacao", "character", ""),
    c("esavi", "*", "ds_nome_fabricante", "fabricante",
      "Fabricante do imunobiologico", "character", ""),
    c("esavi", "*", "ds_relacao_imuno",
      "relacao_imunobiologico_evento",
      "Relacao do imunobiologico com o evento", "character", ""),
    c("esavi", "*", "co_imuno", "codigo_imunobiologico",
      "Codigo do imunobiologico", "character", ""),
    c("esavi", "*", "ds_imuno", "imunobiologico",
      "Imunobiologico", "character", ""),
    c("esavi", "*", "ds_lote_imuno", "lote",
      "Lote do imunobiologico", "character", ""),
    c("esavi", "*", "ds_dose_imuno", "dose",
      "Dose do imunobiologico", "character", ""),
    c("esavi", "*", "ds_medicamento", "medicamento",
      "Medicamento informado", "character", ""),
    c("esavi", "*", "ds_relacao_medicamento",
      "relacao_medicamento_evento",
      "Relacao do medicamento com o evento", "character", ""),
    c("sindrome_gripal", "*", "estadoNotificacao",
      "estado_notificacao", "Estado da notificacao", "character", ""),
    c("sindrome_gripal", "*", "estadoNotificacaoIBGE",
      "codigo_uf_notificacao", "Codigo IBGE da UF de notificacao",
      "character", ""),
    c("sindrome_gripal", "*", "municipioNotificacao",
      "municipio_notificacao", "Municipio da notificacao",
      "character", ""),
    c("sindrome_gripal", "*", "municipioNotificacaoIBGE",
      "codigo_municipio_notificacao",
      "Codigo IBGE do municipio de notificacao", "character", ""),
    c("sindrome_gripal", "*", "profissionalSaude",
      "profissional_saude", "Profissional de saude", "logical", ""),
    c("sindrome_gripal", "*", "profissionalSeguranca",
      "profissional_seguranca", "Profissional de seguranca",
      "logical", ""),
    c("sindrome_gripal", "*", "cbo", "cbo",
      "Familia ocupacional CBO", "character", ""),
    c("sindrome_gripal", "*", "idade", "idade",
      "Idade", "numeric", ""),
    c("sindrome_gripal", "*", "sexo", "sexo",
      "Sexo", "character", ""),
    c("sindrome_gripal", "*", "racaCor", "raca_cor",
      "Raca ou cor", "character", ""),
    c("sindrome_gripal", "*", "codigoContemComunidadeTradicional",
      "codigo_comunidade_tradicional",
      "Indicador de comunidade tradicional", "character", ""),
    c("sindrome_gripal", "*", "estado", "estado_residencia",
      "Estado de residencia", "character", ""),
    c("sindrome_gripal", "*", "estadoIBGE", "codigo_uf_residencia",
      "Codigo IBGE da UF de residencia", "character", ""),
    c("sindrome_gripal", "*", "municipio", "municipio_residencia",
      "Municipio de residencia", "character", ""),
    c("sindrome_gripal", "*", "municipioIBGE",
      "codigo_municipio_residencia",
      "Codigo IBGE do municipio de residencia", "character", ""),
    c("sindrome_gripal", "*", "codigoEstrategiaCovid",
      "codigo_estrategia_testagem",
      "Codigo da estrategia de testagem", "character", ""),
    c("sindrome_gripal", "*", "codigoBuscaAtivaAssintomatico",
      "codigo_busca_ativa_assintomatico",
      "Codigo da busca ativa de assintomatico", "character", ""),
    c("sindrome_gripal", "*", "outroBuscaAtivaAssintomatico",
      "outra_busca_ativa_assintomatico",
      "Outra busca ativa de assintomatico", "character", ""),
    c("sindrome_gripal", "*", "codigoTriagemPopulacaoEspecifica",
      "codigo_triagem_populacao_especifica",
      "Codigo da triagem de populacao especifica", "character", ""),
    c("sindrome_gripal", "*", "outroTriagemPopulacaoEspecifica",
      "outra_triagem_populacao_especifica",
      "Outra triagem de populacao especifica", "character", ""),
    c("sindrome_gripal", "*", "codigoLocalRealizacaoTestagem",
      "codigo_local_testagem", "Codigo do local de testagem",
      "character", ""),
    c("sindrome_gripal", "*", "outroLocalRealizacaoTestagem",
      "outro_local_testagem", "Outro local de testagem",
      "character", ""),
    c("sindrome_gripal", "*", "dataNotificacao", "data_notificacao",
      "Data da notificacao", "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "sintomas", "sintomas",
      "Sintomas informados", "character", ""),
    c("sindrome_gripal", "*", "outrosSintomas", "outros_sintomas",
      "Outros sintomas", "character", ""),
    c("sindrome_gripal", "*", "dataInicioSintomas",
      "data_inicio_sintomas", "Data de inicio dos sintomas",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "condicoes", "condicoes",
      "Condicoes informadas", "character", ""),
    c("sindrome_gripal", "*", "outrasCondicoes",
      "outras_condicoes", "Outras condicoes", "character", ""),
    c("sindrome_gripal", "*", "codigoRecebeuVacina",
      "codigo_recebeu_vacina",
      "Indicador de recebimento de vacina", "character", ""),
    c("sindrome_gripal", "*", "codigoDosesVacina",
      "codigo_doses_vacina", "Codigo das doses recebidas",
      "character", ""),
    c("sindrome_gripal", "*", "dataPrimeiraDose",
      "data_primeira_dose", "Data da primeira dose",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "dataSegundaDose",
      "data_segunda_dose", "Data da segunda dose",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "codigoLaboratorioPrimeiraDose",
      "codigo_laboratorio_primeira_dose",
      "Laboratorio da primeira dose", "character", ""),
    c("sindrome_gripal", "*", "codigoLaboratorioSegundaDose",
      "codigo_laboratorio_segunda_dose",
      "Laboratorio da segunda dose", "character", ""),
    c("sindrome_gripal", "*", "lotePrimeiraDose",
      "lote_primeira_dose", "Lote da primeira dose", "character", ""),
    c("sindrome_gripal", "*", "loteSegundaDose",
      "lote_segunda_dose", "Lote da segunda dose", "character", ""),
    c("sindrome_gripal", "*", "totalTestesRealizados",
      "total_testes_realizados", "Total de testes realizados",
      "numeric", ""),
    c("sindrome_gripal", "*", "codigoTipoTeste1",
      "codigo_tipo_teste_1", "Tipo do primeiro teste", "character", ""),
    c("sindrome_gripal", "*", "codigoEstadoTeste1",
      "codigo_estado_teste_1", "Estado do primeiro teste",
      "character", ""),
    c("sindrome_gripal", "*", "dataColetaTeste1",
      "data_coleta_teste_1", "Data da primeira coleta",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "codigoResultadoTeste1",
      "codigo_resultado_teste_1", "Resultado do primeiro teste",
      "character", ""),
    c("sindrome_gripal", "*", "codigoFabricanteTeste1",
      "codigo_fabricante_teste_1", "Fabricante do primeiro teste",
      "character", ""),
    c("sindrome_gripal", "*", "codigoTipoTeste2",
      "codigo_tipo_teste_2", "Tipo do segundo teste", "character", ""),
    c("sindrome_gripal", "*", "codigoEstadoTeste2",
      "codigo_estado_teste_2", "Estado do segundo teste",
      "character", ""),
    c("sindrome_gripal", "*", "dataColetaTeste2",
      "data_coleta_teste_2", "Data da segunda coleta",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "codigoResultadoTeste2",
      "codigo_resultado_teste_2", "Resultado do segundo teste",
      "character", ""),
    c("sindrome_gripal", "*", "codigoFabricanteTeste2",
      "codigo_fabricante_teste_2", "Fabricante do segundo teste",
      "character", ""),
    c("sindrome_gripal", "*", "codigoTipoTeste3",
      "codigo_tipo_teste_3", "Tipo do terceiro teste", "character", ""),
    c("sindrome_gripal", "*", "codigoEstadoTeste3",
      "codigo_estado_teste_3", "Estado do terceiro teste",
      "character", ""),
    c("sindrome_gripal", "*", "dataColetaTeste3",
      "data_coleta_teste_3", "Data da terceira coleta",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "codigoResultadoTeste3",
      "codigo_resultado_teste_3", "Resultado do terceiro teste",
      "character", ""),
    c("sindrome_gripal", "*", "codigoFabricanteTeste3",
      "codigo_fabricante_teste_3", "Fabricante do terceiro teste",
      "character", ""),
    c("sindrome_gripal", "*", "codigoTipoTeste4",
      "codigo_tipo_teste_4", "Tipo do quarto teste", "character", ""),
    c("sindrome_gripal", "*", "codigoEstadoTeste4",
      "codigo_estado_teste_4", "Estado do quarto teste",
      "character", ""),
    c("sindrome_gripal", "*", "dataColetaTeste4",
      "data_coleta_teste_4", "Data da quarta coleta",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "codigoResultadoTeste4",
      "codigo_resultado_teste_4", "Resultado do quarto teste",
      "character", ""),
    c("sindrome_gripal", "*", "codigoFabricanteTeste4",
      "codigo_fabricante_teste_4", "Fabricante do quarto teste",
      "character", ""),
    c("sindrome_gripal", "*", "evolucaoCaso", "evolucao",
      "Evolucao do caso", "character", ""),
    c("sindrome_gripal", "*", "classificacaoFinal",
      "classificacao_final", "Classificacao final", "character", ""),
    c("sindrome_gripal", "*", "dataEncerramento",
      "data_encerramento", "Data do encerramento",
      "date", "%d/%m/%Y|%Y-%m-%d"),
    c("sindrome_gripal", "*", "origem", "origem",
      "Origem do registro", "character", ""),
    c("sindrome_gripal", "*", "excluido", "excluido",
      "Registro excluido", "logical", ""),
    c("sindrome_gripal", "*", "validado", "validado",
      "Registro validado", "logical", ""),
    c("pni_doses", "*", "co_documento", "id_documento",
      "Identificador do documento RNDS", "character", ""),
    c("pni_doses", "*", "co_paciente", "id_paciente",
      "Identificador anonimo do paciente", "character", ""),
    c("pni_doses", "*", "tp_sexo_paciente", "sexo",
      "Sexo biologico", "character", ""),
    c("pni_doses", "*", "co_raca_cor_paciente", "codigo_raca_cor",
      "Codigo de raca/cor", "character", ""),
    c("pni_doses", "*", "no_raca_cor_paciente", "raca_cor",
      "Raca/cor", "character", ""),
    c("pni_doses", "*", "co_municipio_paciente",
      "codigo_municipio_residencia",
      "Codigo IBGE do municipio de residencia", "character", ""),
    c("pni_doses", "*", "co_pais_paciente",
      "codigo_pais_residencia", "Codigo do pais de residencia",
      "character", ""),
    c("pni_doses", "*", "no_municipio_paciente",
      "municipio_residencia", "Municipio de residencia",
      "character", ""),
    c("pni_doses", "*", "no_pais_paciente", "pais_residencia",
      "Pais de residencia", "character", ""),
    c("pni_doses", "*", "sg_uf_paciente", "uf_residencia",
      "UF de residencia", "character", ""),
    c("pni_doses", "*", "nu_cep_paciente", "cep_residencia",
      "CEP de residencia", "character", ""),
    c("pni_doses", "*", "ds_nacionalidade_paciente",
      "nacionalidade", "Nacionalidade do paciente", "character", ""),
    c("pni_doses", "*", "st_vida_paciente", "situacao_vida",
      "Situacao de vida do paciente", "character", ""),
    c("pni_doses", "*", "no_etnia_indigena_paciente",
      "etnia_indigena", "Etnia indigena do paciente", "character", ""),
    c("pni_doses", "*", "co_etnia_indigena_paciente",
      "codigo_etnia_indigena", "Codigo da etnia indigena",
      "character", ""),
    c("pni_doses", "*", "nu_idade_paciente", "idade",
      "Idade do paciente", "numeric", ""),
    c("pni_doses", "*", "co_cnes_estabelecimento", "cnes",
      "Codigo CNES do estabelecimento", "character", ""),
    c("pni_doses", "*", "no_razao_social_estabelecimento",
      "razao_social_estabelecimento",
      "Razao social do estabelecimento", "character", ""),
    c("pni_doses", "*", "no_fantasia_estalecimento",
      "nome_fantasia_estabelecimento",
      "Nome fantasia do estabelecimento", "character", ""),
    c("pni_doses", "*", "co_municipio_estabelecimento",
      "codigo_municipio_estabelecimento",
      "Codigo IBGE do municipio do estabelecimento", "character", ""),
    c("pni_doses", "*", "no_municipio_estabelecimento",
      "municipio_estabelecimento", "Municipio do estabelecimento",
      "character", ""),
    c("pni_doses", "*", "sg_uf_estabelecimento",
      "uf_estabelecimento", "UF do estabelecimento", "character", ""),
    c("pni_doses", "*", "co_troca_documento",
      "id_documento_substituido", "Documento substituido ou alterado",
      "character", ""),
    c("pni_doses", "*", "co_vacina", "codigo_vacina",
      "Codigo da vacina", "character", ""),
    c("pni_doses", "*", "sg_vacina", "sigla_vacina",
      "Sigla da vacina", "character", ""),
    c("pni_doses", "*", "ds_vacina", "vacina",
      "Vacina administrada", "character", ""),
    c("pni_doses", "*", "dt_vacina", "data_vacinacao",
      "Data da vacinacao", "date", "%Y-%m-%d|%d/%m/%Y"),
    c("pni_doses", "*", "co_dose_vacina", "codigo_dose",
      "Codigo da dose", "character", ""),
    c("pni_doses", "*", "ds_dose_vacina", "dose",
      "Descricao da dose", "character", ""),
    c("pni_doses", "*", "co_local_aplicacao",
      "codigo_local_aplicacao", "Codigo do local de aplicacao",
      "character", ""),
    c("pni_doses", "*", "ds_local_aplicacao", "local_aplicacao",
      "Local de aplicacao", "character", ""),
    c("pni_doses", "*", "co_via_administracao",
      "codigo_via_administracao", "Codigo da via de administracao",
      "character", ""),
    c("pni_doses", "*", "ds_via_administracao",
      "via_administracao", "Via de administracao", "character", ""),
    c("pni_doses", "*", "co_lote_vacina", "lote",
      "Lote da vacina", "character", ""),
    c("pni_doses", "*", "ds_vacina_fabricante", "fabricante",
      "Fabricante da vacina", "character", ""),
    c("pni_doses", "*", "dt_entrada_datalake",
      "entrada_datalake_em", "Entrada no datalake", "datetime",
      "%Y-%m-%dT%H:%M:%OS|%Y-%m-%d %H:%M:%OS"),
    c("pni_doses", "*", "dt_entrada_rnds", "entrada_rnds_em",
      "Entrada na RNDS", "datetime",
      "%Y-%m-%dT%H:%M:%OS|%Y-%m-%d %H:%M:%OS"),
    c("pni_doses", "*", "co_sistema_origem",
      "codigo_sistema_origem", "Codigo do sistema de origem",
      "character", ""),
    c("pni_doses", "*", "ds_sistema_origem", "sistema_origem",
      "Sistema de origem", "character", ""),
    c("pni_doses", "*", "co_identificador_sistema",
      "codigo_identificador_sistema",
      "Codigo identificador do sistema de origem", "character", ""),
    c("pni_doses", "*", "st_documento", "situacao_documento",
      "Situacao do documento", "character", ""),
    c("pni_doses", "*", "co_estrategia_vacinacao",
      "codigo_estrategia_vacinacao",
      "Codigo da estrategia de vacinacao", "character", ""),
    c("pni_doses", "*", "ds_estrategia_vacinacao",
      "estrategia_vacinacao", "Estrategia de vacinacao",
      "character", ""),
    c("pni_doses", "*", "co_origem_registro",
      "codigo_origem_registro", "Codigo da origem do registro",
      "character", ""),
    c("pni_doses", "*", "ds_origem_registro", "origem_registro",
      "Origem do registro", "character", ""),
    c("pni_doses", "*", "co_vacina_grupo_atendimento",
      "codigo_grupo_atendimento",
      "Codigo do grupo de atendimento", "character", ""),
    c("pni_doses", "*", "ds_vacina_grupo_atendimento",
      "grupo_atendimento", "Grupo de atendimento", "character", ""),
    c("pni_doses", "*", "co_vacina_categoria_atendimento",
      "codigo_categoria_atendimento",
      "Codigo da categoria de atendimento", "character", ""),
    c("pni_doses", "*", "ds_vacina_categoria_atendimento",
      "categoria_atendimento", "Categoria de atendimento",
      "character", ""),
    c("pni_doses", "*", "co_vacina_fabricante",
      "codigo_fabricante", "Codigo do fabricante da vacina",
      "character", ""),
    c("pni_doses", "*", "ds_condicao_maternal",
      "condicao_maternal", "Condicao maternal", "character", ""),
    c("pni_doses", "*", "co_tipo_estabelecimento",
      "codigo_tipo_estabelecimento",
      "Codigo do tipo de estabelecimento", "character", ""),
    c("pni_doses", "*", "ds_tipo_estabelecimento",
      "tipo_estabelecimento", "Tipo de estabelecimento",
      "character", ""),
    c("pni_doses", "*", "co_natureza_estabelecimento",
      "codigo_natureza_estabelecimento",
      "Codigo da natureza do estabelecimento", "character", ""),
    c("pni_doses", "*", "ds_natureza_estabelecimento",
      "natureza_estabelecimento", "Natureza do estabelecimento",
      "character", ""),
    c("pni_doses", "*", "co_condicao_maternal",
      "codigo_condicao_maternal", "Codigo da condicao maternal",
      "character", ""),
    c("pni_doses", "*", "no_uf_paciente", "estado_residencia",
      "Estado de residencia", "character", ""),
    c("pni_doses", "*", "dt_deleted", "excluido_em",
      "Data de exclusao logica", "datetime",
      "%Y-%m-%dT%H:%M:%OS|%Y-%m-%d %H:%M:%OS"),
    c("pni_doses", "*", "dt_deletado_rnds", "excluido_rnds_em",
      "Data de exclusao do registro na RNDS", "datetime",
      "%Y-%m-%dT%H:%M:%OS|%Y-%m-%d %H:%M:%OS|%Y-%m-%d"),
    c("pni_doses", "*", "ds_identificador_sistema",
      "identificador_sistema",
      "Descricao do identificador do sistema", "character", ""),
    c("pni_doses", "*", "no_uf_estabelecimento",
      "estado_estabelecimento", "Estado do estabelecimento",
      "character", ""),
    c("ocupacao_hospitalar", "*", "_id", "id_registro",
      "Identificador do registro", "character", ""),
    c("ocupacao_hospitalar", "*", "dataNotificacao",
      "data_notificacao", "Data da notificacao", "datetime",
      "%Y-%m-%dT%H:%M:%OSZ|%Y-%m-%dT%H:%M:%OS"),
    c("ocupacao_hospitalar", "*", "cnes", "cnes",
      "Codigo CNES", "character", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoSuspeitoCli",
      "ocupacao_suspeito_clinico", "Leitos clinicos suspeitos",
      "numeric", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoSuspeitoUti",
      "ocupacao_suspeito_uti", "Leitos UTI suspeitos", "numeric", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoConfirmadoCli",
      "ocupacao_confirmado_clinico", "Leitos clinicos confirmados",
      "numeric", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoConfirmadoUti",
      "ocupacao_confirmado_uti", "Leitos UTI confirmados", "numeric", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoCovidUti",
      "ocupacao_covid_uti", "Leitos UTI ocupados por COVID-19",
      "numeric", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoCovidCli",
      "ocupacao_covid_clinico", "Leitos clinicos ocupados por COVID-19",
      "numeric", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoHospitalarUti",
      "ocupacao_hospitalar_uti", "Ocupacao hospitalar UTI",
      "numeric", ""),
    c("ocupacao_hospitalar", "*", "ocupacaoHospitalarCli",
      "ocupacao_hospitalar_clinico", "Ocupacao hospitalar clinica",
      "numeric", ""),
    c("ocupacao_hospitalar", "*", "saidaSuspeitaObitos",
      "saidas_suspeitas_obitos", "Obitos suspeitos", "numeric", ""),
    c("ocupacao_hospitalar", "*", "saidaSuspeitaAltas",
      "saidas_suspeitas_altas", "Altas suspeitas", "numeric", ""),
    c("ocupacao_hospitalar", "*", "saidaConfirmadaObitos",
      "saidas_confirmadas_obitos", "Obitos confirmados", "numeric", ""),
    c("ocupacao_hospitalar", "*", "saidaConfirmadaAltas",
      "saidas_confirmadas_altas", "Altas confirmadas", "numeric", ""),
    c("ocupacao_hospitalar", "*", "origem", "origem",
      "Origem do registro", "character", ""),
    c("ocupacao_hospitalar", "*", "_p_usuario", "usuario",
      "Identificador interno do usuario", "character", ""),
    c("ocupacao_hospitalar", "*", "estadoNotificacao",
      "estado_notificacao", "Estado da notificacao", "character", ""),
    c("ocupacao_hospitalar", "*", "municipioNotificacao",
      "municipio_notificacao", "Municipio da notificacao",
      "character", ""),
    c("ocupacao_hospitalar", "*", "estado", "estado",
      "Estado do estabelecimento", "character", ""),
    c("ocupacao_hospitalar", "*", "municipio", "municipio",
      "Municipio do estabelecimento", "character", ""),
    c("ocupacao_hospitalar", "*", "excluido", "excluido",
      "Registro excluido", "logical", ""),
    c("ocupacao_hospitalar", "*", "validado", "validado",
      "Registro validado", "logical", ""),
    c("ocupacao_hospitalar", "*", "_created_at", "criado_em",
      "Criacao do registro", "datetime",
      "%Y-%m-%dT%H:%M:%OSZ|%Y-%m-%dT%H:%M:%OS"),
    c("ocupacao_hospitalar", "*", "_updated_at", "atualizado_em",
      "Atualizacao do registro", "datetime",
      "%Y-%m-%dT%H:%M:%OSZ|%Y-%m-%dT%H:%M:%OS")
  )
  result <- as.data.frame(
    do.call(rbind, rows),
    stringsAsFactors = FALSE
  )
  names(result) <- c(
    "sistema", "tipo", "campo", "campo_padronizado",
    "descricao", "classe", "formato"
  )
  result
})

#' Inspect curated DATASUS field dictionaries
#'
#' Returns the fields currently standardized by [datasus_padronizar()].
#' Contemporary schemas cover the variables documented in the official
#' ESAVI, e-SUS Notifica, PNI and hospital-occupancy dictionaries. Fields
#' added by a source after the packaged dictionary was released remain
#' untouched and can be detected with [datasus_validar_esquema()].
#'
#' @param sistema System: `"sim"`, `"sinasc"`, `"sih"`, `"esavi"`,
#'   `"sindrome_gripal"`, `"pni_doses"` or `"ocupacao_hospitalar"`.
#' @param tipo Optional raw file type. `NULL` selects the default type.
#'
#' @return A data frame with original field, standardized field, description,
#'   target class and date format.
#' @export
#'
#' @examples
#' datasus_dicionario("sim")
#' datasus_dicionario("sih", "RD")
datasus_dicionario <- function(sistema, tipo = NULL) {
  if (!is.character(sistema) || length(sistema) != 1L ||
      is.na(sistema) || !nzchar(trimws(sistema))) {
    stop("The 'sistema' argument must be a single system name",
         call. = FALSE)
  }
  sistema <- tolower(trimws(sistema))
  supported <- unique(.datasus_dictionary$sistema)
  if (!sistema %in% supported) {
    stop(
      "Unknown dictionary system '", sistema, "'. Supported systems are: ",
      paste(supported, collapse = ", "),
      call. = FALSE
    )
  }
  if (sistema %in% names(.microdata_default_type)) {
    metadata <- .microdata_resolve(sistema, tipo)
    selected_type <- metadata$tipo
  } else {
    if (!is.null(tipo) && (!is.character(tipo) || length(tipo) != 1L ||
        is.na(tipo) || toupper(tipo) != "*")) {
      stop(
        "The 'tipo' argument is not used for contemporary OpenDataSUS ",
        "systems",
        call. = FALSE
      )
    }
    selected_type <- "*"
  }
  result <- .datasus_dictionary[
    .datasus_dictionary$sistema == sistema &
      .datasus_dictionary$tipo %in% c("*", selected_type),
    ,
    drop = FALSE
  ]
  row.names(result) <- NULL
  result
}

.datasus_parse_date <- function(value, format) {
  if (inherits(value, "Date")) {
    return(value)
  }
  value <- trimws(as.character(value))
  value[!nzchar(value)] <- NA_character_
  formats <- strsplit(format, "|", fixed = TRUE)[[1L]]
  result <- as.Date(rep(NA_character_, length(value)))
  for (candidate in formats) {
    missing <- is.na(result) & !is.na(value)
    result[missing] <- suppressWarnings(
      as.Date(value[missing], format = candidate)
    )
  }
  result
}

.datasus_parse_datetime <- function(value, format) {
  if (inherits(value, "POSIXct")) {
    return(value)
  }
  value <- trimws(as.character(value))
  value[!nzchar(value)] <- NA_character_
  formats <- strsplit(format, "|", fixed = TRUE)[[1L]]
  result <- as.POSIXct(
    rep(NA_character_, length(value)),
    tz = "UTC"
  )
  for (candidate in formats) {
    missing <- is.na(result) & !is.na(value)
    result[missing] <- suppressWarnings(as.POSIXct(
      value[missing],
      format = candidate,
      tz = "UTC"
    ))
  }
  result
}

.datasus_parse_logical <- function(value) {
  if (is.logical(value)) {
    return(value)
  }
  normalized <- tolower(trimws(as.character(value)))
  result <- rep(NA, length(normalized))
  result[normalized %in% c("true", "t", "1", "sim", "yes")] <- TRUE
  result[
    normalized %in% c("false", "f", "0", "nao", "n\u00e3o", "no")
  ] <- FALSE
  result
}

.datasus_parse_character <- function(value) {
  if (is.numeric(value)) {
    result <- format(
      value,
      scientific = FALSE,
      trim = TRUE,
      digits = 15L
    )
    result[is.na(value)] <- NA_character_
    return(result)
  }
  as.character(value)
}

#' Standardize common fields in DATASUS microdata
#'
#' Renames curated fields and converts documented dates, identifiers, numeric
#' measures and logical indicators. Unrecognized columns and coded category
#' values are preserved exactly as published.
#'
#' @param dados A data frame containing raw DATASUS records.
#' @param sistema System accepted by [datasus_dicionario()].
#' @param tipo Optional raw file type. `NULL` selects the default type.
#'
#' @return The input data frame with curated fields renamed and typed.
#'   Provenance is preserved and the applied dictionary is attached as
#'   attribute `"datasus_dicionario"`.
#' @export
#'
#' @examples
#' example <- data.frame(
#'   MUNIC_RES = "120040",
#'   DT_INTER = "20240110",
#'   DIAG_PRINC = "J18"
#' )
#' datasus_padronizar(example, "sih")
datasus_padronizar <- function(dados, sistema, tipo = NULL) {
  if (!is.data.frame(dados)) {
    stop("The 'dados' argument must be a data frame", call. = FALSE)
  }
  dictionary <- datasus_dicionario(sistema, tipo)
  provenance <- attr(dados, "datasus_proveniencia", exact = TRUE)

  lower_names <- tolower(names(dados))
  available <- tolower(dictionary$campo) %in% lower_names
  applied <- dictionary[available, , drop = FALSE]
  for (index in seq_len(nrow(applied))) {
    source <- applied$campo[[index]]
    target <- applied$campo_padronizado[[index]]
    source_index <- match(tolower(source), tolower(names(dados)))
    target_index <- match(tolower(target), tolower(names(dados)))
    if (!identical(source, target) && !is.na(target_index) &&
        target_index != source_index) {
      stop(
        "Cannot standardize field '", source,
        "' because target field '", target, "' already exists",
        call. = FALSE
      )
    }
    names(dados)[source_index] <- target
    if (identical(applied$classe[[index]], "date")) {
      dados[[target]] <- .datasus_parse_date(
        dados[[target]], applied$formato[[index]]
      )
    } else if (identical(applied$classe[[index]], "datetime")) {
      dados[[target]] <- .datasus_parse_datetime(
        dados[[target]], applied$formato[[index]]
      )
    } else if (identical(applied$classe[[index]], "character")) {
      dados[[target]] <- .datasus_parse_character(dados[[target]])
    } else if (identical(applied$classe[[index]], "numeric")) {
      dados[[target]] <- suppressWarnings(as.numeric(dados[[target]]))
    } else if (identical(applied$classe[[index]], "logical")) {
      dados[[target]] <- .datasus_parse_logical(dados[[target]])
    }
  }

  attr(dados, "datasus_dicionario") <- applied
  if (!is.null(provenance)) {
    attr(dados, "datasus_proveniencia") <- provenance
  }
  dados
}

.datasus_class_compatible <- function(value, expected) {
  switch(
    expected,
    character = is.character(value),
    numeric = is.numeric(value),
    logical = is.logical(value),
    date = inherits(value, "Date"),
    datetime = inherits(value, "POSIXct"),
    FALSE
  )
}

#' Validate curated DATASUS schemas
#'
#' Compares the columns of a raw or standardized data frame with fields in
#' [datasus_dicionario()]. This detects missing fields and incompatible target
#' classes without treating newly introduced source columns as errors.
#'
#' @param dados A data frame containing raw or standardized records.
#' @param sistema System accepted by [datasus_dicionario()].
#' @param tipo Optional raw file type for SIM, SINASC or SIH.
#' @param campos Optional source or standardized field names to require.
#'   `NULL` checks every curated field for the selected system.
#' @param estrito Whether missing or incompatibly typed standardized fields
#'   should raise an error.
#'
#' @return A data frame with source and standardized field names, presence,
#'   observed column and class, expected class and status. Attribute
#'   `"campos_extras"` lists columns outside the curated dictionary.
#' @export
#'
#' @examples
#' dados <- data.frame(
#'   dt_vacina = "2025-01-10",
#'   co_municipio_paciente = "5002704"
#' )
#' datasus_validar_esquema(dados, "pni_doses")
datasus_validar_esquema <- function(dados, sistema, tipo = NULL,
                                     campos = NULL, estrito = FALSE) {
  if (!is.data.frame(dados)) {
    stop("The 'dados' argument must be a data frame", call. = FALSE)
  }
  if (!is.logical(estrito) || length(estrito) != 1L || is.na(estrito)) {
    stop("'estrito' must be TRUE or FALSE", call. = FALSE)
  }
  dictionary <- datasus_dicionario(sistema, tipo)
  if (!is.null(campos)) {
    if (!is.character(campos) || !length(campos) ||
        anyNA(campos) || any(!nzchar(campos))) {
      stop("'campos' must contain valid field names", call. = FALSE)
    }
    selected <- tolower(dictionary$campo) %in% tolower(campos) |
      tolower(dictionary$campo_padronizado) %in% tolower(campos)
    unknown <- campos[
      !tolower(campos) %in% tolower(c(
        dictionary$campo,
        dictionary$campo_padronizado
      ))
    ]
    if (length(unknown)) {
      stop(
        "Unknown curated field(s): ",
        paste(unique(unknown), collapse = ", "),
        call. = FALSE
      )
    }
    dictionary <- dictionary[selected, , drop = FALSE]
  }

  data_names <- tolower(names(dados))
  rows <- lapply(seq_len(nrow(dictionary)), function(index) {
    source <- dictionary$campo[[index]]
    target <- dictionary$campo_padronizado[[index]]
    source_index <- match(tolower(source), data_names)
    target_index <- match(tolower(target), data_names)
    column_index <- if (!is.na(target_index)) target_index else source_index
    present <- !is.na(column_index)
    standardized <- present && !is.na(target_index)
    observed_class <- if (present) {
      paste(class(dados[[column_index]]), collapse = "/")
    } else {
      NA_character_
    }
    compatible <- if (standardized) {
      .datasus_class_compatible(
        dados[[column_index]],
        dictionary$classe[[index]]
      )
    } else {
      NA
    }
    status <- if (!present) {
      "ausente"
    } else if (!standardized) {
      "presente_bruto"
    } else if (isTRUE(compatible)) {
      "padronizado"
    } else {
      "classe_divergente"
    }
    data.frame(
      campo = source,
      campo_padronizado = target,
      presente = present,
      coluna_observada = if (present) names(dados)[[column_index]] else NA,
      classe_observada = observed_class,
      classe_esperada = dictionary$classe[[index]],
      status = status,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  row.names(result) <- NULL
  curated <- unique(tolower(c(
    dictionary$campo,
    dictionary$campo_padronizado
  )))
  attr(result, "campos_extras") <- names(dados)[
    !tolower(names(dados)) %in% curated
  ]
  problematic <- result$status %in% c("ausente", "classe_divergente")
  if (estrito && any(problematic)) {
    stop(
      "Schema validation failed for field(s): ",
      paste(result$campo[problematic], collapse = ", "),
      call. = FALSE
    )
  }
  result
}
