#' datasus: Brazilian public health data
#'
#' @description
#' `datasus` provides a stable interface to aggregated TABNET queries,
#' OpenDataSUS resources, record-level DATASUS files, territorial references
#' and epidemiological analysis helpers. Discovery functions can be used
#' before any download, and local catalogs and transformations remain usable
#' without network access.
#'
#' @section Main workflow:
#' Start with [datasus_catalogo()] for supported TABNET datasets,
#' [opendatasus_catalogo()] for contemporary portal datasets, or
#' [microdados_catalogo()] for raw DBC/DBF file families. Plan requests with
#' [datasus_opcoes()], [opendatasus_recursos()], [opendatasus_arquivos()] or
#' [microdados_arquivos()] before querying or downloading data.
#'
#' Use [opendatasus_baixar()] and [microdados_baixar()] when another engine
#' will read the files, or [opendatasus_ler()] and [microdados_ler()] for
#' in-memory workflows. System-specific functions such as [sim()], [sinasc()],
#' [sih_producao()] and [cnes()] provide concise entry points for common data
#' products.
#'
#' @section Reproducibility and validation:
#' Downloaded objects carry source, consultation time, local path and checksum
#' metadata retrievable with [datasus_proveniencia()]. Use
#' [datasus_dicionario()] and [datasus_validar_esquema()] to inspect curated
#' fields and detect missing or type-incompatible analysis fields. Cache paths
#' use [tools::R_user_dir()] by default and can be changed with option
#' `datasus.cache_dir`.
#'
#' @section Compatibility:
#' Historical `sim_*()` and `sinasc_nv_*()` functions remain available for
#' existing scripts. Their public signatures are preserved; new workflows
#' should prefer [sim()] and [sinasc()]. Network integrations are tested only
#' by explicit or scheduled smoke tests, while regular package tests use local
#' fixtures and mocks.
#'
#' @docType package
#' @name datasus-package
#' @aliases datasus
"_PACKAGE"
