# datasus 0.16.0

* Added four task-oriented vignettes covering TABNET access, modern
  OpenDataSUS surveillance, geography and epidemiological analysis, and
  memory-efficient processing of large microdata files.
* Expanded the contemporary schema registry from analysis-critical subsets
  to the complete fields documented for ESAVI, e-SUS Notifica, PNI doses and
  the current hospital-occupancy CSV layout.
* Added a separate five-minute default download timeout, configurable through
  `options(datasus.download_timeout = ...)`, while retaining the shorter
  metadata-request timeout.
* Raw DBC/DBF reads now preserve the order requested in `colunas`, even when
  the physical DBF stores those fields in a different order. Empty files
  inside OpenDataSUS ZIP resources now produce an explicit source-
  unavailable error.

* Extended `datasus_dicionario()` and `datasus_padronizar()` with curated,
  officially documented schemas for ESAVI, e-SUS syndrome-gripal, individual
  PNI doses and COVID-19 hospital occupancy.
* Added `datasus_validar_esquema()` to report missing analysis-critical
  fields, raw versus standardized columns and incompatible target classes.
* Added `colunas` and `normalizar` to contemporary wrappers and column
  selection to `opendatasus_ler()`.
* Added `opendatasus_arquivos()` to expand resources that publish one or more
  downloadable file links inside their descriptions.
* Fixed e-SUS syndrome-gripal downloads for historical state resources split
  into multiple lots, while applying `n_max` globally across all parts.
* Added `opendatasus_processar()` for bounded-memory CSV processing with a
  callback, configurable chunk size, multipart support and optional schema
  normalization.

# datasus 0.15.0

* Added `esavi()` for anonymous, continuously updated vaccination-safety
  notifications from the e-SUS Notifica ESAVI module.
* Added `esus_sindrome_gripal()` with validated year and state selection for
  annual mild and moderate influenza-like illness files.
* Added `pni_doses()` for individual monthly National Immunization Program
  dose records, including dynamic latest-year and latest-month discovery.
* Added `ocupacao_hospitalar()` for annual COVID-19 clinical and intensive
  care bed occupancy records.
* Annual dataset and resource resolution uses live OpenDataSUS metadata and
  preserves the existing atomic cache, checksum and provenance contracts.

# datasus 0.14.0

* Added `calcular_indicador()` for grouped rates, proportions, ratios and
  case-fatality calculations with explicit aggregation and denominator
  semantics.
* Added `taxa_mortalidade()`, `taxa_incidencia()`, `letalidade()` and
  `proporcao()` as epidemiological shortcuts.
* Added exact Poisson intervals for grouped rates and exact binomial intervals
  for grouped proportions and case fatality.
* Added `juntar_populacao()` for validated many-to-one joins between event
  tables and population denominators, including provenance preservation.
* Added `populacao_padrao()` with normalized WHO 2000--2025, Segi and
  Scandinavian age-standardization weights.

# datasus 0.13.0

* Added `datasus_territorios()` with an offline IBGE hierarchy covering
  5,571 current municipal records, 27 states and five macroregions.
* Added `normalizar_codigo_ibge()`, `validar_codigo_ibge()` and
  `extrair_codigo_ibge()` for seven-digit IBGE and six-digit DATASUS codes.
* Added `adicionar_territorio()` for validated geographic joins and
  `completar_territorios()` for missing territory-period combinations.
* TABNET and raw-microdata state arguments now also accept two-digit IBGE
  codes and full state names.
* Territorial outputs include source and extraction metadata through
  `datasus_proveniencia()`.

# datasus 0.12.0

* Added unified `sim()` and `sinasc()` interfaces backed by the declarative
  TABNET catalog and query engine.
* Added explicit municipality, state-specific municipality and region/state
  query forms through the `abrangencia` and `uf` arguments.
* Integrated SIM and SINASC with `datasus_catalogo()`, `datasus_opcoes()` and
  `datasus_proveniencia()`.
* Replaced the 15 historical SIM/SINASC implementations with thin
  compatibility wrappers. Their signatures remain available but now emit a
  deprecation warning directing users to `sim()` or `sinasc()`.

# datasus 0.11.0

* Added `calcular_taxa()` and exact Poisson intervals with
  `intervalo_taxa()`.
* Added `semana_epidemiologica()` and `calendario_epidemiologico()` using the
  Sunday-to-Saturday convention followed by Brazilian surveillance calendars.
* Added `media_movel()` with right, centered and left alignment.
* Added `padronizar_idade()` for direct age standardization, Poisson standard
  errors, optional confidence limits and explicit standard-weight coverage.

# datasus 0.10.0

* Added `microdados_catalogo()`, `microdados_arquivos()`,
  `microdados_baixar()` and `microdados_ler()` for record-level DBC files.
* Added `sim_microdados()`, `sinasc_microdados()` and `sih_microdados()`.
* Added current native DBC/DBF decoding through `datasusr`, replacing the
  archived `read.dbc` dependency.
* Added `datasus_dicionario()` and `datasus_padronizar()` with conservative
  field mappings and date conversion for common SIM, SINASC and SIH fields.
* Unified the cache root used by OpenDataSUS and raw DATASUS microdata.

# datasus 0.9.0

* Added a generic OpenDataSUS client with catalog search, resource discovery,
  atomic downloads and CSV/JSON readers.
* Added `sivep_gripe()`, `sinan_dengue()` and `sinan_mpox()` for current
  surveillance microdata.
* Added a configurable disk cache with automatic refresh when resource
  metadata changes, plus URL, timestamp and MD5 provenance metadata.
* Added `datasus_proveniencia()` to retrieve provenance from downloaded files
  and parsed datasets.

# datasus 0.8.0

* Added `pni_imunizacoes()` for applied doses and vaccination coverage.
* Added `siscan()` with 15 cervical and breast cancer exam datasets.
* Added `sisvan()` for the two historical nutritional surveillance series.
* Added `financiamento_sus()` for the three financial tables linked by the
  DATASUS catalog.
* Added support for the `dhdat`/`webtabx` engine used by PNI and SISCAN,
  including safe parsing of Google Visualization table definitions.
* Made primary form field names dynamic so period selection works across both
  TABNET engines.
* Expanded the local catalog to 118 datasets.

# datasus 0.7.0

* Added `populacao_residente()` with six census, estimate, projection and
  retroprojection datasets.
* Added `sih_morbidade()` with eight general and external-causes datasets.
* Added `sinan()` with 46 notifiable diseases and conditions.
* Expanded `datasus_catalogo()` to 96 datasets and added explicit geographic
  scope metadata.
* Added support for TABNET datasets with fixed Brazil/UF forms and for
  mixed-case endpoint names.

# datasus 0.6.0

* Added a declarative catalog covering SIH/SUS, SIA/SUS and CNES.
* Added `sih_producao()`, `sia_producao()` and `cnes()` with a common API for
  dimensions, measures, periods and named filters.
* Added `datasus_catalogo()` for offline dataset discovery and
  `datasus_opcoes()` for live form introspection.
* Added support for monthly competencies and Brazilian decimal values in
  TABNET results.
* Added live smoke tests for all three new system families.

# datasus 0.5.0

* Rebuilt the TABNET transport around a shared, retry-aware HTTP client.
* Added explicit Latin-1 parsing for current DATASUS responses.
* Fixed incorrect `evitb10` request endpoints.
* Fixed vector handling in the `periodo` argument.
* Made package builds and vignettes independent of live network access.
* Added automated tests and continuous integration.

# datasus 0.4.0

* 9 new functions related to he SIM's database.
* Better filter handling

# datasus 0.1.0

* Initial version



