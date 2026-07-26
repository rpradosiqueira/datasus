# Regenerate the internal territorial reference from the official IBGE API.
#
# Source documentation:
# https://servicodados.ibge.gov.br/api/docs/localidades

url <- paste0(
  "https://servicodados.ibge.gov.br/api/v1/localidades/municipios",
  "?orderBy=nome"
)

raw <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
immediate <- raw[["regiao-imediata"]]
intermediate <- immediate[["regiao-intermediaria"]]
uf <- intermediate[["UF"]]
region <- uf[["regiao"]]
micro <- raw[["microrregiao"]]
meso <- micro[["mesorregiao"]]

.datasus_municipios <- data.frame(
  codigo_municipio = sprintf("%07d", raw$id),
  municipio = enc2utf8(raw$nome),
  codigo_uf = sprintf("%02d", uf$id),
  uf = enc2utf8(uf$sigla),
  unidade_federacao = enc2utf8(uf$nome),
  codigo_regiao = as.character(region$id),
  sigla_regiao = enc2utf8(region$sigla),
  regiao = enc2utf8(region$nome),
  codigo_regiao_imediata = sprintf("%06d", immediate$id),
  regiao_imediata = enc2utf8(immediate$nome),
  codigo_regiao_intermediaria = sprintf("%04d", intermediate$id),
  regiao_intermediaria = enc2utf8(intermediate$nome),
  codigo_microrregiao = sprintf("%05d", micro$id),
  microrregiao = enc2utf8(micro$nome),
  codigo_mesorregiao = sprintf("%04d", meso$id),
  mesorregiao = enc2utf8(meso$nome),
  stringsAsFactors = FALSE
)
.datasus_municipios <- .datasus_municipios[
  order(.datasus_municipios$codigo_municipio),
  ,
  drop = FALSE
]
rownames(.datasus_municipios) <- NULL

stopifnot(
  nrow(.datasus_municipios) == 5571L,
  !anyDuplicated(.datasus_municipios$codigo_municipio),
  "5101837" %in% .datasus_municipios$codigo_municipio
)

.datasus_territorios_meta <- list(
  fonte = "IBGE API de Localidades",
  url = url,
  documentacao = "https://servicodados.ibge.gov.br/api/docs/localidades",
  atualizado_em = as.Date("2026-07-26"),
  municipios = nrow(.datasus_municipios)
)

save(
  .datasus_municipios,
  .datasus_territorios_meta,
  file = "R/sysdata.rda",
  compress = "xz",
  version = 2
)
