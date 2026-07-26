# Health services systems --------------------------------------------------

.datasus_catalog <- local({
  rows <- list(
    c("sih", "aih_rd_internacao", "qi", "produ\u00e7\u00e3o hospitalar",
      "AIH consolidadas, por local de interna\u00e7\u00e3o, a partir de 2008"),
    c("sih", "aih_rd_internacao_1992_2007", "pi", "produ\u00e7\u00e3o hospitalar",
      "AIH consolidadas, por local de interna\u00e7\u00e3o, de 1992 a 2007"),
    c("sih", "aih_rd_residencia", "qr", "produ\u00e7\u00e3o hospitalar",
      "AIH consolidadas, por local de resid\u00eancia, a partir de 2008"),
    c("sih", "aih_rd_residencia_1995_2007", "pr", "produ\u00e7\u00e3o hospitalar",
      "AIH consolidadas, por local de resid\u00eancia, de 1995 a 2007"),
    c("sih", "aih_rd_gestor", "qg", "produ\u00e7\u00e3o hospitalar",
      "AIH consolidadas, por gestor, a partir de 2008"),
    c("sih", "aih_sp_internacao", "spa", "produ\u00e7\u00e3o hospitalar",
      "AIH detalhadas, por local de interna\u00e7\u00e3o, a partir de 2008"),
    c("sih", "aih_sp_residencia", "spr", "produ\u00e7\u00e3o hospitalar",
      "AIH detalhadas, por local de resid\u00eancia, a partir de 2008"),
    c("sih", "aih_sp_gestor", "spg", "produ\u00e7\u00e3o hospitalar",
      "AIH detalhadas, por gestor, a partir de 2008"),
    c("sia", "atendimento", "qa", "produ\u00e7\u00e3o ambulatorial",
      "Produ\u00e7\u00e3o por local de atendimento, a partir de 2008"),
    c("sia", "atendimento_1994_2007", "pa", "produ\u00e7\u00e3o ambulatorial",
      "Produ\u00e7\u00e3o por local de atendimento, de 1994 a 2007"),
    c("sia", "residencia", "qb", "produ\u00e7\u00e3o ambulatorial",
      "Produ\u00e7\u00e3o por local de resid\u00eancia, a partir de 2008"),
    c("sia", "gestor", "qg", "produ\u00e7\u00e3o ambulatorial",
      "Produ\u00e7\u00e3o por gestor, a partir de 2008"),
    c("cnes", "estabelecimentos", "estab", "estabelecimentos",
      "Tipos de estabelecimentos"),
    c("cnes", "nivel_atencao", "atenc", "estabelecimentos",
      "N\u00edvel de aten\u00e7\u00e3o"),
    c("cnes", "servico_classificacao_ate_2008_02", "servcl",
      "estabelecimentos", "Servi\u00e7o/classifica\u00e7\u00e3o at\u00e9 fevereiro de 2008"),
    c("cnes", "servico_classificacao", "servc2", "estabelecimentos",
      "Servi\u00e7o/classifica\u00e7\u00e3o a partir de mar\u00e7o de 2008"),
    c("cnes", "habilitacao", "hab", "estabelecimentos", "Habilita\u00e7\u00e3o"),
    c("cnes", "atendimento_ambulatorio", "atamb", "estabelecimentos",
      "Tipo de atendimento prestado: ambulat\u00f3rio"),
    c("cnes", "atendimento_internacao", "atint", "estabelecimentos",
      "Tipo de atendimento prestado: interna\u00e7\u00e3o"),
    c("cnes", "atendimento_sadt", "atsadt", "estabelecimentos",
      "Tipo de atendimento prestado: apoio a diagnose e terapia"),
    c("cnes", "atendimento_urgencia", "aturg", "estabelecimentos",
      "Tipo de atendimento prestado: urg\u00eancia"),
    c("cnes", "atendimento_vigilancia", "atvig", "estabelecimentos",
      "Tipo de atendimento prestado: vigil\u00e2ncia epidemiol\u00f3gica ou sanit\u00e1ria"),
    c("cnes", "atendimento_farmacia_cooperativa", "atout",
      "estabelecimentos", "Tipo de atendimento prestado: farm\u00e1cia ou cooperativa"),
    c("cnes", "consultorios", "consul", "recursos f\u00edsicos",
      "Ambulat\u00f3rio: consult\u00f3rios"),
    c("cnes", "leitos_reposo_observacao", "amblei", "recursos f\u00edsicos",
      "Ambulat\u00f3rio: leitos de repouso/observa\u00e7\u00e3o"),
    c("cnes", "leitos_internacao", "leiint", "recursos f\u00edsicos",
      "Hospitalar: leitos de interna\u00e7\u00e3o"),
    c("cnes", "leitos_complementares", "leiuti", "recursos f\u00edsicos",
      "Hospitalar: leitos complementares"),
    c("cnes", "instalacoes_obstetricia_neonatologia", "leiobs",
      "recursos f\u00edsicos", "Instala\u00e7\u00f5es f\u00edsicas de obstetr\u00edcia e neonatologia"),
    c("cnes", "urgencia_consultorios", "rurgc", "recursos f\u00edsicos",
      "Urg\u00eancia: consult\u00f3rios"),
    c("cnes", "urgencia_leitos_observacao", "recurg", "recursos f\u00edsicos",
      "Urg\u00eancia: leitos de repouso/observa\u00e7\u00e3o"),
    c("cnes", "equipamentos", "equipo", "recursos f\u00edsicos", "Equipamentos"),
    c("cnes", "ocupacoes", "proc02", "recursos humanos",
      "Ocupa\u00e7\u00f5es segundo a CBO 2002, a partir de agosto de 2007"),
    c("cnes", "profissionais", "prid02", "recursos humanos",
      "Profissionais segundo a CBO 2002, a partir de agosto de 2007"),
    c("cnes", "ocupacoes_ate_2007_07", "profoc", "recursos humanos",
      "Ocupa\u00e7\u00f5es segundo a CBO 1994, at\u00e9 julho de 2007"),
    c("cnes", "profissionais_ate_2007_07", "profid", "recursos humanos",
      "Profissionais segundo a CBO 1994, at\u00e9 julho de 2007"),
    c("cnes", "equipes", "equipe", "equipes de sa\u00fade", "Equipes de sa\u00fade"),
    c("sih", "geral_internacao", "ni", "morbidade hospitalar",
      "Morbidade geral por local de interna\u00e7\u00e3o, a partir de 2008"),
    c("sih", "geral_residencia", "nr", "morbidade hospitalar",
      "Morbidade geral por local de resid\u00eancia, a partir de 2008"),
    c("sih", "causas_externas_internacao", "fi", "morbidade hospitalar",
      "Causas externas por local de interna\u00e7\u00e3o, a partir de 2008"),
    c("sih", "causas_externas_residencia", "fr", "morbidade hospitalar",
      "Causas externas por local de resid\u00eancia, a partir de 2008"),
    c("sih", "geral_internacao_1984_2007", "mi", "morbidade hospitalar",
      "Morbidade geral por local de interna\u00e7\u00e3o, de 1984 a 2007"),
    c("sih", "geral_residencia_1995_2007", "mr", "morbidade hospitalar",
      "Morbidade geral por local de resid\u00eancia, de 1995 a 2007"),
    c("sih", "causas_externas_internacao_1998_2007", "ei",
      "morbidade hospitalar",
      "Causas externas por local de interna\u00e7\u00e3o, de 1998 a 2007"),
    c("sih", "causas_externas_residencia_1998_2007", "er",
      "morbidade hospitalar",
      "Causas externas por local de resid\u00eancia, de 1998 a 2007"),
    c("populacao", "censos_projecoes", "pop", "popula\u00e7\u00e3o",
      "Censos, contagem e proje\u00e7\u00f5es intercensit\u00e1rias"),
    c("populacao", "estimativa_tcu", "popt", "popula\u00e7\u00e3o",
      "Estimativas utilizadas pelo TCU para as cotas do FPM"),
    c("populacao", "retroprojecao_brasil", "reprojpop",
      "popula\u00e7\u00e3o", "Retroproje\u00e7\u00e3o da popula\u00e7\u00e3o do Brasil"),
    c("populacao", "retroprojecao_uf", "reprojpop",
      "popula\u00e7\u00e3o", "Retroproje\u00e7\u00e3o da popula\u00e7\u00e3o das UFs"),
    c("populacao", "projecao_uf", "projpop", "popula\u00e7\u00e3o",
      "Proje\u00e7\u00e3o da popula\u00e7\u00e3o das UFs, edi\u00e7\u00e3o 2018"),
    c("populacao", "estimativa_municipal", "popsvs",
      "popula\u00e7\u00e3o", "Estimativas municipais por sexo e idade, 2000 a 2021"),
    c("sinan", "acidente_trabalho", "acgr", "agravos de notifica\u00e7\u00e3o",
      "Acidente de trabalho"),
    c("sinan", "animais_peconhentos", "animais",
      "agravos de notifica\u00e7\u00e3o", "Acidente por animais pe\u00e7onhentos"),
    c("sinan", "material_biologico", "acbi",
      "agravos de notifica\u00e7\u00e3o",
      "Acidente de trabalho com exposi\u00e7\u00e3o a material biol\u00f3gico"),
    c("sinan", "atendimento_antirrabico", "antr",
      "agravos de notifica\u00e7\u00e3o", "Atendimento antirr\u00e1bico humano"),
    c("sinan", "botulismo", "botu", "agravos de notifica\u00e7\u00e3o",
      "Botulismo"),
    c("sinan", "cancer_trabalho", "canc",
      "agravos de notifica\u00e7\u00e3o", "C\u00e2ncer relacionado ao trabalho"),
    c("sinan", "colera", "colera", "agravos de notifica\u00e7\u00e3o",
      "C\u00f3lera"),
    c("sinan", "coqueluche", "coque", "agravos de notifica\u00e7\u00e3o",
      "Coqueluche"),
    c("sinan", "dermatose_trabalho", "derm",
      "agravos de notifica\u00e7\u00e3o", "Dermatose relacionada ao trabalho"),
    c("sinan", "dengue_ate_2013", "dengue",
      "agravos de notifica\u00e7\u00e3o", "Dengue at\u00e9 2013"),
    c("sinan", "dengue", "dengueb", "agravos de notifica\u00e7\u00e3o",
      "Dengue de 2014 em diante"),
    c("sinan", "difteria", "difteri", "agravos de notifica\u00e7\u00e3o",
      "Difteria"),
    c("sinan", "chagas_aguda", "chagas",
      "agravos de notifica\u00e7\u00e3o", "Doen\u00e7a de Chagas aguda"),
    c("sinan", "doencas_exantematicas", "exant",
      "agravos de notifica\u00e7\u00e3o", "Doen\u00e7as exantem\u00e1ticas"),
    c("sinan", "esquistossomose", "esquisto",
      "agravos de notifica\u00e7\u00e3o", "Esquistossomose"),
    c("sinan", "chikungunya", "chikun",
      "agravos de notifica\u00e7\u00e3o", "Febre de chikungunya"),
    c("sinan", "febre_maculosa", "febremaculosa",
      "agravos de notifica\u00e7\u00e3o", "Febre maculosa"),
    c("sinan", "febre_tifoide", "febretifoide",
      "agravos de notifica\u00e7\u00e3o", "Febre tif\u00f3ide"),
    c("sinan", "hantavirose", "hanta",
      "agravos de notifica\u00e7\u00e3o", "Hantavirose"),
    c("sinan", "hepatite", "hepa", "agravos de notifica\u00e7\u00e3o",
      "Hepatite"),
    c("sinan", "influenza_pandemica", "influ",
      "agravos de notifica\u00e7\u00e3o", "Influenza pand\u00eamica"),
    c("sinan", "intoxicacao_exogena", "Intox",
      "agravos de notifica\u00e7\u00e3o", "Intoxica\u00e7\u00e3o ex\u00f3gena"),
    c("sinan", "leishmaniose_visceral", "leishv",
      "agravos de notifica\u00e7\u00e3o", "Leishmaniose visceral"),
    c("sinan", "leishmaniose_tegumentar", "lta",
      "agravos de notifica\u00e7\u00e3o", "Leishmaniose tegumentar americana"),
    c("sinan", "leptospirose", "lepto",
      "agravos de notifica\u00e7\u00e3o", "Leptospirose"),
    c("sinan", "ler_dort", "lerdor", "agravos de notifica\u00e7\u00e3o",
      "LER/DORT"),
    c("sinan", "malaria", "mala", "agravos de notifica\u00e7\u00e3o",
      "Mal\u00e1ria"),
    c("sinan", "meningite", "menin", "agravos de notifica\u00e7\u00e3o",
      "Meningite"),
    c("sinan", "paralisia_flacida_aguda", "pfa",
      "agravos de notifica\u00e7\u00e3o", "Paralisia fl\u00e1cida aguda"),
    c("sinan", "pair_trabalho", "pair",
      "agravos de notifica\u00e7\u00e3o", "PAIR relacionado ao trabalho"),
    c("sinan", "peste", "peste", "agravos de notifica\u00e7\u00e3o",
      "Peste"),
    c("sinan", "pneumoconiose_trabalho", "pneu",
      "agravos de notifica\u00e7\u00e3o",
      "Pneumoconiose relacionada ao trabalho"),
    c("sinan", "raiva_humana", "raiva",
      "agravos de notifica\u00e7\u00e3o", "Raiva humana"),
    c("sinan", "rotavirus", "rota", "agravos de notifica\u00e7\u00e3o",
      "Rotav\u00edrus"),
    c("sinan", "sifilis_adquirida", "sifilisadquirida",
      "agravos de notifica\u00e7\u00e3o", "S\u00edfilis adquirida"),
    c("sinan", "sifilis_congenita", "sifilis",
      "agravos de notifica\u00e7\u00e3o", "S\u00edfilis cong\u00eanita"),
    c("sinan", "sifilis_gestante", "sifilisgestante",
      "agravos de notifica\u00e7\u00e3o", "S\u00edfilis em gestante"),
    c("sinan", "sindrome_rubeola_congenita", "srubeolac",
      "agravos de notifica\u00e7\u00e3o",
      "S\u00edndrome da rub\u00e9ola cong\u00eanita"),
    c("sinan", "tetano_acidental", "tetacid",
      "agravos de notifica\u00e7\u00e3o", "T\u00e9tano acidental"),
    c("sinan", "tetano_neonatal", "tetneo",
      "agravos de notifica\u00e7\u00e3o", "T\u00e9tano neonatal"),
    c("sinan", "toxoplasmose_congenita", "toxocongenita",
      "agravos de notifica\u00e7\u00e3o", "Toxoplasmose cong\u00eanita"),
    c("sinan", "toxoplasmose_gestacional", "toxogestacional",
      "agravos de notifica\u00e7\u00e3o", "Toxoplasmose gestacional"),
    c("sinan", "transtorno_mental_trabalho", "transmental",
      "agravos de notifica\u00e7\u00e3o",
      "Transtorno mental relacionado ao trabalho"),
    c("sinan", "varicela", "varicela", "agravos de notifica\u00e7\u00e3o",
      "Varicela"),
    c("sinan", "violencia_interpessoal_autoprovocada", "viole",
      "agravos de notifica\u00e7\u00e3o",
      "Viol\u00eancia interpessoal ou autoprovocada"),
    c("sinan", "zika", "zika", "agravos de notifica\u00e7\u00e3o",
      "Zika v\u00edrus"),
    c("pni", "doses_aplicadas", "dpni", "imuniza\u00e7\u00f5es",
      "Doses aplicadas, s\u00e9rie legada at\u00e9 2022"),
    c("pni", "cobertura", "cpni", "imuniza\u00e7\u00f5es",
      "Cobertura vacinal, s\u00e9rie legada at\u00e9 2022"),
    c("siscan", "citologia_colo_residencia", "cito_colo_resid",
      "citologia do colo", "Exames por local de resid\u00eancia"),
    c("siscan", "citologia_colo_atendimento", "cito_colo_atend",
      "citologia do colo", "Exames por local de atendimento"),
    c("siscan", "citologia_colo_pacientes", "cito_colo_pac",
      "citologia do colo", "Exames por paciente"),
    c("siscan", "histologia_colo_residencia", "histo_resid",
      "histologia do colo", "Exames por local de resid\u00eancia"),
    c("siscan", "histologia_colo_atendimento", "histo_atend",
      "histologia do colo", "Exames por local de atendimento"),
    c("siscan", "histologia_colo_pacientes", "histo_pac",
      "histologia do colo", "Exames por paciente"),
    c("siscan", "mamografia_residencia", "mamografia_resid",
      "mamografia", "Exames por local de resid\u00eancia"),
    c("siscan", "mamografia_atendimento", "mamografia_atend",
      "mamografia", "Exames por local de atendimento"),
    c("siscan", "mamografia_pacientes", "mamografia_pac",
      "mamografia", "Exames por paciente"),
    c("siscan", "citologia_mama_residencia", "CITOMAMA_RESID",
      "citologia da mama", "Exames por local de resid\u00eancia"),
    c("siscan", "citologia_mama_atendimento", "CITOMAMA_ATEND",
      "citologia da mama", "Exames por local de atendimento"),
    c("siscan", "citologia_mama_pacientes", "CITOMAMA_PAC",
      "citologia da mama", "Exames por paciente"),
    c("siscan", "histologia_mama_residencia", "HISTMAMA_RESID_",
      "histologia da mama", "Exames por local de resid\u00eancia"),
    c("siscan", "histologia_mama_atendimento", "HISTMAMA_ATEND_",
      "histologia da mama", "Exames por local de atendimento"),
    c("siscan", "histologia_mama_pacientes", "HISTMAMA_RESID_PAC_",
      "histologia da mama", "Exames por paciente"),
    c("sisvan", "atencao_basica", "acom_",
      "vigil\u00e2ncia nutricional",
      "Estado nutricional de usu\u00e1rios da Aten\u00e7\u00e3o B\u00e1sica"),
    c("sisvan", "bolsa_familia", "ACOMP_",
      "vigil\u00e2ncia nutricional",
      "Estado nutricional de benefici\u00e1rios do Bolsa Fam\u00edlia"),
    c("financiamento", "recursos_federais", "rs",
      "financiamento do SUS",
      "Recursos federais do SUS por munic\u00edpio, s\u00e9rie legada"),
    c("financiamento", "producao_prestador", "rp",
      "financiamento do SUS",
      "Valores aprovados da produ\u00e7\u00e3o SUS por prestador"),
    c("financiamento", "guia_pagamento", "gp",
      "financiamento do SUS", "Guia de autoriza\u00e7\u00e3o de pagamento"),
    c("sim", "obitos", "obt10", "estat\u00edsticas vitais",
      "\u00d3bitos por causas segundo a CID-10"),
    c("sim", "mortalidade_infantil", "inf10", "estat\u00edsticas vitais",
      "\u00d3bitos infantis por causas segundo a CID-10"),
    c("sim", "causas_evitaveis_0_4", "evita10", "estat\u00edsticas vitais",
      "\u00d3bitos por causas evit\u00e1veis em menores de cinco anos"),
    c("sim", "causas_evitaveis_5_74", "evitb10", "estat\u00edsticas vitais",
      "\u00d3bitos por causas evit\u00e1veis de cinco a 74 anos"),
    c("sinasc", "nascidos_vivos", "nv", "estat\u00edsticas vitais",
      "Nascidos vivos")
  )

  result <- as.data.frame(
    do.call(rbind, rows),
    stringsAsFactors = FALSE
  )
  names(result) <- c(
    "sistema", "conjunto", "prefixo", "categoria", "descricao"
  )
  result$escopo <- "all"
  result$escopo[
    result$sistema == "populacao" &
      result$conjunto %in% c(
        "retroprojecao_brasil",
        "estimativa_municipal"
      )
  ] <- "br"
  result$escopo[
    result$sistema == "populacao" &
      result$conjunto %in% c(
        "retroprojecao_uf",
        "projecao_uf"
      )
  ] <- "uf"
  result$escopo[
    result$sistema == "sinan" &
      result$conjunto == "influenza_pandemica"
  ] <- "br"
  result$escopo[result$sistema == "pni"] <- "br"
  result$escopo[result$sistema == "financiamento"] <- "br"

  result$rota <- result$sistema
  result$rota[result$sistema == "populacao"] <- "ibge"
  result$rota[result$sistema == "sinan"] <- "sinannet"
  result$rota[result$sistema == "pni"] <- "bd_pni"
  result$rota[result$sistema == "siscan"] <- "SISCAN"
  result$rota[
    result$sistema == "siscan" &
      result$conjunto %in% c(
        "histologia_colo_residencia",
        "histologia_colo_atendimento",
        "histologia_colo_pacientes",
        "mamografia_residencia",
        "mamografia_atendimento",
        "mamografia_pacientes"
      )
  ] <- "siscan"
  result$rota[
    result$sistema == "sisvan" &
      result$conjunto == "bolsa_familia"
  ] <- "bolsa"
  result$rota[result$sistema == "financiamento"] <- "recsus"
  result$rota[
    result$sistema == "financiamento" &
      result$conjunto == "guia_pagamento"
  ] <- "gap"

  result$motor <- "tabcgi"
  result$motor[result$sistema %in% c("pni", "siscan")] <- "webtabx"
  result
})

.datasus_default_dataset <- c(
  sih = "aih_rd_internacao",
  sia = "atendimento",
  cnes = "estabelecimentos",
  populacao = "estimativa_municipal",
  sinan = "dengue",
  pni = "doses_aplicadas",
  siscan = "citologia_colo_residencia",
  sisvan = "atencao_basica",
  financiamento = "recursos_federais",
  sim = "obitos",
  sinasc = "nascidos_vivos"
)

.datasus_resolve_dataset <- function(sistema, conjunto = NULL) {
  sistema <- match.arg(sistema, names(.datasus_default_dataset))
  if (is.null(conjunto)) {
    conjunto <- unname(.datasus_default_dataset[[sistema]])
  }
  if (!is.character(conjunto) || length(conjunto) != 1L ||
      is.na(conjunto)) {
    stop("The 'conjunto' argument must be a single dataset name",
         call. = FALSE)
  }

  available <- .datasus_catalog[
    .datasus_catalog$sistema == sistema,
    ,
    drop = FALSE
  ]
  selected <- available[available$conjunto == conjunto, , drop = FALSE]
  if (!nrow(selected)) {
    stop(
      "Unknown ", toupper(sistema), " dataset '", conjunto,
      "'. Use datasus_catalogo('", sistema, "') to inspect valid datasets.",
      call. = FALSE
    )
  }
  selected
}

.datasus_dataset_urls <- function(metadata, uf = NULL,
                                  abrangencia = NULL) {
  scope <- metadata$escopo
  tabnet_system <- metadata$rota

  if (identical(scope, "all")) {
    if (!is.null(abrangencia)) {
      abrangencia <- match.arg(abrangencia, c("municipio", "uf"))
    }
    if (!is.null(uf) && identical(abrangencia, "uf")) {
      stop(
        "Use 'uf = NULL' when 'abrangencia = \"uf\"'",
        call. = FALSE
      )
    }
    if (!is.null(uf)) {
      uf <- .tabnet_validate_uf(uf)
      level <- "state"
    } else if (identical(abrangencia, "uf")) {
      level <- "national"
    } else {
      level <- "municipality"
    }
  } else {
    if (!is.null(uf)) {
      uf <- .tabnet_validate_uf(uf)
    }
    level <- switch(
      scope,
      br = "municipality",
      uf = "national",
      stop("Unknown dataset scope '", scope, "'", call. = FALSE)
    )
  }

  if (identical(metadata$motor, "webtabx")) {
    suffix <- switch(
      level,
      state = .tabnet_validate_uf(uf),
      municipality = "br",
      national = "uf"
    )
    definition <- paste0(
      tabnet_system, "/", metadata$prefixo, suffix, ".def"
    )
    return(list(
      form = paste0(
        "https://tabnet.datasus.gov.br/cgi/dhdat.exe?",
        definition
      ),
      query = paste0(
        "https://tabnet.datasus.gov.br/cgi/webtabx.exe?",
        definition
      ),
      definition = definition
    ))
  }

  list(
    form = .tabnet_url(
      tabnet_system,
      metadata$prefixo,
      level,
      uf = uf,
      action = "form"
    ),
    query = .tabnet_url(
      tabnet_system,
      metadata$prefixo,
      level,
      uf = uf,
      action = "query"
    )
  )
}

.datasus_uf_names <- c(
  ac = "Acre", al = "Alagoas", ap = "Amap\u00e1", am = "Amazonas",
  ba = "Bahia", ce = "Cear\u00e1", df = "Distrito Federal",
  es = "Esp\u00edrito Santo", go = "Goi\u00e1s", ma = "Maranh\u00e3o",
  mt = "Mato Grosso", ms = "Mato Grosso do Sul", mg = "Minas Gerais",
  pa = "Par\u00e1", pb = "Para\u00edba", pr = "Paran\u00e1",
  pe = "Pernambuco", pi = "Piau\u00ed", rj = "Rio de Janeiro",
  rn = "Rio Grande do Norte", rs = "Rio Grande do Sul",
  ro = "Rond\u00f4nia", rr = "Roraima", sc = "Santa Catarina",
  sp = "S\u00e3o Paulo", se = "Sergipe", to = "Tocantins"
)

.datasus_add_fixed_scope_uf <- function(metadata, options, filtros, uf) {
  if (is.null(uf) || identical(metadata$escopo, "all")) {
    return(filtros)
  }

  uf <- .tabnet_validate_uf(uf)
  candidates <- intersect(
    c("unidade_da_federacao", "unidade_federacao", "uf"),
    names(options$filtros)
  )
  if (!length(candidates)) {
    stop(
      "The dataset '", metadata$conjunto,
      "' uses a fixed geographic form and does not expose a compatible ",
      "state filter. Use 'uf = NULL' and inspect datasus_opcoes().",
      call. = FALSE
    )
  }

  filter <- candidates[1L]
  if (filter %in% names(filtros)) {
    stop(
      "Do not combine 'uf' with 'filtros$", filter, "'",
      call. = FALSE
    )
  }
  filtros[[filter]] <- unname(.datasus_uf_names[[uf]])
  filtros
}

.datasus_query <- function(sistema, conjunto, uf, linha, coluna,
                           conteudo, periodo, filtros,
                           abrangencia = NULL,
                           filtros_posicionais = FALSE) {
  metadata <- .datasus_resolve_dataset(sistema, conjunto)
  urls <- .datasus_dataset_urls(metadata, uf, abrangencia)
  page <- .tabnet_read_html(urls$form)
  options <- .tabnet_form_options(page)
  if (isTRUE(filtros_posicionais)) {
    if (length(filtros) != length(options$filtros)) {
      stop(
        "The current TABNET form is incompatible with the legacy function. ",
        "Use ", sistema, "() and datasus_opcoes() with named filters.",
        call. = FALSE
      )
    }
    names(filtros) <- names(options$filtros)
  }
  filtros <- .datasus_add_fixed_scope_uf(
    metadata,
    options,
    filtros,
    uf
  )
  fields <- .tabnet_build_fields(
    options,
    linha = linha,
    coluna = coluna,
    conteudo = conteudo,
    periodo = periodo,
    filtros = filtros
  )
  if (identical(metadata$motor, "webtabx")) {
    fields$formato <- NULL
    fields$mostre <- NULL
    fields$nomedef <- urls$definition
    fields$grafico <- ""
    fields$button1 <- "Mostra"
  }

  response <- .tabnet_post(urls$query, .tabnet_encode_fields(fields))
  result <- if (identical(metadata$motor, "webtabx")) {
    .tabnet_parse_webtabx(response)
  } else {
    .tabnet_parse_table(response)
  }
  attr(result, "datasus_proveniencia") <- list(
    fonte = "DATASUS TABNET",
    sistema = sistema,
    conjunto = conjunto,
    abrangencia = if (is.null(abrangencia)) metadata$escopo else abrangencia,
    uf = if (is.null(uf)) NA_character_ else toupper(uf),
    url = urls$query,
    consultado_em = Sys.time()
  )
  result
}

#' List supported DATASUS datasets
#'
#' Returns the local catalog used by the health-services query functions.
#' This function does not access the network.
#'
#' @param sistema Optional system name: `"sih"`, `"sia"`, `"cnes"`,
#'   `"populacao"`, `"sinan"`, `"pni"`, `"siscan"`, `"sisvan"` or
#'   `"financiamento"`, `"sim"` or `"sinasc"`.
#'
#' @return A data frame with the system, dataset name, category, description
#'   and geographic form scope.
#' @export
#'
#' @examples
#' datasus_catalogo()
#' datasus_catalogo("cnes")
datasus_catalogo <- function(sistema = NULL) {
  result <- .datasus_catalog
  if (!is.null(sistema)) {
    sistema <- match.arg(sistema, names(.datasus_default_dataset))
    result <- result[result$sistema == sistema, , drop = FALSE]
  }
  result$prefixo <- NULL
  result$rota <- NULL
  result$motor <- NULL
  rownames(result) <- NULL
  result
}

#' Inspect the options of a DATASUS query
#'
#' Reads the current TABNET form and returns its row, column, measure,
#' period and filter choices. Filter names in the returned `filtros` list
#' are the names accepted by the query functions.
#'
#' @param sistema System name accepted by [datasus_catalogo()].
#' @param conjunto Dataset name listed by [datasus_catalogo()]. When omitted,
#'   uses the default dataset for the system.
#' @param uf State abbreviation, two-digit IBGE code or state name. When
#'   `NULL`, uses the Brazil form.
#' @param abrangencia Optional geographic detail for SIM and SINASC:
#'   `"municipio"` for the nationwide municipal form or `"uf"` for the
#'   region/state form. Other systems retain their existing behavior.
#'
#' @return An object of class `datasus_opcoes`. Its components are data frames
#'   named `linha`, `coluna`, `conteudo` and `periodo`, plus a named list
#'   `filtros`.
#' @export
#'
#' @examples
#' \dontrun{
#' op <- datasus_opcoes("sih", uf = "MS")
#' op$conteudo
#' op$filtros$carater_atendimento
#' }
datasus_opcoes <- function(sistema, conjunto = NULL, uf = NULL,
                           abrangencia = NULL) {
  sistema <- match.arg(sistema, names(.datasus_default_dataset))
  metadata <- .datasus_resolve_dataset(sistema, conjunto)
  urls <- .datasus_dataset_urls(metadata, uf, abrangencia)
  result <- .tabnet_form_options(.tabnet_read_html(urls$form))
  attr(result, "sistema") <- sistema
  attr(result, "conjunto") <- metadata$conjunto
  class(result) <- c("datasus_opcoes", "list")
  result
}

#' @export
print.datasus_opcoes <- function(x, ...) {
  cat(
    "Op\u00e7\u00f5es TABNET para ",
    toupper(attr(x, "sistema")),
    " / ",
    attr(x, "conjunto"),
    "\n",
    sep = ""
  )
  cat(
    "  ",
    nrow(x$linha), " linhas; ",
    nrow(x$coluna), " colunas; ",
    nrow(x$conteudo), " conte\u00fados; ",
    nrow(x$periodo), " per\u00edodos\n",
    sep = ""
  )
  if (length(x$filtros)) {
    cat("  Filtros: ", paste(names(x$filtros), collapse = ", "), "\n",
        sep = "")
  }
  invisible(x)
}

#' Query hospital production from SIH/SUS
#'
#' @param conjunto Dataset name listed by `datasus_catalogo("sih")`.
#' @param uf Two-letter state abbreviation. `NULL` uses the Brazil form.
#' @param linha Row dimension. `NULL` selects the form's first dimension.
#' @param coluna Column dimension. `NULL` selects `"Não ativa"`.
#' @param conteudo Measure label, raw TABNET value, or one-based index.
#' @param periodo `"last"` for the latest competence, an exact period label or
#'   raw value, a four-digit year (all available competences in that year), or
#'   a vector of these values.
#' @param filtros Named list of filters. Names and choices are available from
#'   [datasus_opcoes()].
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' sih_producao(uf = "MS")
#' sih_producao(
#'   uf = "MS",
#'   linha = "Município",
#'   conteudo = "Internações",
#'   periodo = 2025,
#'   filtros = list(carater_atendimento = "Urgência")
#' )
#' }
sih_producao <- function(conjunto = "aih_rd_internacao", uf = NULL,
                         linha = NULL, coluna = NULL, conteudo = 1,
                         periodo = "last", filtros = list()) {
  .datasus_query(
    "sih", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query ambulatory production from SIA/SUS
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' sia_producao(uf = "MS")
#' sia_producao(
#'   linha = "UF",
#'   conteudo = "Qtd.aprovada",
#'   periodo = "last"
#' )
#' }
sia_producao <- function(conjunto = "atendimento", uf = NULL,
                         linha = NULL, coluna = NULL, conteudo = 1,
                         periodo = "last", filtros = list()) {
  .datasus_query(
    "sia", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query the National Registry of Health Establishments (CNES)
#'
#' Supports establishments, physical resources, human resources and health
#' teams. Use `datasus_catalogo("cnes")` to list the available datasets.
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' cnes(uf = "MS")
#' cnes(
#'   conjunto = "leitos_internacao",
#'   uf = "MS",
#'   periodo = "last"
#' )
#' }
cnes <- function(conjunto = "estabelecimentos", uf = NULL,
                 linha = NULL, coluna = NULL, conteudo = 1,
                 periodo = "last", filtros = list()) {
  .datasus_query(
    "cnes", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query hospital morbidity from SIH/SUS
#'
#' Provides general morbidity and external-causes views by place of
#' hospitalization or residence. Use `datasus_catalogo("sih")` and select
#' rows whose category is `"morbidade hospitalar"` to list the datasets.
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' sih_morbidade(uf = "MS")
#' sih_morbidade(
#'   uf = "MS",
#'   linha = "Capítulo CID-10",
#'   conteudo = "Internações",
#'   periodo = 2025
#' )
#' }
sih_morbidade <- function(conjunto = "geral_internacao", uf = NULL,
                          linha = NULL, coluna = NULL, conteudo = 1,
                          periodo = "last", filtros = list()) {
  .datasus_query(
    "sih", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query resident population estimates
#'
#' Provides census, projection, retroprojection and municipal estimate
#' datasets published in the DATASUS population section. The default dataset
#' contains municipal estimates by sex and age from 2000 through 2021.
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' populacao_residente(uf = "MS", periodo = 2021)
#' populacao_residente(
#'   linha = "Unidade da Federação",
#'   periodo = c(2020, 2021)
#' )
#' }
populacao_residente <- function(conjunto = "estimativa_municipal",
                                uf = NULL, linha = NULL, coluna = NULL,
                                conteudo = 1, periodo = "last",
                                filtros = list()) {
  if (identical(conjunto, "estimativa_municipal") &&
      !is.null(uf) && is.null(linha)) {
    linha <- "Munic\u00edpio"
  }
  .datasus_query(
    "populacao", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query notifiable diseases and conditions from SINAN
#'
#' Supports the disease-specific TABNET datasets listed by
#' `datasus_catalogo("sinan")`. Because each disease has its own form, inspect
#' dimensions, periods and filters with `datasus_opcoes("sinan", agravo)`.
#' Febre amarela is not included because its official link points to a
#' separate open-data platform rather than a TABNET form.
#'
#' @param agravo Notifiable disease or condition listed by
#'   `datasus_catalogo("sinan")`.
#' @param uf Two-letter state abbreviation. `NULL` uses the Brazil form.
#' @param linha Row dimension. `NULL` selects the form's first dimension.
#' @param coluna Column dimension. `NULL` selects `"Não ativa"`.
#' @param conteudo Measure label, raw TABNET value, or one-based index.
#' @param periodo `"last"` for the latest available period, an exact period
#'   label or raw value, a four-digit year, or a vector of these values.
#' @param filtros Named list of filters. Names and choices are available from
#'   [datasus_opcoes()].
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' sinan("dengue", uf = "MS")
#' sinan(
#'   "violencia_interpessoal_autoprovocada",
#'   uf = "MS",
#'   periodo = 2024
#' )
#' }
sinan <- function(agravo = "dengue", uf = NULL, linha = NULL,
                  coluna = NULL, conteudo = 1, periodo = "last",
                  filtros = list()) {
  .datasus_query(
    "sinan", agravo, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query legacy immunization data from PNI
#'
#' Provides applied doses and vaccination coverage from the legacy TABNET
#' series, whose latest available year is currently 2022. DATASUS reports that
#' coverage and applied-dose data are under review for integration
#' inconsistencies.
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' pni_imunizacoes(uf = "MS")
#' pni_imunizacoes(
#'   conjunto = "cobertura",
#'   uf = "MS",
#'   periodo = 2022
#' )
#' }
pni_imunizacoes <- function(conjunto = "doses_aplicadas", uf = NULL,
                            linha = NULL, coluna = NULL, conteudo = 1,
                            periodo = "last", filtros = list()) {
  if (!is.null(uf) && is.null(linha)) {
    linha <- "Munic\u00edpio"
  }
  .datasus_query(
    "pni", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query cancer screening and diagnostic exams from SISCAN
#'
#' Supports cervical cytology and histology, mammography, and breast cytology
#' and histology by residence, attendance location or patient.
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' siscan(uf = "MS")
#' siscan(
#'   conjunto = "mamografia_residencia",
#'   uf = "MS",
#'   periodo = 2025
#' )
#' }
siscan <- function(conjunto = "citologia_colo_residencia", uf = NULL,
                   linha = NULL, coluna = NULL, conteudo = 1,
                   periodo = "last", filtros = list()) {
  .datasus_query(
    "siscan", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query legacy nutritional surveillance data from SISVAN
#'
#' Provides the historical TABNET series for Primary Care users and Bolsa
#' Familia beneficiaries. These are legacy series and should not be
#' interpreted as the current SISVAN coverage.
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' sisvan(uf = "MS")
#' sisvan(conjunto = "bolsa_familia", uf = "MS")
#' }
sisvan <- function(conjunto = "atencao_basica", uf = NULL,
                   linha = NULL, coluna = NULL, conteudo = 1,
                   periodo = "last", filtros = list()) {
  .datasus_query(
    "sisvan", conjunto, uf, linha, coluna, conteudo, periodo, filtros
  )
}

#' Query legacy SUS financing tables
#'
#' Provides the three financial tables linked by the DATASUS TABNET catalog:
#' federal resources, approved production values by provider, and payment
#' authorization guides. Availability periods vary and may be historical.
#'
#' @inheritParams sih_producao
#'
#' @return A data frame containing the TABNET result.
#' @export
#'
#' @examples
#' \dontrun{
#' financiamento_sus(uf = "MS")
#' financiamento_sus(conjunto = "producao_prestador", uf = "MS")
#' }
financiamento_sus <- function(conjunto = "recursos_federais", uf = NULL,
                              linha = NULL, coluna = NULL, conteudo = 1,
                              periodo = "last", filtros = list()) {
  .datasus_query(
    "financiamento",
    conjunto,
    uf,
    linha,
    coluna,
    conteudo,
    periodo,
    filtros
  )
}
