#!/usr/bin/env Rscript

# Organiza as tabelas finais nos formatos usados pelos gráficos.
# Confere os denominadores antes de preparar os dados de exibição.


script_path <- function() {
  argumento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(argumento)) stop("Execute este arquivo com Rscript.", call. = FALSE)
  normalizePath(sub("^--file=", "", argumento[[1L]]), mustWork = FALSE)
}
raiz_codigo <- local({
  p <- dirname(script_path())
  repeat {
    if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break
    anterior <- dirname(p)
    if (identical(anterior, p)) stop("Bootstrap não localizado.", call. = FALSE)
    p <- anterior
  }
  p
})
source(file.path(raiz_codigo, "pipeline", "_bootstrap.R"))

obrigatorio <- function(path, descricao) {
  if (!file.exists(path)) stop("HOLD: arquivo ausente para ", descricao, ": ", path, call. = FALSE)
  path
}
ler <- function(path) as_character_frame(read_machine_csv(obrigatorio(path, basename(path))))
contar <- function(dados, colunas, nome = "n") {
  if (!nrow(dados)) return(data.frame(stringsAsFactors = FALSE))
  resultado <- aggregate(rep(1L, nrow(dados)), dados[colunas], sum)
  names(resultado)[ncol(resultado)] <- nome
  resultado
}
normalizar_sim_nao <- function(x) {
  x <- tolower(texto_limpo(x))
  ifelse(x == "yes", "Sim", ifelse(x == "unclear", "Incerto", "Não"))
}
completar_grade <- function(contagens, grade, chaves) {
  saida <- merge(grade, contagens, by = chaves, all.x = TRUE, sort = FALSE)
  saida$n[is.na(saida$n)] <- 0L
  saida
}

cp13 <- obrigatorio(
  file.path(CFG$pipeline_dir, "data_extraction", "data_extraction_package_CP13_COMPLETE_33.xlsx"),
  "extração final"
)
cp14_dominios <- ler(file.path(CFG$pipeline_dir, "methodological_appraisal", "cp14", "final", "CP14_DOMAIN_JUDGMENTS_FINAL.csv"))
cp17 <- file.path(CFG$pipeline_dir, "synthesis", "cp17", "tables")
prontidao <- ler(file.path(cp17, "CP17_TABLE_CLINICAL_READINESS.csv"))
desempenho <- ler(file.path(cp17, "CP17_TABLE_VALIDATION_PERFORMANCE.csv"))
metanalise <- ler(file.path(cp17, "CP17_META_ANALYSIS_FEASIBILITY.csv"))
sensibilidade <- ler(file.path(cp17, "CP17_TABLE_SENSITIVITY_ANALYSES.csv"))
tehai <- ler(file.path(CFG$pipeline_dir, "tehai", "synthesis", "TEHAI_SUBCOMPONENT_SCORE_DISTRIBUTION.csv"))
classificacao <- ler(file.path(PROJECT_ROOT, "dados_humanos", "visualizacao", "classificacao_unidades_modelo_tarefa.csv"))

validate_required_columns(prontidao, c(
  "record_id", "model_task_id", "readiness_applicability", "readiness_level",
  "thermography_protocol_replicable", "risk_of_bias_summary", "reporting_quality_summary",
  "code_available", "data_available", "model_available", "fairness_or_subgroup_bias_evaluated"
), "prontidão clínica")
validate_required_columns(classificacao, c(
  "model_task_id", "construto", "independencia_padrao_referencia", "categoria_validacao", "modalidade_entrada"
), "classificação humana para figuras")
validar_chave_unica(prontidao, "model_task_id", "prontidão clínica")
validar_chave_unica(classificacao, "model_task_id", "classificação humana para figuras")
if (!setequal(prontidao$model_task_id, classificacao$model_task_id)) {
  stop("HOLD: a classificação visual não cobre exatamente as 50 unidades modelo–tarefa.", call. = FALSE)
}
if (nrow(prontidao) != 50L) stop("HOLD: são esperadas 50 unidades modelo–tarefa.", call. = FALSE)
classificacao <- classificacao[match(prontidao$model_task_id, classificacao$model_task_id), , drop = FALSE]
base_unidades <- cbind(prontidao, classificacao[setdiff(names(classificacao), "model_task_id")])

construtos <- c("Dor direta", "Comportamento relacionado à dor", "Condição dolorosa", "Resposta terapêutica", "Tarefa auxiliar")
estagios <- c("Não classificável", "Nível 0", "Nível 1", "Nível 2", "Níveis 3–4")
if (any(!base_unidades$construto %in% construtos)) stop("HOLD: construto fora do vocabulário aprovado.", call. = FALSE)
base_unidades$estagio_clinico <- ifelse(
  base_unidades$readiness_applicability != "yes", "Não classificável",
  ifelse(base_unidades$readiness_level %in% c("3", "4"), "Níveis 3–4", paste("Nível", base_unidades$readiness_level))
)

prisma <- ler(file.path(CFG$pipeline_dir, "reporting", "cp16", "tables", "CP16_PRISMA_FLOW_VERIFIED.csv"))
validate_required_columns(prisma, c("display_order", "label", "n"), "fluxo PRISMA")
fig1 <- data.frame(
  etapa = c(
    "Registros identificados", "Duplicatas exatas removidas", "Registros triados",
    "Excluídos por título/resumo", "Limpeza tardia de duplicatas exatas por título",
    "Relatórios buscados", "Relatórios não recuperados", "Textos completos avaliados",
    "Textos completos excluídos", "Estudos incluídos"
  ),
  categoria_fluxo = c("Fluxo principal", "Exclusão ou limpeza", "Fluxo principal", "Exclusão ou limpeza", "Exclusão ou limpeza", "Fluxo principal", "Exclusão ou limpeza", "Fluxo principal", "Exclusão ou limpeza", "Fluxo principal"),
  n = as.integer(prisma$n),
  ordem_exibicao = as.integer(prisma$display_order),
  stringsAsFactors = FALSE
)
fig1 <- fig1[order(fig1$ordem_exibicao), , drop = FALSE]

fig2a <- contar(base_unidades, c("construto", "estagio_clinico"))
fig2a <- completar_grade(fig2a, expand.grid(construto = construtos, estagio_clinico = estagios, stringsAsFactors = FALSE), c("construto", "estagio_clinico"))
modalidades <- sort(unique(base_unidades$modalidade_entrada), method = "radix")
fig2b <- contar(base_unidades, c("construto", "estagio_clinico", "modalidade_entrada"))
relatos <- aggregate(record_id ~ construto + estagio_clinico + modalidade_entrada, base_unidades, function(x) length(unique(x)))
names(relatos)[names(relatos) == "record_id"] <- "n_relatos"
fig2b <- merge(fig2b, relatos, by = c("construto", "estagio_clinico", "modalidade_entrada"), all = TRUE)
fig2b <- completar_grade(fig2b, expand.grid(construto = construtos, estagio_clinico = estagios, modalidade_entrada = modalidades, stringsAsFactors = FALSE), c("construto", "estagio_clinico", "modalidade_entrada"))
fig2b$n_relatos[is.na(fig2b$n_relatos)] <- 0L

clinicas <- base_unidades[base_unidades$readiness_applicability == "yes", , drop = FALSE]
fig3 <- contar(clinicas, c("construto", "independencia_padrao_referencia", "categoria_validacao", "estagio_clinico"))

mapa_estado <- function(x) {
  mapa <- c(
    yes = "Sim", no = "Não", unclear = "Incerto", high = "Alto", low = "Baixo",
    partial = "Parcial", incomplete = "Incompleto", complete = "Completo",
    sim = "Sim", "não" = "Não", incerto = "Incerto", alto = "Alto", baixo = "Baixo",
    parcial = "Parcial", incompleto = "Incompleto", completo = "Completo"
  )
  unname(mapa[tolower(texto_limpo(x))])
}
linhas_indicador <- function(dados, campo, indicador, dominio, denominador, ordem, unidade) {
  categoria <- mapa_estado(dados[[campo]])
  x <- contar(data.frame(categoria_resposta = categoria, stringsAsFactors = FALSE), "categoria_resposta")
  x$dominio <- dominio
  x$indicador <- indicador
  x$denominador <- denominador
  x$unidade_analitica <- unidade
  x$ordem_exibicao <- ordem
  x[, c("dominio", "indicador", "categoria_resposta", "n", "denominador", "unidade_analitica", "ordem_exibicao")]
}
fig4 <- do.call(rbind, list(
  linhas_indicador(clinicas, "risk_of_bias_summary", "Risco de viés global", "Confiança", 38L, 1L, "unidade clínica"),
  linhas_indicador(clinicas, "reporting_quality_summary", "Qualidade do relato", "Relato", 38L, 2L, "unidade clínica")
))
termo <- cp14_dominios[
  cp14_dominios$instrument == "Thermography reporting" & cp14_dominios$phase == "overall" & cp14_dominios$domain == "overall",
  , drop = FALSE
]
if (nrow(termo) != 47L) stop("HOLD: o relato termográfico deve conter 47 unidades aplicáveis.", call. = FALSE)
termo_tmp <- data.frame(valor = termo$domain_judgment, stringsAsFactors = FALSE)
fig4 <- rbind(fig4, linhas_indicador(termo_tmp, "valor", "Relato termográfico global", "Relato", 47L, 3L, "unidade com termografia aplicável"))
fig4 <- rbind(fig4, linhas_indicador(clinicas, "thermography_protocol_replicable", "Protocolo termográfico replicável", "Replicabilidade", 38L, 4L, "unidade clínica"))

validate_required_columns(desempenho, c("subject_level_separation_class", "has_confidence_interval", "has_evaluated_sample_size"), "resultados primários")
if (nrow(desempenho) != 54L) stop("HOLD: são esperados 54 resultados primários.", call. = FALSE)
separacao <- tolower(texto_limpo(desempenho$subject_level_separation_class))
separacao <- ifelse(separacao == "yes", "Sim", ifelse(separacao == "unclear", "Incerto", "Não"))
tmp <- data.frame(valor = separacao, stringsAsFactors = FALSE)
fig4 <- rbind(fig4, linhas_indicador(tmp, "valor", "Separação em nível de participante", "Validação", 54L, 5L, "resultado primário"))
fig4 <- rbind(fig4,
  linhas_indicador(desempenho, "has_confidence_interval", "Intervalo de confiança", "Validação", 54L, 6L, "resultado primário"),
  linhas_indicador(desempenho, "has_evaluated_sample_size", "Denominador avaliado", "Validação", 54L, 7L, "resultado primário"),
  linhas_indicador(clinicas, "code_available", "Código disponível", "Ciência aberta", 38L, 8L, "unidade clínica"),
  linhas_indicador(clinicas, "data_available", "Dados disponíveis", "Ciência aberta", 38L, 9L, "unidade clínica"),
  linhas_indicador(clinicas, "model_available", "Modelo disponível", "Ciência aberta", 38L, 10L, "unidade clínica"),
  linhas_indicador(clinicas, "fairness_or_subgroup_bias_evaluated", "Equidade avaliada", "Equidade", 38L, 11L, "unidade clínica")
)

rotulos_componentes <- c(Capability = "Capacidade", Utility = "Utilidade", Adoption = "Adoção")
rotulos_subcomponentes <- c(
  CAP_01 = "Objetivo", CAP_02 = "Fonte e integridade dos dados", CAP_03 = "Validade interna",
  CAP_04 = "Validade externa", CAP_05 = "Métricas de desempenho", CAP_06 = "Caso de uso",
  UTL_01 = "Alinhamento com o domínio", UTL_02 = "Segurança e qualidade", UTL_03 = "Transparência",
  UTL_04 = "Privacidade", UTL_05 = "Não maleficência", ADP_01 = "Uso em contexto de saúde",
  ADP_02 = "Integração técnica", ADP_03 = "Número de serviços", ADP_04 = "Generalização e contextualização"
)
fig5 <- data.frame(
  componente_tehai = unname(rotulos_componentes[tehai$component]),
  subcomponente_id = tehai$subcomponent_id,
  subcomponente = unname(rotulos_subcomponentes[tehai$subcomponent_id]),
  escore = as.integer(tehai$score),
  n = as.integer(tehai$n),
  denominador_subcomponente = as.integer(tehai$denominator),
  stringsAsFactors = FALSE
)
if (any(is.na(fig5$componente_tehai) | is.na(fig5$subcomponente))) stop("HOLD: identificador TEHAI sem rótulo aprovado.", call. = FALSE)

candidatos <- metanalise[tolower(metanalise$candidate_group_two_or_more_studies) == "yes", , drop = FALSE]
candidatos <- candidatos[order(candidatos$pain_target_category, candidatos$task_class, candidatos$canonical_metric, method = "radix"), , drop = FALSE]
if (nrow(candidatos) != 6L) stop("HOLD: são esperados seis grupos candidatos à síntese.", call. = FALSE)
mapa_criterios <- c(
  heterogeneous_reference_standards = "Padrões de referência heterogêneos",
  heterogeneous_target_or_label_definitions = "Alvos ou definições de rótulo heterogêneos",
  variance_or_confidence_intervals_incomplete = "Incerteza ou variância incompleta",
  evaluated_sample_size_incomplete = "Denominador avaliado incompleto",
  patient_level_independence_not_consistently_demonstrated = "Independência por participante não consistentemente demonstrada"
)
mapa_criterios["heterogeneous_reference_standards"] <- "Padrões de referência heterogêneos"
mapa_criterios["heterogeneous_targets_or_label_definitions"] <- "Alvos ou definições de rótulo heterogêneos"
fig6_linhas <- lapply(seq_len(nrow(candidatos)), function(i) {
  razoes <- trimws(strsplit(candidatos$ineligibility_reasons[[i]], "\\|", perl = TRUE)[[1L]])
  razoes <- intersect(razoes, names(mapa_criterios))
  data.frame(
    grupo_candidato = sprintf("G%02d", i),
    alvo_clinico = candidatos$pain_target_category[[i]],
    familia_tarefa = candidatos$task_class[[i]],
    metrica = candidatos$canonical_metric[[i]],
    criterio_nao_atendido = unname(mapa_criterios[razoes]),
    presenca = "Sim",
    n_estudos = as.integer(candidatos$n_studies[[i]]),
    elegivel_agrupamento_quantitativo = "Não",
    stringsAsFactors = FALSE
  )
})
fig6 <- do.call(rbind, fig6_linhas)

manifesto_busca <- ler(file.path(CFG$processed_dir, "search_manifest.csv"))
validate_required_columns(manifesto_busca, c("source_database", "corpus_type", "query_label", "include_in_import", "relative_path"), "manifesto de busca")
incluidos_busca <- manifesto_busca[manifesto_busca$include_in_import == "TRUE" | grepl("zero", tolower(manifesto_busca$notes)), , drop = FALSE]
s1 <- aggregate(relative_path ~ corpus_type + source_database, incluidos_busca, length)
names(s1)[3] <- "n_arquivos_fonte"
consultas <- aggregate(query_label ~ corpus_type + source_database, incluidos_busca, function(x) length(unique(x[nzchar(x)])))
names(consultas)[3] <- "n_consultas"
contagens_fontes <- ler(file.path(CFG$outputs_dir, "source_level_counts.csv"))
validate_required_columns(contagens_fontes, c("source_database", "corpus_type", "records_imported"), "contagens por fonte")
ocorrencias <- aggregate(as.integer(contagens_fontes$records_imported), contagens_fontes[c("corpus_type", "source_database")], sum, na.rm = TRUE)
names(ocorrencias)[3] <- "ocorrencias_importadas"
s1 <- Reduce(function(x, y) merge(x, y, by = c("corpus_type", "source_database"), all = TRUE), list(s1, consultas, ocorrencias))
tipo_corpus <- c(primary = "Busca principal", clinical_trial_registry_supplementary = "Registro de ensaios", review_registry_overlap = "Registro de revisões/sobreposição")
figs1 <- data.frame(tipo_corpus = unname(tipo_corpus[s1$corpus_type]), fonte = s1$source_database, ocorrencias_importadas = s1$ocorrencias_importadas, n_arquivos_fonte = s1$n_arquivos_fonte, n_consultas = s1$n_consultas)

sobreposicao <- as.data.frame(readxl::read_excel(cp13, sheet = "publication_overlap", col_types = "text"), stringsAsFactors = FALSE)
mapa_sobreposicao <- c(strong_or_probable_overlap = "Forte ou provável", possible_overlap = "Possível", no_overlap_identified = "Sem sobreposição identificada")
figs2 <- contar(data.frame(categoria_sobreposicao = unname(mapa_sobreposicao[sobreposicao$overlap_status]), stringsAsFactors = FALSE), "categoria_sobreposicao", "n_relatos")
figs2$denominador <- 33L

mapa_metricas <- c(accuracy = "Acurácia", auroc = "AUROC", mae = "MAE", classification_success_rate = "Taxa de sucesso da classificação")
figs3 <- contar(data.frame(familia_metrica = ifelse(desempenho$canonical_metric %in% names(mapa_metricas), mapa_metricas[desempenho$canonical_metric], desempenho$canonical_metric), stringsAsFactors = FALSE), "familia_metrica", "n_resultados_primarios")

cenarios <- data.frame(
  cenario = sensibilidade$scenario,
  factivel = ifelse(sensibilidade$feasible == "yes", "Sim", "Não"),
  n_estudos = as.integer(sensibilidade$studies_n),
  denominador_unidades = as.integer(sensibilidade$units_n),
  conclusao_vs_base = sensibilidade$conclusion_vs_base,
  stringsAsFactors = FALSE
)
figs4 <- do.call(rbind, lapply(seq_len(nrow(cenarios)), function(i) data.frame(
  cenario = cenarios$cenario[[i]], factivel = cenarios$factivel[[i]], estagio_clinico = c("Nível 0", "Nível 1", "Nível 2", "Níveis 3–4"),
  n = as.integer(unlist(sensibilidade[i, c("readiness_level_0_n", "readiness_level_1_n", "readiness_level_2_n", "readiness_level_3_or_4_n")], use.names = FALSE)),
  n_estudos = cenarios$n_estudos[[i]], denominador_unidades = cenarios$denominador_unidades[[i]], conclusao_vs_base = cenarios$conclusao_vs_base[[i]], stringsAsFactors = FALSE
)))
figs5 <- fig4[fig4$indicador %in% c("Código disponível", "Dados disponíveis", "Modelo disponível", "Equidade avaliada"), c("indicador", "categoria_resposta", "n", "denominador")]

termica <- as.data.frame(readxl::read_excel(cp13, sheet = "thermal_acquisition"), stringsAsFactors = FALSE)
campos_termicos <- c(
  "Modelo da câmera" = "camera_model", "Faixa espectral" = "spectral_range_um", "Resolução térmica" = "thermal_resolution_px",
  "NETD" = "netd_mk", "Temperatura ambiente" = "room_temperature_c", "Aclimatação" = "acclimatization_min",
  "Emissividade" = "emissivity", "Distância da câmera" = "camera_distance_m", "Definição da ROI" = "roi_definition_method",
  "Pré-processamento" = "preprocessing_steps", "Dados radiométricos brutos" = "raw_radiometric_data"
)
figs6 <- do.call(rbind, lapply(names(campos_termicos), function(rotulo) {
  valor <- termica[[campos_termicos[[rotulo]]]]
  relatado <- sum(!is.na(valor) & nzchar(trimws(as.character(valor))))
  data.frame(variavel = rotulo, categoria_resposta = c("Relatado", "Não relatado"), n = c(relatado, 50L - relatado), denominador = 50L, unidade_analitica = "Registro de aquisição termográfica", stringsAsFactors = FALSE)
}))

saida <- file.path(PROJECT_ROOT, "dados", "saida", "tabelas_figuras")
ensure_dir(saida)
tabelas <- list(
  figura_1_prisma.csv = fig1,
  figura_2_construto_estagio.csv = fig2a,
  figura_2_modalidade.csv = fig2b,
  figura_3_validade.csv = fig3,
  figura_4_metodologia.csv = fig4,
  figura_5_tehai.csv = fig5,
  figura_6_sintese.csv = fig6,
  figura_s1_fontes.csv = figs1,
  figura_s2_sobreposicao.csv = figs2,
  figura_s3_metricas.csv = figs3,
  figura_s4_sensibilidade.csv = figs4,
  figura_s5_ciencia_aberta.csv = figs5,
  figura_s6_termografia.csv = figs6
)
invisible(Map(function(x, nome) stable_write_csv(x, file.path(saida, nome)), tabelas, names(tabelas)))

esperados <- c(
  estudos_incluidos = 33L, unidades = 50L, resultados_primarios = 54L,
  termografia_aplicavel = 47L, unidades_clinicas = 38L, nao_classificaveis = 12L,
  nivel_0 = 11L, nivel_1 = 24L, nivel_2 = 3L, niveis_3_4 = 0L,
  tehai_sistemas = 29L, tehai_avaliacoes = 435L, grupos_sintese = 6L
)
observados <- c(
  estudos_incluidos = length(unique(prontidao$record_id)), unidades = nrow(prontidao), resultados_primarios = nrow(desempenho),
  termografia_aplicavel = nrow(termo), unidades_clinicas = nrow(clinicas), nao_classificaveis = sum(base_unidades$estagio_clinico == "Não classificável"),
  nivel_0 = sum(base_unidades$estagio_clinico == "Nível 0"), nivel_1 = sum(base_unidades$estagio_clinico == "Nível 1"),
  nivel_2 = sum(base_unidades$estagio_clinico == "Nível 2"), niveis_3_4 = sum(base_unidades$estagio_clinico == "Níveis 3–4"),
  tehai_sistemas = unique(fig5$denominador_subcomponente), tehai_avaliacoes = sum(fig5$n), grupos_sintese = length(unique(fig6$grupo_candidato))
)
checks <- data.frame(verificacao = names(esperados), esperado = as.integer(esperados), observado = as.integer(observados[names(esperados)]), stringsAsFactors = FALSE)
checks$status <- ifelse(checks$esperado == checks$observado, "PASS", "FAIL")
stable_write_csv(checks, file.path(saida, "00_VALIDACAO.csv"))
if (any(checks$status != "PASS")) stop("HOLD: as tabelas de exibição falharam nos invariantes científicos.", call. = FALSE)
cat("Tabelas de exibição preparadas e validadas em: ", saida, "\n", sep = "")
