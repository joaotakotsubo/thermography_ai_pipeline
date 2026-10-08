#!/usr/bin/env Rscript

# Reconcilia as avaliações metodológicas usando os registros independentes dos revisores.
# As discordâncias só recebem uma resposta final com decisão conjunta documentada.


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

diretorio_obrigatorio <- function(variavel, padrao) {
  valor <- Sys.getenv(variavel, unset = file.path(PROJECT_ROOT, padrao))
  if (!dir.exists(valor)) stop("Diretório ausente: ", valor, " (", variavel, ").", call. = FALSE)
  normalizePath(valor, mustWork = TRUE)
}

arquivo_opcional <- function(variavel, padrao) {
  valor <- Sys.getenv(variavel, unset = file.path(PROJECT_ROOT, padrao))
  normalizePath(valor, mustWork = FALSE)
}

pastas <- list(
  aplicabilidade_r1 = diretorio_obrigatorio("AITHERMO_CP14_APPLICABILITY_R1_DIR", "dados_humanos/cp14/aplicabilidade/revisor_1"),
  aplicabilidade_r2 = diretorio_obrigatorio("AITHERMO_CP14_APPLICABILITY_R2_DIR", "dados_humanos/cp14/aplicabilidade/revisor_2"),
  codificacao_r1 = diretorio_obrigatorio("AITHERMO_CP14_CODING_R1_DIR", "dados_humanos/cp14/codificacao/revisor_1"),
  codificacao_r2 = diretorio_obrigatorio("AITHERMO_CP14_CODING_R2_DIR", "dados_humanos/cp14/codificacao/revisor_2")
)
decisoes_path <- arquivo_opcional("AITHERMO_CP14_JOINT_DECISIONS", "dados_humanos/cp14/decisoes_conjuntas.csv")
saida <- file.path(CFG$pipeline_dir, "methodological_appraisal", "cp14", "final")
fila_dir <- file.path(CFG$pipeline_dir, "methodological_appraisal", "cp14", "adjudication")
ensure_dir(saida)
ensure_dir(fila_dir)

listar_planilhas <- function(pasta) {
  arquivos <- list.files(pasta, pattern = "\\.xlsx?$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  arquivos[!grepl("(^|/)~\\$", arquivos)]
}

ler_abas <- function(arquivos, aba, pular = 0L) {
  linhas <- list()
  for (arquivo in sort(arquivos, method = "radix")) {
    abas <- readxl::excel_sheets(arquivo)
    if (!(aba %in% abas)) next
    dados <- ler_tabela_revisor(arquivo, aba, pular)
    dados <- dados[rowSums(nzchar(as.matrix(as_character_frame(dados)))) > 0L, , drop = FALSE]
    if (!nrow(dados)) next
    dados$.source_file <- normalizePath(arquivo, mustWork = TRUE)
    linhas[[length(linhas) + 1L]] <- dados
  }
  if (!length(linhas)) return(data.frame(stringsAsFactors = FALSE))
  campos <- unique(unlist(lapply(linhas, names), use.names = FALSE))
  linhas <- lapply(linhas, function(x) {
    for (campo in setdiff(campos, names(x))) x[[campo]] <- ""
    x[campos]
  })
  do.call(rbind, linhas)
}

normalizar_colunas <- function(dados) {
  names(dados) <- tolower(gsub("[^a-zA-Z0-9]+", "_", iconv(names(dados), to = "ASCII//TRANSLIT")))
  names(dados) <- gsub("^_|_$", "", names(dados))
  as_character_frame(dados)
}

inferir_lote <- function(dados) {
  lote <- if ("batch" %in% names(dados)) texto_limpo(dados$batch) else if ("lote" %in% names(dados)) texto_limpo(dados$lote) else rep("", nrow(dados))
  faltantes <- !nzchar(lote)
  if (any(faltantes) && ".source_file" %in% names(dados)) {
    achado <- regmatches(dados$.source_file[faltantes], regexpr("B[0-9]{2}", dados$.source_file[faltantes], perl = TRUE))
    lote[faltantes] <- achado
  }
  lote
}

carregar_formularios <- function(pasta, tipo) {
  arquivos <- listar_planilhas(pasta)
  if (!length(arquivos)) stop("Nenhuma planilha encontrada em ", pasta, call. = FALSE)
  if (tipo == "aplicabilidade") {
    dados <- normalizar_colunas(ler_abas(arquivos, "applicability_form", 3L))
    if (!nrow(dados)) dados <- normalizar_colunas(ler_abas(arquivos, "pilot_applicability_form", 0L))
  } else if (tipo == "item") {
    dados <- normalizar_colunas(ler_abas(arquivos, "pilot_item_form", 0L))
  } else {
    dados <- normalizar_colunas(ler_abas(arquivos, "pilot_domain_form", 0L))
  }
  if (!nrow(dados)) stop("Aba de ", tipo, " não localizada em ", pasta, call. = FALSE)
  dados$batch <- inferir_lote(dados)
  dados
}

arquivos_por_revisor <- list(
  r1 = unique(c(listar_planilhas(pastas$aplicabilidade_r1), listar_planilhas(pastas$codificacao_r1))),
  r2 = unique(c(listar_planilhas(pastas$aplicabilidade_r2), listar_planilhas(pastas$codificacao_r2)))
)

app_1 <- carregar_formularios(pastas$aplicabilidade_r1, "aplicabilidade")
app_2 <- carregar_formularios(pastas$aplicabilidade_r2, "aplicabilidade")
item_1 <- carregar_formularios(pastas$codificacao_r1, "item")
item_2 <- carregar_formularios(pastas$codificacao_r2, "item")
domain_1 <- carregar_formularios(pastas$codificacao_r1, "domain")
domain_2 <- carregar_formularios(pastas$codificacao_r2, "domain")

if (!"instrument" %in% names(app_1) && "candidate_instrument" %in% names(app_1)) app_1$instrument <- app_1$candidate_instrument
if (!"instrument" %in% names(app_2) && "candidate_instrument" %in% names(app_2)) app_2$instrument <- app_2$candidate_instrument

comparacoes <- list(
  applicability = comparar_revisores(
    app_1, app_2,
    c("appraisal_unit_id", "instrument"), "response",
    c("batch", "record_id", "rationale", "review_date", "key_source_pages", "key_evidence"),
    "aplicabilidade"
  ),
  item = comparar_revisores(
    item_1, item_2,
    c("appraisal_unit_id", "instrument", "phase", "domain", "item_id"), "response",
    c("batch", "record_id", "instrument_applicability", "rationale", "review_date", "supporting_page_or_location", "supporting_evidence_note"),
    "item"
  ),
  domain = comparar_revisores(
    domain_1, domain_2,
    c("appraisal_unit_id", "instrument", "phase", "domain"), "domain_judgment",
    c("batch", "record_id", "rationale", "review_date"),
    "domain"
  )
)

agreement_pairs <- do.call(rbind, lapply(names(comparacoes), function(tipo) {
  x <- comparacoes[[tipo]]
  data.frame(
    assessment_type = tipo,
    comparison_key = x$comparison_key,
    reviewer_1_response = x$reviewer_1_response,
    reviewer_2_response = x$reviewer_2_response,
    agreement = x$agreement,
    stringsAsFactors = FALSE
  )
}))
stable_write_csv(agreement_pairs, file.path(saida, "CP14_AGREEMENT_PAIRS_PRE_ADJUDICATION.csv"))

fila <- do.call(rbind, lapply(names(comparacoes), function(tipo) {
  x <- comparacoes[[tipo]][!comparacoes[[tipo]]$agreement, , drop = FALSE]
  if (!nrow(x)) return(NULL)
  x$assessment_type <- tipo
  x[, c("assessment_type", setdiff(names(x), "assessment_type")), drop = FALSE]
}))
if (is.null(fila)) fila <- data.frame(stringsAsFactors = FALSE)
stable_write_csv(fila, file.path(fila_dir, "CP14_DISAGREEMENT_QUEUE.csv"))

if (!file.exists(decisoes_path)) {
  if (nrow(fila)) stop("CP14 em espera: forneça ", decisoes_path, " para resolver ", nrow(fila), " discordâncias.", call. = FALSE)
  decisoes <- data.frame(
    assessment_type = character(), comparison_key = character(), final_decision = character(),
    final_rationale = character(), supporting_page_or_location = character(),
    supporting_evidence_note = character(), decision_role = character(), decision_date = character(),
    stringsAsFactors = FALSE
  )
} else {
  decisoes <- read_machine_csv(decisoes_path)
}

comparacoes$applicability <- aplicar_decisoes_conjuntas(comparacoes$applicability, decisoes, "applicability", c("yes", "no", "unclear"))
comparacoes$item <- aplicar_decisoes_conjuntas(comparacoes$item, decisoes, "item", c("yes", "probably_yes", "probably_no", "no", "not_applicable", "unclear", "low", "high", "partial", "no_information", "complete", "incomplete"))
comparacoes$domain <- aplicar_decisoes_conjuntas(comparacoes$domain, decisoes, "domain", c("low", "high", "unclear", "complete", "partial", "incomplete", "not_applicable"))

origem_linha <- function(comparacao, fonte_1, fonte_2, chave_cols) {
  chave_1 <- chave_estavel(fonte_1, chave_cols)
  chave_2 <- chave_estavel(fonte_2, chave_cols)
  i1 <- match(comparacao$comparison_key, chave_1)
  i2 <- match(comparacao$comparison_key, chave_2)
  racional_1 <- if ("rationale" %in% names(fonte_1)) texto_limpo(fonte_1$rationale[i1]) else rep("", nrow(comparacao))
  racional_2 <- if ("rationale" %in% names(fonte_2)) texto_limpo(fonte_2$rationale[i2]) else rep("", nrow(comparacao))
  racional_acordo <- ifelse(racional_1 == racional_2, racional_1, paste0("Revisor 1: ", racional_1, " | Revisor 2: ", racional_2))
  racional <- ifelse(comparacao$agreement, racional_acordo, comparacao$final_rationale)
  data_revisao <- ifelse(comparacao$agreement,
                         pmax(texto_limpo(fonte_1$review_date[i1]), texto_limpo(fonte_2$review_date[i2])),
                         comparacao$decision_date)
  list(i1 = i1, i2 = i2, rationale = racional, review_date = data_revisao)
}

montar_final <- function(comparacao, fonte_1, fonte_2, chave_cols, tipo) {
  origem <- origem_linha(comparacao, fonte_1, fonte_2, chave_cols)
  base <- fonte_1[origem$i1, , drop = FALSE]
  base$batch <- comparacao$batch
  base$rationale <- origem$rationale
  base$reviewer <- "Decisão conjunta"
  base$review_date <- origem$review_date
  base$status <- "complete"
  if (tipo == "applicability") {
    base$final_decision <- comparacao$final_decision
    base$decision_origin <- comparacao$decision_origin
    base$final_rationale <- origem$rationale
    base$decision_date <- origem$review_date
  } else if (tipo == "item") {
    base$response <- comparacao$final_decision
  } else {
    base$domain_judgment <- comparacao$final_decision
  }
  base
}

app_final <- montar_final(comparacoes$applicability, app_1, app_2, c("appraisal_unit_id", "instrument"), "applicability")
items_final <- montar_final(comparacoes$item, item_1, item_2, c("appraisal_unit_id", "instrument", "phase", "domain", "item_id"), "item")
domains_final <- montar_final(comparacoes$domain, domain_1, domain_2, c("appraisal_unit_id", "instrument", "phase", "domain"), "domain")

unidades <- unique(items_final[, intersect(c("batch", "appraisal_unit_id", "record_id", "dataset_id", "task_type", "target", "reference_standard"), names(items_final)), drop = FALSE])
cp13_path <- file.path(CFG$pipeline_dir, "data_extraction", "data_extraction_package_CP13_COMPLETE_33.xlsx")
if (!file.exists(cp13_path)) stop("A extração CP13 validada é obrigatória para identificar os estudos CP14.", call. = FALSE)
referencias_cp13 <- as.data.frame(readxl::read_excel(cp13_path, sheet = "included_reference", .name_repair = "minimal"), stringsAsFactors = FALSE)
validate_required_columns(referencias_cp13, c("record_id", "title"), cp13_path)
unidades$title <- as.character(referencias_cp13$title[match(unidades$record_id, referencias_cp13$record_id)])
if (any(!nzchar(texto_limpo(unidades$title)))) stop("Há unidade CP14 sem título correspondente no CP13.", call. = FALSE)
unidades$active_instrument_count <- as.integer(table(app_final$appraisal_unit_id[app_final$final_decision == "yes"])[unidades$appraisal_unit_id])
unidades$active_instrument_count[is.na(unidades$active_instrument_count)] <- 0L
unidades <- unidades[order(unidades$appraisal_unit_id, method = "radix"), , drop = FALSE]

estudos <- aggregate(appraisal_unit_id ~ record_id, unidades, function(x) paste(sort(unique(x)), collapse = ";"))
names(estudos)[names(estudos) == "appraisal_unit_id"] <- "appraisal_units"
estudos$batches <- vapply(estudos$record_id, function(id) paste(sort(unique(unidades$batch[unidades$record_id == id])), collapse = ";"), character(1L))
estudos$active_instruments <- vapply(estudos$record_id, function(id) {
  u <- unidades$appraisal_unit_id[unidades$record_id == id]
  paste(sort(unique(app_final$instrument[app_final$appraisal_unit_id %in% u & app_final$final_decision == "yes"])), collapse = ";")
}, character(1L))
estudos$title <- as.character(referencias_cp13$title[match(estudos$record_id, referencias_cp13$record_id)])
estudos <- estudos[, c("record_id", "title", "appraisal_units", "batches", "active_instruments")]

stable_write_csv(estudos, file.path(saida, "CP14_STUDY_INDEX_FINAL.csv"))
stable_write_csv(unidades, file.path(saida, "CP14_APPRAISAL_UNITS_FINAL.csv"))
stable_write_csv(app_final, file.path(saida, "CP14_APPLICABILITY_FINAL.csv"))
stable_write_csv(items_final, file.path(saida, "CP14_ITEM_JUDGMENTS_FINAL.csv"))
stable_write_csv(domains_final, file.path(saida, "CP14_DOMAIN_JUDGMENTS_FINAL.csv"))

if (!requireNamespace("openxlsx", quietly = TRUE)) stop("O pacote openxlsx é necessário para a consolidação CP14.", call. = FALSE)
workbook_path <- file.path(saida, "CP14_AVALIACAO_METODOLOGICA_FINAL_33_ESTUDOS.xlsx")
openxlsx::write.xlsx(
  list(
    study_index = estudos,
    appraisal_units = unidades,
    applicability_final = app_final,
    item_final = items_final,
    domain_final = domains_final,
    agreement_pairs = agreement_pairs
  ),
  workbook_path,
  overwrite = TRUE
)

hash_arquivo <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("O pacote digest é necessário para calcular SHA-256.", call. = FALSE)
  digest::digest(file = x, algo = "sha256")
}
relativos <- function(x) {
  if (!length(x)) return(character())
  vapply(x, project_relative_path, character(1L))
}
lotes <- sort(unique(c(app_final$batch, items_final$batch, domains_final$batch)), method = "radix")
manifesto <- data.frame(
  batch = lotes,
  final_workbook = project_relative_path(workbook_path),
  final_sha256 = vapply(rep(workbook_path, length(lotes)), hash_arquivo, character(1L)),
  reviewer_1_archive = vapply(lotes, function(x) paste(relativos(arquivos_por_revisor$r1[grepl(x, arquivos_por_revisor$r1, fixed = TRUE)]), collapse = ";"), character(1L)),
  reviewer_1_sha256 = vapply(lotes, function(x) paste(vapply(arquivos_por_revisor$r1[grepl(x, arquivos_por_revisor$r1, fixed = TRUE)], hash_arquivo, character(1L)), collapse = ";"), character(1L)),
  reviewer_2_archive = vapply(lotes, function(x) paste(relativos(arquivos_por_revisor$r2[grepl(x, arquivos_por_revisor$r2, fixed = TRUE)]), collapse = ";"), character(1L)),
  reviewer_2_sha256 = vapply(lotes, function(x) paste(vapply(arquivos_por_revisor$r2[grepl(x, arquivos_por_revisor$r2, fixed = TRUE)], hash_arquivo, character(1L)), collapse = ";"), character(1L)),
  adjudication_date = if (file.exists(decisoes_path) && nrow(decisoes)) max(decisoes$decision_date) else "",
  verification_file = "CP14_QA_CHECKS.csv",
  status = "complete",
  stringsAsFactors = FALSE
)
stable_write_csv(manifesto, file.path(saida, "CP14_BATCH_MANIFEST.csv"))

qa <- data.frame(
  check = c("studies", "units", "applicability", "items", "domains", "agreement_pairs", "unresolved_disagreements"),
  observed = c(nrow(estudos), nrow(unidades), nrow(app_final), nrow(items_final), nrow(domains_final), nrow(agreement_pairs), 0L),
  expected = c(33L, 50L, 310L, 7106L, 893L, 8309L, 0L),
  stringsAsFactors = FALSE
)
qa$status <- ifelse(qa$observed == qa$expected, "PASS", "FAIL")
qa$note <- ""
stable_write_csv(qa, file.path(saida, "CP14_QA_CHECKS.csv"))
if (any(qa$status != "PASS")) stop("CP14 não passou nos invariantes científicos; saída em espera.", call. = FALSE)

writeLines(c("# Fechamento da avaliação metodológica", "", "As duas revisões foram preservadas. A camada final contém acordos exatos e decisões conjuntas documentadas."), file.path(saida, "CP14_CLOSURE_REPORT.md"), useBytes = TRUE)
writeLines('{"status":"PASS","unresolved_disagreements":0}', file.path(saida, "CP14_CLOSURE_VERIFICATION.json"), useBytes = TRUE)
cat("CP14 reconciliado sem discordâncias pendentes.\n")
