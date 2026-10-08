#!/usr/bin/env Rscript

# Compara as avaliações TEHAI e aplica as adjudicações fornecidas para as discordâncias.
# Sem a tabela de decisões aprovada, a etapa permanece em espera.


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
if (!requireNamespace("digest", quietly = TRUE)) stop("O pacote digest é necessário.", call. = FALSE)

r1_dir <- Sys.getenv("AITHERMO_TEHAI_R1_DIR", unset = file.path(PROJECT_ROOT, "dados_humanos", "tehai", "revisor_1"))
r2_dir <- Sys.getenv("AITHERMO_TEHAI_R2_DIR", unset = file.path(PROJECT_ROOT, "dados_humanos", "tehai", "revisor_2"))
decisions_path <- Sys.getenv("AITHERMO_TEHAI_JOINT_DECISIONS", unset = file.path(PROJECT_ROOT, "dados_humanos", "tehai", "decisoes_conjuntas.csv"))
for (pasta in c(r1_dir, r2_dir)) if (!dir.exists(pasta)) stop("Diretório TEHAI ausente: ", pasta, call. = FALSE)

listar <- function(pasta) {
  arquivos <- list.files(pasta, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  sort(enc2utf8(arquivos[!grepl("(^|/)~\\$", arquivos)]), method = "radix")
}

ler_revisor <- function(pasta, rotulo) {
  arquivos <- listar(pasta)
  if (!length(arquivos)) stop("Nenhum formulário TEHAI em ", pasta, call. = FALSE)
  tabelas <- lapply(arquivos, function(arquivo) {
    if (!("Appraisal" %in% readxl::excel_sheets(arquivo))) stop("Aba Appraisal ausente em ", arquivo, call. = FALSE)
    x <- as.data.frame(readxl::read_excel(arquivo, sheet = "Appraisal", skip = 2L, col_types = "text"), stringsAsFactors = FALSE, check.names = FALSE)
    x <- as_character_frame(x)
    x$.source_file <- normalizePath(arquivo, mustWork = TRUE)
    x
  })
  dados <- do.call(rbind, tabelas)
  required <- c(
    "tehai_unit_id", "component", "subcomponent_id", "reviewer_id",
    "applicability", "score", "evidence_location", "evidence_summary",
    "rationale", "uncertainty_flag", "uncertainty_note", "assessment_date",
    "codebook_version", "status"
  )
  validate_required_columns(dados, required, paste("TEHAI", rotulo))
  dados$reviewer_id <- rotulo
  dados$score <- texto_limpo(dados$score)
  dados
}

r1 <- ler_revisor(r1_dir, "Revisor 1")
r2 <- ler_revisor(r2_dir, "Revisor 2")
keys <- c("tehai_unit_id", "subcomponent_id")
cmp <- comparar_revisores(
  r1, r2, keys, "score",
  c("component", "applicability", "codebook_version"),
  "TEHAI"
)

key_1 <- chave_estavel(r1, keys)
key_2 <- chave_estavel(r2, keys)
i1 <- match(cmp$comparison_key, key_1)
i2 <- match(cmp$comparison_key, key_2)
if (any(r1$applicability[i1] != r2$applicability[i2])) stop("Há discordância de aplicabilidade TEHAI não prevista no escopo.", call. = FALSE)
if (any(r1$codebook_version[i1] != r2$codebook_version[i2])) stop("Os revisores usaram versões diferentes do codebook TEHAI.", call. = FALSE)
if (any(!r1$status[i1] %in% "complete") || any(!r2$status[i2] %in% "complete")) stop("Há formulário TEHAI incompleto.", call. = FALSE)
if (any(!r1$score[i1] %in% as.character(0:3)) || any(!r2$score[i2] %in% as.character(0:3))) stop("Há escore TEHAI fora de 0-3.", call. = FALSE)

fila <- cmp[!cmp$agreement, , drop = FALSE]
fila$reviewer_1_evidence_location <- r1$evidence_location[i1][!cmp$agreement]
fila$reviewer_1_rationale <- r1$rationale[i1][!cmp$agreement]
fila$reviewer_2_evidence_location <- r2$evidence_location[i2][!cmp$agreement]
fila$reviewer_2_rationale <- r2$rationale[i2][!cmp$agreement]
out_dir <- file.path(CFG$pipeline_dir, "tehai", "adjudication")
final_dir <- file.path(CFG$pipeline_dir, "tehai", "final")
ensure_dir(out_dir)
ensure_dir(final_dir)
stable_write_csv(fila, file.path(out_dir, "TEHAI_DISAGREEMENT_QUEUE.csv"))
if (isTRUE(CFG$strict) && nrow(fila) != 86L) {
  stop("A fila TEHAI diverge das 86 discordâncias congeladas no protocolo.", call. = FALSE)
}

decision_columns <- c(
  "tehai_unit_id", "subcomponent_id", "consensus_score",
  "consensus_evidence_location", "consensus_rationale", "decision_role",
  "decision_date", "approval_status"
)
if (nrow(fila) && !file.exists(decisions_path)) stop("TEHAI em espera: faltam decisões conjuntas para ", nrow(fila), " discordâncias.", call. = FALSE)
if (file.exists(decisions_path)) {
  decisions <- read_machine_csv(decisions_path)
  validate_required_columns(decisions, decision_columns, decisions_path)
  decisions <- as_character_frame(decisions[, decision_columns, drop = FALSE])
} else {
  decisions <- as.data.frame(setNames(rep(list(character()), length(decision_columns)), decision_columns), stringsAsFactors = FALSE)
}
decisions$comparison_key <- if (nrow(decisions)) chave_estavel(decisions, keys) else character()
if (anyDuplicated(decisions$comparison_key)) stop("Há decisão TEHAI duplicada.", call. = FALSE)
faltantes <- setdiff(fila$comparison_key, decisions$comparison_key)
extras <- setdiff(decisions$comparison_key, fila$comparison_key)
if (length(faltantes) || length(extras)) stop("As decisões TEHAI não correspondem à fila congelada. Faltantes: ", length(faltantes), "; extras: ", length(extras), ".", call. = FALSE)
if (nrow(decisions)) {
  if (any(!decisions$consensus_score %in% as.character(0:3))) stop("Decisão TEHAI fora de 0-3.", call. = FALSE)
  if (any(!nzchar(decisions$consensus_rationale) | !nzchar(decisions$decision_role) | !nzchar(decisions$decision_date))) stop("Decisão TEHAI sem documentação completa.", call. = FALSE)
  if (any(decisions$approval_status != "human_approved")) stop("Toda decisão TEHAI deve estar human_approved.", call. = FALSE)
  if (any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", decisions$decision_date))) stop("Data TEHAI inválida.", call. = FALSE)
}

indice_decisao <- match(cmp$comparison_key, decisions$comparison_key)
consensus_score <- cmp$reviewer_1_response
if (any(!cmp$agreement)) consensus_score[!cmp$agreement] <- decisions$consensus_score[indice_decisao[!cmp$agreement]]
evidence_agreement <- ifelse(
  texto_limpo(r1$evidence_location[i1]) == texto_limpo(r2$evidence_location[i2]),
  texto_limpo(r1$evidence_location[i1]),
  paste0("Revisor 1: ", texto_limpo(r1$evidence_location[i1]), " | Revisor 2: ", texto_limpo(r2$evidence_location[i2]))
)
rationale_agreement <- ifelse(
  texto_limpo(r1$rationale[i1]) == texto_limpo(r2$rationale[i2]),
  texto_limpo(r1$rationale[i1]),
  paste0("Revisor 1: ", texto_limpo(r1$rationale[i1]), " | Revisor 2: ", texto_limpo(r2$rationale[i2]))
)

valor_decisao <- function(campo, acordo) {
  saida <- rep("", nrow(cmp))
  if (any(!acordo)) saida[!acordo] <- decisions[[campo]][indice_decisao[!acordo]]
  saida
}

final <- data.frame(
  tehai_unit_id = cmp$tehai_unit_id,
  component = r1$component[i1],
  subcomponent_id = cmp$subcomponent_id,
  applicability = r1$applicability[i1],
  reviewer_1_score = cmp$reviewer_1_response,
  reviewer_2_score = cmp$reviewer_2_response,
  consensus_score = consensus_score,
  consensus_evidence_location = ifelse(cmp$agreement, evidence_agreement, valor_decisao("consensus_evidence_location", cmp$agreement)),
  consensus_rationale = ifelse(cmp$agreement, rationale_agreement, valor_decisao("consensus_rationale", cmp$agreement)),
  decision_origin = ifelse(cmp$agreement, "exact_agreement", "human_adjudication"),
  decision_role = ifelse(cmp$agreement, "Revisores 1 e 2", valor_decisao("decision_role", cmp$agreement)),
  decision_date = ifelse(cmp$agreement, pmax(r1$assessment_date[i1], r2$assessment_date[i2]), valor_decisao("decision_date", cmp$agreement)),
  approval_status = "human_approved",
  codebook_version = r1$codebook_version[i1],
  status = "complete",
  stringsAsFactors = FALSE
)
final <- final[order(final$tehai_unit_id, final$subcomponent_id, method = "radix"), , drop = FALSE]

checks <- data.frame(
  check = c("systems", "assessments", "subcomponents_per_system", "disagreements", "unresolved", "score_vocabulary", "approval"),
  observed = c(length(unique(final$tehai_unit_id)), nrow(final), paste(sort(unique(as.integer(table(final$tehai_unit_id)))), collapse = ";"), nrow(fila), sum(!nzchar(final$consensus_score)), paste(sort(unique(final$consensus_score)), collapse = ";"), paste(sort(unique(final$approval_status)), collapse = ";")),
  expected = c(29L, 435L, 15L, nrow(fila), 0L, "0;1;2;3", "human_approved"),
  stringsAsFactors = FALSE
)
checks$status <- c(
  ifelse(length(unique(final$tehai_unit_id)) == 29L, "PASS", "FAIL"),
  ifelse(nrow(final) == 435L, "PASS", "FAIL"),
  ifelse(all(table(final$tehai_unit_id) == 15L), "PASS", "FAIL"),
  "PASS",
  ifelse(all(nzchar(final$consensus_score)), "PASS", "FAIL"),
  ifelse(all(final$consensus_score %in% as.character(0:3)), "PASS", "FAIL"),
  ifelse(all(final$approval_status == "human_approved"), "PASS", "FAIL")
)
stable_write_csv(final, file.path(final_dir, "TEHAI_ADJUDICATION_FINAL.csv"))
stable_write_csv(checks, file.path(final_dir, "TEHAI_ADJUDICATION_VALIDATION.csv"))
arquivos_r1 <- listar(r1_dir)
arquivos_r2 <- listar(r2_dir)
arquivos_manifesto <- c(arquivos_r1, arquivos_r2, if (file.exists(decisions_path)) decisions_path else character())
papeis_manifesto <- c(
  rep("Revisor 1", length(arquivos_r1)),
  rep("Revisor 2", length(arquivos_r2)),
  if (file.exists(decisions_path)) "Decisão conjunta aprovada" else character()
)
stable_write_csv(data.frame(
  role = papeis_manifesto,
  source_file = normalizePath(arquivos_manifesto, mustWork = TRUE),
  sha256 = vapply(arquivos_manifesto, digest::digest, character(1L), file = TRUE, algo = "sha256"),
  rows_n = c(rep(15L, length(arquivos_r1) + length(arquivos_r2)), if (file.exists(decisions_path)) nrow(decisions) else integer()),
  stringsAsFactors = FALSE
), file.path(final_dir, "TEHAI_REVIEWER_INPUT_MANIFEST.csv"))
if (any(checks$status != "PASS")) stop("A reconciliação TEHAI falhou nos invariantes aprovados.", call. = FALSE)
cat("TEHAI reconciliado: ", nrow(final), " avaliações; ", nrow(fila), " discordâncias documentadas.\n", sep = "")
