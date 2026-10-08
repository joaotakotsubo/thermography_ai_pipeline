#!/usr/bin/env Rscript

# Importa as avaliações independentes de prontidão clínica e registra as discordâncias.
# Os arquivos recebidos são preservados e conferidos por SHA-256.

arquivo_ativo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(arquivo_ativo)) stop("Execute este arquivo com Rscript.", call. = FALSE)
p <- dirname(normalizePath(arquivo_ativo[[1L]], mustWork = TRUE))
repeat {
  if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break
  anterior <- dirname(p)
  if (identical(anterior, p)) stop("Bootstrap não localizado.", call. = FALSE)
  p <- anterior
}
source(file.path(p, "pipeline", "_bootstrap.R"))

if (!requireNamespace("readxl", quietly = TRUE)) stop("O pacote readxl é necessário.", call. = FALSE)
if (!requireNamespace("digest", quietly = TRUE)) stop("O pacote digest é necessário.", call. = FALSE)

cp15 <- file.path(CFG$pipeline_dir, "clinical_readiness", "cp15")
staging <- file.path(cp15, "import_staging")
returns <- file.path(cp15, "returns")
reconciliation <- file.path(cp15, "reconciliation")
ensure_dir(staging)
ensure_dir(returns)
ensure_dir(reconciliation)

pastas <- c(
  `Revisor 1` = Sys.getenv("AITHERMO_CP15_REVIEWER_1_DIR", unset = file.path(PROJECT_ROOT, "dados_humanos", "cp15", "reviewer_1")),
  `Revisor 2` = Sys.getenv("AITHERMO_CP15_REVIEWER_2_DIR", unset = file.path(PROJECT_ROOT, "dados_humanos", "cp15", "reviewer_2"))
)
if (any(!dir.exists(pastas))) stop("As duas pastas independentes CP15 precisam ser informadas.", call. = FALSE)

campos_categoricos <- c(
  "readiness_applicability", "readiness_level", "internal_validation_present",
  "external_validation_present", "temporal_validation_present", "multicentre_validation_present",
  "prospective_clinical_workflow_evaluation", "clinical_utility_demonstrated",
  "impact_on_decision_making_reported", "impact_on_patient_outcomes_reported",
  "workflow_or_cost_effectiveness_reported", "model_calibration_reported",
  "model_explainability_reported", "uncertainty_estimation_reported", "code_available",
  "data_available", "model_available", "fairness_or_subgroup_bias_evaluated",
  "regulatory_or_deployment_discussed", "thermography_protocol_replicable",
  "risk_of_bias_summary", "reporting_quality_summary"
)
campos_identidade <- c("model_task_id", "study_id", "record_id", "title", "dataset_id", "task_type", "target")
campos_narrativos <- c("review_date", "applicability_basis", "readiness_basis", "readiness_limitations", "supporting_quote_or_location", "reviewer_notes")
campos_obrigatorios <- c("lot_id", campos_identidade, "reviewer", campos_categoricos, campos_narrativos, "completion_status", "consistency_flag")

linhas <- list()
auditoria <- list()
manifesto <- list()
for (papel in names(pastas)) {
  arquivos <- sort(enc2utf8(list.files(pastas[[papel]], pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)), method = "radix")
  if (length(arquivos) != 6L) stop(papel, " deve conter exatamente seis planilhas CP15.", call. = FALSE)
  for (arquivo in arquivos) {
    abas <- readxl::excel_sheets(arquivo)
    if (!"readiness_form" %in% abas) stop("Aba readiness_form ausente em ", arquivo, call. = FALSE)
    dados <- suppressMessages(readxl::read_excel(arquivo, sheet = "readiness_form", .name_repair = "minimal"))
    dados <- as_character_frame(as.data.frame(dados, stringsAsFactors = FALSE))
    validate_required_columns(dados, campos_obrigatorios, arquivo)
    dados <- dados[, campos_obrigatorios, drop = FALSE]
    if (!nrow(dados) || any(!nzchar(texto_limpo(dados$model_task_id)))) stop("Devolutiva CP15 vazia ou sem identificador.", call. = FALSE)
    if (any(texto_limpo(dados$reviewer) != papel)) stop("O papel declarado não corresponde à pasta em ", arquivo, call. = FALSE)
    hash <- digest::digest(arquivo, file = TRUE, algo = "sha256")
    lote <- unique(texto_limpo(dados$lot_id))
    if (length(lote) != 1L || !nzchar(lote)) stop("Cada planilha CP15 deve conter um único lot_id.", call. = FALSE)
    destino_dir <- file.path(returns, if (papel == "Revisor 1") "reviewer_1" else "reviewer_2", lote)
    ensure_dir(destino_dir)
    destino <- file.path(destino_dir, basename(arquivo))
    if (!isTRUE(file.copy(arquivo, destino, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE))) stop("Falha ao preservar devolutiva CP15.", call. = FALSE)
    hash_destino <- digest::digest(destino, file = TRUE, algo = "sha256")
    if (!identical(hash, hash_destino)) stop("SHA-256 divergente na cópia CP15.", call. = FALSE)
    dados$return_role <- papel
    dados$return_lot_id <- lote
    dados$source_file <- normalizePath(arquivo)
    dados$source_sha256 <- hash
    linhas[[length(linhas) + 1L]] <- dados
    auditoria[[length(auditoria) + 1L]] <- data.frame(
      lot_id = lote, reviewer = papel, source_file = normalizePath(arquivo),
      source_sha256 = hash, row_count = nrow(dados), complete_rows = sum(dados$completion_status == "complete"),
      status = if (all(dados$completion_status == "complete") && !anyDuplicated(dados$model_task_id)) "PASS" else "FAIL",
      stringsAsFactors = FALSE
    )
    manifesto[[length(manifesto) + 1L]] <- data.frame(
      lot_id = lote, reviewer = papel, original_file = normalizePath(arquivo), original_sha256 = hash,
      archived_file = project_relative_path(destino), archived_sha256 = hash_destino,
      archived_mode = "preserved", structural_status = "PASS", scientific_internal_consistency = "not_reassessed",
      stringsAsFactors = FALSE
    )
  }
}

rows <- do.call(rbind, linhas)
audit <- do.call(rbind, auditoria)
manifest <- do.call(rbind, manifesto)
rows <- rows[order(rows$return_role, rows$return_lot_id, rows$model_task_id, method = "radix"), , drop = FALSE]
if (nrow(rows) != 100L || nrow(audit) != 12L || any(audit$status != "PASS")) stop("As devolutivas CP15 devem formar 100 linhas completas em 12 arquivos.", call. = FALSE)

r1 <- rows[rows$return_role == "Revisor 1", , drop = FALSE]
r2 <- rows[rows$return_role == "Revisor 2", , drop = FALSE]
if (nrow(r1) != 50L || nrow(r2) != 50L || anyDuplicated(r1$model_task_id) || anyDuplicated(r2$model_task_id) || !setequal(r1$model_task_id, r2$model_task_id)) {
  stop("Cada revisor CP15 deve cobrir exatamente as mesmas 50 unidades.", call. = FALSE)
}
paired <- merge(r1, r2, by = "model_task_id", suffixes = c("_r1", "_r2"), all = TRUE, sort = FALSE)
for (campo in setdiff(campos_identidade, "model_task_id")) {
  if (!identical(texto_limpo(paired[[paste0(campo, "_r1")]]), texto_limpo(paired[[paste0(campo, "_r2")]]))) {
    stop("Identidade divergente entre revisores CP15: ", campo, call. = FALSE)
  }
}

comparacoes <- list()
for (i in seq_len(nrow(paired))) {
  ambos_aplicaveis <- texto_limpo(paired$readiness_applicability_r1[[i]]) == "yes" && texto_limpo(paired$readiness_applicability_r2[[i]]) == "yes"
  for (campo in campos_categoricos) {
    comparar <- campo == "readiness_applicability" || ambos_aplicaveis
    v1 <- texto_limpo(paired[[paste0(campo, "_r1")]][[i]])
    v2 <- texto_limpo(paired[[paste0(campo, "_r2")]][[i]])
    comparacoes[[length(comparacoes) + 1L]] <- data.frame(
      lot_id = paired$lot_id_r1[[i]], model_task_id = paired$model_task_id[[i]], record_id = paired$record_id_r1[[i]],
      field = campo, compared = if (comparar) "yes" else "no", reviewer_1_value = v1, reviewer_2_value = v2,
      agreement = if (!comparar) "not_compared" else if (identical(v1, v2)) "yes" else "no",
      stringsAsFactors = FALSE
    )
  }
}
field_agreement <- do.call(rbind, comparacoes)
discordance <- field_agreement[field_agreement$compared == "yes" & field_agreement$agreement == "no", , drop = FALSE]

kappa_simples <- function(a, b) {
  a <- texto_limpo(a); b <- texto_limpo(b)
  manter <- nzchar(a) & nzchar(b); a <- a[manter]; b <- b[manter]
  if (!length(a)) return(NA_real_)
  categorias <- sort(unique(c(a, b)), method = "radix")
  observado <- mean(a == b)
  esperado <- sum(vapply(categorias, function(x) mean(a == x) * mean(b == x), numeric(1L)))
  if (isTRUE(all.equal(esperado, 1))) return(if (isTRUE(all.equal(observado, 1))) 1 else NA_real_)
  (observado - esperado) / (1 - esperado)
}
summary_rows <- lapply(campos_categoricos, function(campo) {
  x <- field_agreement[field_agreement$field == campo & field_agreement$compared == "yes", , drop = FALSE]
  data.frame(
    field = campo, compared_pairs = nrow(x), exact_agreements = sum(x$agreement == "yes"),
    disagreements = sum(x$agreement == "no"), percent_agreement = if (nrow(x)) mean(x$agreement == "yes") else NA_real_,
    cohen_kappa = kappa_simples(x$reviewer_1_value, x$reviewer_2_value), stringsAsFactors = FALSE
  )
})
agreement_summary <- do.call(rbind, summary_rows)

unit_summary <- do.call(rbind, lapply(sort(unique(field_agreement$model_task_id), method = "radix"), function(id) {
  x <- discordance[discordance$model_task_id == id, , drop = FALSE]
  y <- field_agreement[field_agreement$model_task_id == id, , drop = FALSE]
  data.frame(
    lot_id = unique(y$lot_id)[[1L]], model_task_id = id, record_id = unique(y$record_id)[[1L]],
    compared_field_count = sum(y$compared == "yes"), categorical_disagreement_count = nrow(x),
    discordant_fields = paste(x$field, collapse = "; "), adjudication_required = if (nrow(x)) "yes" else "no",
    stringsAsFactors = FALSE
  )
}))

stable_write_csv(rows, file.path(staging, "CP15_RETURN_ROWS.csv"))
stable_write_csv(audit, file.path(staging, "CP15_RETURN_AUDIT.csv"))
stable_write_csv(manifest, file.path(returns, "CP15_RETURN_MANIFEST.csv"))
stable_write_csv(field_agreement, file.path(reconciliation, "CP15_FIELD_AGREEMENT_PRE_ADJUDICATION.csv"))
stable_write_csv(discordance, file.path(reconciliation, "CP15_DISCORDANCE_MAP.csv"))
stable_write_csv(unit_summary, file.path(reconciliation, "CP15_UNIT_RECONCILIATION.csv"))
stable_write_csv(agreement_summary, file.path(reconciliation, "CP15_AGREEMENT_SUMMARY.csv"))
cat("Devolutivas CP15 importadas: 100 linhas; discordâncias congeladas: ", nrow(discordance), ".\n", sep = "")
