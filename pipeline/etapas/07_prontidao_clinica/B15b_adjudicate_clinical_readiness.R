#!/usr/bin/env Rscript

# Aplica as decisões conjuntas de prontidão clínica às discordâncias registradas.
# Toda decisão precisa corresponder à unidade e ao item que foram avaliados.


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

if (!requireNamespace("openxlsx", quietly = TRUE)) stop("O pacote openxlsx é necessário.", call. = FALSE)
if (!requireNamespace("digest", quietly = TRUE)) stop("O pacote digest é necessário.", call. = FALSE)

cp15 <- file.path(CFG$pipeline_dir, "clinical_readiness", "cp15")
staging <- file.path(cp15, "import_staging")
reconciliacao <- file.path(cp15, "reconciliation")
adjudicacao <- file.path(cp15, "adjudication")
final <- file.path(cp15, "final")
ensure_dir(adjudicacao)
ensure_dir(final)

rows_path <- file.path(staging, "CP15_RETURN_ROWS.csv")
discordance_path <- file.path(reconciliacao, "CP15_DISCORDANCE_MAP.csv")
decisions_path <- Sys.getenv(
  "AITHERMO_CP15_ADJUDICATION_FILE",
  unset = file.path(PROJECT_ROOT, "dados_humanos", "cp15", "decisoes_conjuntas.csv")
)
for (arquivo in c(rows_path, discordance_path, decisions_path)) {
  if (!file.exists(arquivo)) stop("Entrada CP15 ausente: ", arquivo, call. = FALSE)
}

rows <- read_machine_csv(rows_path)
discordance <- read_machine_csv(discordance_path)
decisions <- read_machine_csv(decisions_path)

required_decisions <- c(
  "model_task_id", "field", "joint_value", "adjudication_rationale",
  "decision_role", "decision_date"
)
validate_required_columns(decisions, required_decisions, decisions_path)
decisions <- as_character_frame(decisions[, required_decisions, drop = FALSE])
discordance$key <- paste(discordance$model_task_id, discordance$field, sep = "::")
decisions$key <- paste(decisions$model_task_id, decisions$field, sep = "::")
if (anyDuplicated(decisions$key)) stop("Há decisões CP15 duplicadas.", call. = FALSE)
faltantes <- setdiff(discordance$key, decisions$key)
extras <- setdiff(decisions$key, discordance$key)
if (length(faltantes) || length(extras)) {
  stop("A tabela conjunta CP15 não corresponde à fila congelada. Faltantes: ", length(faltantes), "; extras: ", length(extras), ".", call. = FALSE)
}
for (campo in c("joint_value", "adjudication_rationale", "decision_role", "decision_date")) {
  if (any(!nzchar(texto_limpo(decisions[[campo]])))) stop("Campo CP15 vazio: ", campo, call. = FALSE)
}
if (any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", decisions$decision_date))) stop("As datas CP15 devem usar AAAA-MM-DD.", call. = FALSE)
if (any(texto_limpo(decisions$decision_role) != "Decisão conjunta")) {
  stop("Todas as discordâncias CP15 precisam de decisão conjunta documentada.", call. = FALSE)
}

decisions <- merge(
  discordance[, c("lot_id", "model_task_id", "record_id", "field", "reviewer_1_value", "reviewer_2_value")],
  decisions[, required_decisions],
  by = c("model_task_id", "field"), all = FALSE, sort = FALSE
)
decisions <- decisions[order(decisions$lot_id, decisions$model_task_id, decisions$field, method = "radix"), , drop = FALSE]
data_decisao_final <- max(decisions$decision_date)
decision_log_path <- file.path(adjudicacao, "CP15_ADJUDICATION_LOG.csv")
stable_write_csv(decisions, decision_log_path)

categorical_fields <- c(
  "readiness_applicability", "readiness_level", "internal_validation_present",
  "external_validation_present", "temporal_validation_present",
  "multicentre_validation_present", "prospective_clinical_workflow_evaluation",
  "clinical_utility_demonstrated", "impact_on_decision_making_reported",
  "impact_on_patient_outcomes_reported", "workflow_or_cost_effectiveness_reported",
  "model_calibration_reported", "model_explainability_reported",
  "uncertainty_estimation_reported", "code_available", "data_available",
  "model_available", "fairness_or_subgroup_bias_evaluated",
  "regulatory_or_deployment_discussed", "thermography_protocol_replicable",
  "risk_of_bias_summary", "reporting_quality_summary"
)
narrative_fields <- c(
  "applicability_basis", "readiness_basis", "readiness_limitations",
  "supporting_quote_or_location", "reviewer_notes"
)
metadata_fields <- c("model_task_id", "study_id", "record_id", "title", "dataset_id", "task_type", "target")
validate_required_columns(rows, c("return_role", "return_lot_id", "review_date", metadata_fields, categorical_fields, narrative_fields), rows_path)

longest_text <- function(x) {
  x <- unique(texto_limpo(x))
  x <- x[nzchar(x)]
  if (!length(x)) return("")
  x[[which.max(nchar(x, type = "chars"))]]
}
union_text <- function(x) {
  x <- unique(texto_limpo(x))
  paste(x[nzchar(x)], collapse = " | ")
}
decision_value <- function(id, field) {
  x <- decisions$joint_value[decisions$model_task_id == id & decisions$field == field]
  if (length(x) != 1L) "" else x[[1L]]
}

r1 <- rows[rows$return_role == "Revisor 1", , drop = FALSE]
r2 <- rows[rows$return_role == "Revisor 2", , drop = FALSE]
ids <- sort(unique(rows$model_task_id), method = "radix")
if (!setequal(ids, r1$model_task_id) || !setequal(ids, r2$model_task_id)) stop("Os revisores CP15 não cobrem as mesmas unidades.", call. = FALSE)
if (anyDuplicated(r1$model_task_id) || anyDuplicated(r2$model_task_id)) stop("Há unidade CP15 duplicada em um dos revisores.", call. = FALSE)

final_rows <- vector("list", length(ids))
unit_logs <- vector("list", length(ids))
for (i in seq_along(ids)) {
  id <- ids[[i]]
  a <- r1[r1$model_task_id == id, , drop = FALSE]
  b <- r2[r2$model_task_id == id, , drop = FALSE]
  nomes <- c("lot_id", metadata_fields, "reviewer", "review_date", categorical_fields, narrative_fields, "completion_status", "consistency_flag")
  registro <- as.list(setNames(rep("", length(nomes)), nomes))
  registro$lot_id <- a$return_lot_id[[1L]]
  for (campo in metadata_fields) registro[[campo]] <- texto_limpo(a[[campo]][[1L]])
  registro$reviewer <- "Decisão conjunta"

  unit_decisions <- decisions[decisions$model_task_id == id, , drop = FALSE]
  registro$review_date <- data_decisao_final
  valor_app_1 <- texto_limpo(a$readiness_applicability[[1L]])
  valor_app_2 <- texto_limpo(b$readiness_applicability[[1L]])
  registro$readiness_applicability <- if (identical(valor_app_1, valor_app_2)) valor_app_1 else decision_value(id, "readiness_applicability")
  aplicavel <- identical(registro$readiness_applicability, "yes")
  for (campo in setdiff(categorical_fields, "readiness_applicability")) {
    valor_1 <- texto_limpo(a[[campo]][[1L]])
    valor_2 <- texto_limpo(b[[campo]][[1L]])
    decisao <- decision_value(id, campo)
    valor_revisor_aplicavel <- if (aplicavel && xor(valor_app_1 == "yes", valor_app_2 == "yes")) {
      if (valor_app_1 == "yes") valor_1 else valor_2
    } else {
      ""
    }
    registro[[campo]] <- if (identical(valor_1, valor_2)) {
      valor_1
    } else if (nzchar(decisao)) {
      decisao
    } else {
      valor_revisor_aplicavel
    }
  }

  if (!aplicavel) {
    for (campo in setdiff(categorical_fields, "readiness_applicability")) registro[[campo]] <- ""
    for (campo in setdiff(narrative_fields, "applicability_basis")) registro[[campo]] <- ""
  }
  bases_app <- c(
    if (valor_app_1 == registro$readiness_applicability) texto_limpo(a$applicability_basis) else "",
    if (valor_app_2 == registro$readiness_applicability) texto_limpo(b$applicability_basis) else ""
  )
  registro$applicability_basis <- longest_text(bases_app)
  if (aplicavel) {
    bases_nivel <- c(
      if (texto_limpo(a$readiness_level) == registro$readiness_level) texto_limpo(a$readiness_basis) else "",
      if (texto_limpo(b$readiness_level) == registro$readiness_level) texto_limpo(b$readiness_basis) else ""
    )
    registro$readiness_basis <- longest_text(bases_nivel)
    registro$readiness_limitations <- longest_text(c(a$readiness_limitations, b$readiness_limitations))
    registro$supporting_quote_or_location <- union_text(c(a$supporting_quote_or_location, b$supporting_quote_or_location))
  }
  registro$reviewer_notes <- if (!nrow(unit_decisions)) {
    "Concordância categórica integral entre os dois pareceres independentes. A redação conjunta preserva a justificativa mais específica e as localizações de evidência complementares."
  } else {
    resolucoes <- paste0(unit_decisions$field, "=", unit_decisions$joint_value)
    paste0(
      "Decisão conjunta após confronto com CP13/CP14. Resoluções: ",
      paste(resolucoes, collapse = "; "),
      ". As razões individualizadas constam no log de adjudicação."
    )
  }
  registro$completion_status <- "complete"
  registro$consistency_flag <- if (aplicavel) "OK" else "NOT_CLASSIFIED"
  final_rows[[i]] <- as.data.frame(registro, stringsAsFactors = FALSE)
  unit_logs[[i]] <- data.frame(
    lot_id = registro$lot_id, model_task_id = id, record_id = registro$record_id,
    reviewer_1_applicability = a$readiness_applicability,
    reviewer_2_applicability = b$readiness_applicability,
    joint_applicability = registro$readiness_applicability,
    reviewer_1_level = a$readiness_level, reviewer_2_level = b$readiness_level,
    joint_level = registro$readiness_level,
    adjudicated_field_count = nrow(unit_decisions),
    adjudicated_fields = paste(unit_decisions$field, collapse = "; "),
    final_status = registro$consistency_flag,
    stringsAsFactors = FALSE
  )
}

final_data <- do.call(rbind, final_rows)
unit_log <- do.call(rbind, unit_logs)
ordem_final <- c(
  "lot_id", "model_task_id", "study_id", "record_id", "title", "dataset_id", "task_type", "target",
  "reviewer", "review_date", "readiness_applicability", "applicability_basis",
  "readiness_level", "readiness_basis",
  setdiff(categorical_fields, c("readiness_applicability", "readiness_level")),
  "readiness_limitations", "supporting_quote_or_location", "reviewer_notes",
  "completion_status", "consistency_flag"
)
final_data <- final_data[, ordem_final, drop = FALSE]
final_data <- final_data[order(final_data$lot_id, final_data$model_task_id, method = "radix"), , drop = FALSE]
ternary <- setdiff(categorical_fields, c("readiness_applicability", "readiness_level", "risk_of_bias_summary", "reporting_quality_summary"))
applicable <- final_data$readiness_applicability == "yes"
unresolved <- sum(!nzchar(as.matrix(final_data[applicable, categorical_fields, drop = FALSE])))
checks <- data.frame(
  check = c("final_rows", "unique_units", "mapped_disagreements", "applicability_vocabulary", "ternary_vocabulary", "readiness_levels", "unresolved_values"),
  observed = c(nrow(final_data), length(unique(final_data$model_task_id)), nrow(decisions), paste(sort(unique(final_data$readiness_applicability)), collapse = ";"), "checked", paste(sort(unique(final_data$readiness_level[applicable])), collapse = ";"), unresolved),
  expected = c(length(ids), length(ids), nrow(discordance), "yes;no;unclear", "yes/no/unclear", "0-4", 0L),
  stringsAsFactors = FALSE
)
checks$status <- c(
  ifelse(nrow(final_data) == length(ids), "PASS", "FAIL"),
  ifelse(length(unique(final_data$model_task_id)) == length(ids), "PASS", "FAIL"),
  ifelse(nrow(decisions) == nrow(discordance), "PASS", "FAIL"),
  ifelse(all(final_data$readiness_applicability %in% c("yes", "no", "unclear")), "PASS", "FAIL"),
  ifelse(all(unlist(final_data[applicable, ternary, drop = FALSE]) %in% c("yes", "no", "unclear")), "PASS", "FAIL"),
  ifelse(all(final_data$readiness_level[applicable] %in% as.character(0:4)), "PASS", "FAIL"),
  ifelse(unresolved == 0L, "PASS", "FAIL")
)
checks$note <- ""

final_csv <- file.path(final, "CP15_PRONTIDAO_CLINICA_DECISAO_CONJUNTA.csv")
unit_path <- file.path(adjudicacao, "CP15_UNIT_ADJUDICATION_SUMMARY.csv")
verification_path <- file.path(adjudicacao, "CP15_ADJUDICATION_VERIFICATION.csv")
distribution_path <- file.path(final, "CP15_READINESS_LEVEL_DISTRIBUTION.csv")
stable_write_csv(final_data, final_csv)
stable_write_csv(unit_log, unit_path)
stable_write_csv(checks, verification_path)
distribution <- as.data.frame(table(
  readiness_applicability = final_data$readiness_applicability,
  readiness_level = ifelse(nzchar(final_data$readiness_level), final_data$readiness_level, "not_classified")
), stringsAsFactors = FALSE)
stable_write_csv(distribution[distribution$Freq > 0L, , drop = FALSE], distribution_path)
if (any(checks$status != "PASS")) stop("A adjudicação CP15 falhou nos controles internos.", call. = FALSE)

agreement_path <- file.path(reconciliacao, "CP15_AGREEMENT_SUMMARY.csv")
manifest_path <- file.path(cp15, "returns", "CP15_RETURN_MANIFEST.csv")
if (!file.exists(agreement_path) || !file.exists(manifest_path)) {
  stop("Os resumos de concordância e proveniência dos revisores CP15 são obrigatórios.", call. = FALSE)
}
agreement <- read_machine_csv(agreement_path)
manifest <- read_machine_csv(manifest_path)

regras <- data.frame(
  section = c("governança", "acordo", "discordância", "não aplicável", "preservação"),
  rule = c(
    "Os dois pareceres independentes permanecem imutáveis.",
    "Valores categóricos idênticos são transportados sem recodificação.",
    "Cada discordância exige correspondência exata com uma decisão conjunta documentada.",
    "Campos científicos posteriores à aplicabilidade permanecem vazios quando a unidade não é classificável.",
    "A camada final é derivada e não sobrescreve os arquivos dos revisores."
  ),
  stringsAsFactors = FALSE
)
writeLines(
  c("# Regras de adjudicação da prontidão clínica", "", paste0("- ", regras$rule)),
  file.path(adjudicacao, "CP15_ADJUDICATION_RULES.md"),
  useBytes = TRUE
)

fontes <- c(
  CP13 = file.path(CFG$pipeline_dir, "data_extraction", "data_extraction_package_CP13_COMPLETE_33.xlsx"),
  CP14 = file.path(CFG$pipeline_dir, "methodological_appraisal", "cp14", "final", "CP14_AVALIACAO_METODOLOGICA_FINAL_33_ESTUDOS.xlsx")
)
if (any(!file.exists(fontes))) stop("As fontes aprovadas CP13 e CP14 são obrigatórias para fechar CP15.", call. = FALSE)
source_manifest <- data.frame(
  source = names(fontes),
  file = project_relative_path(unname(fontes)),
  sha256 = vapply(unname(fontes), digest::digest, character(1L), file = TRUE, algo = "sha256"),
  role = c("extração aprovada", "avaliação metodológica aprovada"),
  status = "verificada",
  stringsAsFactors = FALSE
)

distribution_clean <- distribution[distribution$Freq > 0L, , drop = FALSE]
resumo <- data.frame(
  indicador = c("relatos incluídos", "unidades modelo–tarefa", "unidades classificáveis", "unidades não classificáveis", "discordâncias adjudicadas"),
  valor = c(length(unique(final_data$record_id)), nrow(final_data), sum(applicable), sum(!applicable), nrow(decisions)),
  stringsAsFactors = FALSE
)
workbook_path <- file.path(final, "CP15_PRONTIDAO_CLINICA_FINAL_33_ESTUDOS.xlsx")
openxlsx::write.xlsx(
  list(
    summary = resumo,
    final_readiness = final_data,
    level_distribution = distribution_clean,
    adjudication_log = decisions,
    unit_adjudication = unit_log,
    agreement_summary = agreement,
    return_manifest = manifest,
    adjudication_rules = regras,
    source_manifest = source_manifest
  ),
  workbook_path,
  overwrite = TRUE
)
stable_write_csv(source_manifest, file.path(final, "CP15_SOURCE_MANIFEST.csv"))
cat("CP15 reconciliado: ", nrow(decisions), " campos adjudicados em ", sum(unit_log$adjudicated_field_count > 0L), " unidades.\n", sep = "")
