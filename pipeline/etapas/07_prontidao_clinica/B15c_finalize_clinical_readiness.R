#!/usr/bin/env Rscript

# Confere e consolida a camada final de prontidão clínica.
# Divergências de estrutura, contagem ou origem interrompem a finalização.

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
final_dir <- file.path(cp15, "final")
arquivos <- c(
  csv = file.path(final_dir, "CP15_PRONTIDAO_CLINICA_DECISAO_CONJUNTA.csv"),
  workbook = file.path(final_dir, "CP15_PRONTIDAO_CLINICA_FINAL_33_ESTUDOS.xlsx"),
  distribution = file.path(final_dir, "CP15_READINESS_LEVEL_DISTRIBUTION.csv"),
  source_manifest = file.path(final_dir, "CP15_SOURCE_MANIFEST.csv"),
  reviewer_manifest = file.path(cp15, "returns", "CP15_RETURN_MANIFEST.csv"),
  discordance = file.path(cp15, "reconciliation", "CP15_DISCORDANCE_MAP.csv"),
  adjudication = file.path(cp15, "adjudication", "CP15_ADJUDICATION_LOG.csv")
)
faltantes <- arquivos[!file.exists(arquivos)]
if (length(faltantes)) stop("Arquivos CP15 ausentes: ", paste(project_relative_path(faltantes), collapse = ", "), call. = FALSE)

dados <- read_machine_csv(arquivos[["csv"]])
manifesto_fontes <- read_machine_csv(arquivos[["source_manifest"]])
manifesto_revisores <- read_machine_csv(arquivos[["reviewer_manifest"]])
discordancias <- read_machine_csv(arquivos[["discordance"]])
adjudicacao <- read_machine_csv(arquivos[["adjudication"]])

campos_ternarios <- c(
  "internal_validation_present", "external_validation_present", "temporal_validation_present",
  "multicentre_validation_present", "prospective_clinical_workflow_evaluation",
  "clinical_utility_demonstrated", "impact_on_decision_making_reported",
  "impact_on_patient_outcomes_reported", "workflow_or_cost_effectiveness_reported",
  "model_calibration_reported", "model_explainability_reported", "uncertainty_estimation_reported",
  "code_available", "data_available", "model_available", "fairness_or_subgroup_bias_evaluated",
  "regulatory_or_deployment_discussed", "thermography_protocol_replicable"
)
campos_obrigatorios <- c(
  "model_task_id", "record_id", "reviewer", "readiness_applicability", "readiness_level",
  "completion_status", "consistency_flag", campos_ternarios
)
validate_required_columns(dados, campos_obrigatorios, arquivos[["csv"]])

aplicavel <- dados$readiness_applicability == "yes"
niveis <- suppressWarnings(as.integer(dados$readiness_level))
distribuicao <- vapply(0:4, function(n) sum(aplicavel & niveis == n, na.rm = TRUE), integer(1L))
ternarios <- unlist(dados[aplicavel, campos_ternarios, drop = FALSE], use.names = FALSE)

checks <- data.frame(
  check = c(
    "final_rows", "unique_units", "included_studies", "applicable_units",
    "not_classified_units", "readiness_distribution", "stage_3_4",
    "ternary_cells", "ternary_vocabulary", "completion_status",
    "decision_role", "discordance_mapping"
  ),
  observed = c(
    nrow(dados), length(unique(dados$model_task_id)), length(unique(dados$record_id)),
    sum(aplicavel), sum(!aplicavel), paste(distribuicao, collapse = ";"),
    sum(distribuicao[4:5]), length(ternarios), paste(sort(unique(ternarios)), collapse = ";"),
    paste(sort(unique(dados$completion_status)), collapse = ";"),
    paste(sort(unique(dados$reviewer)), collapse = ";"), nrow(adjudicacao)
  ),
  expected = c("50", "50", "33", "38", "12", "11;24;3;0;0", "0", "684", "no;unclear;yes", "complete", "Decisão conjunta", nrow(discordancias)),
  stringsAsFactors = FALSE
)
checks$status <- c(
  nrow(dados) == 50L,
  length(unique(dados$model_task_id)) == 50L && !anyDuplicated(dados$model_task_id),
  length(unique(dados$record_id)) == 33L,
  sum(aplicavel) == 38L,
  sum(!aplicavel) == 12L,
  identical(unname(distribuicao), c(11L, 24L, 3L, 0L, 0L)),
  sum(distribuicao[4:5]) == 0L,
  length(ternarios) == 684L,
  all(ternarios %in% c("yes", "no", "unclear")),
  all(dados$completion_status == "complete"),
  identical(unique(dados$reviewer), "Decisão conjunta"),
  nrow(adjudicacao) == nrow(discordancias) && setequal(
    paste(adjudicacao$model_task_id, adjudicacao$field, sep = "::"),
    paste(discordancias$model_task_id, discordancias$field, sep = "::")
  )
)
checks$status <- ifelse(checks$status, "PASS", "FAIL")

abas_esperadas <- c(
  "summary", "final_readiness", "level_distribution", "adjudication_log",
  "unit_adjudication", "agreement_summary", "return_manifest",
  "adjudication_rules", "source_manifest"
)
abas <- readxl::excel_sheets(arquivos[["workbook"]])
wb_final <- suppressMessages(readxl::read_excel(arquivos[["workbook"]], sheet = "final_readiness", .name_repair = "minimal"))
checks <- rbind(
  checks,
  data.frame(
    check = c("workbook_sheets", "workbook_units"),
    observed = c(paste(abas, collapse = ";"), nrow(wb_final)),
    expected = c(paste(abas_esperadas, collapse = ";"), "50"),
    status = c(if (identical(abas, abas_esperadas)) "PASS" else "FAIL", if (nrow(wb_final) == 50L && setequal(wb_final$model_task_id, dados$model_task_id)) "PASS" else "FAIL"),
    stringsAsFactors = FALSE
  )
)

validate_required_columns(manifesto_fontes, c("file", "sha256"), arquivos[["source_manifest"]])
fontes <- file.path(PROJECT_ROOT, manifesto_fontes$file)
fontes_ok <- all(file.exists(fontes)) && identical(
  unname(vapply(fontes, digest::digest, character(1L), file = TRUE, algo = "sha256")),
  unname(as.character(manifesto_fontes$sha256))
)
checks <- rbind(checks, data.frame(check = "source_hashes", observed = if (fontes_ok) "verified" else "mismatch", expected = "verified", status = if (fontes_ok) "PASS" else "FAIL", stringsAsFactors = FALSE))

validate_required_columns(manifesto_revisores, c("archived_file", "archived_sha256"), arquivos[["reviewer_manifest"]])
revisores <- file.path(PROJECT_ROOT, manifesto_revisores$archived_file)
revisores_ok <- all(file.exists(revisores)) && identical(
  unname(vapply(revisores, digest::digest, character(1L), file = TRUE, algo = "sha256")),
  unname(as.character(manifesto_revisores$archived_sha256))
)
checks <- rbind(checks, data.frame(check = "reviewer_hashes", observed = if (revisores_ok) "verified" else "mismatch", expected = "verified", status = if (revisores_ok) "PASS" else "FAIL", stringsAsFactors = FALSE))

qa_path <- file.path(final_dir, "CP15_FINAL_QA.csv")
stable_write_csv(checks, qa_path)
if (any(checks$status != "PASS")) stop("O fechamento CP15 falhou. Consulte ", qa_path, call. = FALSE)

writeLines(
  c(
    "# Fechamento da prontidão clínica", "",
    "Os dois pareceres independentes foram preservados e conferidos por SHA-256.",
    "As discordâncias foram resolvidas exclusivamente pela tabela de decisões conjuntas.",
    "A distribuição final é: 12 unidades não classificáveis; 11 no nível 0; 24 no nível 1; 3 no nível 2; nenhuma nos níveis 3 ou 4."
  ),
  file.path(final_dir, "CP15_CLOSURE_REPORT.md"),
  useBytes = TRUE
)
cat("CP15 validado: 50 unidades, 33 estudos e todos os controles aprovados.\n")
