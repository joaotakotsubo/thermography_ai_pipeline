#!/usr/bin/env Rscript

# Importa as avaliações independentes de elegibilidade dos textos completos.
# Confere a estrutura e a identificação dos registros antes da comparação.


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

eligibility_columns <- c(
  "record_id", "title_duplicate_group_id", "title", "authors", "year",
  "journal_or_source", "doi", "pmid", "url", "publication_type",
  "candidate_flags", "source_databases", "reason_for_full_text",
  "reviewer_1_decisions", "reviewer_2_decisions", "agreement_statuses",
  "pre_adjudicated_decisions", "pre_adjudication_reasons",
  "pre_adjudicator_notes", "decision_rule", "pdf_filename", "pdf_path",
  "full_text_decision", "primary_exclusion_reason",
  "secondary_exclusion_reason", "decision_notes", "reviewer", "decision_date"
)
unavailable_columns <- c(
  "record_id", "title", "authors", "year", "journal_or_source", "doi",
  "url", "retrieval_status", "online_search_status", "author_contact_status",
  "retrieval_note", "reviewer_1_decisions", "reviewer_2_decisions",
  "pre_adjudicated_decisions", "decision_rule"
)
decision_values <- c("include_for_extraction", "exclude_full_text", "unclear_needs_discussion")
reason_values <- c(
  "", "no_ai_model", "no_thermal_input", "no_pain_target", "not_human",
  "not_original", "abstract_only", "registry_no_results",
  "no_extractable_model_data", "wrong_population", "wrong_outcome",
  "duplicate", "language_unavailable", "other"
)

input_paths <- c(
  reviewer_1 = Sys.getenv("AITHERMO_FULL_TEXT_R1", unset = file.path(PROJECT_ROOT, "dados_humanos", "cp11", "revisor_1.xlsx")),
  reviewer_2 = Sys.getenv("AITHERMO_FULL_TEXT_R2", unset = file.path(PROJECT_ROOT, "dados_humanos", "cp11", "revisor_2.xlsx"))
)
conformance_path <- Sys.getenv(
  "AITHERMO_CP11C_CONFORMANCE_FILE",
  unset = file.path(PROJECT_ROOT, "dados_humanos", "cp11", "ajustes_conformidade.csv")
)
if (any(!file.exists(input_paths))) stop("Planilha de texto completo ausente: ", paste(input_paths[!file.exists(input_paths)], collapse = "; "), call. = FALSE)

normalizar_data <- function(x) {
  x <- texto_limpo(x)
  numerico <- suppressWarnings(as.numeric(x))
  usar_excel <- nzchar(x) & !is.na(numerico) & !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
  x[usar_excel] <- format(as.Date(numerico[usar_excel], origin = "1899-12-30"), "%Y-%m-%d")
  x
}

ler_pacote <- function(path) {
  abas <- readxl::excel_sheets(path)
  if (!all(c("eligibility", "unavailable_reference") %in% abas)) stop("Abas obrigatórias ausentes em ", path, call. = FALSE)
  eligibility <- as.data.frame(readxl::read_excel(path, sheet = "eligibility", col_types = "text"), stringsAsFactors = FALSE, check.names = FALSE)
  unavailable <- as.data.frame(readxl::read_excel(path, sheet = "unavailable_reference", col_types = "text"), stringsAsFactors = FALSE, check.names = FALSE)
  validate_required_columns(eligibility, eligibility_columns, path)
  validate_required_columns(unavailable, unavailable_columns, path)
  list(
    eligibility = as_character_frame(eligibility[, eligibility_columns, drop = FALSE]),
    unavailable = as_character_frame(unavailable[, unavailable_columns, drop = FALSE])
  )
}

if (file.exists(conformance_path)) {
  conformance <- read_machine_csv(conformance_path)
  conformance_columns <- c(
    "record_id", "reviewer_id", "action", "reason", "authorization_id",
    "decision_date", "retrieval_status", "online_search_status", "author_contact_status"
  )
  validate_required_columns(conformance, conformance_columns, conformance_path)
  conformance <- as_character_frame(conformance[, conformance_columns, drop = FALSE])
  if (anyDuplicated(paste(conformance$reviewer_id, conformance$record_id))) stop("Há ajuste de conformidade duplicado.", call. = FALSE)
  if (any(conformance$action != "move_to_unavailable")) stop("Ação de conformidade não reconhecida.", call. = FALSE)
  if (any(!nzchar(conformance$reason) | !nzchar(conformance$authorization_id))) stop("Todo ajuste requer razão e autorização.", call. = FALSE)
} else {
  conformance <- data.frame(
    record_id = character(), reviewer_id = character(), action = character(),
    reason = character(), authorization_id = character(), decision_date = character(),
    retrieval_status = character(), online_search_status = character(),
    author_contact_status = character(), stringsAsFactors = FALSE
  )
}

saida_root <- file.path(CFG$pipeline_dir, "full_text", "eligibility")
reviewer_root <- file.path(saida_root, "reviewer_inputs")
conformance_dir <- file.path(saida_root, "conformance")
ensure_dir(reviewer_root)
ensure_dir(conformance_dir)
audit_rows <- list()
manifest_rows <- list()

for (indice in seq_along(input_paths)) {
  reviewer_id <- names(input_paths)[[indice]]
  reviewer_label <- if (reviewer_id == "reviewer_1") "Revisor 1" else "Revisor 2"
  pacote <- ler_pacote(input_paths[[indice]])
  eligibility <- pacote$eligibility
  unavailable <- pacote$unavailable
  eligibility$reviewer <- reviewer_label
  eligibility$decision_date <- normalizar_data(eligibility$decision_date)

  regras <- conformance[conformance$reviewer_id %in% c(reviewer_id, "both"), , drop = FALSE]
  for (j in seq_len(nrow(regras))) {
    regra <- regras[j, , drop = FALSE]
    posicao <- which(eligibility$record_id == regra$record_id)
    if (length(posicao) != 1L) stop("O ajuste de conformidade não encontrou exatamente uma linha: ", regra$record_id, call. = FALSE)
    linha <- eligibility[posicao, , drop = FALSE]
    nova <- data.frame(
      record_id = linha$record_id, title = linha$title, authors = linha$authors,
      year = linha$year, journal_or_source = linha$journal_or_source,
      doi = linha$doi, url = linha$url,
      retrieval_status = regra$retrieval_status,
      online_search_status = regra$online_search_status,
      author_contact_status = regra$author_contact_status,
      retrieval_note = paste(regra$reason, "Autorização:", regra$authorization_id),
      reviewer_1_decisions = linha$reviewer_1_decisions,
      reviewer_2_decisions = linha$reviewer_2_decisions,
      pre_adjudicated_decisions = linha$pre_adjudicated_decisions,
      decision_rule = linha$decision_rule,
      stringsAsFactors = FALSE
    )
    unavailable <- unavailable[unavailable$record_id != regra$record_id, , drop = FALSE]
    unavailable <- rbind(unavailable, nova[, unavailable_columns, drop = FALSE])
    eligibility <- eligibility[-posicao, , drop = FALSE]
    audit_rows[[length(audit_rows) + 1L]] <- data.frame(
      reviewer_id = reviewer_id, record_id = regra$record_id,
      action = regra$action, reason = regra$reason,
      authorization_id = regra$authorization_id,
      decision_date = regra$decision_date,
      stringsAsFactors = FALSE
    )
  }

  if (anyDuplicated(eligibility$record_id) || anyDuplicated(unavailable$record_id)) stop("Há record_id duplicado no retorno normalizado.", call. = FALSE)
  if (length(intersect(eligibility$record_id, unavailable$record_id))) stop("Um registro aparece simultaneamente como avaliado e indisponível.", call. = FALSE)
  if (any(!eligibility$full_text_decision %in% decision_values)) stop("Decisão de texto completo fora do codebook.", call. = FALSE)
  if (any(!eligibility$primary_exclusion_reason %in% reason_values) || any(!eligibility$secondary_exclusion_reason %in% reason_values)) stop("Motivo de exclusão fora do codebook.", call. = FALSE)
  if (any(eligibility$full_text_decision == "exclude_full_text" & !nzchar(eligibility$primary_exclusion_reason))) stop("Exclusão sem motivo primário.", call. = FALSE)
  if (any(eligibility$full_text_decision == "include_for_extraction" & (nzchar(eligibility$primary_exclusion_reason) | nzchar(eligibility$secondary_exclusion_reason)))) stop("Inclusão com motivo de exclusão.", call. = FALSE)
  if (any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", eligibility$decision_date))) stop("Data de decisão inválida.", call. = FALSE)

  destino <- file.path(reviewer_root, reviewer_id)
  ensure_dir(destino)
  csv_name <- paste0("full_text_eligibility_", reviewer_id, ".csv")
  xlsx_name <- paste0("full_text_eligibility_", reviewer_id, ".xlsx")
  csv_path <- file.path(destino, csv_name)
  unavailable_path <- file.path(destino, paste0("full_text_eligibility_", reviewer_id, "_unavailable_reference.csv"))
  stable_write_csv(eligibility[order(eligibility$record_id), ], csv_path, eligibility_columns)
  stable_write_csv(unavailable[order(unavailable$record_id), ], unavailable_path, unavailable_columns)
  require_openxlsx()
  openxlsx::write.xlsx(list(eligibility = eligibility, unavailable_reference = unavailable), file.path(destino, xlsx_name), overwrite = TRUE)
  manifest_rows[[indice]] <- data.frame(
    reviewer_id = reviewer_id, reviewer_label = reviewer_label,
    source_workbook = input_paths[[indice]], normalized_csv = project_relative_path(csv_path),
    eligibility_records = nrow(eligibility), unavailable_reference_records = nrow(unavailable),
    full_text_stage_total = nrow(eligibility) + nrow(unavailable), stringsAsFactors = FALSE
  )
}

manifest <- do.call(rbind, manifest_rows)
if (length(unique(manifest$full_text_stage_total)) != 1L) stop("Os revisores têm totais diferentes no estágio de texto completo.", call. = FALSE)
if (CFG$strict && unique(manifest$full_text_stage_total) != 42L) stop("O total protegido de textos completos deve ser 42.", call. = FALSE)
audit <- if (length(audit_rows)) do.call(rbind, audit_rows) else data.frame(
  reviewer_id = character(), record_id = character(), action = character(),
  reason = character(), authorization_id = character(), decision_date = character(),
  stringsAsFactors = FALSE
)
stable_write_csv(manifest, file.path(conformance_dir, "full_text_eligibility_conformance_reviewer_manifest.csv"))
stable_write_csv(audit, file.path(conformance_dir, "full_text_eligibility_conformance_audit.csv"))
write_checkpoint(
  id = "CP11C", name = "import_full_text_eligibility_returns",
  what_ran = "Importação independente dos dois pareceres e aplicação exclusiva de ajustes operacionais autorizados externamente.",
  numbers = c(reviewer_packages = nrow(manifest), full_text_stage_total = unique(manifest$full_text_stage_total), conformance_actions = nrow(audit)),
  review_items = c("Confirmar a imutabilidade dos arquivos de origem.", "Confirmar cada autorização de conformidade."),
  outputs = c(file.path(conformance_dir, "full_text_eligibility_conformance_reviewer_manifest.csv"), file.path(conformance_dir, "full_text_eligibility_conformance_audit.csv")),
  gate_question = "A importação independente e os ajustes autorizados podem seguir para reconciliação?"
)
cat("Retornos de texto completo importados sem decisões científicas embutidas.\n")
