#!/usr/bin/env Rscript

# Aplica as decisões finais de elegibilidade fornecidas pelos revisores.
# Consolida os estudos incluídos sem substituir os julgamentos humanos.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP12")

CONSENSUS_ADJUDICATOR <- "Decisão conjunta"

FULL_TEXT_ELIGIBILITY_COLUMNS <- c(
  "record_id",
  "title_duplicate_group_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "pmid",
  "url",
  "publication_type",
  "candidate_flags",
  "source_databases",
  "reason_for_full_text",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "agreement_statuses",
  "pre_adjudicated_decisions",
  "pre_adjudication_reasons",
  "pre_adjudicator_notes",
  "decision_rule",
  "pdf_filename",
  "pdf_path",
  "full_text_decision",
  "primary_exclusion_reason",
  "secondary_exclusion_reason",
  "decision_notes",
  "reviewer",
  "decision_date"
)

FULL_TEXT_UNAVAILABLE_COLUMNS <- c(
  "record_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "url",
  "retrieval_status",
  "online_search_status",
  "author_contact_status",
  "retrieval_note",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "pre_adjudicated_decisions",
  "decision_rule"
)

FULL_TEXT_RECONCILIATION_COLUMNS <- c(
  "record_id",
  "title_duplicate_group_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "pmid",
  "url",
  "publication_type",
  "candidate_flags",
  "source_databases",
  "reason_for_full_text",
  "pdf_filename",
  "pdf_path",
  "reviewer_1_full_text_decision",
  "reviewer_1_primary_exclusion_reason",
  "reviewer_1_secondary_exclusion_reason",
  "reviewer_1_decision_notes",
  "reviewer_1_decision_date",
  "reviewer_2_full_text_decision",
  "reviewer_2_primary_exclusion_reason",
  "reviewer_2_secondary_exclusion_reason",
  "reviewer_2_decision_notes",
  "reviewer_2_decision_date",
  "decision_agreement_status",
  "exclusion_reason_agreement_status",
  "adjudication_required",
  "adjudication_priority",
  "adjudication_trigger",
  "preliminary_final_full_text_decision",
  "preliminary_primary_exclusion_reason",
  "preliminary_secondary_exclusion_reason",
  "preliminary_decision_rule",
  "next_action"
)

FINAL_AUDIT_COLUMNS <- c(
  "audit_id",
  "record_id",
  "title",
  "cp12_adjudication_required",
  "cp12_adjudication_priority",
  "cp12_preliminary_decision",
  "final_full_text_decision",
  "final_primary_exclusion_reason",
  "final_secondary_exclusion_reason",
  "final_decision_status",
  "adjudication_basis",
  "decision_notes",
  "adjudicator",
  "adjudication_date",
  "script"
)

FINAL_VALIDATION_COLUMNS <- c(
  "severity",
  "issue_type",
  "record_id",
  "field",
  "value",
  "details"
)

FINAL_COUNT_COLUMNS <- c("metric", "value", "note")

FINAL_SOURCE_COLUMNS <- c(
  "source_id",
  "source_type",
  "input_path",
  "cached_consensus_path",
  "input_md5",
  "cached_consensus_md5",
  "sheets",
  "standardization"
)

full_text_final_decision_values <- function() {
  c("include_for_extraction", "exclude_full_text")
}

full_text_exclusion_reason_values <- function() {
  c(
    "no_ai_model",
    "no_thermal_input",
    "no_pain_target",
    "not_human",
    "not_original",
    "abstract_only",
    "registry_no_results",
    "no_extractable_model_data",
    "wrong_population",
    "wrong_outcome",
    "duplicate",
    "language_unavailable",
    "other"
  )
}

read_required_sheet <- function(path, sheet, columns) {
  data <- read_input_xlsx_sheet(path, sheet, columns)
  data <- as_character_frame(data[, columns, drop = FALSE])
  data <- data[nzchar(trimws(data$record_id)), , drop = FALSE]
  data$record_id <- trimws(data$record_id)
  data
}

read_summary_sheet <- function(path) {
  data <- read_input_xlsx_sheet(path, "summary", FINAL_COUNT_COLUMNS)
  data <- as_character_frame(data[, FINAL_COUNT_COLUMNS, drop = FALSE])
  data <- data[nzchar(trimws(data$metric)), , drop = FALSE]
  data$metric <- trimws(data$metric)
  data
}

require_readxl <- function() {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "Package 'readxl' is required by CP12B to import Excel-authored ",
      "adjudication workbooks.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

input_xlsx_sheets <- function(path) {
  require_readxl()
  suppressWarnings(readxl::excel_sheets(path))
}

read_input_xlsx_sheet <- function(path, sheet, columns) {
  require_readxl()
  sheets <- input_xlsx_sheets(path)
  if (!(sheet %in% sheets)) {
    stop("Workbook is missing required sheet '", sheet, "': ", project_relative_path(path), call. = FALSE)
  }
  data <- as.data.frame(
    suppressWarnings(readxl::read_excel(path, sheet = sheet, col_types = "text")),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (missing_col in setdiff(columns, names(data))) {
    data[[missing_col]] <- ""
  }
  as_character_frame(data[, columns, drop = FALSE])
}

validation_issue <- function(severity, issue_type, record_id, field, value, details) {
  data.frame(
    severity = severity,
    issue_type = issue_type,
    record_id = record_id,
    field = field,
    value = value,
    details = details,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, FINAL_VALIDATION_COLUMNS, drop = FALSE]
}

bind_validation_issues <- function(rows) {
  rows <- rows[vapply(rows, function(x) !is.null(x) && nrow(x) > 0L, logical(1L))]
  if (length(rows) == 0L) {
    return(empty_named_frame(FINAL_VALIDATION_COLUMNS))
  }
  do.call(rbind, rows)[, FINAL_VALIDATION_COLUMNS, drop = FALSE]
}

duplicate_record_issues <- function(data, label) {
  duplicate_ids <- unique(data$record_id[duplicated(data$record_id)])
  if (length(duplicate_ids) == 0L) {
    return(empty_named_frame(FINAL_VALIDATION_COLUMNS))
  }
  do.call(rbind, lapply(duplicate_ids, function(record_id) {
    validation_issue(
      "BLOCKER",
      paste0("duplicate_record_id_in_", label),
      record_id,
      "record_id",
      record_id,
      paste0("Each ", label, " record_id must appear once.")
    )
  }))
}

set_mismatch_issues <- function(observed, expected, label) {
  missing_ids <- setdiff(expected, observed)
  extra_ids <- setdiff(observed, expected)
  rows <- list()
  if (length(missing_ids) > 0L) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(missing_ids, function(record_id) {
      validation_issue("BLOCKER", paste0("missing_", label, "_record"), record_id, "record_id", record_id, "Expected record_id is absent from the adjudicated workbook.")
    }))
  }
  if (length(extra_ids) > 0L) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(extra_ids, function(record_id) {
      validation_issue("BLOCKER", paste0("unexpected_", label, "_record"), record_id, "record_id", record_id, "Record_id is not present in the CP12 reference set.")
    }))
  }
  bind_validation_issues(rows)
}

eligibility_validation_issues <- function(eligibility, reconciliation) {
  allowed_decisions <- full_text_final_decision_values()
  allowed_reasons <- c(full_text_exclusion_reason_values(), "")
  rec_by_id <- reconciliation[match(eligibility$record_id, reconciliation$record_id), , drop = FALSE]

  rows <- list(duplicate_record_issues(eligibility, "eligibility"))

  invalid_decision <- !(eligibility$full_text_decision %in% allowed_decisions)
  if (any(invalid_decision)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(invalid_decision), function(i) {
      validation_issue("BLOCKER", "invalid_full_text_decision", eligibility$record_id[[i]], "full_text_decision", eligibility$full_text_decision[[i]], "Final adjudication must be include_for_extraction or exclude_full_text.")
    }))
  }

  invalid_primary <- !(eligibility$primary_exclusion_reason %in% allowed_reasons)
  if (any(invalid_primary)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(invalid_primary), function(i) {
      validation_issue("BLOCKER", "invalid_primary_exclusion_reason", eligibility$record_id[[i]], "primary_exclusion_reason", eligibility$primary_exclusion_reason[[i]], "Primary exclusion reason must use the full-text codebook.")
    }))
  }

  invalid_secondary <- !(eligibility$secondary_exclusion_reason %in% allowed_reasons)
  if (any(invalid_secondary)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(invalid_secondary), function(i) {
      validation_issue("BLOCKER", "invalid_secondary_exclusion_reason", eligibility$record_id[[i]], "secondary_exclusion_reason", eligibility$secondary_exclusion_reason[[i]], "Secondary exclusion reason must use the full-text codebook or remain blank.")
    }))
  }

  include_with_reason <- eligibility$full_text_decision == "include_for_extraction" &
    (nzchar(eligibility$primary_exclusion_reason) | nzchar(eligibility$secondary_exclusion_reason))
  if (any(include_with_reason)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(include_with_reason), function(i) {
      validation_issue("BLOCKER", "include_has_exclusion_reason", eligibility$record_id[[i]], "primary_exclusion_reason/secondary_exclusion_reason", paste(eligibility$primary_exclusion_reason[[i]], eligibility$secondary_exclusion_reason[[i]], sep = ";"), "Included records must not carry exclusion reason codes.")
    }))
  }

  exclude_without_reason <- eligibility$full_text_decision == "exclude_full_text" &
    !nzchar(eligibility$primary_exclusion_reason)
  if (any(exclude_without_reason)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(exclude_without_reason), function(i) {
      validation_issue("BLOCKER", "exclude_missing_primary_reason", eligibility$record_id[[i]], "primary_exclusion_reason", "", "Excluded full texts require a primary exclusion reason.")
    }))
  }

  missing_notes <- !nzchar(eligibility$decision_notes)
  if (any(missing_notes)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(missing_notes), function(i) {
      validation_issue("BLOCKER", "missing_decision_notes", eligibility$record_id[[i]], "decision_notes", "", "Final eligibility decisions require a decision note for auditability.")
    }))
  }

  missing_reviewer <- !nzchar(eligibility$reviewer)
  if (any(missing_reviewer)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(missing_reviewer), function(i) {
      validation_issue("BLOCKER", "missing_consensus_reviewer_label", eligibility$record_id[[i]], "reviewer", "", "Final eligibility decisions require the consensus reviewer label.")
    }))
  }

  bad_dates <- !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", eligibility$decision_date)
  if (any(bad_dates)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(bad_dates), function(i) {
      validation_issue("BLOCKER", "invalid_decision_date", eligibility$record_id[[i]], "decision_date", eligibility$decision_date[[i]], "Decision date must be ISO text YYYY-MM-DD.")
    }))
  }

  changed_auto_decision <- rec_by_id$adjudication_required == "no" &
    nzchar(rec_by_id$preliminary_final_full_text_decision) &
    rec_by_id$preliminary_final_full_text_decision != eligibility$full_text_decision
  if (any(changed_auto_decision, na.rm = TRUE)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(changed_auto_decision), function(i) {
      validation_issue("BLOCKER", "auto_resolved_decision_changed", eligibility$record_id[[i]], "full_text_decision", eligibility$full_text_decision[[i]], "Rows auto-resolved by CP12 reviewer agreement must preserve the CP12 preliminary final decision.")
    }))
  }

  bind_validation_issues(rows)
}

audit_status <- function(rec_row, final_decision) {
  if (identical(rec_row$adjudication_required, "yes")) {
    if (identical(final_decision, "include_for_extraction")) {
      return("human_consensus_resolved_include")
    }
    return("human_consensus_resolved_exclude")
  }
  "reviewer_concordant"
}

audit_basis <- function(rec_row) {
  if (identical(rec_row$adjudication_required, "yes")) {
    return("resolved_after_cp12_adjudication_required")
  }
  "inherited_from_reviewer_agreement"
}

build_final_audit <- function(eligibility, reconciliation) {
  rec_by_id <- reconciliation[match(eligibility$record_id, reconciliation$record_id), , drop = FALSE]
  audit <- data.frame(
    audit_id = sprintf("CP12B_AUDIT_%05d", seq_len(nrow(eligibility))),
    record_id = eligibility$record_id,
    title = eligibility$title,
    cp12_adjudication_required = rec_by_id$adjudication_required,
    cp12_adjudication_priority = rec_by_id$adjudication_priority,
    cp12_preliminary_decision = rec_by_id$preliminary_final_full_text_decision,
    final_full_text_decision = eligibility$full_text_decision,
    final_primary_exclusion_reason = eligibility$primary_exclusion_reason,
    final_secondary_exclusion_reason = eligibility$secondary_exclusion_reason,
    final_decision_status = mapply(audit_status, split(rec_by_id, seq_len(nrow(rec_by_id))), eligibility$full_text_decision, USE.NAMES = FALSE),
    adjudication_basis = vapply(split(rec_by_id, seq_len(nrow(rec_by_id))), audit_basis, character(1L)),
    decision_notes = eligibility$decision_notes,
    adjudicator = eligibility$reviewer,
    adjudication_date = eligibility$decision_date,
    script = "pipeline/scripts/C12b_import_full_text_adjudication_final.R",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  audit[, FINAL_AUDIT_COLUMNS, drop = FALSE]
}

write_final_adjudication_xlsx <- function(eligibility, extraction_queue, exclusions, unavailable, audit, validation, counts, source_manifest, path) {
  write_review_xlsx(
    list(
      final_eligibility_decisions = eligibility,
      extraction_queue = extraction_queue,
      full_text_exclusions = exclusions,
      unavailable_reference = unavailable,
      adjudication_audit = audit,
      validation_issues = validation,
      counts = counts,
      source_manifest = source_manifest
    ),
    path
  )
  invisible(path)
}

eligibility_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text", "eligibility"))
reconciliation_dir <- file.path(eligibility_dir, "reconciliation")
final_dir <- ensure_dir(file.path(eligibility_dir, "final_adjudication"))
source_dir <- ensure_dir(file.path(final_dir, "source"))

env_path <- Sys.getenv("AITHERMO_FULL_TEXT_ADJUDICATED_FINAL_XLSX", unset = "")
cached_consensus_path <- file.path(source_dir, "full_text_eligibility_adjudicated_final_consensus_input.xlsx")
input_path <- if (nzchar(env_path)) {
  env_path
} else {
  cached_consensus_path
}

if (!file.exists(input_path)) {
  stop(
    "CP12B input workbook is missing. Provide AITHERMO_FULL_TEXT_ADJUDICATED_FINAL_XLSX ",
    "or restore the cached workbook at: ", project_relative_path(cached_consensus_path),
    call. = FALSE
  )
}

reconciliation_path <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation.csv")
unavailable_reference_path <- file.path(reconciliation_dir, "full_text_eligibility_unavailable_reference_reconciled.csv")
if (!file.exists(reconciliation_path) || !file.exists(unavailable_reference_path)) {
  stop("CP12B requires CP12 reconciliation outputs before importing final adjudication.", call. = FALSE)
}

reconciliation <- read_machine_csv(reconciliation_path)
validate_required_columns(reconciliation, FULL_TEXT_RECONCILIATION_COLUMNS, reconciliation_path)
reconciliation <- as_character_frame(reconciliation[, FULL_TEXT_RECONCILIATION_COLUMNS, drop = FALSE])
reconciliation$record_id <- trimws(reconciliation$record_id)

unavailable_reference <- read_machine_csv(unavailable_reference_path)
validate_required_columns(unavailable_reference, FULL_TEXT_UNAVAILABLE_COLUMNS, unavailable_reference_path)
unavailable_reference <- as_character_frame(unavailable_reference[, FULL_TEXT_UNAVAILABLE_COLUMNS, drop = FALSE])
unavailable_reference$record_id <- trimws(unavailable_reference$record_id)

eligibility <- read_required_sheet(input_path, "eligibility", FULL_TEXT_ELIGIBILITY_COLUMNS)
unavailable <- read_required_sheet(input_path, "unavailable_reference", FULL_TEXT_UNAVAILABLE_COLUMNS)
summary_input <- read_summary_sheet(input_path)

eligibility$reviewer <- CONSENSUS_ADJUDICATOR
summary_input$value[summary_input$metric == "adjudicator"] <- CONSENSUS_ADJUDICATOR

eligibility <- eligibility[order(eligibility$record_id, method = "radix"), , drop = FALSE]
unavailable <- unavailable[order(unavailable$record_id, method = "radix"), , drop = FALSE]
reconciliation <- reconciliation[order(reconciliation$record_id, method = "radix"), , drop = FALSE]
unavailable_reference <- unavailable_reference[order(unavailable_reference$record_id, method = "radix"), , drop = FALSE]

validation_issues <- bind_validation_issues(list(
  duplicate_record_issues(reconciliation, "cp12_reconciliation"),
  duplicate_record_issues(unavailable_reference, "cp12_unavailable_reference"),
  duplicate_record_issues(unavailable, "final_unavailable_reference"),
  set_mismatch_issues(eligibility$record_id, reconciliation$record_id, "eligibility"),
  set_mismatch_issues(unavailable$record_id, unavailable_reference$record_id, "unavailable_reference"),
  eligibility_validation_issues(eligibility, reconciliation)
))

audit <- build_final_audit(eligibility, reconciliation)

extraction_queue <- eligibility[eligibility$full_text_decision == "include_for_extraction", , drop = FALSE]
exclusions <- eligibility[eligibility$full_text_decision == "exclude_full_text", , drop = FALSE]

counts_values <- list(
  full_text_stage_total = nrow(eligibility) + nrow(unavailable),
  final_eligibility_records = nrow(eligibility),
  unavailable_reference_records = nrow(unavailable),
  include_for_extraction = nrow(extraction_queue),
  exclude_full_text = nrow(exclusions),
  adjudication_required_records_resolved = sum(reconciliation$adjudication_required == "yes"),
  auto_resolved_records_preserved = sum(reconciliation$adjudication_required == "no"),
  exclusion_no_ai_model = sum(exclusions$primary_exclusion_reason == "no_ai_model"),
  exclusion_wrong_outcome = sum(exclusions$primary_exclusion_reason == "wrong_outcome"),
  validation_blockers = sum(validation_issues$severity == "BLOCKER"),
  validation_warnings = sum(validation_issues$severity == "WARNING")
)

counts <- data.frame(
  metric = names(counts_values),
  value = as.character(unlist(counts_values, use.names = FALSE)),
  note = c(
    "Final full-text stage denominator: adjudicable records plus unavailable references.",
    "Full texts with final eligibility decisions.",
    "Full-text-stage records not scientifically excluded because the full text was unavailable.",
    "Records proceeding to data extraction.",
    "Records excluded after full-text eligibility assessment.",
    "CP12 rows requiring adjudication that now have final decisions.",
    "CP12 reviewer-agreement rows preserved in final adjudication.",
    "Full-text exclusions whose primary reason is no eligible AI/computational model.",
    "Full-text exclusions whose primary reason is wrong outcome or modeled construct.",
    "Blocking validation issues in CP12B.",
    "Non-blocking validation warnings in CP12B."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, FINAL_COUNT_COLUMNS, drop = FALSE]

source_manifest <- data.frame(
  source_id = "CP12B_SOURCE_00001",
  source_type = ifelse(normalizePath(input_path, mustWork = FALSE) == normalizePath(cached_consensus_path, mustWork = FALSE), "cached_consensus_workbook", "external_adjudicated_workbook"),
  input_path = project_relative_path(input_path),
  cached_consensus_path = project_relative_path(cached_consensus_path),
  input_md5 = file_md5(input_path),
  cached_consensus_md5 = "",
  sheets = paste(input_xlsx_sheets(input_path), collapse = ";"),
  standardization = "Final adjudicator fields standardized to Decisão conjunta.",
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, FINAL_SOURCE_COLUMNS, drop = FALSE]

final_decisions_csv <- file.path(final_dir, "full_text_eligibility_final_decisions.csv")
final_decisions_xlsx <- file.path(final_dir, "full_text_eligibility_final_decisions.xlsx")
extraction_queue_csv <- file.path(final_dir, "full_text_eligibility_extraction_queue.csv")
extraction_queue_xlsx <- file.path(final_dir, "full_text_eligibility_extraction_queue.xlsx")
exclusions_csv <- file.path(final_dir, "full_text_eligibility_full_text_exclusions.csv")
exclusions_xlsx <- file.path(final_dir, "full_text_eligibility_full_text_exclusions.xlsx")
unavailable_csv <- file.path(final_dir, "full_text_eligibility_unavailable_reference_final.csv")
unavailable_xlsx <- file.path(final_dir, "full_text_eligibility_unavailable_reference_final.xlsx")
audit_csv <- file.path(final_dir, "full_text_eligibility_adjudication_final_audit.csv")
audit_xlsx <- file.path(final_dir, "full_text_eligibility_adjudication_final_audit.xlsx")
validation_csv <- file.path(final_dir, "full_text_eligibility_adjudication_final_validation_issues.csv")
validation_xlsx <- file.path(final_dir, "full_text_eligibility_adjudication_final_validation_issues.xlsx")
counts_csv <- file.path(final_dir, "full_text_eligibility_adjudication_final_counts.csv")
counts_xlsx <- file.path(final_dir, "full_text_eligibility_adjudication_final_counts.xlsx")
source_manifest_csv <- file.path(final_dir, "full_text_eligibility_adjudication_final_source_manifest.csv")
source_manifest_xlsx <- file.path(final_dir, "full_text_eligibility_adjudication_final_source_manifest.xlsx")
combined_xlsx <- file.path(final_dir, "full_text_eligibility_adjudication_final_package.xlsx")

stable_write_csv(eligibility, final_decisions_csv, FULL_TEXT_ELIGIBILITY_COLUMNS)
stable_write_csv(extraction_queue, extraction_queue_csv, FULL_TEXT_ELIGIBILITY_COLUMNS)
stable_write_csv(exclusions, exclusions_csv, FULL_TEXT_ELIGIBILITY_COLUMNS)
stable_write_csv(unavailable, unavailable_csv, FULL_TEXT_UNAVAILABLE_COLUMNS)
stable_write_csv(audit, audit_csv, FINAL_AUDIT_COLUMNS)
stable_write_csv(validation_issues, validation_csv, FINAL_VALIDATION_COLUMNS)
stable_write_csv(counts, counts_csv, FINAL_COUNT_COLUMNS)

write_review_xlsx(list(final_eligibility_decisions = eligibility), final_decisions_xlsx)
write_review_xlsx(list(full_text_eligibility_extraction_queue = extraction_queue), extraction_queue_xlsx)
write_review_xlsx(list(full_text_eligibility_full_text_exclusions = exclusions), exclusions_xlsx)
write_review_xlsx(list(full_text_eligibility_unavailable_reference_final = unavailable), unavailable_xlsx)
write_review_xlsx(list(full_text_eligibility_adjudication_final_audit = audit), audit_xlsx)
write_review_xlsx(list(full_text_eligibility_adjudication_final_validation_issues = validation_issues), validation_xlsx)
write_review_xlsx(list(full_text_eligibility_adjudication_final_counts = counts), counts_xlsx)

write_review_xlsx(
  list(
    eligibility = eligibility,
    unavailable_reference = unavailable,
    codebook = read_input_xlsx_sheet(input_path, "codebook", c("field", "allowed_value", "meaning", "required_when")),
    adjudication_audit = audit,
    summary = summary_input
  ),
  cached_consensus_path
)
source_manifest$cached_consensus_md5 <- file_md5(cached_consensus_path)

stable_write_csv(source_manifest, source_manifest_csv, FINAL_SOURCE_COLUMNS)
write_review_xlsx(list(full_text_eligibility_adjudication_final_source_manifest = source_manifest), source_manifest_xlsx)

write_final_adjudication_xlsx(
  eligibility = eligibility,
  extraction_queue = extraction_queue,
  exclusions = exclusions,
  unavailable = unavailable,
  audit = audit,
  validation = validation_issues,
  counts = counts,
  source_manifest = source_manifest,
  path = combined_xlsx
)

warnings <- character()
if (sum(validation_issues$severity == "BLOCKER") > 0L) {
  warnings <- c(warnings, paste0(sum(validation_issues$severity == "BLOCKER"), " blocker validation issue(s) must be corrected before data extraction."))
}
if (nrow(unavailable) > 0L) {
  warnings <- c(warnings, paste0(nrow(unavailable), " full-text-stage record(s) remain classified as unavailable_reference."))
}
if (nrow(exclusions) > 0L) {
  warnings <- c(warnings, paste0(nrow(exclusions), " full-text exclusion(s) will be counted in PRISMA as excluded after full-text assessment."))
}

outputs <- c(
  final_decisions_csv,
  final_decisions_xlsx,
  extraction_queue_csv,
  extraction_queue_xlsx,
  exclusions_csv,
  exclusions_xlsx,
  unavailable_csv,
  unavailable_xlsx,
  audit_csv,
  audit_xlsx,
  validation_csv,
  validation_xlsx,
  counts_csv,
  counts_xlsx,
  source_manifest_csv,
  source_manifest_xlsx,
  cached_consensus_path,
  combined_xlsx,
  file.path(CFG$checkpoints_dir, "CP12B_import_full_text_adjudication_final.md")
)

review_file <- write_checkpoint(
  id = "CP12B",
  name = "import_full_text_adjudication_final",
  what_ran = paste(
    "Imported the final full-text eligibility adjudication workbook, validated",
    "the record sets against CP12, standardized the adjudicator label as",
    "Decisão conjunta, and wrote the canonical",
    "final eligibility, exclusion, unavailable-reference, audit, and extraction",
    "queue outputs."
  ),
  numbers = stats::setNames(counts$value, counts$metric),
  review_items = c(
    "Inspect full_text_eligibility_adjudication_final_package.xlsx.",
    "Confirm the final include/exclude counts and the four full-text exclusions.",
    "Confirm the 33-row extraction queue before starting data extraction.",
    "Confirm unavailable_reference records are access failures rather than scientific exclusions."
  ),
  outputs = outputs,
  warnings = warnings,
  gate_question = "Approve CP12B only after the final eligibility decisions, exclusion reasons, unavailable-reference handling, and extraction queue have been inspected."
)

cat("CP12B final full-text adjudication import complete\n")
cat("Final eligibility records:", nrow(eligibility), "\n")
cat("Included for extraction:", nrow(extraction_queue), "\n")
cat("Excluded after full text:", nrow(exclusions), "\n")
cat("Unavailable references:", nrow(unavailable), "\n")
cat("Validation issues:", nrow(validation_issues), "\n")
cat("Final package:", project_relative_path(combined_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
