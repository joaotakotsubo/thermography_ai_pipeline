#!/usr/bin/env Rscript

# Aplica as correções operacionais autorizadas na conferência da triagem.
# As mudanças precisam corresponder aos registros e às decisões documentadas.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

CORRECTION_COLUMNS <- c(
  "record_id",
  "corrected_pre_adjudicated_decision",
  "corrected_pre_adjudication_reason",
  "corrected_pre_adjudicator_notes",
  "correction_date_local",
  "correction_source",
  "correction_rationale"
)

CORRECTION_AUDIT_COLUMNS <- c(
  "record_id",
  "previous_pre_adjudicated_decision",
  "previous_pre_adjudication_reason",
  "previous_pre_adjudicator_notes",
  "corrected_pre_adjudicated_decision",
  "corrected_pre_adjudication_reason",
  "corrected_pre_adjudicator_notes",
  "correction_date_local",
  "correction_source",
  "correction_rationale",
  "correction_status"
)

pre_adjudication_xlsx_path <- file.path(
  CFG$screening_dir,
  "title_deduplication",
  "title_abstract_pre_adjudication_template_title_deduped.xlsx"
)
pre_adjudication_csv_path <- file.path(
  CFG$screening_dir,
  "title_deduplication",
  "title_abstract_pre_adjudication_template_title_deduped.csv"
)
corrections_path <- file.path(
  CFG$screening_dir,
  "title_deduplication",
  "pre_adjudication_scope_corrections.csv"
)
audit_path <- file.path(
  CFG$screening_dir,
  "title_deduplication",
  "pre_adjudication_scope_correction_audit.csv"
)

for (path in c(pre_adjudication_xlsx_path, corrections_path)) {
  if (!file.exists(path)) {
    stop("Required input is missing: ", project_relative_path(path), call. = FALSE)
  }
}

pre_adjudication <- read_review_xlsx_sheet(
  pre_adjudication_xlsx_path,
  "pre_adjudication",
  SCREENING_PRE_ADJUDICATION_COLUMNS
)
codebook <- read_review_xlsx_sheet(
  pre_adjudication_xlsx_path,
  "codebook",
  SCREENING_PRE_ADJUDICATION_CODEBOOK_COLUMNS
)
corrections <- read_machine_csv(corrections_path)
existing_audit <- read_csv_or_empty(audit_path, CORRECTION_AUDIT_COLUMNS)

validate_required_columns(pre_adjudication, SCREENING_PRE_ADJUDICATION_COLUMNS, pre_adjudication_xlsx_path)
validate_required_columns(codebook, SCREENING_PRE_ADJUDICATION_CODEBOOK_COLUMNS, pre_adjudication_xlsx_path)
validate_required_columns(corrections, CORRECTION_COLUMNS, corrections_path)
validate_required_columns(existing_audit, CORRECTION_AUDIT_COLUMNS, audit_path)

pre_adjudication <- as_character_frame(pre_adjudication[, SCREENING_PRE_ADJUDICATION_COLUMNS, drop = FALSE])
codebook <- as_character_frame(codebook[, SCREENING_PRE_ADJUDICATION_CODEBOOK_COLUMNS, drop = FALSE])
corrections <- as_character_frame(corrections[, CORRECTION_COLUMNS, drop = FALSE])
existing_audit <- as_character_frame(existing_audit[, CORRECTION_AUDIT_COLUMNS, drop = FALSE])

if (any(!nzchar(trimws(corrections$record_id)))) {
  stop("Correction table contains blank record_id value(s).", call. = FALSE)
}
if (any(duplicated(corrections$record_id))) {
  stop("Correction table contains duplicate record_id value(s).", call. = FALSE)
}

corrections$corrected_pre_adjudicated_decision <- normalize_pre_adjudication_decision(
  corrections$corrected_pre_adjudicated_decision
)

invalid_decisions <- !(corrections$corrected_pre_adjudicated_decision %in% pre_adjudication_allowed_decisions())
if (any(invalid_decisions)) {
  stop(
    "Invalid corrected_pre_adjudicated_decision value(s): ",
    paste(unique(corrections$corrected_pre_adjudicated_decision[invalid_decisions]), collapse = ", "),
    call. = FALSE
  )
}

audit_rows <- list()
for (i in seq_len(nrow(corrections))) {
  record_id <- corrections$record_id[[i]]
  row_index <- match(record_id, pre_adjudication$record_id)
  if (is.na(row_index)) {
    stop("Correction record_id not found in pre-adjudication workbook: ", record_id, call. = FALSE)
  }

  previous <- pre_adjudication[row_index, , drop = FALSE]
  existing_row <- existing_audit[existing_audit$record_id == record_id, , drop = FALSE]
  already_applied <- identical(
    previous$pre_adjudicated_decision,
    corrections$corrected_pre_adjudicated_decision[[i]]
  ) &&
    identical(
      previous$pre_adjudication_reason,
      corrections$corrected_pre_adjudication_reason[[i]]
    ) &&
    identical(
      previous$pre_adjudicator_notes,
      corrections$corrected_pre_adjudicator_notes[[i]]
    )

  pre_adjudication$pre_adjudicated_decision[[row_index]] <- corrections$corrected_pre_adjudicated_decision[[i]]
  pre_adjudication$pre_adjudication_reason[[row_index]] <- corrections$corrected_pre_adjudication_reason[[i]]
  pre_adjudication$pre_adjudicator_notes[[row_index]] <- corrections$corrected_pre_adjudicator_notes[[i]]

  if (already_applied && nrow(existing_row) > 0L) {
    audit_rows[[length(audit_rows) + 1L]] <- existing_row[1L, , drop = FALSE]
  } else {
    audit_rows[[length(audit_rows) + 1L]] <- data.frame(
      record_id = record_id,
      previous_pre_adjudicated_decision = previous$pre_adjudicated_decision,
      previous_pre_adjudication_reason = previous$pre_adjudication_reason,
      previous_pre_adjudicator_notes = previous$pre_adjudicator_notes,
      corrected_pre_adjudicated_decision = corrections$corrected_pre_adjudicated_decision[[i]],
      corrected_pre_adjudication_reason = corrections$corrected_pre_adjudication_reason[[i]],
      corrected_pre_adjudicator_notes = corrections$corrected_pre_adjudicator_notes[[i]],
      correction_date_local = corrections$correction_date_local[[i]],
      correction_source = corrections$correction_source[[i]],
      correction_rationale = corrections$correction_rationale[[i]],
      correction_status = "applied",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
}

audit <- do.call(rbind, audit_rows)
audit <- audit[, CORRECTION_AUDIT_COLUMNS, drop = FALSE]

stable_write_csv(pre_adjudication, pre_adjudication_csv_path, SCREENING_PRE_ADJUDICATION_COLUMNS)
stable_write_csv(audit, audit_path, CORRECTION_AUDIT_COLUMNS)
write_screening_pre_adjudication_xlsx(pre_adjudication, codebook, pre_adjudication_xlsx_path)

write_checkpoint(
  id = "CP10D",
  name = "pre_adjudication_scope_corrections",
  what_ran = "Applied explicit human scope correction(s) to the canonical title-deduplicated pre-adjudication workbook before recalculating CP10C final title/abstract decisions.",
  numbers = c(
    corrections_applied = nrow(audit),
    corrected_records = paste(audit$record_id, collapse = "; ")
  ),
  review_items = c(
    "Inspect pre_adjudication_scope_correction_audit.csv to verify the previous and corrected values.",
    "Rerun B10c_apply_pre_adjudication.R after applying these corrections.",
    "Confirm corrected records are excluded or retained as intended in title_abstract_final_decisions.xlsx."
  ),
  outputs = c(
    corrections_path,
    audit_path,
    pre_adjudication_csv_path,
    pre_adjudication_xlsx_path
  ),
  warnings = character(),
  gate_question = "Approve CP10D only after confirming that each scope correction is scientifically justified and CP10C has been regenerated."
)

cat("Applied pre-adjudication corrections:", nrow(audit), "\n")
cat("Updated workbook:", project_relative_path(pre_adjudication_xlsx_path), "\n")
cat("Updated CSV:", project_relative_path(pre_adjudication_csv_path), "\n")
cat("Audit log:", project_relative_path(audit_path), "\n")
