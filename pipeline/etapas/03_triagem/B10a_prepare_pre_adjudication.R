#!/usr/bin/env Rscript

# Organiza as discordâncias da triagem para a conferência conjunta.
# Prepara o material de revisão sem escolher uma resposta em nome dos revisores.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP09")

conflicts_path <- file.path(CFG$screening_dir, "reconciliation", "screening_conflicts.csv")
blinded_path <- file.path(CFG$screening_dir, "title_abstract_worksheet_blinded.csv")

if (!file.exists(conflicts_path)) {
  stop(
    "Missing CP10 conflict file. Run pipeline/scripts/B10_screening_reconciliation.R first: ",
    project_relative_path(conflicts_path),
    call. = FALSE
  )
}
if (!file.exists(blinded_path)) {
  stop("Missing CP09 blinded worksheet: ", project_relative_path(blinded_path), call. = FALSE)
}

conflicts <- read_machine_csv(conflicts_path)
blinded <- read_machine_csv(blinded_path)

pre_adjudication <- screening_pre_adjudication_template(
  conflicts = conflicts,
  blinded_records = blinded
)
codebook <- screening_pre_adjudication_codebook()

pre_adjudication_dir <- ensure_dir(file.path(CFG$screening_dir, "pre_adjudication"))
pre_adjudication_csv <- file.path(pre_adjudication_dir, "title_abstract_pre_adjudication_template.csv")
pre_adjudication_xlsx <- file.path(pre_adjudication_dir, "title_abstract_pre_adjudication_template.xlsx")
codebook_csv <- file.path(pre_adjudication_dir, "title_abstract_pre_adjudication_codebook.csv")

stable_write_csv(pre_adjudication, pre_adjudication_csv, SCREENING_PRE_ADJUDICATION_COLUMNS)
stable_write_csv(codebook, codebook_csv, SCREENING_PRE_ADJUDICATION_CODEBOOK_COLUMNS)
write_screening_pre_adjudication_xlsx(pre_adjudication, codebook, pre_adjudication_xlsx)

priority_counts <- table(factor(
  pre_adjudication$pre_adjudication_priority,
  levels = c("P1_any_include_conflict", "P2_exclude_uncertain_conflict", "P3_other_conflict")
))

checkpoint_warnings <- character()
if (nrow(pre_adjudication) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    "Pre-adjudication workbook awaits human completion; this script does not resolve scientific disagreements."
  )
}

review_file <- write_checkpoint(
  id = "CP10A",
  name = "title_abstract_pre_adjudication_template",
  what_ran = paste(
    "Converted CP10 title/abstract conflicts into a dedicated human pre-adjudication",
    "workbook. The original reviewer decisions remain read-only evidence; only the",
    "pre_adjudication columns are intended for human edits."
  ),
  numbers = c(
    conflict_records_in_template = nrow(pre_adjudication),
    P1_any_include_conflicts = unname(priority_counts[["P1_any_include_conflict"]]),
    P2_exclude_uncertain_conflicts = unname(priority_counts[["P2_exclude_uncertain_conflict"]]),
    P3_other_conflicts = unname(priority_counts[["P3_other_conflict"]]),
    human_editable_columns = 3
  ),
  review_items = c(
    "Open title_abstract_pre_adjudication_template.xlsx and review the pre_adjudication sheet.",
    "Fill only pre_adjudicated_decision, pre_adjudication_reason, and pre_adjudicator_notes.",
    "Use full_text when the record should continue to full-text assessment.",
    "Use exclude only when the title/abstract is clearly outside the protocol scope.",
    "Use needs_discussion when the conflict cannot be resolved safely at title/abstract level."
  ),
  outputs = c(
    pre_adjudication_csv,
    pre_adjudication_xlsx,
    codebook_csv,
    file.path(CFG$checkpoints_dir, "CP10A_title_abstract_pre_adjudication_template.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP10A only after the pre-adjudication workbook has been completed and inspected."
)

cat("CP10A title/abstract pre-adjudication template complete\n")
cat("Conflict records in template:", nrow(pre_adjudication), "\n")
cat("P1 any-include conflicts:", unname(priority_counts[["P1_any_include_conflict"]]), "\n")
cat("P2 exclude-uncertain conflicts:", unname(priority_counts[["P2_exclude_uncertain_conflict"]]), "\n")
cat("P3 other conflicts:", unname(priority_counts[["P3_other_conflict"]]), "\n")
cat("Pre-adjudication workbook:", project_relative_path(pre_adjudication_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
