#!/usr/bin/env Rscript

# Prepara os registros e os formulários para a triagem independente.
# As decisões de inclusão e exclusão serão preenchidas pelos revisores.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP08")

machine_csv_read <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character"
  )
}

unique_join <- function(x) {
  x <- sort(unique(trimws(norm_empty_if_na(x))), method = "radix")
  x <- x[nzchar(x)]
  paste(x, collapse = "; ")
}

aggregate_survivor_sources <- function(records, audit) {
  source_databases <- records$source_database
  source_files <- records$source_file

  if (nrow(audit) == 0L) {
    return(data.frame(
      record_id = records$record_id,
      source_databases = source_databases,
      source_files = source_files,
      stringsAsFactors = FALSE
    ))
  }

  for (i in seq_len(nrow(records))) {
    record_id <- records$record_id[[i]]
    rows <- audit[audit$survivor_record_id == record_id, , drop = FALSE]
    source_databases[[i]] <- unique_join(c(records$source_database[[i]], rows$absorbed_source_database))
    source_files[[i]] <- unique_join(c(records$source_file[[i]], rows$absorbed_source_file))
  }

  data.frame(
    record_id = records$record_id,
    source_databases = source_databases,
    source_files = source_files,
    stringsAsFactors = FALSE
  )
}

primary_path <- file.path(CFG$processed_dir, "records_after_dedup_primary.csv")
audit_path <- file.path(CFG$processed_dir, "dedup_audit_log.csv")
fuzzy_path <- file.path(CFG$processed_dir, "fuzzy_duplicate_candidates.csv")
preprint_path <- file.path(CFG$processed_dir, "preprint_publication_candidates.csv")

if (!file.exists(primary_path)) {
  stop("Missing CP06 output: ", project_relative_path(primary_path), call. = FALSE)
}
if (!file.exists(audit_path)) {
  stop("Missing CP06 output: ", project_relative_path(audit_path), call. = FALSE)
}
if (!file.exists(fuzzy_path)) {
  stop("Missing CP06 output: ", project_relative_path(fuzzy_path), call. = FALSE)
}
if (!file.exists(preprint_path)) {
  stop("Missing CP06 output: ", project_relative_path(preprint_path), call. = FALSE)
}

primary <- machine_csv_read(primary_path)
audit <- machine_csv_read(audit_path)
fuzzy <- machine_csv_read(fuzzy_path)
preprint <- machine_csv_read(preprint_path)

source_map <- aggregate_survivor_sources(primary, audit)
primary$source_databases <- source_map$source_databases[match(primary$record_id, source_map$record_id)]
primary$source_files <- source_map$source_files[match(primary$record_id, source_map$record_id)]
primary$candidate_flags <- screening_candidate_flags(primary)

worksheet <- data.frame(
  record_id = primary$record_id,
  source_databases = primary$source_databases,
  source_files = primary$source_files,
  title = primary$title,
  abstract = primary$abstract,
  keywords = primary$keywords,
  authors = primary$authors,
  year = primary$year,
  journal_or_source = primary$journal_or_source,
  doi = primary$doi,
  pmid = primary$pmid,
  url = primary$url,
  publication_type = primary$publication_type,
  candidate_flags = primary$candidate_flags,
  reviewer_1_decision = "",
  reviewer_1_reason = "",
  reviewer_2_decision = "",
  reviewer_2_reason = "",
  conflict_flag = "",
  final_decision = "",
  final_reason = "",
  notes = "",
  stringsAsFactors = FALSE
)
worksheet <- worksheet[order(worksheet$record_id, method = "radix"), SCREENING_WORKSHEET_COLUMNS, drop = FALSE]

blinded <- worksheet[, SCREENING_BLINDED_COLUMNS, drop = FALSE]
codebook <- screening_codebook()

worksheet_csv_path <- file.path(CFG$screening_dir, "title_abstract_worksheet.csv")
worksheet_xlsx_path <- file.path(CFG$screening_dir, "title_abstract_worksheet.xlsx")
blinded_csv_path <- file.path(CFG$screening_dir, "title_abstract_worksheet_blinded.csv")
blinded_xlsx_path <- file.path(CFG$screening_dir, "title_abstract_worksheet_blinded.xlsx")
codebook_csv_path <- file.path(CFG$screening_dir, "screening_codebook.csv")
codebook_xlsx_path <- file.path(CFG$screening_dir, "screening_codebook.xlsx")
worksheet_sheet <- "title_abstract_worksheet"
blinded_sheet <- "screening_blinded"
codebook_sheet <- "screening_codebook"

stable_write_csv(worksheet, worksheet_csv_path, SCREENING_WORKSHEET_COLUMNS)
stable_write_csv(blinded, blinded_csv_path, SCREENING_BLINDED_COLUMNS)
stable_write_csv(codebook, codebook_csv_path, SCREENING_CODEBOOK_COLUMNS)
write_review_xlsx(
  stats::setNames(list(worksheet, codebook), c(worksheet_sheet, codebook_sheet)),
  worksheet_xlsx_path
)
write_review_xlsx(
  stats::setNames(list(blinded, codebook), c(blinded_sheet, codebook_sheet)),
  blinded_xlsx_path
)
write_review_xlsx(
  stats::setNames(list(codebook), codebook_sheet),
  codebook_xlsx_path
)

decision_columns <- c("reviewer_1_decision", "reviewer_2_decision", "final_decision")
prefilled_decisions <- sum(vapply(decision_columns, function(column) sum(nzchar(worksheet[[column]])), integer(1L)))
missing_abstract <- sum(grepl("flag_missing_abstract", worksheet$candidate_flags, fixed = TRUE))
records_with_flags <- sum(nzchar(worksheet$candidate_flags))
xlsx_truncated_cells <- sum(vapply(
  worksheet,
  function(column) sum(nchar(as.character(column), type = "chars", allowNA = FALSE, keepNA = FALSE) > XLSX_CELL_TEXT_LIMIT),
  integer(1L)
))
fuzzy_record_ids <- unique(c(fuzzy$record_id_1, fuzzy$record_id_2))
fuzzy_record_ids <- fuzzy_record_ids[nzchar(fuzzy_record_ids)]
preprint_record_ids <- unique(c(preprint$record_id_1, preprint$record_id_2))
preprint_record_ids <- preprint_record_ids[nzchar(preprint_record_ids)]

checkpoint_warnings <- character()
if (prefilled_decisions > 0L) {
  checkpoint_warnings <- c(checkpoint_warnings, "Reviewer decision fields are not blank; this must be corrected before screening.")
}
if (nrow(fuzzy) > 0L || nrow(preprint) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0(
      nrow(fuzzy),
      " fuzzy candidate pairs and ",
      nrow(preprint),
      " preprint/publication candidate pairs remain queued for CP06b/human adjudication; CP09 did not remove them."
    )
  )
}
if (missing_abstract > 0L) {
  checkpoint_warnings <- c(checkpoint_warnings, paste0(missing_abstract, " screening records have missing abstracts."))
}
if (xlsx_truncated_cells > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0(xlsx_truncated_cells, " cells exceed the XLSX display limit and were marked as truncated in XLSX only; full text remains in CSV.")
  )
}

review_file <- write_checkpoint(
  id = "CP09",
  name = "prepare_screening",
  what_ran = paste(
    "Prepared title/abstract screening worksheets from the CP06 exact-deduplicated",
    "primary records, exported reviewer-facing XLSX workbooks and machine-readable",
    "CSV audit files, and left all human decision fields blank."
  ),
  numbers = c(
    screening_records = nrow(worksheet),
    blinded_screening_records = nrow(blinded),
    records_with_candidate_flags = records_with_flags,
    records_missing_abstract = missing_abstract,
    xlsx_cells_truncated_for_display = xlsx_truncated_cells,
    fuzzy_candidate_pairs_not_removed = nrow(fuzzy),
    fuzzy_records_in_candidate_pairs = length(fuzzy_record_ids),
    preprint_publication_candidate_pairs_not_removed = nrow(preprint),
    preprint_publication_records_in_candidate_pairs = length(preprint_record_ids),
    prefilled_decision_cells = prefilled_decisions
  ),
  review_items = c(
    "Open title_abstract_worksheet.xlsx for reviewer-facing screening.",
    "Open title_abstract_worksheet_blinded.xlsx if reviewers should screen without source/authorship fields.",
    "Confirm reviewer decision fields are blank before distribution.",
    "Confirm unresolved fuzzy/preprint candidates are acceptable as flagged/not-removed before screening begins.",
    "Use screening_codebook.xlsx for allowed values and field definitions."
  ),
  outputs = c(
    worksheet_csv_path,
    worksheet_xlsx_path,
    blinded_csv_path,
    blinded_xlsx_path,
    codebook_csv_path,
    codebook_xlsx_path,
    file.path(CFG$checkpoints_dir, "CP09_prepare_screening.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP09 only if the screening worksheets are ready to hand to human reviewers."
)

if (prefilled_decisions > 0L) {
  stop("CP09 produced non-empty reviewer decision fields. Review screening worksheets before proceeding.", call. = FALSE)
}

cat("CP09 prepare screening complete\n")
cat("Screening records:", nrow(worksheet), "\n")
cat("Records with candidate flags:", records_with_flags, "\n")
cat("Missing abstracts:", missing_abstract, "\n")
cat("Worksheet XLSX:", project_relative_path(worksheet_xlsx_path), "\n")
cat("Blinded worksheet XLSX:", project_relative_path(blinded_xlsx_path), "\n")
cat("Codebook XLSX:", project_relative_path(codebook_xlsx_path), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
