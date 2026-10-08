#!/usr/bin/env Rscript

# Confere a presença dos estudos de referência quando essa entrada está disponível.
# A ausência de documentação não é tratada como uma verificação concluída.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP07")

machine_csv_read <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character"
  )
}

sentinel_report_row <- function(
  sentinel,
  doi_normalized,
  pmid_normalized,
  arxiv_id_normalized,
  title_normalized,
  match_key,
  recovered_record = NULL,
  found_in_normalized_primary = FALSE,
  found_in_any_processed_stream = FALSE
) {
  recovered <- !is.null(recovered_record)
  status <- "RECOVERED"
  if (!nzchar(match_key)) {
    status <- "NEEDS_SENTINEL_IDENTIFIER"
  } else if (!recovered && isTRUE(found_in_normalized_primary)) {
    status <- "MISSING_AFTER_DEDUP_OR_SCREENING_SET"
  } else if (!recovered && isTRUE(found_in_any_processed_stream)) {
    status <- "FOUND_OUTSIDE_PRIMARY_STREAM"
  } else if (!recovered) {
    status <- "NOT_RECOVERED_SEARCH_SENSITIVITY_WARNING"
  }

  data.frame(
    sentinel_id = sentinel$sentinel_id,
    sentinel_label = sentinel$sentinel_label,
    title = sentinel$title,
    doi = sentinel$doi,
    pmid = sentinel$pmid,
    arxiv_id = sentinel$arxiv_id,
    doi_normalized = doi_normalized,
    pmid_normalized = pmid_normalized,
    arxiv_id_normalized = arxiv_id_normalized,
    title_normalized = title_normalized,
    match_key = match_key,
    recovered_in_primary = ifelse(recovered, "yes", "no"),
    recovered_record_id = if (recovered) recovered_record$record_id else "",
    recovered_record_source_id = if (recovered) recovered_record$record_source_id else "",
    recovered_source_database = if (recovered) recovered_record$source_database else "",
    recovered_title = if (recovered) recovered_record$title else "",
    found_in_normalized_primary = ifelse(isTRUE(found_in_normalized_primary), "yes", "no"),
    found_in_imported_primary = ifelse(isTRUE(found_in_normalized_primary), "yes", "no"),
    found_in_any_processed_stream = ifelse(isTRUE(found_in_any_processed_stream), "yes", "no"),
    status = status,
    notes = sentinel$notes,
    stringsAsFactors = FALSE
  )
}

primary_path <- file.path(CFG$processed_dir, "records_after_dedup_primary.csv")
normalized_primary_path <- file.path(CFG$processed_dir, "normalized_records_primary.csv")
normalized_all_path <- file.path(CFG$processed_dir, "normalized_records_all.csv")

if (!file.exists(primary_path)) {
  stop("Missing CP06 output: ", project_relative_path(primary_path), call. = FALSE)
}
if (!file.exists(normalized_primary_path)) {
  stop("Missing CP05 output: ", project_relative_path(normalized_primary_path), call. = FALSE)
}
if (!file.exists(normalized_all_path)) {
  stop("Missing CP05 output: ", project_relative_path(normalized_all_path), call. = FALSE)
}

primary <- machine_csv_read(primary_path)
normalized_primary <- machine_csv_read(normalized_primary_path)
normalized_all <- machine_csv_read(normalized_all_path)
sentinel_input <- read_sentinel_inputs()
sentinels <- sentinel_input$sentinels

stable_write_csv(sentinels, sentinel_input$csv_path, SENTINEL_COLUMNS)

if (nrow(sentinels) == 0L) {
  report <- empty_named_frame(SENTINEL_REPORT_COLUMNS)
} else {
  rows <- vector("list", nrow(sentinels))
  for (i in seq_len(nrow(sentinels))) {
    sentinel <- sentinels[i, , drop = FALSE]
    doi_normalized <- normalize_doi(sentinel$doi)
    pmid_normalized <- normalize_pmid(sentinel$pmid)
    arxiv_id_normalized <- normalize_arxiv_id(sentinel$arxiv_id)
    title_normalized <- normalize_text_key(sentinel$title)
    match_key <- sentinel_match_key(doi_normalized, pmid_normalized, arxiv_id_normalized, title_normalized)

    primary_match <- sentinel_find_first_match(match_key, primary)
    normalized_primary_match <- sentinel_find_first_match(match_key, normalized_primary)
    any_processed_match <- sentinel_find_first_match(match_key, normalized_all)

    recovered_record <- NULL
    if (!is.na(primary_match)) {
      recovered_record <- primary[primary_match, , drop = FALSE]
    }

    rows[[i]] <- sentinel_report_row(
      sentinel = sentinel,
      doi_normalized = doi_normalized,
      pmid_normalized = pmid_normalized,
      arxiv_id_normalized = arxiv_id_normalized,
      title_normalized = title_normalized,
      match_key = match_key,
      recovered_record = recovered_record,
      found_in_normalized_primary = !is.na(normalized_primary_match),
      found_in_any_processed_stream = !is.na(any_processed_match)
    )
  }
  report <- do.call(rbind, rows)
  report <- report[order(report$sentinel_id, report$sentinel_label, report$title, method = "radix"), , drop = FALSE]
}

report_path <- file.path(CFG$outputs_dir, "sentinel_recovery_report.csv")
report_xlsx_path <- file.path(CFG$outputs_dir, "sentinel_recovery_report.xlsx")

stable_write_csv(report, report_path, SENTINEL_REPORT_COLUMNS)
write_review_xlsx(
  list(
    sentinel_recovery_report = report,
    sentinels = sentinels
  ),
  report_xlsx_path
)

n_recovered <- if (nrow(report) == 0L) 0L else sum(report$status == "RECOVERED")
n_missed <- if (nrow(report) == 0L) 0L else sum(!(report$status %in% c("RECOVERED")))

checkpoint_warnings <- character()
if (nrow(sentinels) == 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    "Sentinel template is empty; CP08 created/validated templates and did not block Phase A."
  )
}
if (length(sentinel_input$duplicate_ids) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0("Duplicate sentinel_id values found: ", paste(sentinel_input$duplicate_ids, collapse = ", "), ".")
  )
}
if (n_missed > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0(n_missed, " populated sentinel rows were not recovered in the primary deduplicated set; review as search-sensitivity warnings.")
  )
}

review_file <- write_checkpoint(
  id = "CP08",
  name = "sentinel_recovery",
  what_ran = paste(
    "Created or read the sentinel template, synchronized the machine-readable",
    "sentinels CSV, checked populated sentinels against the exact-deduplicated",
    "primary records by DOI/PMID/arXiv/title keys, and generated CSV/XLSX",
    "reports for human review."
  ),
  numbers = c(
    sentinels_provided = nrow(sentinels),
    sentinels_recovered = n_recovered,
    sentinels_not_recovered_or_incomplete = n_missed,
    duplicate_sentinel_ids = length(sentinel_input$duplicate_ids)
  ),
  review_items = c(
    "Open sentinels.xlsx to add known eligible or landmark sentinel records if needed.",
    "Open sentinel_recovery_report.xlsx to inspect recovery status.",
    "Treat misses as search-sensitivity warnings, not automatic eligibility decisions.",
    "Confirm the empty sentinel template is acceptable before proceeding if no sentinels are provided."
  ),
  outputs = c(
    sentinel_input$csv_path,
    sentinel_input$workbook_path,
    report_path,
    report_xlsx_path,
    file.path(CFG$checkpoints_dir, "CP08_sentinel_recovery.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP08 only if sentinel recovery warnings are acceptable for preparing screening worksheets."
)

cat("CP08 sentinel recovery complete\n")
cat("Sentinels provided:", nrow(sentinels), "\n")
cat("Sentinels recovered:", n_recovered, "\n")
cat("Sentinels not recovered/incomplete:", n_missed, "\n")
cat("Sentinel workbook:", project_relative_path(sentinel_input$workbook_path), "\n")
cat("Recovery report workbook:", project_relative_path(report_xlsx_path), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
