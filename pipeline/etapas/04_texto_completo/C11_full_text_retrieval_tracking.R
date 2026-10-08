#!/usr/bin/env Rscript

# Organiza o acompanhamento da obtenção dos textos completos.
# Mantém o estado de cada documento separado da decisão de elegibilidade.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP10C")

FINAL_FULL_TEXT_QUEUE_COLUMNS <- c(
  "record_id",
  "title_duplicate_group_id",
  "duplicate_group_record_ids",
  "absorbed_duplicate_record_ids",
  "title",
  "abstract",
  "keywords",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "pmid",
  "url",
  "publication_type",
  "candidate_flags",
  "source_databases",
  "source_files",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "pre_adjudicated_decisions",
  "reason_for_full_text",
  "retrieval_status",
  "retrieval_notes"
)

RETRIEVAL_UPDATE_COLUMNS <- c(
  "record_id",
  "retrieval_status",
  "retrieval_attempt_date_local",
  "online_search_status",
  "author_contact_status",
  "library_request_status",
  "retrieval_sources_checked",
  "retrieval_note",
  "updated_by",
  "updated_at_local"
)

RETRIEVAL_TRACKING_COLUMNS <- c(
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
  "source_files",
  "reason_for_full_text",
  "expected_pdf_filename",
  "expected_pdf_path",
  "pdf_present",
  "retrieval_status",
  "retrieval_attempt_date_local",
  "online_search_status",
  "author_contact_status",
  "library_request_status",
  "retrieval_sources_checked",
  "retrieval_note",
  "updated_by",
  "updated_at_local"
)

PDF_INVENTORY_COLUMNS <- c(
  "pdf_filename",
  "pdf_path",
  "matched_record_id",
  "matched_queue_record",
  "file_size_bytes",
  "file_modified_time"
)

RETRIEVAL_SUMMARY_COLUMNS <- c("metric", "value")

RETRIEVAL_CODEBOOK_COLUMNS <- c(
  "field",
  "allowed_value",
  "meaning",
  "used_by"
)

allowed_retrieval_statuses <- function() {
  c(
    "not_started",
    "retrieved",
    "not_found_after_documented_search",
    "awaiting_library",
    "author_contact_pending",
    "excluded_before_full_text"
  )
}

read_retrieval_updates <- function(path) {
  updates <- read_csv_or_empty(path, RETRIEVAL_UPDATE_COLUMNS)
  validate_required_columns(updates, RETRIEVAL_UPDATE_COLUMNS, path)
  updates <- as_character_frame(updates[, RETRIEVAL_UPDATE_COLUMNS, drop = FALSE])
  updates$record_id <- trimws(updates$record_id)
  updates$retrieval_status <- tolower(trimws(updates$retrieval_status))
  updates <- updates[nzchar(updates$record_id), , drop = FALSE]

  if (nrow(updates) == 0L) {
    return(updates)
  }

  duplicated_records <- unique(updates$record_id[duplicated(updates$record_id)])
  if (length(duplicated_records) > 0L) {
    stop(
      "Retrieval status update table contains duplicate record_id value(s): ",
      paste(duplicated_records, collapse = ", "),
      call. = FALSE
    )
  }

  invalid_status <- !(updates$retrieval_status %in% allowed_retrieval_statuses())
  if (any(invalid_status)) {
    stop(
      "Invalid retrieval_status value(s): ",
      paste(unique(updates$retrieval_status[invalid_status]), collapse = ", "),
      ". Allowed values: ",
      paste(allowed_retrieval_statuses(), collapse = ", "),
      call. = FALSE
    )
  }

  missing_note <- updates$retrieval_status != "not_started" & !nzchar(trimws(updates$retrieval_note))
  if (any(missing_note)) {
    stop(
      "Non-not_started retrieval updates require retrieval_note for record_id(s): ",
      paste(updates$record_id[missing_note], collapse = ", "),
      call. = FALSE
    )
  }

  updates
}

build_pdf_inventory <- function(pdf_dir, queue_record_ids) {
  pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(pdf_files) == 0L) {
    return(empty_named_frame(PDF_INVENTORY_COLUMNS))
  }

  matched_record_id <- sub("\\.pdf$", "", basename(pdf_files), ignore.case = TRUE)
  info <- file.info(pdf_files)

  data.frame(
    pdf_filename = basename(pdf_files),
    pdf_path = project_relative_path(pdf_files),
    matched_record_id = matched_record_id,
    matched_queue_record = ifelse(matched_record_id %in% queue_record_ids, "yes", "no"),
    file_size_bytes = as.character(info$size),
    file_modified_time = format(info$mtime, "%Y-%m-%d %H:%M:%S %Z"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, PDF_INVENTORY_COLUMNS, drop = FALSE]
}

apply_pdf_presence <- function(tracking, pdf_inventory) {
  if (nrow(pdf_inventory) == 0L) {
    tracking$pdf_present <- "no"
    return(tracking)
  }

  present_ids <- pdf_inventory$matched_record_id[pdf_inventory$matched_queue_record == "yes"]
  tracking$pdf_present <- ifelse(tracking$record_id %in% present_ids, "yes", "no")
  needs_retrieved_status <- tracking$pdf_present == "yes" & tracking$retrieval_status == "not_started"
  tracking$retrieval_status[needs_retrieved_status] <- "retrieved"
  tracking$retrieval_note[needs_retrieved_status] <- "PDF file present in pipeline/full_text/pdfs."
  tracking
}

build_tracking_table <- function(queue, updates, pdf_inventory, pdf_dir) {
  queue <- as_character_frame(queue)
  queue$expected_pdf_filename <- paste0(queue$record_id, ".pdf")
  queue$expected_pdf_path <- project_relative_path(file.path(pdf_dir, queue$expected_pdf_filename))
  queue$pdf_present <- "no"
  queue$retrieval_status <- "not_started"
  queue$retrieval_attempt_date_local <- ""
  queue$online_search_status <- ""
  queue$author_contact_status <- ""
  queue$library_request_status <- ""
  queue$retrieval_sources_checked <- ""
  queue$retrieval_note <- ""
  queue$updated_by <- ""
  queue$updated_at_local <- ""

  missing_updates <- setdiff(updates$record_id, queue$record_id)
  if (length(missing_updates) > 0L) {
    stop(
      "Retrieval status update record_id(s) are absent from the final full-text queue: ",
      paste(missing_updates, collapse = ", "),
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(updates))) {
    row_index <- match(updates$record_id[[i]], queue$record_id)
    for (column in setdiff(RETRIEVAL_UPDATE_COLUMNS, "record_id")) {
      queue[[column]][[row_index]] <- updates[[column]][[i]]
    }
  }

  queue <- apply_pdf_presence(queue, pdf_inventory)
  queue[, RETRIEVAL_TRACKING_COLUMNS, drop = FALSE]
}

build_summary <- function(tracking, pdf_inventory) {
  status_counts <- table(factor(tracking$retrieval_status, levels = allowed_retrieval_statuses()))
  unavailable_statuses <- c("not_found_after_documented_search", "awaiting_library")

  values <- c(
    final_full_text_queue_records = nrow(tracking),
    pdf_files_in_pipeline = nrow(pdf_inventory),
    pdf_files_matched_to_queue = sum(pdf_inventory$matched_queue_record == "yes"),
    records_with_pdf_present = sum(tracking$pdf_present == "yes"),
    records_not_found_after_documented_search = sum(tracking$retrieval_status == "not_found_after_documented_search"),
    records_awaiting_library = sum(tracking$retrieval_status == "awaiting_library"),
    records_not_started_or_unresolved = sum(tracking$retrieval_status == "not_started"),
    records_unavailable_for_full_text_review = sum(tracking$retrieval_status %in% unavailable_statuses)
  )

  values <- c(values, stats::setNames(as.integer(status_counts), paste0("status_", names(status_counts))))
  data.frame(
    metric = names(values),
    value = as.character(unname(values)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, RETRIEVAL_SUMMARY_COLUMNS, drop = FALSE]
}

build_codebook <- function() {
  data.frame(
    field = c(
      rep("retrieval_status", length(allowed_retrieval_statuses())),
      "online_search_status",
      "author_contact_status",
      "library_request_status",
      "pdf_present",
      "expected_pdf_filename"
    ),
    allowed_value = c(
      "not_started",
      "retrieved",
      "not_found_after_documented_search",
      "awaiting_library",
      "author_contact_pending",
      "excluded_before_full_text",
      "free text; recommended values include not_started, not_found_online, found_online",
      "free text; recommended values include not_attempted, author_contact_pending, author_contact_unsuccessful, author_contact_successful",
      "free text; recommended values include not_requested, requested, fulfilled, unavailable",
      "yes/no",
      "REC_PRIMARY_XXXXX.pdf"
    ),
    meaning = c(
      "No documented retrieval attempt or PDF has been recorded yet.",
      "A PDF or complete full-text report is available for full-text review.",
      "The record was retained after title/abstract screening, but full text was not retrieved after documented online searching and/or contact attempts.",
      "A library, interlibrary loan, institutional, or archive request is still pending.",
      "Author contact is pending and retrieval is not closed.",
      "The record was removed before full-text retrieval by an auditable correction, not by CP11.",
      "Operational note describing the online search outcome.",
      "Operational note describing author-contact outcome.",
      "Operational note describing library or institutional request outcome.",
      "Whether the expected PDF is currently present in pipeline/full_text/pdfs.",
      "Recommended deterministic filename for matching PDFs to retained records."
    ),
    used_by = c(
      rep("full_text_retrieval_tracking", length(allowed_retrieval_statuses())),
      "full_text_retrieval_status_updates",
      "full_text_retrieval_status_updates",
      "full_text_retrieval_status_updates",
      "pdf_inventory/full_text_retrieval_tracking",
      "pdf_inventory/full_text_retrieval_tracking"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, RETRIEVAL_CODEBOOK_COLUMNS, drop = FALSE]
}

queue_path <- file.path(CFG$pipeline_dir, "full_text", "final_full_text_retrieval_queue.csv")
tracking_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text", "retrieval_tracking"))
pdf_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text", "pdfs"))
updates_path <- file.path(tracking_dir, "full_text_retrieval_status_updates.csv")

if (!file.exists(queue_path)) {
  stop("Required final full-text queue is missing: ", project_relative_path(queue_path), call. = FALSE)
}
if (!file.exists(updates_path)) {
  stable_write_csv(empty_named_frame(RETRIEVAL_UPDATE_COLUMNS), updates_path, RETRIEVAL_UPDATE_COLUMNS)
}

queue <- read_machine_csv(queue_path)
validate_required_columns(queue, FINAL_FULL_TEXT_QUEUE_COLUMNS, queue_path)
updates <- read_retrieval_updates(updates_path)
pdf_inventory <- build_pdf_inventory(pdf_dir, queue$record_id)
tracking <- build_tracking_table(queue, updates, pdf_inventory, pdf_dir)
summary <- build_summary(tracking, pdf_inventory)
codebook <- build_codebook()
unavailable <- tracking[tracking$retrieval_status %in% c("not_found_after_documented_search", "awaiting_library"), , drop = FALSE]
pending <- tracking[tracking$retrieval_status == "not_started", , drop = FALSE]

tracking_csv <- file.path(tracking_dir, "full_text_retrieval_tracking.csv")
tracking_xlsx <- file.path(tracking_dir, "full_text_retrieval_tracking.xlsx")
summary_csv <- file.path(tracking_dir, "full_text_retrieval_summary.csv")
summary_xlsx <- file.path(tracking_dir, "full_text_retrieval_summary.xlsx")
unavailable_csv <- file.path(tracking_dir, "full_text_unavailable_records.csv")
unavailable_xlsx <- file.path(tracking_dir, "full_text_unavailable_records.xlsx")
pending_csv <- file.path(tracking_dir, "full_text_retrieval_pending_records.csv")
pending_xlsx <- file.path(tracking_dir, "full_text_retrieval_pending_records.xlsx")
pdf_inventory_csv <- file.path(tracking_dir, "pdf_inventory.csv")
pdf_inventory_xlsx <- file.path(tracking_dir, "pdf_inventory.xlsx")
codebook_csv <- file.path(tracking_dir, "full_text_retrieval_codebook.csv")
codebook_xlsx <- file.path(tracking_dir, "full_text_retrieval_codebook.xlsx")

stable_write_csv(tracking, tracking_csv, RETRIEVAL_TRACKING_COLUMNS)
stable_write_csv(summary, summary_csv, RETRIEVAL_SUMMARY_COLUMNS)
stable_write_csv(unavailable, unavailable_csv, RETRIEVAL_TRACKING_COLUMNS)
stable_write_csv(pending, pending_csv, RETRIEVAL_TRACKING_COLUMNS)
stable_write_csv(pdf_inventory, pdf_inventory_csv, PDF_INVENTORY_COLUMNS)
stable_write_csv(codebook, codebook_csv, RETRIEVAL_CODEBOOK_COLUMNS)

write_review_xlsx(list(full_text_retrieval_tracking = tracking), tracking_xlsx)
write_review_xlsx(list(full_text_retrieval_summary = summary), summary_xlsx)
write_review_xlsx(list(full_text_unavailable_records = unavailable), unavailable_xlsx)
write_review_xlsx(list(full_text_retrieval_pending_records = pending), pending_xlsx)
write_review_xlsx(list(pdf_inventory = pdf_inventory), pdf_inventory_xlsx)
write_review_xlsx(list(full_text_retrieval_codebook = codebook), codebook_xlsx)

warnings <- character()
if (nrow(pending) > 0L) {
  warnings <- c(
    warnings,
    paste0(
      nrow(pending),
      " record(s) remain not_started/unresolved. CP11 retrieval is not complete until retrieved PDFs or documented unavailable statuses are recorded."
    )
  )
}
if (nrow(pdf_inventory) == 0L) {
  warnings <- c(warnings, "No PDF files were found in pipeline/full_text/pdfs.")
}

review_file <- write_checkpoint(
  id = "CP11",
  name = "full_text_retrieval_tracking",
  what_ran = paste(
    "Applied documented full-text retrieval status updates to the final",
    "title/abstract queue, inventoried PDFs in pipeline/full_text/pdfs, and",
    "generated reviewer-facing tracking, unavailable-record, pending-record,",
    "PDF-inventory, and summary workbooks."
  ),
  numbers = stats::setNames(summary$value, summary$metric),
  review_items = c(
    "Review full_text_retrieval_tracking.xlsx and confirm every retained record has the correct retrieval status.",
    "Review full_text_unavailable_records.xlsx and confirm unavailable full texts were searched/contacted sufficiently.",
    "Place retrieved PDFs in pipeline/full_text/pdfs using the expected REC_PRIMARY_XXXXX.pdf naming convention.",
    "Rerun CP11 after adding PDFs or additional retrieval-status updates.",
    "Do not treat not_found_after_documented_search as scientific exclusion; it is a retrieval outcome for PRISMA."
  ),
  outputs = c(
    updates_path,
    tracking_csv,
    tracking_xlsx,
    summary_csv,
    summary_xlsx,
    unavailable_csv,
    unavailable_xlsx,
    pending_csv,
    pending_xlsx,
    pdf_inventory_csv,
    pdf_inventory_xlsx,
    codebook_csv,
    codebook_xlsx,
    file.path(CFG$checkpoints_dir, "CP11_full_text_retrieval_tracking.md")
  ),
  warnings = warnings,
  gate_question = paste(
    "Approve CP11 only after all retrievable PDFs are present in",
    "pipeline/full_text/pdfs or all unavailable records have documented",
    "search/contact/library-request outcomes."
  )
)

cat("CP11 full-text retrieval tracking complete\n")
cat("Final full-text queue records:", nrow(tracking), "\n")
cat("PDFs present in pipeline:", nrow(pdf_inventory), "\n")
cat("Records with PDF present:", sum(tracking$pdf_present == "yes"), "\n")
cat("Unavailable after documented search:", sum(tracking$retrieval_status == "not_found_after_documented_search"), "\n")
cat("Not started/unresolved:", nrow(pending), "\n")
cat("Tracking workbook:", project_relative_path(tracking_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
