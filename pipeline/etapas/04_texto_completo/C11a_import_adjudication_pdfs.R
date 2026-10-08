#!/usr/bin/env Rscript

# Importa os documentos fornecidos para a conferência dos textos completos.
# Mantém o vínculo entre o arquivo recebido e o registro bibliográfico correspondente.


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

PDF_IMPORT_AUDIT_COLUMNS <- c(
  "record_id",
  "title",
  "duplicate_group_record_ids",
  "matched_source_record_id",
  "source_pdf_filename",
  "source_pdf_path",
  "target_pdf_filename",
  "target_pdf_path",
  "copy_status",
  "source_md5",
  "target_md5",
  "source_size_bytes",
  "target_size_bytes",
  "note"
)

UNMATCHED_PDF_COLUMNS <- c(
  "source_pdf_filename",
  "source_record_id",
  "source_pdf_path",
  "source_md5",
  "source_size_bytes",
  "note"
)

split_record_ids <- function(x) {
  ids <- unlist(strsplit(norm_empty_if_na(x), ";\\s*", perl = TRUE), use.names = FALSE)
  ids <- trimws(ids)
  ids[nzchar(ids)]
}

choose_source_pdf <- function(record_id, duplicate_group_record_ids, pdf_by_id) {
  candidates <- unique(c(record_id, split_record_ids(duplicate_group_record_ids)))
  candidates <- candidates[nzchar(candidates)]
  available <- candidates[candidates %in% names(pdf_by_id)]
  if (length(available) == 0L) {
    return(list(record_id = "", path = "", note = "No matching source PDF found."))
  }
  if (record_id %in% available) {
    return(list(record_id = record_id, path = pdf_by_id[[record_id]], note = "Matched retained record_id."))
  }
  available <- sort(available, method = "radix")
  list(
    record_id = available[[1L]],
    path = pdf_by_id[[available[[1L]]]],
    note = "Matched absorbed duplicate record_id; target filename normalized to retained record_id."
  )
}

default_source_pdf_dir <- file.path(CFG$pipeline_dir, "full_text", "pdf_import_staging")
source_pdf_dir <- Sys.getenv("AITHERMO_SOURCE_PDF_DIR", unset = default_source_pdf_dir)
source_pdf_dir <- normalizePath(source_pdf_dir, mustWork = TRUE)
target_pdf_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text", "pdfs"))
tracking_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text", "retrieval_tracking"))
queue_path <- file.path(CFG$pipeline_dir, "full_text", "final_full_text_retrieval_queue.csv")

if (!file.exists(queue_path)) {
  stop("Required final full-text queue is missing: ", project_relative_path(queue_path), call. = FALSE)
}

queue <- read_machine_csv(queue_path)
validate_required_columns(queue, FINAL_FULL_TEXT_QUEUE_COLUMNS, queue_path)
queue <- as_character_frame(queue[, FINAL_FULL_TEXT_QUEUE_COLUMNS, drop = FALSE])

source_pdfs <- list.files(source_pdf_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
source_ids <- sub("\\.pdf$", "", basename(source_pdfs), ignore.case = TRUE)
pdf_by_id <- stats::setNames(source_pdfs, source_ids)

if (any(duplicated(source_ids))) {
  duplicated_ids <- unique(source_ids[duplicated(source_ids)])
  stop(
    "Source PDF folder contains duplicate REC_PRIMARY PDF names: ",
    paste(duplicated_ids, collapse = ", "),
    call. = FALSE
  )
}

audit_rows <- vector("list", nrow(queue))
used_source_ids <- character()

for (i in seq_len(nrow(queue))) {
  record_id <- queue$record_id[[i]]
  selected <- choose_source_pdf(record_id, queue$duplicate_group_record_ids[[i]], pdf_by_id)
  target_path <- file.path(target_pdf_dir, paste0(record_id, ".pdf"))

  copy_status <- "missing_source_pdf"
  source_md5 <- ""
  source_size <- ""
  target_md5 <- ""
  target_size <- ""
  source_filename <- ""
  source_path_display <- ""
  note <- selected$note

  if (nzchar(selected$path)) {
    used_source_ids <- c(used_source_ids, selected$record_id)
    source_filename <- basename(selected$path)
    source_path_display <- selected$path
    source_md5 <- file_md5(selected$path)
    source_size <- as.character(file.info(selected$path)$size)

    if (file.exists(target_path) && identical(file_md5(target_path), source_md5)) {
      copy_status <- "already_present_identical"
    } else {
      copied <- file.copy(selected$path, target_path, overwrite = TRUE, copy.mode = FALSE, copy.date = FALSE)
      if (!isTRUE(copied)) {
        stop("Failed to copy PDF for record_id: ", record_id, call. = FALSE)
      }
      copy_status <- "copied"
    }

    target_md5 <- file_md5(target_path)
    target_size <- as.character(file.info(target_path)$size)
  }

  audit_rows[[i]] <- data.frame(
    record_id = record_id,
    title = queue$title[[i]],
    duplicate_group_record_ids = queue$duplicate_group_record_ids[[i]],
    matched_source_record_id = selected$record_id,
    source_pdf_filename = source_filename,
    source_pdf_path = source_path_display,
    target_pdf_filename = paste0(record_id, ".pdf"),
    target_pdf_path = project_relative_path(target_path),
    copy_status = copy_status,
    source_md5 = source_md5,
    target_md5 = target_md5,
    source_size_bytes = source_size,
    target_size_bytes = target_size,
    note = note,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

audit <- do.call(rbind, audit_rows)
audit <- audit[, PDF_IMPORT_AUDIT_COLUMNS, drop = FALSE]

unmatched_ids <- setdiff(source_ids, used_source_ids)
if (length(unmatched_ids) > 0L) {
  unmatched_rows <- lapply(sort(unmatched_ids, method = "radix"), function(id) {
    path <- pdf_by_id[[id]]
    data.frame(
      source_pdf_filename = basename(path),
      source_record_id = id,
      source_pdf_path = path,
      source_md5 = file_md5(path),
      source_size_bytes = as.character(file.info(path)$size),
      note = "Source PDF did not match any retained record_id or duplicate_group_record_ids in the final full-text queue.",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  unmatched <- do.call(rbind, unmatched_rows)
} else {
  unmatched <- empty_named_frame(UNMATCHED_PDF_COLUMNS)
}
unmatched <- unmatched[, UNMATCHED_PDF_COLUMNS, drop = FALSE]

audit_csv <- file.path(tracking_dir, "pdf_import_from_adjudication_audit.csv")
audit_xlsx <- file.path(tracking_dir, "pdf_import_from_adjudication_audit.xlsx")
unmatched_csv <- file.path(tracking_dir, "pdf_import_from_adjudication_unmatched.csv")
unmatched_xlsx <- file.path(tracking_dir, "pdf_import_from_adjudication_unmatched.xlsx")

stable_write_csv(audit, audit_csv, PDF_IMPORT_AUDIT_COLUMNS)
stable_write_csv(unmatched, unmatched_csv, UNMATCHED_PDF_COLUMNS)
write_review_xlsx(list(pdf_import_from_adjudication_audit = audit), audit_xlsx)
write_review_xlsx(list(pdf_import_from_adjudication_unmatched = unmatched), unmatched_xlsx)

cat("CP11A import adjudication PDFs complete\n")
cat("Source PDF folder:", source_pdf_dir, "\n")
cat("Source PDFs:", length(source_pdfs), "\n")
cat("Queue records:", nrow(queue), "\n")
cat("Copied:", sum(audit$copy_status == "copied"), "\n")
cat("Already present identical:", sum(audit$copy_status == "already_present_identical"), "\n")
cat("Missing source PDFs:", sum(audit$copy_status == "missing_source_pdf"), "\n")
cat("Unmatched source PDFs:", nrow(unmatched), "\n")
cat("Audit workbook:", project_relative_path(audit_xlsx), "\n")
