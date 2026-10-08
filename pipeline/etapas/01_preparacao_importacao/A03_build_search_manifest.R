#!/usr/bin/env Rscript

# Organiza o manifesto das buscas e define quais arquivos entram na importação.
# Mantém separados os registros bibliográficos, os registros de ensaios e as buscas de sobreposição.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP02")

MANIFEST_COLUMNS <- c(
  "manifest_id",
  "source_bucket_raw",
  "source_bucket_normalized",
  "source_database",
  "corpus_type",
  "query_label",
  "canonical_relative_path",
  "canonical_absolute_path",
  "file_format",
  "parser",
  "md5",
  "sha256",
  "records_retrieved_reported",
  "records_exported_reported",
  "export_role",
  "include_in_import",
  "empty_result_export",
  "is_redundant_file_copy",
  "is_api_provenance",
  "is_combined_crosscheck",
  "invalid_reason",
  "notes"
)

VALIDATION_COLUMNS <- c(
  "check_id",
  "severity",
  "check_name",
  "status",
  "n_affected",
  "details"
)

manifest_export_role <- function(row) {
  role <- row[["role"]]
  file_name <- row[["file_name"]]
  source_bucket <- row[["source_bucket_normalized"]]
  is_ct_empty <- file_dedup_is_ct_empty_query_evidence(source_bucket, row[["relative_path"]], role) &&
    grepl("_results[.]csv$", file_name)

  if (identical(role, "export") && is_ct_empty) {
    return("empty_result_export")
  }
  if (identical(role, "export")) {
    return("primary_export")
  }
  if (identical(role, "api_provenance")) {
    return("api_provenance")
  }
  if (identical(role, "combined_crosscheck")) {
    return("combined_crosscheck")
  }
  if (identical(role, "log")) {
    return("search_log")
  }
  if (identical(role, "script")) {
    return("script")
  }
  if (identical(role, "doc")) {
    return("protocol_document")
  }
  if (identical(role, "os_metadata")) {
    return("os_metadata")
  }
  if (identical(role, "directory")) {
    return("excluded_nondata")
  }
  "invalid_attempt"
}

manifest_query_label <- function(file_name) {
  label <- gsub("[.](bibtex|bib|ris|csv|nbib|xml|json|txt)$", "", file_name, ignore.case = TRUE)
  label <- sub("^[0-9]{4}-[0-9]{2}-[0-9]{2}_", "", label)
  label <- sub("^[0-9]{8}_", "", label)
  label <- gsub("_v[0-9]+([_.][0-9]+)?", "", label)
  label <- gsub("_page_[0-9]{4}$", "", label)
  label <- gsub("_results$", "", label)
  label <- gsub("[.](bibtex|bib|ris|csv|nbib|xml|json|txt)$", "", label, ignore.case = TRUE)
  label <- sub("[.]+$", "", label)
  label <- gsub("[[:space:]]+", " ", label)
  trimws(label)
}

count_csv_records <- function(path) {
  if (!file.exists(path)) {
    return("")
  }
  result <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    return("")
  }
  as.character(nrow(result))
}

manifest_invalid_reason <- function(export_role, include_in_import, is_redundant, parser, route_known) {
  if (identical(include_in_import, "TRUE")) {
    return("")
  }
  if (identical(export_role, "empty_result_export") && identical(is_redundant, "TRUE")) {
    return("empty_result_export_no_records; redundant_file_copy")
  }
  if (identical(export_role, "empty_result_export")) {
    return("empty_result_export_no_records")
  }
  if (identical(is_redundant, "TRUE")) {
    return("redundant_file_copy")
  }
  if (identical(export_role, "api_provenance")) {
    return("api_provenance_not_imported")
  }
  if (identical(export_role, "combined_crosscheck")) {
    return("combined_crosscheck_not_primary_input")
  }
  if (identical(export_role, "search_log")) {
    return("search_log_not_imported")
  }
  if (identical(export_role, "script")) {
    return("script_not_imported")
  }
  if (identical(export_role, "protocol_document")) {
    return("protocol_document_not_imported")
  }
  if (identical(export_role, "os_metadata")) {
    return("os_metadata_not_imported")
  }
  if (!route_known || !nzchar(parser)) {
    return("no_parser_for_source_or_format")
  }
  "excluded_nondata"
}

manifest_notes <- function(export_role, include_in_import, is_redundant, empty_result, canonical_relative_path, relative_path) {
  notes <- paste0("Original BUSCAS file: ", relative_path, ".")
  if (identical(include_in_import, "TRUE")) {
    notes <- c(notes, "Canonical import input selected by CP03.")
  }
  if (identical(is_redundant, "TRUE")) {
    notes <- c(notes, paste0("Byte-identical redundant copy; canonical file is ", canonical_relative_path, "."))
  }
  if (identical(empty_result, "TRUE")) {
    notes <- c(notes, "ClinicalTrials query executed with zero exported records; retained as evidence.")
  }
  if (identical(export_role, "api_provenance")) {
    notes <- c(notes, "JSON API page retained as provenance; CSV result export is canonical input when present.")
  }
  if (identical(export_role, "combined_crosscheck")) {
    notes <- c(notes, "Supplied combined/deduplicated file retained only for reconciliation cross-check.")
  }
  if (identical(export_role, "search_log")) {
    notes <- c(notes, "Search log or manifest retained for audit trail.")
  }
  if (length(notes) == 0L) {
    notes <- "Excluded non-import file retained for audit visibility."
  }
  paste(unique(notes), collapse = " ")
}

validation_row <- function(check_id, severity, check_name, status, n_affected, details) {
  data.frame(
    check_id = check_id,
    severity = severity,
    check_name = check_name,
    status = status,
    n_affected = as.character(n_affected),
    details = details,
    stringsAsFactors = FALSE
  )
}

dedup_path <- file.path(CFG$processed_dir, "file_dedup_decisions.csv")
inventory_path <- file.path(CFG$processed_dir, "inventory_verified.csv")
if (!file.exists(inventory_path)) {
  stop("CP01 output is missing: ", inventory_path, call. = FALSE)
}
if (!file.exists(dedup_path)) {
  stop("CP02 output is missing: ", dedup_path, call. = FALSE)
}

dedup <- utils::read.csv(dedup_path, stringsAsFactors = FALSE, check.names = FALSE)
dedup_files <- dedup[dedup$item_type == "file", , drop = FALSE]
dedup_files <- dedup_files[order(dedup_files$relative_path, method = "radix"), , drop = FALSE]

routes <- lapply(dedup_files$source_bucket_normalized, source_route_for_bucket)
source_database <- vapply(routes, `[[`, character(1L), "source_database")
corpus_type <- vapply(routes, `[[`, character(1L), "corpus_type")
parser <- vapply(routes, `[[`, character(1L), "parser")
route_known <- nzchar(source_database)

export_role <- apply(dedup_files, 1L, manifest_export_role)
empty_result_export <- export_role == "empty_result_export"
is_redundant <- dedup_files$is_redundant_file_copy == "yes"
is_api_provenance <- export_role == "api_provenance"
is_combined_crosscheck <- export_role == "combined_crosscheck"
file_format <- mapply(file_format_from_extension, dedup_files$file_extension, dedup_files$file_name, USE.NAMES = FALSE)
canonical_relative_path <- ifelse(nzchar(dedup_files$canonical_relative_path), dedup_files$canonical_relative_path, dedup_files$relative_path)
canonical_absolute_path <- normalizePath(file.path(CFG$buscas_dir, canonical_relative_path), mustWork = FALSE)

include_in_import <- export_role == "primary_export" &
  !is_redundant &
  route_known &
  nzchar(parser)

records_exported_reported <- rep("", nrow(dedup_files))
csv_count_rows <- which(export_role %in% c("primary_export", "empty_result_export", "combined_crosscheck") & file_format == "CSV")
if (length(csv_count_rows) > 0L) {
  records_exported_reported[csv_count_rows] <- vapply(
    canonical_absolute_path[csv_count_rows],
    count_csv_records,
    character(1L)
  )
}

manifest <- data.frame(
  manifest_id = "",
  source_bucket_raw = dedup_files$source_bucket,
  source_bucket_normalized = dedup_files$source_bucket_normalized,
  source_database = ifelse(route_known, source_database, "BUSCAS audit/nondata"),
  corpus_type = corpus_type,
  query_label = vapply(dedup_files$file_name, manifest_query_label, character(1L)),
  canonical_relative_path = canonical_relative_path,
  canonical_absolute_path = canonical_absolute_path,
  file_format = file_format,
  parser = ifelse(export_role %in% c("primary_export", "empty_result_export", "combined_crosscheck"), parser, ""),
  md5 = dedup_files$md5,
  sha256 = dedup_files$sha256,
  records_retrieved_reported = "",
  records_exported_reported = records_exported_reported,
  export_role = export_role,
  include_in_import = ifelse(include_in_import, "TRUE", "FALSE"),
  empty_result_export = ifelse(empty_result_export, "TRUE", "FALSE"),
  is_redundant_file_copy = ifelse(is_redundant, "TRUE", "FALSE"),
  is_api_provenance = ifelse(is_api_provenance, "TRUE", "FALSE"),
  is_combined_crosscheck = ifelse(is_combined_crosscheck, "TRUE", "FALSE"),
  invalid_reason = mapply(
    manifest_invalid_reason,
    export_role = export_role,
    include_in_import = ifelse(include_in_import, "TRUE", "FALSE"),
    is_redundant = ifelse(is_redundant, "TRUE", "FALSE"),
    parser = parser,
    route_known = route_known,
    USE.NAMES = FALSE
  ),
  notes = mapply(
    manifest_notes,
    export_role = export_role,
    include_in_import = ifelse(include_in_import, "TRUE", "FALSE"),
    is_redundant = ifelse(is_redundant, "TRUE", "FALSE"),
    empty_result = ifelse(empty_result_export, "TRUE", "FALSE"),
    canonical_relative_path = canonical_relative_path,
    relative_path = dedup_files$relative_path,
    USE.NAMES = FALSE
  ),
  stringsAsFactors = FALSE
)

manifest <- manifest[order(
  manifest$source_bucket_normalized,
  manifest$export_role,
  manifest$canonical_relative_path,
  manifest$query_label,
  method = "radix"
), , drop = FALSE]
manifest$manifest_id <- sprintf("MANIFEST_%05d", seq_len(nrow(manifest)))

included <- manifest$include_in_import == "TRUE"
validation <- list(
  validation_row(
    "MANIFEST_CHECK_001",
    "info",
    "all_buscas_files_represented",
    ifelse(nrow(manifest) == sum(dedup$item_type == "file"), "PASS", "FAIL"),
    nrow(manifest),
    "Manifest should include every file row from file_dedup_decisions.csv."
  ),
  validation_row(
    "MANIFEST_CHECK_002",
    "error",
    "included_files_have_parser",
    ifelse(all(nzchar(manifest$parser[included])), "PASS", "FAIL"),
    sum(included & !nzchar(manifest$parser)),
    "Every included file must declare the parser that CP04 will dispatch."
  ),
  validation_row(
    "MANIFEST_CHECK_003",
    "error",
    "included_files_have_source_database",
    ifelse(all(nzchar(manifest$source_database[included])), "PASS", "FAIL"),
    sum(included & !nzchar(manifest$source_database)),
    "Every included file must be mapped to a source database."
  ),
  validation_row(
    "MANIFEST_CHECK_004",
    "error",
    "included_files_are_not_redundant",
    ifelse(!any(included & manifest$is_redundant_file_copy == "TRUE"), "PASS", "FAIL"),
    sum(included & manifest$is_redundant_file_copy == "TRUE"),
    "Redundant byte-identical copies must not be imported."
  ),
  validation_row(
    "MANIFEST_CHECK_005",
    "error",
    "api_provenance_not_imported",
    ifelse(!any(included & manifest$is_api_provenance == "TRUE"), "PASS", "FAIL"),
    sum(included & manifest$is_api_provenance == "TRUE"),
    "JSON API pages are provenance only when CSV result exports exist."
  ),
  validation_row(
    "MANIFEST_CHECK_006",
    "error",
    "combined_crosscheck_not_imported",
    ifelse(!any(included & manifest$is_combined_crosscheck == "TRUE"), "PASS", "FAIL"),
    sum(included & manifest$is_combined_crosscheck == "TRUE"),
    "Supplied combined/deduplicated files are reconciliation cross-checks only."
  ),
  validation_row(
    "MANIFEST_CHECK_007",
    "error",
    "empty_result_exports_not_imported",
    ifelse(!any(included & manifest$empty_result_export == "TRUE"), "PASS", "FAIL"),
    sum(included & manifest$empty_result_export == "TRUE"),
    "Zero-row ClinicalTrials result exports remain query evidence but do not enter record import."
  ),
  validation_row(
    "MANIFEST_CHECK_008",
    "error",
    "clinicaltrials_empty_exports_marked",
    ifelse(sum(manifest$empty_result_export == "TRUE") == 3L, "PASS", "FAIL"),
    sum(manifest$empty_result_export == "TRUE"),
    "ClinicalTrials result exports for queries 08, 15, and 18 must be marked empty_result_export."
  ),
  validation_row(
    "MANIFEST_CHECK_009",
    "error",
    "canonical_paths_exist",
    ifelse(all(file.exists(manifest$canonical_absolute_path)), "PASS", "FAIL"),
    sum(!file.exists(manifest$canonical_absolute_path)),
    "Every manifest canonical path must resolve under BUSCAS."
  ),
  validation_row(
    "MANIFEST_CHECK_010",
    "error",
    "manifest_ids_unique",
    ifelse(!anyDuplicated(manifest$manifest_id), "PASS", "FAIL"),
    anyDuplicated(manifest$manifest_id),
    "Manifest identifiers must be unique after stable sorting."
  )
)

validation_log <- do.call(rbind, validation)
failed_validation <- validation_log[validation_log$status != "PASS", , drop = FALSE]
warnings <- if (nrow(failed_validation) == 0L) {
  character()
} else {
  paste(failed_validation$check_id, failed_validation$check_name, failed_validation$details)
}

manifest_path <- file.path(CFG$processed_dir, "search_manifest.csv")
validation_path <- file.path(CFG$logs_dir, "manifest_validation_log.csv")

stable_write_csv(manifest, manifest_path, MANIFEST_COLUMNS)
stable_write_csv(validation_log, validation_path, VALIDATION_COLUMNS)

review_file <- write_checkpoint(
  id = "CP03",
  name = "search_manifest",
  what_ran = paste(
    "Built the single search manifest from CP01 inventory verification and CP02",
    "file-level deduplication decisions. Each BUSCAS file is now routed as an",
    "importable canonical export, provenance, cross-check, log, script, document,",
    "metadata, or excluded nondata file."
  ),
  numbers = c(
    manifest_rows = nrow(manifest),
    include_in_import = sum(manifest$include_in_import == "TRUE"),
    primary_exports = sum(manifest$export_role == "primary_export"),
    empty_result_exports = sum(manifest$empty_result_export == "TRUE"),
    api_provenance = sum(manifest$is_api_provenance == "TRUE"),
    combined_crosscheck = sum(manifest$is_combined_crosscheck == "TRUE"),
    validation_failures = nrow(failed_validation),
    warnings = length(warnings)
  ),
  review_items = c(
    "Confirm all included files have source_database, corpus_type, file_format, and parser.",
    "Confirm JSON API pages are provenance only and combined/deduplicated files are cross-checks only.",
    "Confirm ClinicalTrials 08, 15, and 18 are marked as empty_result_export and not imported as records.",
    "Review manifest_validation_log.csv before CP04 parser implementation."
  ),
  outputs = c(
    manifest_path,
    validation_path,
    file.path(CFG$checkpoints_dir, "CP03_search_manifest.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = warnings,
  gate_question = "Approve CP03 only if the manifest routing is correct for CP04 import."
)

cat("CP03 search manifest complete\n")
cat("Manifest rows:", nrow(manifest), "\n")
cat("Include in import:", sum(manifest$include_in_import == "TRUE"), "\n")
cat("Validation failures:", nrow(failed_validation), "\n")
cat("Warnings:", length(warnings), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
