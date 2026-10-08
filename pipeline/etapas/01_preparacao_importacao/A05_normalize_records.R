#!/usr/bin/env Rscript

# Padroniza os campos usados na identificação e comparação dos registros.
# Os campos de origem são mantidos para permitir a conferência das transformações.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP04")

NORMALIZED_RECORD_COLUMNS <- c(
  "record_source_id",
  "manifest_id",
  "corpus_type",
  "source_database",
  "query_label",
  "source_file",
  "raw_record_index",
  "title",
  "authors",
  "first_author",
  "year",
  "publication_date",
  "journal_or_source",
  "doi",
  "pmid",
  "pmcid",
  "arxiv_id",
  "nct_id",
  "crd_id",
  "osf_id",
  "url",
  "abstract",
  "keywords",
  "language",
  "publication_type",
  "document_type",
  "raw_record_text",
  "raw_record_json",
  "import_warning",
  "import_error",
  "doi_normalized",
  "pmid_normalized",
  "arxiv_id_normalized",
  "nct_id_normalized",
  "crd_id_normalized",
  "osf_id_normalized",
  "title_normalized",
  "first_author_normalized",
  "id_key"
)

NORMALIZATION_WARNING_COLUMNS <- c(
  "warning_id",
  "severity",
  "record_source_id",
  "manifest_id",
  "source_file",
  "warning_type",
  "details"
)

machine_csv_read <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character"
  )
}

empty_normalized_records <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(NORMALIZED_RECORD_COLUMNS)), NORMALIZED_RECORD_COLUMNS),
    stringsAsFactors = FALSE
  )
}

empty_normalization_warnings <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(NORMALIZATION_WARNING_COLUMNS)), NORMALIZATION_WARNING_COLUMNS),
    stringsAsFactors = FALSE
  )
}

require_columns <- function(data, columns, path) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      "Input file is missing required columns: ",
      paste(missing, collapse = ", "),
      " in ",
      project_relative_path(path),
      call. = FALSE
    )
  }
}

warning_rows_for <- function(records, indexes, severity, warning_type, details) {
  if (length(indexes) == 0L) {
    return(empty_normalization_warnings())
  }

  data.frame(
    warning_id = "",
    severity = severity,
    record_source_id = records$record_source_id[indexes],
    manifest_id = records$manifest_id[indexes],
    source_file = records$source_file[indexes],
    warning_type = warning_type,
    details = details,
    stringsAsFactors = FALSE
  )
}

derive_id_keys <- function(records) {
  n <- nrow(records)
  keys <- character(n)
  methods <- character(n)
  source_database_key <- normalize_compact_key(records$source_database)
  source_identifier_key <- normalize_source_fallback(records$source_record_id, records$raw_record_id)
  journal_key <- normalize_text_key(records$journal_or_source)

  for (i in seq_len(n)) {
    if (nzchar(records$doi_normalized[[i]])) {
      keys[[i]] <- paste0("doi:", records$doi_normalized[[i]])
      methods[[i]] <- "doi"
    } else if (nzchar(records$pmid_normalized[[i]])) {
      keys[[i]] <- paste0("pmid:", records$pmid_normalized[[i]])
      methods[[i]] <- "pmid"
    } else if (nzchar(records$arxiv_id_normalized[[i]])) {
      keys[[i]] <- paste0("arxiv:", records$arxiv_id_normalized[[i]])
      methods[[i]] <- "arxiv"
    } else if (
      identical(records$corpus_type[[i]], "clinical_trial_registry_supplementary") &&
        nzchar(records$nct_id_normalized[[i]])
    ) {
      keys[[i]] <- paste0("nct:", records$nct_id_normalized[[i]])
      methods[[i]] <- "nct_supplementary"
    } else if (
      identical(records$corpus_type[[i]], "review_registry_overlap") &&
        nzchar(records$crd_id_normalized[[i]])
    ) {
      keys[[i]] <- paste0("crd:", records$crd_id_normalized[[i]])
      methods[[i]] <- "crd_overlap"
    } else if (
      identical(records$corpus_type[[i]], "review_registry_overlap") &&
        nzchar(records$osf_id_normalized[[i]])
    ) {
      keys[[i]] <- paste0("osf:", records$osf_id_normalized[[i]])
      methods[[i]] <- "osf_overlap"
    } else if (
      nzchar(records$title_normalized[[i]]) &&
        nzchar(records$first_author_normalized[[i]]) &&
        nzchar(records$year[[i]])
    ) {
      keys[[i]] <- paste(
        "title_author_year",
        records$title_normalized[[i]],
        records$first_author_normalized[[i]],
        records$year[[i]],
        sep = ":"
      )
      methods[[i]] <- "title_author_year"
    } else if (
      nzchar(records$title_normalized[[i]]) &&
        nzchar(records$year[[i]]) &&
        nzchar(journal_key[[i]])
    ) {
      keys[[i]] <- paste(
        "title_year_source",
        records$title_normalized[[i]],
        records$year[[i]],
        journal_key[[i]],
        sep = ":"
      )
      methods[[i]] <- "title_year_source"
    } else if (nzchar(source_identifier_key[[i]])) {
      keys[[i]] <- paste(
        "source_record",
        source_database_key[[i]],
        source_identifier_key[[i]],
        sep = ":"
      )
      methods[[i]] <- "source_record_fallback"
    } else {
      keys[[i]] <- paste0("record_source:", records$record_source_id[[i]])
      methods[[i]] <- "record_source_fallback"
    }
  }

  data.frame(id_key = keys, id_key_method = methods, stringsAsFactors = FALSE)
}

imported_path <- file.path(CFG$processed_dir, "imported_records_raw.csv")
if (!file.exists(imported_path)) {
  stop("Missing CP04 output: ", project_relative_path(imported_path), call. = FALSE)
}

imported <- machine_csv_read(imported_path)
required_imported_columns <- c(
  "record_source_id",
  "manifest_id",
  "source_database",
  "corpus_type",
  "query_label",
  "source_file",
  "raw_record_index",
  "raw_record_id",
  "title",
  "authors",
  "year",
  "publication_date",
  "journal_or_source",
  "doi",
  "pmid",
  "pmcid",
  "arxiv_id",
  "nct_id",
  "crd_id",
  "osf_id",
  "url",
  "abstract",
  "keywords",
  "language",
  "publication_type",
  "document_type",
  "source_record_id",
  "raw_record_text",
  "raw_record_json",
  "import_warning",
  "import_error"
)
require_columns(imported, required_imported_columns, imported_path)

normalized <- data.frame(
  record_source_id = imported$record_source_id,
  manifest_id = imported$manifest_id,
  corpus_type = imported$corpus_type,
  source_database = imported$source_database,
  query_label = imported$query_label,
  source_file = imported$source_file,
  raw_record_index = imported$raw_record_index,
  raw_record_id = norm_space(imported$raw_record_id),
  source_record_id = norm_space(imported$source_record_id),
  title = norm_space(imported$title),
  authors = norm_space(imported$authors),
  first_author = extract_first_author(imported$authors),
  year = normalize_year(imported$publication_date, imported$year),
  publication_date = norm_space(imported$publication_date),
  journal_or_source = norm_space(imported$journal_or_source),
  doi = norm_space(imported$doi),
  pmid = norm_space(imported$pmid),
  pmcid = norm_space(imported$pmcid),
  arxiv_id = norm_space(imported$arxiv_id),
  nct_id = norm_space(imported$nct_id),
  crd_id = norm_space(imported$crd_id),
  osf_id = norm_space(imported$osf_id),
  url = norm_space(imported$url),
  abstract = norm_space(imported$abstract),
  keywords = norm_space(imported$keywords),
  language = norm_space(imported$language),
  publication_type = norm_space(imported$publication_type),
  document_type = norm_space(imported$document_type),
  raw_record_text = imported$raw_record_text,
  raw_record_json = imported$raw_record_json,
  import_warning = norm_space(imported$import_warning),
  import_error = norm_space(imported$import_error),
  doi_normalized = normalize_doi(imported$doi, imported$raw_record_id, imported$source_record_id, imported$url, imported$raw_record_text),
  pmid_normalized = normalize_pmid(imported$pmid),
  arxiv_id_normalized = normalize_arxiv_id(imported$arxiv_id, imported$url, imported$raw_record_id, imported$source_record_id),
  nct_id_normalized = normalize_nct_id(imported$nct_id, imported$url, imported$raw_record_text),
  crd_id_normalized = normalize_crd_id(imported$crd_id, imported$url, imported$raw_record_json, imported$raw_record_text),
  osf_id_normalized = normalize_osf_id(imported$osf_id, imported$url),
  title_normalized = normalize_text_key(imported$title),
  first_author_normalized = normalize_first_author(imported$authors),
  id_key = "",
  stringsAsFactors = FALSE
)

id_key_info <- derive_id_keys(normalized)
normalized$id_key <- id_key_info$id_key

normalization_warnings <- rbind(
  warning_rows_for(
    normalized,
    which(!nzchar(normalized$title_normalized)),
    "warning",
    "missing_title",
    "Normalized title is empty; review imported source metadata before screening."
  ),
  warning_rows_for(
    normalized,
    which(!nzchar(normalized$year)),
    "warning",
    "unparsed_year",
    "No four-digit publication year could be derived from publication_date or year."
  ),
  warning_rows_for(
    normalized,
    which(id_key_info$id_key_method == "title_year_source"),
    "warning",
    "id_key_title_year_source_fallback",
    "Author key was unavailable; id_key used title, year, and journal/source for deterministic matching."
  ),
  warning_rows_for(
    normalized,
    which(id_key_info$id_key_method %in% c("source_record_fallback", "record_source_fallback")),
    "warning",
    "id_key_source_fallback",
    "Bibliographic/registry/title-based key was unavailable; id_key used source-specific record identity."
  ),
  warning_rows_for(
    normalized,
    which(!nzchar(normalized$id_key)),
    "error",
    "missing_id_key",
    "No deterministic id_key could be derived."
  )
)

if (nrow(normalization_warnings) > 0L) {
  normalization_warnings <- normalization_warnings[
    order(
      normalization_warnings$severity,
      normalization_warnings$record_source_id,
      normalization_warnings$warning_type,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  normalization_warnings$warning_id <- sprintf("NORMWARN_%05d", seq_len(nrow(normalization_warnings)))
}

normalized <- normalized[order(normalized$record_source_id, method = "radix"), NORMALIZED_RECORD_COLUMNS, drop = FALSE]

normalized_all_path <- file.path(CFG$processed_dir, "normalized_records_all.csv")
normalized_primary_path <- file.path(CFG$processed_dir, "normalized_records_primary.csv")
normalized_ctgov_path <- file.path(CFG$processed_dir, "normalized_records_ctgov.csv")
normalized_overlap_path <- file.path(CFG$processed_dir, "normalized_records_overlap.csv")
normalized_other_methods_path <- file.path(CFG$processed_dir, "normalized_records_other_methods.csv")
warnings_path <- file.path(CFG$logs_dir, "normalization_warnings.csv")

normalized_primary <- normalized[normalized$corpus_type == "primary", , drop = FALSE]
normalized_ctgov <- normalized[normalized$corpus_type == "clinical_trial_registry_supplementary", , drop = FALSE]
normalized_overlap <- normalized[normalized$corpus_type == "review_registry_overlap", , drop = FALSE]
normalized_other_methods <- normalized[
  !(normalized$corpus_type %in% c("primary", "clinical_trial_registry_supplementary", "review_registry_overlap")),
  ,
  drop = FALSE
]

stable_write_csv(normalized, normalized_all_path, NORMALIZED_RECORD_COLUMNS)
stable_write_csv(normalized_primary, normalized_primary_path, NORMALIZED_RECORD_COLUMNS)
stable_write_csv(normalized_ctgov, normalized_ctgov_path, NORMALIZED_RECORD_COLUMNS)
stable_write_csv(normalized_overlap, normalized_overlap_path, NORMALIZED_RECORD_COLUMNS)
stable_write_csv(normalized_other_methods, normalized_other_methods_path, NORMALIZED_RECORD_COLUMNS)
stable_write_csv(normalization_warnings, warnings_path, NORMALIZATION_WARNING_COLUMNS)

missing_id_key_count <- sum(!nzchar(normalized$id_key))
unresolved_errors <- sum(normalization_warnings$severity == "error")

checkpoint_warnings <- character()
if (nrow(normalization_warnings) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0("normalization_warnings.csv contains ", nrow(normalization_warnings), " rows; review missing metadata and fallback id_key rows before CP06.")
  )
}

review_file <- write_checkpoint(
  id = "CP05",
  name = "normalize",
  what_ran = paste(
    "Normalized imported records into the canonical schema; derived DOI, PMID,",
    "arXiv, NCT, CRD, OSF, title, author, year, and id_key fields; and wrote",
    "corpus-specific normalized record files with logged fallback keys when",
    "source metadata were incomplete."
  ),
  numbers = c(
    records_input = nrow(imported),
    records_normalized = nrow(normalized),
    primary_records = nrow(normalized_primary),
    ctgov_records = nrow(normalized_ctgov),
    overlap_records = nrow(normalized_overlap),
    other_methods_records = nrow(normalized_other_methods),
    missing_title = sum(!nzchar(normalized$title_normalized)),
    unparsed_year = sum(!nzchar(normalized$year)),
    missing_id_key = missing_id_key_count,
    warning_rows = nrow(normalization_warnings)
  ),
  review_items = c(
    "Confirm every record has a non-empty id_key.",
    "Review normalization_warnings.csv, especially missing title/year and source fallback id_key rows.",
    "Confirm corpus-specific normalized files preserve primary, ClinicalTrials, and registry-overlap streams separately.",
    "Confirm no eligibility, duplicate adjudication, or risk-of-bias decisions were made."
  ),
  outputs = c(
    normalized_all_path,
    normalized_primary_path,
    normalized_ctgov_path,
    normalized_overlap_path,
    normalized_other_methods_path,
    warnings_path,
    file.path(CFG$checkpoints_dir, "CP05_normalize.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP05 only if normalized keys and corpus splits are acceptable for CP06 exact deduplication."
)

if (unresolved_errors > 0L || missing_id_key_count > 0L) {
  stop("CP05 normalization produced unresolved key errors. Diagnostics were written to pipeline/logs/normalization_warnings.csv.", call. = FALSE)
}

cat("CP05 normalization complete\n")
cat("Records normalized:", nrow(normalized), "\n")
cat("Primary records:", nrow(normalized_primary), "\n")
cat("ClinicalTrials supplementary records:", nrow(normalized_ctgov), "\n")
cat("Registry overlap records:", nrow(normalized_overlap), "\n")
cat("Warning rows:", nrow(normalization_warnings), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
