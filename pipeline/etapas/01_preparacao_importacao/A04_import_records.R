#!/usr/bin/env Rscript

# Importa os registros dos arquivos aprovados no manifesto de buscas.
# Cada registro mantém sua fonte e sua posição no arquivo original.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP03")

IMPORTED_RECORD_COLUMNS <- c(
  "record_source_id",
  "manifest_id",
  "source_database",
  "source_bucket_normalized",
  "corpus_type",
  "query_label",
  "source_file",
  "raw_record_index",
  "raw_record_id",
  "parser",
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

IMPORT_COUNT_COLUMNS <- c(
  "manifest_id",
  "source_database",
  "corpus_type",
  "source_file",
  "parser",
  "records_exported_reported",
  "records_parsed",
  "count_difference",
  "parser_shortfall_fraction",
  "count_status",
  "notes"
)

IMPORT_WARNING_COLUMNS <- c(
  "warning_id",
  "severity",
  "manifest_id",
  "source_file",
  "raw_record_index",
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

empty_imported_records <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(IMPORTED_RECORD_COLUMNS)), IMPORTED_RECORD_COLUMNS),
    stringsAsFactors = FALSE
  )
}

empty_import_warnings <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(IMPORT_WARNING_COLUMNS)), IMPORT_WARNING_COLUMNS),
    stringsAsFactors = FALSE
  )
}

warning_row <- function(severity, manifest_id, source_file, raw_record_index, warning_type, details) {
  data.frame(
    warning_id = "",
    severity = severity,
    manifest_id = manifest_id,
    source_file = source_file,
    raw_record_index = as.character(raw_record_index),
    warning_type = warning_type,
    details = details,
    stringsAsFactors = FALSE
  )
}

as_reported_count <- function(value) {
  value <- trimws(as.character(value))
  if (!nzchar(value)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(value))
}

count_status_for_file <- function(reported, parsed_count) {
  if (is.na(reported)) {
    return(c(status = "not_reported", difference = "", shortfall = ""))
  }

  difference <- parsed_count - reported
  shortfall <- if (reported <= 0) 0 else max(reported - parsed_count, 0) / reported
  status <- "match"
  if (difference != 0) {
    status <- "mismatch_within_tolerance"
  }
  if (shortfall > CFG$parser_shortfall_tolerance) {
    status <- "shortfall_exceeds_tolerance"
  }
  c(status = status, difference = as.character(difference), shortfall = sprintf("%.6f", shortfall))
}

add_provenance <- function(parsed, manifest_row) {
  if (nrow(parsed) == 0L) {
    return(empty_imported_records())
  }

  parsed <- parsed[, PARSER_OUTPUT_COLUMNS, drop = FALSE]
  out <- data.frame(
    record_source_id = "",
    manifest_id = manifest_row$manifest_id,
    source_database = manifest_row$source_database,
    source_bucket_normalized = manifest_row$source_bucket_normalized,
    corpus_type = manifest_row$corpus_type,
    query_label = manifest_row$query_label,
    source_file = manifest_row$canonical_relative_path,
    raw_record_index = parsed$raw_record_index,
    raw_record_id = parsed$raw_record_id,
    parser = manifest_row$parser,
    stringsAsFactors = FALSE
  )

  parsed_payload <- parsed[, setdiff(PARSER_OUTPUT_COLUMNS, c("raw_record_index", "raw_record_id")), drop = FALSE]
  out <- cbind(out, parsed_payload, stringsAsFactors = FALSE)
  out$raw_record_id[!nzchar(out$raw_record_id)] <- paste0(out$manifest_id[!nzchar(out$raw_record_id)], ":", out$raw_record_index[!nzchar(out$raw_record_id)])
  out[, IMPORTED_RECORD_COLUMNS, drop = FALSE]
}

manifest_path <- file.path(CFG$processed_dir, "search_manifest.csv")
if (!file.exists(manifest_path)) {
  stop("CP03 output is missing: ", manifest_path, call. = FALSE)
}

manifest <- machine_csv_read(manifest_path)
included <- manifest[manifest$include_in_import == "TRUE", , drop = FALSE]
included <- included[order(included$manifest_id, method = "radix"), , drop = FALSE]

all_records <- list()
count_rows <- list()
warning_rows <- list()
parser_errors <- character()

for (i in seq_len(nrow(included))) {
  manifest_row <- included[i, , drop = FALSE]
  source_file <- manifest_row$canonical_relative_path[[1L]]
  source_path <- manifest_row$canonical_absolute_path[[1L]]
  parser <- manifest_row$parser[[1L]]

  parsed <- tryCatch(
    dispatch_manifest_parser(source_path, parser),
    error = function(e) {
      parser_errors <<- c(parser_errors, paste0(manifest_row$manifest_id[[1L]], ": ", conditionMessage(e)))
      empty_parser_output()
    }
  )

  records_with_provenance <- add_provenance(parsed, manifest_row)
  all_records[[length(all_records) + 1L]] <- records_with_provenance

  reported <- as_reported_count(manifest_row$records_exported_reported[[1L]])
  status <- count_status_for_file(reported, nrow(parsed))
  count_rows[[length(count_rows) + 1L]] <- data.frame(
    manifest_id = manifest_row$manifest_id,
    source_database = manifest_row$source_database,
    corpus_type = manifest_row$corpus_type,
    source_file = source_file,
    parser = parser,
    records_exported_reported = ifelse(is.na(reported), "", as.character(reported)),
    records_parsed = as.character(nrow(parsed)),
    count_difference = status[["difference"]],
    parser_shortfall_fraction = status[["shortfall"]],
    count_status = status[["status"]],
    notes = ifelse(status[["status"]] == "not_reported", "No machine-readable source count available in manifest.", ""),
    stringsAsFactors = FALSE
  )

  if (length(parser_errors) > 0L && grepl(paste0("^", manifest_row$manifest_id[[1L]], ":"), parser_errors[[length(parser_errors)]])) {
    warning_rows[[length(warning_rows) + 1L]] <- warning_row(
      severity = "error",
      manifest_id = manifest_row$manifest_id[[1L]],
      source_file = source_file,
      raw_record_index = "",
      warning_type = "parser_error",
      details = parser_errors[[length(parser_errors)]]
    )
  }

  if (nrow(records_with_provenance) > 0L) {
    missing_title <- which(!nzchar(records_with_provenance$title))
    missing_year <- which(!nzchar(records_with_provenance$year))
    parser_warnings <- which(nzchar(records_with_provenance$import_warning))
    parser_record_errors <- which(nzchar(records_with_provenance$import_error))

    if (length(missing_title) > 0L) {
      warning_rows <- c(warning_rows, lapply(missing_title, function(row_index) {
        warning_row("warning", manifest_row$manifest_id[[1L]], source_file, records_with_provenance$raw_record_index[[row_index]], "missing_title", "Parsed record has an empty title field.")
      }))
    }
    if (length(missing_year) > 0L) {
      warning_rows <- c(warning_rows, lapply(missing_year, function(row_index) {
        warning_row("warning", manifest_row$manifest_id[[1L]], source_file, records_with_provenance$raw_record_index[[row_index]], "missing_year", "Parsed record has an empty year field.")
      }))
    }
    if (length(parser_warnings) > 0L) {
      warning_rows <- c(warning_rows, lapply(parser_warnings, function(row_index) {
        warning_row("warning", manifest_row$manifest_id[[1L]], source_file, records_with_provenance$raw_record_index[[row_index]], "parser_warning", records_with_provenance$import_warning[[row_index]])
      }))
    }
    if (length(parser_record_errors) > 0L) {
      warning_rows <- c(warning_rows, lapply(parser_record_errors, function(row_index) {
        warning_row("error", manifest_row$manifest_id[[1L]], source_file, records_with_provenance$raw_record_index[[row_index]], "record_parse_error", records_with_provenance$import_error[[row_index]])
      }))
    }
  }
}

imported <- if (length(all_records) == 0L) {
  empty_imported_records()
} else {
  do.call(rbind, all_records)
}

if (nrow(imported) > 0L) {
  imported <- imported[order(
    imported$source_database,
    imported$manifest_id,
    as.integer(imported$raw_record_index),
    imported$raw_record_id,
    method = "radix"
  ), , drop = FALSE]
  imported$record_source_id <- sprintf("SRC_%07d", seq_len(nrow(imported)))
}

import_counts <- if (length(count_rows) == 0L) {
  as.data.frame(setNames(rep(list(character()), length(IMPORT_COUNT_COLUMNS)), IMPORT_COUNT_COLUMNS), stringsAsFactors = FALSE)
} else {
  do.call(rbind, count_rows)
}
import_counts <- import_counts[order(import_counts$manifest_id, method = "radix"), , drop = FALSE]

import_warnings <- if (length(warning_rows) == 0L) {
  empty_import_warnings()
} else {
  do.call(rbind, warning_rows)
}
if (nrow(import_warnings) > 0L) {
  import_warnings <- import_warnings[order(import_warnings$severity, import_warnings$manifest_id, import_warnings$raw_record_index, import_warnings$warning_type, method = "radix"), , drop = FALSE]
  import_warnings$warning_id <- sprintf("IMPORTWARN_%05d", seq_len(nrow(import_warnings)))
}

shortfall_errors <- import_counts$count_status == "shortfall_exceeds_tolerance"
parser_error_count <- sum(import_warnings$severity == "error")
if (any(shortfall_errors)) {
  import_warnings <- rbind(
    import_warnings,
    do.call(rbind, lapply(which(shortfall_errors), function(row_index) {
      warning_row(
        severity = "error",
        manifest_id = import_counts$manifest_id[[row_index]],
        source_file = import_counts$source_file[[row_index]],
        raw_record_index = "",
        warning_type = "parser_shortfall_exceeds_tolerance",
        details = paste0(
          "Reported records = ", import_counts$records_exported_reported[[row_index]],
          "; parsed records = ", import_counts$records_parsed[[row_index]],
          "; shortfall fraction = ", import_counts$parser_shortfall_fraction[[row_index]], "."
        )
      )
    }))
  )
  import_warnings <- import_warnings[order(import_warnings$severity, import_warnings$manifest_id, import_warnings$raw_record_index, import_warnings$warning_type, method = "radix"), , drop = FALSE]
  import_warnings$warning_id <- sprintf("IMPORTWARN_%05d", seq_len(nrow(import_warnings)))
}

imported_path <- file.path(CFG$processed_dir, "imported_records_raw.csv")
counts_path <- file.path(CFG$logs_dir, "import_counts.csv")
warnings_path <- file.path(CFG$logs_dir, "import_warnings.csv")

stable_write_csv(imported, imported_path, IMPORTED_RECORD_COLUMNS)
stable_write_csv(import_counts, counts_path, IMPORT_COUNT_COLUMNS)
stable_write_csv(import_warnings, warnings_path, IMPORT_WARNING_COLUMNS)

unresolved_errors <- sum(import_warnings$severity == "error")
if (unresolved_errors > 0L) {
  stop("CP04 import produced unresolved parser/count errors. Diagnostics were written to pipeline/logs/import_warnings.csv.", call. = FALSE)
}

checkpoint_warnings <- character()
if (nrow(import_warnings) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0("import_warnings.csv contains ", nrow(import_warnings), " non-fatal warning rows; review missing title/year fields before screening.")
  )
}

review_file <- write_checkpoint(
  id = "CP04",
  name = "import",
  what_ran = paste(
    "Imported only manifest rows with include_in_import = TRUE, dispatched each",
    "file to its declared parser, preserved source provenance, assigned stable",
    "record_source_id values after sorting, and wrote per-file count diagnostics."
  ),
  numbers = c(
    files_imported = nrow(included),
    records_imported = nrow(imported),
    count_rows = nrow(import_counts),
    warning_rows = nrow(import_warnings),
    missing_title = sum(import_warnings$warning_type == "missing_title"),
    missing_year = sum(import_warnings$warning_type == "missing_year"),
    parser_errors = unresolved_errors
  ),
  review_items = c(
    "Confirm sum(import_counts$records_parsed) equals rows in imported_records_raw.csv.",
    "Review import_warnings.csv for missing title/year fields before CP05 normalization.",
    "Confirm no parser_shortfall_exceeds_tolerance or parser_error rows are present.",
    "Confirm ClinicalTrials, PROSPERO, and OSF remain separate corpus types."
  ),
  outputs = c(
    imported_path,
    counts_path,
    warnings_path,
    file.path(CFG$checkpoints_dir, "CP04_import.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP04 only if all parser outputs and count diagnostics are acceptable for CP05 normalization."
)

cat("CP04 import complete\n")
cat("Files imported:", nrow(included), "\n")
cat("Records imported:", nrow(imported), "\n")
cat("Warning rows:", nrow(import_warnings), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
