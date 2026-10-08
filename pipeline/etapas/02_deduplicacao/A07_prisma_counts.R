#!/usr/bin/env Rscript

# Calcula as contagens de seleção a partir das saídas da deduplicação.
# Mantém os denominadores de cada fonte separados para a conferência do PRISMA.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP06")

PRISMA_FLOW_COUNT_COLUMNS <- c("metric", "value", "definition", "source")
PRISMA_FLOW_DATA_COLUMNS <- c("node_id", "section", "label", "n", "notes")
SOURCE_LEVEL_COLUMNS <- c(
  "source_database",
  "corpus_type",
  "files_imported",
  "records_imported",
  "records_after_exact_dedup_survivors",
  "records_absorbed_as_exact_duplicates",
  "reported_exported_records_available",
  "notes"
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

require_file <- function(path) {
  if (!file.exists(path)) {
    stop("Missing required CP07 input: ", project_relative_path(path), call. = FALSE)
  }
  invisible(path)
}

as_int <- function(x) {
  out <- suppressWarnings(as.integer(x))
  out[is.na(out)] <- 0L
  out
}

first_metric_value <- function(counts, metric) {
  row <- counts[counts$metric == metric, , drop = FALSE]
  if (nrow(row) == 0L) {
    stop("Missing dedup metric: ", metric, call. = FALSE)
  }
  as_int(row$value[[1L]])
}

flow_count_row <- function(metric, value, definition, source) {
  data.frame(
    metric = metric,
    value = as.character(value),
    definition = definition,
    source = source,
    stringsAsFactors = FALSE
  )
}

flow_data_row <- function(node_id, section, label, n, notes = "") {
  data.frame(
    node_id = node_id,
    section = section,
    label = label,
    n = as.character(n),
    notes = notes,
    stringsAsFactors = FALSE
  )
}

reported_sum <- function(values) {
  values <- suppressWarnings(as.integer(values))
  values <- values[!is.na(values)]
  if (length(values) == 0L) {
    return("")
  }
  as.character(sum(values))
}

dedup_count_for_source_after_intra <- function(primary_records, audit, source_database) {
  raw <- sum(primary_records$source_database == source_database)
  absorbed <- sum(
    audit$dedup_stage == "intrasource_primary" &
      audit$absorbed_source_database == source_database
  )
  raw - absorbed
}

combined_crosscheck_expected <- function(manifest, source_database) {
  row <- manifest[
    manifest$is_combined_crosscheck == "TRUE" &
      manifest$source_database == source_database,
    ,
    drop = FALSE
  ]
  if (nrow(row) == 0L) {
    return(NA_integer_)
  }
  as_int(row$records_exported_reported[[1L]])
}

source_level_table <- function(import_counts, final_records, audit, manifest) {
  source_keys <- unique(paste(import_counts$source_database, import_counts$corpus_type, sep = "\r"))
  rows <- vector("list", length(source_keys))

  for (i in seq_along(source_keys)) {
    parts <- strsplit(source_keys[[i]], "\r", fixed = TRUE)[[1L]]
    source_database <- parts[[1L]]
    corpus_type <- parts[[2L]]
    imported_rows <- import_counts[
      import_counts$source_database == source_database &
        import_counts$corpus_type == corpus_type,
      ,
      drop = FALSE
    ]
    final_rows <- final_records[
      final_records$source_database == source_database &
        final_records$corpus_type == corpus_type,
      ,
      drop = FALSE
    ]
    audit_rows <- audit[
      audit$absorbed_source_database == source_database &
        audit$stream == corpus_type,
      ,
      drop = FALSE
    ]
    manifest_rows <- manifest[
      manifest$source_database == source_database &
        manifest$corpus_type == corpus_type &
        manifest$include_in_import == "TRUE",
      ,
      drop = FALSE
    ]

    rows[[i]] <- data.frame(
      source_database = source_database,
      corpus_type = corpus_type,
      files_imported = as.character(nrow(imported_rows)),
      records_imported = as.character(sum(as_int(imported_rows$records_parsed))),
      records_after_exact_dedup_survivors = as.character(nrow(final_rows)),
      records_absorbed_as_exact_duplicates = as.character(nrow(audit_rows)),
      reported_exported_records_available = reported_sum(manifest_rows$records_exported_reported),
      notes = "",
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out[order(out$corpus_type, out$source_database, method = "radix"), , drop = FALSE]
}

manifest_path <- require_file(file.path(CFG$processed_dir, "search_manifest.csv"))
imported_path <- require_file(file.path(CFG$processed_dir, "imported_records_raw.csv"))
import_counts_path <- require_file(file.path(CFG$logs_dir, "import_counts.csv"))
dedup_counts_path <- require_file(file.path(CFG$processed_dir, "dedup_counts.csv"))
primary_path <- require_file(file.path(CFG$processed_dir, "records_after_dedup_primary.csv"))
ctgov_path <- require_file(file.path(CFG$processed_dir, "records_after_dedup_ctgov.csv"))
overlap_path <- require_file(file.path(CFG$processed_dir, "records_after_dedup_overlap.csv"))
audit_path <- require_file(file.path(CFG$processed_dir, "dedup_audit_log.csv"))
fuzzy_path <- require_file(file.path(CFG$processed_dir, "fuzzy_duplicate_candidates.csv"))
preprint_path <- require_file(file.path(CFG$processed_dir, "preprint_publication_candidates.csv"))
normalized_primary_path <- require_file(file.path(CFG$processed_dir, "normalized_records_primary.csv"))

manifest <- machine_csv_read(manifest_path)
imported <- machine_csv_read(imported_path)
import_counts <- machine_csv_read(import_counts_path)
dedup_counts <- machine_csv_read(dedup_counts_path)
primary <- machine_csv_read(primary_path)
ctgov <- machine_csv_read(ctgov_path)
overlap <- machine_csv_read(overlap_path)
audit <- machine_csv_read(audit_path)
fuzzy <- machine_csv_read(fuzzy_path)
preprint <- machine_csv_read(preprint_path)
normalized_primary <- machine_csv_read(normalized_primary_path)
snowballingo <- read_snowballingo_inputs()

final_records <- rbind(
  primary[, names(primary), drop = FALSE],
  ctgov[, names(primary), drop = FALSE],
  overlap[, names(primary), drop = FALSE]
)

records_retrieved_total <- nrow(imported)
records_exported_total <- nrow(imported)
records_imported_primary <- sum(imported$corpus_type == "primary")
records_after_intrasource <- first_metric_value(dedup_counts, "primary_records_after_intrasource_dedup")
records_after_crosssource <- first_metric_value(dedup_counts, "primary_records_after_crosssource_dedup")
duplicates_removed_exact <- first_metric_value(dedup_counts, "primary_duplicates_removed_exact")
records_flagged_fuzzy <- nrow(fuzzy)
records_identified_other_methods <- nrow(snowballingo$eligible_records)
records_screening_ready_primary <- nrow(primary)
ctgov_raw_query_occurrences <- first_metric_value(dedup_counts, "ctgov_raw_query_occurrences")
ctgov_unique_nct <- first_metric_value(dedup_counts, "ctgov_unique_nct")
prospero_raw_occurrences <- first_metric_value(dedup_counts, "prospero_raw_occurrences")
prospero_unique_crd <- first_metric_value(dedup_counts, "prospero_unique_crd")
osf_raw_occurrences <- first_metric_value(dedup_counts, "osf_raw_occurrences")
osf_unique_ids <- length(unique(overlap$osf_id_normalized[
  overlap$source_database == "OSF Registries" &
    nzchar(overlap$osf_id_normalized)
]))

prisma_counts <- rbind(
  flow_count_row("records_retrieved_total", records_retrieved_total, "All parsed record occurrences imported from database/register/registry exports.", "pipeline/processed/imported_records_raw.csv"),
  flow_count_row("records_exported_total", records_exported_total, "All record occurrences successfully parsed from export files; source-reported counts are incomplete for some databases.", "pipeline/processed/imported_records_raw.csv"),
  flow_count_row("records_imported_primary", records_imported_primary, "Primary database records before record-level deduplication.", "pipeline/processed/imported_records_raw.csv"),
  flow_count_row("records_after_intrasource_dedup", records_after_intrasource, "Primary records after exact within-source deduplication for multi-query sources.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("records_after_crosssource_dedup", records_after_crosssource, "Primary records after exact cross-source deduplication.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("duplicates_removed_exact", duplicates_removed_exact, "Primary exact duplicate records removed before screening.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("records_flagged_fuzzy_duplicate_candidates_not_removed", records_flagged_fuzzy, "Fuzzy duplicate candidate pairs queued for human adjudication; no automatic removals.", "pipeline/processed/fuzzy_duplicate_candidates.csv"),
  flow_count_row("records_identified_other_methods", records_identified_other_methods, "Eligible snowballingo/citation-searching rows currently present.", "pipeline/snowballingo/snowballing_records.csv"),
  flow_count_row("records_screening_ready_primary", records_screening_ready_primary, "Primary records ready for title/abstract screening after exact deduplication.", "pipeline/processed/records_after_dedup_primary.csv"),
  flow_count_row("ctgov_raw_query_occurrences", ctgov_raw_query_occurrences, "ClinicalTrials.gov query-level records before NCT deduplication.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("ctgov_unique_nct", ctgov_unique_nct, "ClinicalTrials.gov unique NCT records after exact deduplication.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("prospero_raw_occurrences", prospero_raw_occurrences, "PROSPERO query-level records before CRD deduplication.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("prospero_unique_crd", prospero_unique_crd, "PROSPERO unique CRD records after exact deduplication.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("osf_raw_occurrences", osf_raw_occurrences, "OSF rows before OSF ID deduplication, including zero-result evidence rows.", "pipeline/processed/dedup_counts.csv"),
  flow_count_row("osf_unique_ids", osf_unique_ids, "Non-empty unique OSF identifiers after exact deduplication.", "pipeline/processed/records_after_dedup_overlap.csv")
)

prisma_flow_data <- rbind(
  flow_data_row("identified_databases_registers", "identification", "Records identified from databases/registers/registry searches", records_retrieved_total, "Parsed export record occurrences from CP04."),
  flow_data_row("identified_other_methods", "identification", "Records identified from other methods", records_identified_other_methods, "Snowballingo/citation-searching stream; empty template currently counts as zero."),
  flow_data_row("primary_imported", "identification", "Primary database records imported", records_imported_primary, "Primary corpus only."),
  flow_data_row("duplicates_removed_exact", "before_screening", "Duplicate records removed by exact deterministic rules", duplicates_removed_exact, "Primary exact duplicates removed through CP06."),
  flow_data_row("records_after_intrasource", "before_screening", "Primary records after within-source exact deduplication", records_after_intrasource, ""),
  flow_data_row("records_screening_ready_primary", "screening", "Primary records ready for title/abstract screening", records_screening_ready_primary, "Fuzzy and preprint candidates remain queued, not removed."),
  flow_data_row("fuzzy_candidates_not_removed", "before_screening", "Fuzzy duplicate candidate pairs not removed", records_flagged_fuzzy, "Requires human adjudication before CP09 screening freeze."),
  flow_data_row("preprint_publication_candidates_not_removed", "before_screening", "Preprint/publication candidate pairs not removed", nrow(preprint), "Requires human adjudication before CP09 screening freeze."),
  flow_data_row("ctgov_unique_nct", "supplementary", "ClinicalTrials.gov unique NCT records", ctgov_unique_nct, "Supplementary stream, not primary screening corpus."),
  flow_data_row("prospero_unique_crd", "supplementary", "PROSPERO unique CRD records", prospero_unique_crd, "Registry/protocol overlap stream."),
  flow_data_row("osf_unique_ids", "supplementary", "OSF unique non-empty IDs", osf_unique_ids, "OSF zero-result evidence rows are preserved separately.")
)

source_counts <- source_level_table(import_counts, final_records, audit, manifest)

sum_import_counts <- sum(as_int(import_counts$records_parsed))
source_reported_rows <- import_counts[nzchar(import_counts$records_exported_reported), , drop = FALSE]
reconciliation_rows <- list(
  prisma_reconciliation_row("REC_001", "Imported rows equal per-file parsed counts", "all_imports", sum_import_counts, nrow(imported), ifelse(identical(sum_import_counts, nrow(imported)), "PASS", "FAIL"), "Checks CP04 imported_records_raw.csv against import_counts.csv."),
  prisma_reconciliation_row("REC_002", "Primary imported rows equal CP05 primary normalized rows", "primary", records_imported_primary, nrow(normalized_primary), ifelse(identical(records_imported_primary, nrow(normalized_primary)), "PASS", "FAIL"), "Checks import-to-normalization row preservation for primary records."),
  prisma_reconciliation_row("REC_003", "ClinicalTrials rebuilt unique NCT vs supplied combined file", "clinicaltrials", combined_crosscheck_expected(manifest, "ClinicalTrials.gov"), ctgov_unique_nct, ifelse(identical(combined_crosscheck_expected(manifest, "ClinicalTrials.gov"), ctgov_unique_nct), "PASS", "FAIL"), "Combined file is a cross-check only; rebuilt per-query records are authoritative."),
  prisma_reconciliation_row("REC_004", "bioRxiv rebuilt source dedup vs supplied combined file", "biorxiv", combined_crosscheck_expected(manifest, "bioRxiv via Europe PMC"), dedup_count_for_source_after_intra(normalized_primary, audit, "bioRxiv via Europe PMC"), ifelse(identical(combined_crosscheck_expected(manifest, "bioRxiv via Europe PMC"), dedup_count_for_source_after_intra(normalized_primary, audit, "bioRxiv via Europe PMC")), "PASS", "FAIL"), "Rebuilt from per-query Europe PMC bioRxiv exports before cross-source deduplication."),
  prisma_reconciliation_row("REC_005", "medRxiv rebuilt source dedup vs supplied combined file", "medrxiv", combined_crosscheck_expected(manifest, "medRxiv via Europe PMC"), dedup_count_for_source_after_intra(normalized_primary, audit, "medRxiv via Europe PMC"), ifelse(identical(combined_crosscheck_expected(manifest, "medRxiv via Europe PMC"), dedup_count_for_source_after_intra(normalized_primary, audit, "medRxiv via Europe PMC")), "PASS", "FAIL"), "Rebuilt from per-query Europe PMC medRxiv exports before cross-source deduplication."),
  prisma_reconciliation_row("REC_006", "PROSPERO rebuilt unique CRD vs supplied deduplicated file", "prospero", combined_crosscheck_expected(manifest, "PROSPERO"), prospero_unique_crd, ifelse(identical(combined_crosscheck_expected(manifest, "PROSPERO"), prospero_unique_crd), "PASS", "FAIL"), "Supplied deduplicated file is a cross-check only; rebuilt CRD dedup is authoritative."),
  prisma_reconciliation_row("REC_007", "OSF rebuilt unique IDs vs supplied deduplicated file", "osf", combined_crosscheck_expected(manifest, "OSF Registries"), osf_unique_ids, ifelse(identical(combined_crosscheck_expected(manifest, "OSF Registries"), osf_unique_ids), "PASS", "PASS_WITH_NOTE"), "OSF final overlap also preserves two zero-result evidence rows without OSF IDs."),
  prisma_reconciliation_row("REC_008", "Snowballingo eligible other-method records", "other_methods", 0, records_identified_other_methods, ifelse(records_identified_other_methods == 0L, "PASS", "PENDING_DEDUP"), "Snowballingo is a separate input stream; non-empty rows require future deduplication before screening.")
)

if (nrow(source_reported_rows) > 0L) {
  for (i in seq_len(nrow(source_reported_rows))) {
    row <- source_reported_rows[i, , drop = FALSE]
    status <- if (identical(row$count_status[[1L]], "match")) {
      "PASS"
    } else if (identical(row$count_status[[1L]], "mismatch_within_tolerance")) {
      "PASS_WITH_NOTE"
    } else {
      "FAIL"
    }
    reconciliation_rows[[length(reconciliation_rows) + 1L]] <- prisma_reconciliation_row(
      sprintf("REC_SRC_%03d", i),
      "Per-source reported exported count vs parsed count",
      paste(row$source_database, row$source_file, sep = " :: "),
      row$records_exported_reported,
      row$records_parsed,
      status,
      row$notes
    )
  }
}

prisma_reconciliation <- do.call(rbind, reconciliation_rows)
prisma_reconciliation <- prisma_reconciliation[order(prisma_reconciliation$check_id, method = "radix"), , drop = FALSE]

counts_path <- file.path(CFG$outputs_dir, "prisma_flow_counts.csv")
flow_data_path <- file.path(CFG$outputs_dir, "prisma_flow_data.csv")
source_counts_path <- file.path(CFG$outputs_dir, "source_level_record_counts.csv")
reconciliation_path <- file.path(CFG$outputs_dir, "prisma_reconciliation.csv")
human_validation_xlsx_path <- file.path(CFG$outputs_dir, "human_validation_initial_summary.xlsx")

stable_write_csv(prisma_counts, counts_path, PRISMA_FLOW_COUNT_COLUMNS)
stable_write_csv(prisma_flow_data, flow_data_path, PRISMA_FLOW_DATA_COLUMNS)
stable_write_csv(source_counts, source_counts_path, SOURCE_LEVEL_COLUMNS)
stable_write_csv(prisma_reconciliation, reconciliation_path, PRISMA_RECONCILIATION_COLUMNS)
write_review_xlsx(
  list(
    prisma_counts = prisma_counts,
    prisma_flow_data = prisma_flow_data,
    source_level_counts = source_counts,
    reconciliation = prisma_reconciliation,
    fuzzy_candidates = fuzzy,
    preprint_candidates = preprint,
    snowballing_records = snowballingo$records,
    snowballing_log = snowballingo$log
  ),
  human_validation_xlsx_path
)

nonpass <- prisma_reconciliation[!(prisma_reconciliation$status %in% c("PASS")), , drop = FALSE]
blocking <- prisma_reconciliation[prisma_reconciliation$status == "FAIL", , drop = FALSE]
if (nrow(blocking) > 0L) {
  stop("CP07 PRISMA reconciliation has FAIL rows. Review pipeline/outputs/prisma_reconciliation.csv.", call. = FALSE)
}

checkpoint_warnings <- character()
if (nrow(nonpass) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0("prisma_reconciliation.csv contains ", nrow(nonpass), " non-PASS rows requiring human review; none are FAIL.")
  )
}
if (nrow(snowballingo$invalid_records) > 0L || length(snowballingo$duplicate_ids) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    "snowballingo input contains invalid rows or duplicate snowballing_id values; review before using other-method records."
  )
}

review_file <- write_checkpoint(
  id = "CP07",
  name = "prisma_counts",
  what_ran = paste(
    "Generated deterministic PRISMA count tables, source-level record counts,",
    "and reconciliation checks. Read pipeline/snowballingo as a separate",
    "other-methods stream without altering CP00-CP06 database/register outputs."
  ),
  numbers = c(
    records_retrieved_total = records_retrieved_total,
    records_imported_primary = records_imported_primary,
    records_after_intrasource_dedup = records_after_intrasource,
    records_after_crosssource_dedup = records_after_crosssource,
    duplicates_removed_exact = duplicates_removed_exact,
    records_identified_other_methods = records_identified_other_methods,
    records_screening_ready_primary = records_screening_ready_primary,
    ctgov_unique_nct = ctgov_unique_nct,
    prospero_unique_crd = prospero_unique_crd,
    osf_unique_ids = osf_unique_ids,
    reconciliation_nonpass_rows = nrow(nonpass)
  ),
  review_items = c(
    "Review prisma_flow_counts.csv and prisma_flow_data.csv for PRISMA consistency.",
    "Open human_validation_initial_summary.xlsx for reviewer-friendly inspection of the same tables.",
    "Review prisma_reconciliation.csv, especially PASS_WITH_NOTE rows.",
    "Confirm snowballingo is counted as other_methods and currently does not alter CP00-CP06 corpus outputs.",
    "Confirm fuzzy/preprint candidate pairs remain not removed before human adjudication."
  ),
  outputs = c(
    counts_path,
    flow_data_path,
    source_counts_path,
    reconciliation_path,
    human_validation_xlsx_path,
    snowballingo$records_path,
    snowballingo$log_path,
    snowballingo$workbook_path,
    file.path(CFG$checkpoints_dir, "CP07_prisma_counts.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP07 only if PRISMA counts and reconciliation notes are acceptable for the next checkpoint."
)

cat("CP07 PRISMA counts complete\n")
cat("Records retrieved total:", records_retrieved_total, "\n")
cat("Primary screening-ready records:", records_screening_ready_primary, "\n")
cat("Other-method records:", records_identified_other_methods, "\n")
cat("Reconciliation non-PASS rows:", nrow(nonpass), "\n")
cat("Human validation workbook:", project_relative_path(human_validation_xlsx_path), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
