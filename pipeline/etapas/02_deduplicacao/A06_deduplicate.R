#!/usr/bin/env Rscript

# Aplica as regras de deduplicação e registra o destino de cada registro repetido.
# Pares semelhantes e relações entre preprint e publicação seguem para conferência separada.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP05")

DEDUP_AUDIT_COLUMNS <- c(
  "audit_id",
  "dedup_group_id",
  "stream",
  "dedup_stage",
  "dedup_key_type",
  "dedup_key",
  "survivor_record_id",
  "survivor_record_source_id",
  "survivor_source_database",
  "survivor_source_file",
  "absorbed_record_source_id",
  "absorbed_source_database",
  "absorbed_source_file",
  "absorbed_raw_record_index",
  "absorbed_title",
  "action",
  "reason"
)

DEDUP_COUNT_COLUMNS <- c("metric", "value", "notes")

CANDIDATE_COLUMNS <- c(
  "candidate_id",
  "candidate_class",
  "band",
  "similarity",
  "match_rule",
  "record_id_1",
  "record_source_id_1",
  "source_database_1",
  "year_1",
  "first_author_normalized_1",
  "title_1",
  "record_id_2",
  "record_source_id_2",
  "source_database_2",
  "year_2",
  "first_author_normalized_2",
  "title_2",
  "adjudication_decision",
  "retained_record_id",
  "reviewer",
  "date",
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

empty_audit_log <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(DEDUP_AUDIT_COLUMNS)), DEDUP_AUDIT_COLUMNS),
    stringsAsFactors = FALSE
  )
}

empty_candidate_log <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(CANDIDATE_COLUMNS)), CANDIDATE_COLUMNS),
    stringsAsFactors = FALSE
  )
}

dedup_count_row <- function(metric, value, notes = "") {
  data.frame(
    metric = metric,
    value = as.character(value),
    notes = notes,
    stringsAsFactors = FALSE
  )
}

dedup_candidate_row <- function(
  candidate_class,
  band,
  similarity,
  match_rule,
  record_1,
  record_2
) {
  data.frame(
    candidate_id = "",
    candidate_class = candidate_class,
    band = band,
    similarity = sprintf("%.6f", similarity),
    match_rule = match_rule,
    record_id_1 = record_1$record_id,
    record_source_id_1 = record_1$record_source_id,
    source_database_1 = record_1$source_database,
    year_1 = record_1$year,
    first_author_normalized_1 = record_1$first_author_normalized,
    title_1 = record_1$title,
    record_id_2 = record_2$record_id,
    record_source_id_2 = record_2$record_source_id,
    source_database_2 = record_2$source_database,
    year_2 = record_2$year,
    first_author_normalized_2 = record_2$first_author_normalized,
    title_2 = record_2$title,
    adjudication_decision = "",
    retained_record_id = "",
    reviewer = "",
    date = "",
    notes = "",
    stringsAsFactors = FALSE
  )
}

as_year_number <- function(x) {
  out <- suppressWarnings(as.integer(x))
  out[is.na(out)] <- .Machine$integer.max
  out
}

key_label_for_id_key <- function(id_key) {
  key_type <- record_dedup_key_type(id_key)
  ifelse(key_type %in% c("doi", "pmid", "arxiv", "title_author_year", "title_year_source"), key_type, "other")
}

apply_exact_dedup <- function(records, stream, stage, dedup_key_type, dedup_key, eligible, group_counter_start = 0L) {
  if (nrow(records) == 0L) {
    return(list(records = records, audit = empty_audit_log(), group_counter = group_counter_start))
  }

  eligible <- eligible & nzchar(dedup_key)
  grouped_keys <- sort(unique(dedup_key[eligible & duplicated(dedup_key) | eligible & duplicated(dedup_key, fromLast = TRUE)]), method = "radix")
  keep <- rep(TRUE, nrow(records))
  audit_rows <- list()
  group_counter <- group_counter_start

  for (key in grouped_keys) {
    indexes <- which(eligible & dedup_key == key)
    if (length(indexes) <= 1L) {
      next
    }

    group_counter <- group_counter + 1L
    group_id <- sprintf("DEDUPG_%05d", group_counter)
    survivor_index <- indexes[[record_dedup_survivor_index(records[indexes, , drop = FALSE])]]
    absorbed_indexes <- setdiff(indexes, survivor_index)
    keep[absorbed_indexes] <- FALSE

    for (absorbed_index in absorbed_indexes) {
      audit_rows[[length(audit_rows) + 1L]] <- data.frame(
        audit_id = "",
        dedup_group_id = group_id,
        stream = stream,
        dedup_stage = stage,
        dedup_key_type = dedup_key_type[[survivor_index]],
        dedup_key = key,
        survivor_record_id = "",
        survivor_record_source_id = records$record_source_id[[survivor_index]],
        survivor_source_database = records$source_database[[survivor_index]],
        survivor_source_file = records$source_file[[survivor_index]],
        absorbed_record_source_id = records$record_source_id[[absorbed_index]],
        absorbed_source_database = records$source_database[[absorbed_index]],
        absorbed_source_file = records$source_file[[absorbed_index]],
        absorbed_raw_record_index = records$raw_record_index[[absorbed_index]],
        absorbed_title = records$title[[absorbed_index]],
        action = "absorbed_exact_duplicate",
        reason = paste0("Exact deduplication by ", dedup_key_type[[survivor_index]], " during ", stage, "."),
        stringsAsFactors = FALSE
      )
    }
  }

  audit <- if (length(audit_rows) > 0L) {
    do.call(rbind, audit_rows)
  } else {
    empty_audit_log()
  }

  list(
    records = records[keep, , drop = FALSE],
    audit = audit,
    group_counter = group_counter
  )
}

assign_record_ids <- function(records, prefix) {
  assign_stable_record_ids(records, prefix, "record_source_id")
}

year_within_one <- function(a, b) {
  a_num <- suppressWarnings(as.integer(a))
  b_num <- suppressWarnings(as.integer(b))
  !is.na(a_num) && !is.na(b_num) && abs(a_num - b_num) <= 1L
}

title_length_ratio_ok <- function(a, b, minimum = 0.70) {
  a_len <- nchar(a, type = "chars", allowNA = FALSE, keepNA = FALSE)
  b_len <- nchar(b, type = "chars", allowNA = FALSE, keepNA = FALSE)
  max_len <- max(a_len, b_len)
  if (max_len == 0L) {
    return(FALSE)
  }
  min(a_len, b_len) / max_len >= minimum
}

title_block_token <- function(title) {
  tokens <- strsplit(title, " ", fixed = TRUE)[[1L]]
  tokens <- tokens[nzchar(tokens)]
  stop_tokens <- c(
    "a", "an", "and", "are", "as", "by", "for", "from", "in", "into", "meta",
    "of", "on", "or", "review", "study", "systematic", "the", "to", "using",
    "with"
  )
  informative <- tokens[!(tokens %in% stop_tokens) & nchar(tokens) >= 4L]
  if (length(informative) > 0L) {
    return(informative[[1L]])
  }
  if (length(tokens) > 0L) {
    return(tokens[[1L]])
  }
  ""
}

add_pair_keys <- function(pair_env, indexes) {
  indexes <- sort(unique(indexes))
  if (length(indexes) < 2L) {
    return(invisible(pair_env))
  }

  for (i_pos in seq_len(length(indexes) - 1L)) {
    for (j_pos in (i_pos + 1L):length(indexes)) {
      key <- paste(indexes[[i_pos]], indexes[[j_pos]], sep = "||")
      pair_env[[key]] <- TRUE
    }
  }
  invisible(pair_env)
}

generate_fuzzy_candidates <- function(records) {
  if (nrow(records) < 2L) {
    return(empty_candidate_log())
  }

  rows <- list()
  titles <- records$title_normalized
  authors <- records$first_author_normalized
  years <- suppressWarnings(as.integer(records$year))
  block_tokens <- vapply(titles, title_block_token, character(1L))
  pair_env <- new.env(parent = emptyenv())

  author_groups <- split(seq_len(nrow(records)), authors)
  for (author in names(author_groups)) {
    if (nzchar(author)) {
      add_pair_keys(pair_env, author_groups[[author]])
    }
  }

  title_token_groups <- split(seq_len(nrow(records)), block_tokens)
  for (token in names(title_token_groups)) {
    if (!nzchar(token)) {
      next
    }
    indexes <- title_token_groups[[token]]
    if (length(indexes) < 2L) {
      next
    }
    for (year in sort(unique(years[indexes][!is.na(years[indexes])]), method = "radix")) {
      add_pair_keys(pair_env, indexes[!is.na(years[indexes]) & abs(years[indexes] - year) <= 1L])
    }
  }

  pair_keys <- ls(pair_env, all.names = TRUE)
  if (length(pair_keys) == 0L) {
    return(empty_candidate_log())
  }

  for (pair_key in pair_keys) {
    pair <- as.integer(strsplit(pair_key, "||", fixed = TRUE)[[1L]])
    i <- pair[[1L]]
    j <- pair[[2L]]

    if (!nzchar(titles[[i]])) {
      next
    }
    if (!nzchar(titles[[j]])) {
      next
    }

    same_author <- nzchar(authors[[i]]) && identical(authors[[i]], authors[[j]])
    close_year <- year_within_one(records$year[[i]], records$year[[j]])
    if (!same_author && !close_year) {
      next
    }
    if (!title_length_ratio_ok(titles[[i]], titles[[j]])) {
      next
    }

    similarity <- record_jaro_winkler_similarity(titles[[i]], titles[[j]])
    if (similarity < CFG$fuzzy_review_threshold) {
      next
    }

    band <- if (similarity >= CFG$fuzzy_high_threshold) "high" else "review"
    match_rule <- paste(c(if (same_author) "same_first_author", if (close_year) "year_difference_le_1"), collapse = ";")
    rows[[length(rows) + 1L]] <- dedup_candidate_row(
      "fuzzy_duplicate",
      band,
      similarity,
      match_rule,
      records[i, , drop = FALSE],
      records[j, , drop = FALSE]
    )
  }

  if (length(rows) == 0L) {
    return(empty_candidate_log())
  }

  out <- do.call(rbind, rows)
  out <- out[order(out$band, -as.numeric(out$similarity), out$record_id_1, out$record_id_2, method = "radix"), , drop = FALSE]
  out$candidate_id <- sprintf("FUZZY_%05d", seq_len(nrow(out)))
  out
}

doi_base_key <- function(doi) {
  doi <- norm_empty_if_na(doi)
  doi <- sub("([._-]?v(er)?[0-9]+)$", "", doi, perl = TRUE)
  doi
}

generate_preprint_publication_candidates <- function(records) {
  if (nrow(records) < 2L) {
    return(empty_candidate_log())
  }

  is_preprint <- record_dedup_is_preprint(records)
  preprint_indexes <- which(is_preprint)
  publication_indexes <- which(!is_preprint)
  if (length(preprint_indexes) == 0L || length(publication_indexes) == 0L) {
    return(empty_candidate_log())
  }

  rows <- list()
  titles <- records$title_normalized
  authors <- records$first_author_normalized
  doi_base <- doi_base_key(records$doi_normalized)

  for (i in preprint_indexes) {
    if (!nzchar(titles[[i]])) {
      next
    }
    for (j in publication_indexes) {
      if (!nzchar(titles[[j]])) {
        next
      }

      same_author <- nzchar(authors[[i]]) && identical(authors[[i]], authors[[j]])
      same_doi_base <- nzchar(doi_base[[i]]) && identical(doi_base[[i]], doi_base[[j]])
      if (!same_author && !same_doi_base) {
        next
      }
      if (!title_length_ratio_ok(titles[[i]], titles[[j]], minimum = 0.65)) {
        next
      }

      similarity <- record_jaro_winkler_similarity(titles[[i]], titles[[j]])
      if (similarity < CFG$preprint_publication_threshold) {
        next
      }

      match_rule <- paste(c(if (same_author) "same_first_author", if (same_doi_base) "same_doi_base"), collapse = ";")
      rows[[length(rows) + 1L]] <- dedup_candidate_row(
        "preprint_publication",
        "candidate",
        similarity,
        match_rule,
        records[i, , drop = FALSE],
        records[j, , drop = FALSE]
      )
    }
  }

  if (length(rows) == 0L) {
    return(empty_candidate_log())
  }

  out <- do.call(rbind, rows)
  out <- out[order(-as.numeric(out$similarity), out$record_id_1, out$record_id_2, method = "radix"), , drop = FALSE]
  out$candidate_id <- sprintf("PREPRINT_%05d", seq_len(nrow(out)))
  out
}

normalized_path <- file.path(CFG$processed_dir, "normalized_records_all.csv")
if (!file.exists(normalized_path)) {
  stop("Missing CP05 output: ", project_relative_path(normalized_path), call. = FALSE)
}

records <- machine_csv_read(normalized_path)
if (!"id_key" %in% names(records)) {
  stop("normalized_records_all.csv is missing id_key.", call. = FALSE)
}
if (any(!nzchar(records$id_key))) {
  stop("CP06 cannot run because at least one normalized record has an empty id_key.", call. = FALSE)
}

primary <- records[records$corpus_type == "primary", , drop = FALSE]
ctgov <- records[records$corpus_type == "clinical_trial_registry_supplementary", , drop = FALSE]
overlap <- records[records$corpus_type == "review_registry_overlap", , drop = FALSE]

audit_logs <- list()
group_counter <- 0L

multiquery_primary_sources <- c(
  "ACM Digital Library",
  "Compendex/Engineering Village",
  "IEEE Xplore",
  "bioRxiv via Europe PMC",
  "medRxiv via Europe PMC"
)

primary_intra_key <- paste(primary$source_database, primary$id_key, sep = "||")
primary_intra_type <- paste0("source_plus_", key_label_for_id_key(primary$id_key))
primary_intra_eligible <- primary$source_database %in% multiquery_primary_sources &
  key_label_for_id_key(primary$id_key) %in% c("doi", "pmid", "arxiv", "title_author_year", "title_year_source")

primary_intra_result <- apply_exact_dedup(
  primary,
  stream = "primary",
  stage = "intrasource_primary",
  dedup_key_type = primary_intra_type,
  dedup_key = primary_intra_key,
  eligible = primary_intra_eligible,
  group_counter_start = group_counter
)
primary_after_intra <- primary_intra_result$records
audit_logs[[length(audit_logs) + 1L]] <- primary_intra_result$audit
group_counter <- primary_intra_result$group_counter

primary_cross_key_type <- key_label_for_id_key(primary_after_intra$id_key)
primary_cross_eligible <- primary_cross_key_type %in% c("doi", "pmid", "arxiv", "title_author_year", "title_year_source")
primary_cross_result <- apply_exact_dedup(
  primary_after_intra,
  stream = "primary",
  stage = "crosssource_primary",
  dedup_key_type = primary_cross_key_type,
  dedup_key = primary_after_intra$id_key,
  eligible = primary_cross_eligible,
  group_counter_start = group_counter
)
primary_final <- primary_cross_result$records
audit_logs[[length(audit_logs) + 1L]] <- primary_cross_result$audit
group_counter <- primary_cross_result$group_counter

ctgov_key_type <- rep("nct", nrow(ctgov))
ctgov_result <- apply_exact_dedup(
  ctgov,
  stream = "clinical_trial_registry_supplementary",
  stage = "ctgov_nct",
  dedup_key_type = ctgov_key_type,
  dedup_key = paste0("nct:", ctgov$nct_id_normalized),
  eligible = nzchar(ctgov$nct_id_normalized),
  group_counter_start = group_counter
)
ctgov_final <- ctgov_result$records
audit_logs[[length(audit_logs) + 1L]] <- ctgov_result$audit
group_counter <- ctgov_result$group_counter

overlap_key <- rep("", nrow(overlap))
overlap_key_type <- rep("source_record_fallback", nrow(overlap))
is_prospero <- overlap$source_database == "PROSPERO" & nzchar(overlap$crd_id_normalized)
is_osf <- overlap$source_database == "OSF Registries" & nzchar(overlap$osf_id_normalized)
overlap_key[is_prospero] <- paste0("crd:", overlap$crd_id_normalized[is_prospero])
overlap_key_type[is_prospero] <- "crd"
overlap_key[is_osf] <- paste0("osf:", overlap$osf_id_normalized[is_osf])
overlap_key_type[is_osf] <- "osf"

overlap_result <- apply_exact_dedup(
  overlap,
  stream = "review_registry_overlap",
  stage = "registry_overlap",
  dedup_key_type = overlap_key_type,
  dedup_key = overlap_key,
  eligible = is_prospero | is_osf,
  group_counter_start = group_counter
)
overlap_final <- overlap_result$records
audit_logs[[length(audit_logs) + 1L]] <- overlap_result$audit

primary_final <- assign_record_ids(primary_final, "REC_PRIMARY")
ctgov_final <- assign_record_ids(ctgov_final, "REC_CTGOV")
overlap_final <- assign_record_ids(overlap_final, "REC_OVERLAP")

audit <- do.call(rbind, audit_logs)
if (nrow(audit) > 0L) {
  final_records <- rbind(
    primary_final[, c("record_id", "record_source_id", "source_database", "source_file"), drop = FALSE],
    ctgov_final[, c("record_id", "record_source_id", "source_database", "source_file"), drop = FALSE],
    overlap_final[, c("record_id", "record_source_id", "source_database", "source_file"), drop = FALSE]
  )
  parent_map <- stats::setNames(audit$survivor_record_source_id, audit$absorbed_record_source_id)
  resolve_final_survivor <- function(record_source_id) {
    current <- record_source_id
    seen <- character()
    while (current %in% names(parent_map) && !(current %in% seen)) {
      seen <- c(seen, current)
      current <- unname(parent_map[[current]])
    }
    current
  }

  final_survivor_source_id <- vapply(audit$absorbed_record_source_id, resolve_final_survivor, character(1L))
  final_lookup <- match(final_survivor_source_id, final_records$record_source_id)
  audit$survivor_record_source_id <- final_survivor_source_id
  audit$survivor_record_id <- final_records$record_id[final_lookup]
  audit$survivor_source_database <- final_records$source_database[final_lookup]
  audit$survivor_source_file <- final_records$source_file[final_lookup]
  audit$survivor_record_id[is.na(audit$survivor_record_id)] <- ""
  audit$survivor_source_database[is.na(audit$survivor_source_database)] <- ""
  audit$survivor_source_file[is.na(audit$survivor_source_file)] <- ""
  audit <- audit[order(audit$dedup_group_id, audit$absorbed_record_source_id, method = "radix"), , drop = FALSE]
  audit$audit_id <- sprintf("DEDAUD_%05d", seq_len(nrow(audit)))
}

fuzzy_candidates <- generate_fuzzy_candidates(primary_final)
preprint_candidates <- generate_preprint_publication_candidates(primary_final)

primary_out_path <- file.path(CFG$processed_dir, "records_after_dedup_primary.csv")
ctgov_out_path <- file.path(CFG$processed_dir, "records_after_dedup_ctgov.csv")
overlap_out_path <- file.path(CFG$processed_dir, "records_after_dedup_overlap.csv")
audit_path <- file.path(CFG$processed_dir, "dedup_audit_log.csv")
fuzzy_path <- file.path(CFG$processed_dir, "fuzzy_duplicate_candidates.csv")
preprint_path <- file.path(CFG$processed_dir, "preprint_publication_candidates.csv")
counts_path <- file.path(CFG$processed_dir, "dedup_counts.csv")

dedup_record_columns <- c("record_id", names(records))
stable_write_csv(primary_final, primary_out_path, dedup_record_columns)
stable_write_csv(ctgov_final, ctgov_out_path, dedup_record_columns)
stable_write_csv(overlap_final, overlap_out_path, dedup_record_columns)
stable_write_csv(audit, audit_path, DEDUP_AUDIT_COLUMNS)
stable_write_csv(fuzzy_candidates, fuzzy_path, CANDIDATE_COLUMNS)
stable_write_csv(preprint_candidates, preprint_path, CANDIDATE_COLUMNS)

prospero_raw <- overlap[overlap$source_database == "PROSPERO", , drop = FALSE]
osf_raw <- overlap[overlap$source_database == "OSF Registries", , drop = FALSE]

dedup_counts <- rbind(
  dedup_count_row("primary_records_input", nrow(primary), "CP05 primary records before exact deduplication."),
  dedup_count_row("primary_records_after_intrasource_dedup", nrow(primary_after_intra), "After exact within-source deduplication for multi-query primary sources."),
  dedup_count_row("primary_records_after_crosssource_dedup", nrow(primary_final), "Primary screening-ready records after exact cross-source deduplication."),
  dedup_count_row("primary_duplicates_removed_exact", nrow(primary) - nrow(primary_final), "Exact primary duplicates removed across both exact stages."),
  dedup_count_row("ctgov_raw_query_occurrences", nrow(ctgov), "ClinicalTrials.gov query-level records before NCT deduplication."),
  dedup_count_row("ctgov_unique_nct", nrow(ctgov_final), "ClinicalTrials.gov records after exact NCT deduplication."),
  dedup_count_row("ctgov_duplicates_removed_exact", nrow(ctgov) - nrow(ctgov_final), "ClinicalTrials.gov exact duplicates removed by NCT ID."),
  dedup_count_row("prospero_raw_occurrences", nrow(prospero_raw), "PROSPERO query-level records before CRD deduplication."),
  dedup_count_row("prospero_unique_crd", sum(overlap_final$source_database == "PROSPERO"), "PROSPERO records after exact CRD deduplication."),
  dedup_count_row("osf_raw_occurrences", nrow(osf_raw), "OSF rows before OSF ID deduplication; includes zero-result evidence rows."),
  dedup_count_row("osf_unique_ids_or_rows", sum(overlap_final$source_database == "OSF Registries"), "OSF rows after exact OSF ID deduplication while preserving zero-result evidence rows."),
  dedup_count_row("overlap_records_after_dedup", nrow(overlap_final), "Registry/protocol overlap records after exact registry deduplication."),
  dedup_count_row("exact_dedup_audit_rows", nrow(audit), "One row per absorbed exact duplicate."),
  dedup_count_row("fuzzy_duplicate_candidates_not_removed", nrow(fuzzy_candidates), "Candidate pairs queued for human adjudication; no records removed."),
  dedup_count_row("fuzzy_duplicate_candidates_high", sum(fuzzy_candidates$band == "high"), "High-band fuzzy candidates."),
  dedup_count_row("fuzzy_duplicate_candidates_review", sum(fuzzy_candidates$band == "review"), "Review-band fuzzy candidates."),
  dedup_count_row("preprint_publication_candidates_not_removed", nrow(preprint_candidates), "Candidate preprint/publication pairs queued for human adjudication."),
  dedup_count_row("records_screening_ready_primary", nrow(primary_final), "Primary records available for title/abstract screening after exact deduplication.")
)
stable_write_csv(dedup_counts, counts_path, DEDUP_COUNT_COLUMNS)

checkpoint_warnings <- character()
if (nrow(fuzzy_candidates) > 0L || nrow(preprint_candidates) > 0L) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    paste0(
      "Candidate queues contain ",
      nrow(fuzzy_candidates),
      " fuzzy pairs and ",
      nrow(preprint_candidates),
      " preprint/publication pairs; these are not auto-merged and require human adjudication before CP09 screening freeze."
    )
  )
}

review_file <- write_checkpoint(
  id = "CP06",
  name = "exact_deduplication",
  what_ran = paste(
    "Applied deterministic exact deduplication to primary, ClinicalTrials,",
    "and registry-overlap streams; assigned stable post-dedup record_id values;",
    "logged every absorbed exact duplicate; and generated fuzzy/preprint",
    "candidate queues without auto-merging them."
  ),
  numbers = c(
    primary_input = nrow(primary),
    primary_after_intrasource = nrow(primary_after_intra),
    primary_after_crosssource = nrow(primary_final),
    primary_exact_duplicates_removed = nrow(primary) - nrow(primary_final),
    ctgov_input = nrow(ctgov),
    ctgov_after_dedup = nrow(ctgov_final),
    overlap_input = nrow(overlap),
    overlap_after_dedup = nrow(overlap_final),
    exact_audit_rows = nrow(audit),
    fuzzy_candidate_pairs = nrow(fuzzy_candidates),
    preprint_publication_candidate_pairs = nrow(preprint_candidates)
  ),
  review_items = c(
    "Review dedup_audit_log.csv to confirm exact survivor choices are acceptable.",
    "Confirm fuzzy_duplicate_candidates.csv and preprint_publication_candidates.csv contain adjudication columns but no automatic removals.",
    "Confirm primary, ClinicalTrials, and registry-overlap streams remain separate.",
    "Confirm records_after_dedup_primary.csv is the input for PRISMA and later screening templates."
  ),
  outputs = c(
    primary_out_path,
    ctgov_out_path,
    overlap_out_path,
    audit_path,
    fuzzy_path,
    preprint_path,
    counts_path,
    file.path(CFG$checkpoints_dir, "CP06_exact_deduplication.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP06 only after reviewing exact survivor choices and confirming fuzzy/preprint pairs remain queued for human adjudication."
)

cat("CP06 exact deduplication complete\n")
cat("Primary records after exact dedup:", nrow(primary_final), "\n")
cat("ClinicalTrials records after exact dedup:", nrow(ctgov_final), "\n")
cat("Registry overlap records after exact dedup:", nrow(overlap_final), "\n")
cat("Exact audit rows:", nrow(audit), "\n")
cat("Fuzzy candidate pairs:", nrow(fuzzy_candidates), "\n")
cat("Preprint/publication candidate pairs:", nrow(preprint_candidates), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
