#!/usr/bin/env Rscript

# Aplica as decisões documentadas sobre registros com títulos repetidos.
# Registra as alterações e mantém o vínculo com os registros de origem.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP09")

TITLE_DEDUP_RESOLUTION_COLUMNS <- c(
  "title_duplicate_group_id",
  "title_duplicate_key",
  "record_id",
  "action",
  "retained_record_id",
  "record_rank_class",
  "record_abstract_chars",
  "record_metadata_score",
  "source_database",
  "source_file",
  "record_source_id",
  "year",
  "first_author_normalized",
  "title",
  "doi",
  "pmid",
  "arxiv_id",
  "publication_type",
  "document_type",
  "journal_or_source",
  "screening_agreement_status",
  "screening_full_text_required_by_rule",
  "screening_reviewer_1_decision",
  "screening_reviewer_2_decision",
  "reason"
)

TITLE_DEDUP_TABLE_ACTION_COLUMNS <- c(
  "table_name",
  "title_duplicate_group_id",
  "title_duplicate_key",
  "record_id",
  "action",
  "retained_record_id",
  "reason"
)

title_duplicate_key <- function(records) {
  title <- norm_empty_if_na(records$title_normalized)
  key <- ifelse(
    nzchar(title) & nchar(title, type = "chars", allowNA = FALSE, keepNA = FALSE) >= 35L,
    paste0("exact_title:", title),
    ""
  )
  key
}

title_duplicate_groups <- function(records) {
  key <- title_duplicate_key(records)
  grouped <- split(seq_len(nrow(records)), key)
  grouped[names(grouped) != "" & lengths(grouped) > 1L]
}

survivor_record_id <- function(records, record_ids) {
  rows <- match(record_ids, records$record_id)
  rows <- rows[!is.na(rows)]
  if (length(rows) == 0L) {
    return("")
  }
  if (length(rows) == 1L) {
    return(records$record_id[[rows[[1L]]]])
  }
  group <- records[rows, , drop = FALSE]
  survivor <- record_dedup_survivor_index(group)
  group$record_id[[survivor]]
}

screening_lookup <- function(reconciliation, record_ids, column) {
  values <- reconciliation[[column]][match(record_ids, reconciliation$record_id)]
  values[is.na(values)] <- ""
  values
}

build_resolution_log <- function(records, reconciliation) {
  groups <- title_duplicate_groups(records)
  if (length(groups) == 0L) {
    return(empty_named_frame(TITLE_DEDUP_RESOLUTION_COLUMNS))
  }

  ranks <- record_dedup_rank_frame(records)
  rows <- list()
  group_index <- 0L
  for (key in sort(names(groups), method = "radix")) {
    group_index <- group_index + 1L
    group_id <- sprintf("TITLEDEDUP_%05d", group_index)
    indexes <- groups[[key]]
    retained_id <- survivor_record_id(records, records$record_id[indexes])
    for (i in indexes) {
      action <- ifelse(records$record_id[[i]] == retained_id, "retained", "absorbed_title_duplicate")
      rows[[length(rows) + 1L]] <- data.frame(
        title_duplicate_group_id = group_id,
        title_duplicate_key = key,
        record_id = records$record_id[[i]],
        action = action,
        retained_record_id = retained_id,
        record_rank_class = as.character(ranks$rank_class[[i]]),
        record_abstract_chars = as.character(ranks$abstract_chars[[i]]),
        record_metadata_score = as.character(ranks$metadata_score[[i]]),
        source_database = records$source_database[[i]],
        source_file = records$source_file[[i]],
        record_source_id = records$record_source_id[[i]],
        year = records$year[[i]],
        first_author_normalized = records$first_author_normalized[[i]],
        title = records$title[[i]],
        doi = records$doi[[i]],
        pmid = records$pmid[[i]],
        arxiv_id = records$arxiv_id[[i]],
        publication_type = records$publication_type[[i]],
        document_type = records$document_type[[i]],
        journal_or_source = records$journal_or_source[[i]],
        screening_agreement_status = screening_lookup(reconciliation, records$record_id[[i]], "agreement_status"),
        screening_full_text_required_by_rule = screening_lookup(reconciliation, records$record_id[[i]], "full_text_required_by_rule"),
        screening_reviewer_1_decision = screening_lookup(reconciliation, records$record_id[[i]], "reviewer_1_decision"),
        screening_reviewer_2_decision = screening_lookup(reconciliation, records$record_id[[i]], "reviewer_2_decision"),
        reason = "Exact normalized title duplicate with title length >= 35 characters; retained record selected by existing deterministic bibliographic ranking.",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  out[, TITLE_DEDUP_RESOLUTION_COLUMNS, drop = FALSE]
}

read_pre_adjudication_state <- function(csv_path, xlsx_path) {
  data <- read_machine_csv(csv_path)
  validate_required_columns(data, SCREENING_PRE_ADJUDICATION_COLUMNS, csv_path)
  data <- data[, SCREENING_PRE_ADJUDICATION_COLUMNS, drop = FALSE]

  if (!file.exists(xlsx_path)) {
    return(data)
  }

  xlsx_data <- tryCatch(
    read_review_xlsx_sheet(xlsx_path, "pre_adjudication", SCREENING_PRE_ADJUDICATION_COLUMNS),
    error = function(e) NULL
  )
  if (is.null(xlsx_data) || nrow(xlsx_data) == 0L) {
    return(data)
  }

  human_columns <- c("pre_adjudicated_decision", "pre_adjudication_reason", "pre_adjudicator_notes")
  match_index <- match(data$record_id, xlsx_data$record_id)
  found <- !is.na(match_index)
  for (column in human_columns) {
    values <- data[[column]]
    values[found] <- xlsx_data[[column]][match_index[found]]
    data[[column]] <- values
  }
  data
}

choose_table_survivor <- function(data, records, row_indexes) {
  record_ids <- data$record_id[row_indexes]

  if ("pre_adjudicated_decision" %in% names(data)) {
    filled <- nzchar(normalize_pre_adjudication_decision(data$pre_adjudicated_decision[row_indexes]))
    if (any(filled)) {
      record_ids <- record_ids[filled]
    }
  }

  survivor_record_id(records, record_ids)
}

deduplicate_table_by_title <- function(data, records, table_name, columns) {
  data <- as_character_frame(data)
  validate_required_columns(data, columns, table_name)
  data <- data[, columns, drop = FALSE]

  record_rows <- match(data$record_id, records$record_id)
  keys <- rep("", nrow(data))
  found <- !is.na(record_rows)
  keys[found] <- title_duplicate_key(records[record_rows[found], , drop = FALSE])

  keep <- rep(TRUE, nrow(data))
  action_rows <- list()
  duplicate_keys <- sort(unique(keys[nzchar(keys) & duplicated(keys)]), method = "radix")

  for (key in duplicate_keys) {
    row_indexes <- which(keys == key)
    retained_id <- choose_table_survivor(data, records, row_indexes)
    if (!nzchar(retained_id)) {
      next
    }
    group_id <- resolution_lookup$title_duplicate_group_id[match(key, resolution_lookup$title_duplicate_key)]
    if (is.na(group_id)) {
      group_id <- ""
    }

    for (row_index in row_indexes) {
      action <- ifelse(data$record_id[[row_index]] == retained_id, "retained_in_table", "removed_title_duplicate")
      if (!identical(action, "retained_in_table")) {
        keep[[row_index]] <- FALSE
      }
      action_rows[[length(action_rows) + 1L]] <- data.frame(
        table_name = table_name,
        title_duplicate_group_id = group_id,
        title_duplicate_key = key,
        record_id = data$record_id[[row_index]],
        action = action,
        retained_record_id = retained_id,
        reason = "One row retained for an exact normalized title duplicate group in this table.",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
  }

  actions <- if (length(action_rows) > 0L) {
    do.call(rbind, action_rows)
  } else {
    empty_named_frame(TITLE_DEDUP_TABLE_ACTION_COLUMNS)
  }

  list(
    data = data[keep, columns, drop = FALSE],
    actions = actions[, TITLE_DEDUP_TABLE_ACTION_COLUMNS, drop = FALSE]
  )
}

records_path <- file.path(CFG$processed_dir, "records_after_dedup_primary.csv")
reconciliation_path <- file.path(CFG$screening_dir, "reconciliation", "screening_reconciliation.csv")
conflicts_path <- file.path(CFG$screening_dir, "reconciliation", "screening_conflicts.csv")
full_text_queue_path <- file.path(CFG$screening_dir, "reconciliation", "full_text_candidate_queue.csv")
pre_adjudication_csv_path <- file.path(CFG$screening_dir, "pre_adjudication", "title_abstract_pre_adjudication_template.csv")
pre_adjudication_xlsx_path <- file.path(CFG$screening_dir, "pre_adjudication", "title_abstract_pre_adjudication_template.xlsx")

for (path in c(records_path, reconciliation_path, conflicts_path, full_text_queue_path, pre_adjudication_csv_path)) {
  if (!file.exists(path)) {
    stop("Required input is missing: ", project_relative_path(path), call. = FALSE)
  }
}

records <- read_machine_csv(records_path)
reconciliation <- read_machine_csv(reconciliation_path)
conflicts <- read_machine_csv(conflicts_path)
full_text_queue <- read_machine_csv(full_text_queue_path)
pre_adjudication <- read_pre_adjudication_state(pre_adjudication_csv_path, pre_adjudication_xlsx_path)

validate_required_columns(records, c("record_id", "title", "title_normalized", "first_author_normalized", "year"), records_path)
validate_required_columns(reconciliation, SCREENING_RECONCILIATION_COLUMNS, reconciliation_path)
validate_required_columns(conflicts, SCREENING_RECONCILIATION_COLUMNS, conflicts_path)
validate_required_columns(full_text_queue, FULL_TEXT_QUEUE_COLUMNS, full_text_queue_path)

resolution_log <- build_resolution_log(records, reconciliation)
resolution_lookup <- unique(resolution_log[, c("title_duplicate_group_id", "title_duplicate_key"), drop = FALSE])

reconciliation_result <- deduplicate_table_by_title(
  reconciliation,
  records,
  table_name = "screening_reconciliation",
  columns = SCREENING_RECONCILIATION_COLUMNS
)
conflicts_result <- deduplicate_table_by_title(
  conflicts,
  records,
  table_name = "screening_conflicts",
  columns = SCREENING_RECONCILIATION_COLUMNS
)
full_text_queue_result <- deduplicate_table_by_title(
  full_text_queue,
  records,
  table_name = "full_text_candidate_queue",
  columns = FULL_TEXT_QUEUE_COLUMNS
)
pre_adjudication_result <- deduplicate_table_by_title(
  pre_adjudication,
  records,
  table_name = "title_abstract_pre_adjudication",
  columns = SCREENING_PRE_ADJUDICATION_COLUMNS
)

table_actions <- rbind(
  reconciliation_result$actions,
  conflicts_result$actions,
  full_text_queue_result$actions,
  pre_adjudication_result$actions
)

title_dedup_dir <- ensure_dir(file.path(CFG$screening_dir, "title_deduplication"))
resolution_csv <- file.path(title_dedup_dir, "title_duplicate_resolution.csv")
resolution_xlsx <- file.path(title_dedup_dir, "title_duplicate_resolution.xlsx")
table_actions_csv <- file.path(title_dedup_dir, "title_dedup_table_actions.csv")
table_actions_xlsx <- file.path(title_dedup_dir, "title_dedup_table_actions.xlsx")
reconciliation_csv <- file.path(title_dedup_dir, "screening_reconciliation_title_deduped.csv")
reconciliation_xlsx <- file.path(title_dedup_dir, "screening_reconciliation_title_deduped.xlsx")
conflicts_csv <- file.path(title_dedup_dir, "screening_conflicts_title_deduped.csv")
conflicts_xlsx <- file.path(title_dedup_dir, "screening_conflicts_title_deduped.xlsx")
full_text_queue_csv <- file.path(title_dedup_dir, "full_text_candidate_queue_title_deduped.csv")
full_text_queue_xlsx <- file.path(title_dedup_dir, "full_text_candidate_queue_title_deduped.xlsx")
pre_adjudication_csv <- file.path(title_dedup_dir, "title_abstract_pre_adjudication_template_title_deduped.csv")
pre_adjudication_xlsx <- file.path(title_dedup_dir, "title_abstract_pre_adjudication_template_title_deduped.xlsx")

stable_write_csv(resolution_log, resolution_csv, TITLE_DEDUP_RESOLUTION_COLUMNS)
stable_write_csv(table_actions, table_actions_csv, TITLE_DEDUP_TABLE_ACTION_COLUMNS)
stable_write_csv(reconciliation_result$data, reconciliation_csv, SCREENING_RECONCILIATION_COLUMNS)
stable_write_csv(conflicts_result$data, conflicts_csv, SCREENING_RECONCILIATION_COLUMNS)
stable_write_csv(full_text_queue_result$data, full_text_queue_csv, FULL_TEXT_QUEUE_COLUMNS)
stable_write_csv(pre_adjudication_result$data, pre_adjudication_csv, SCREENING_PRE_ADJUDICATION_COLUMNS)

write_review_xlsx(list(title_duplicate_resolution = resolution_log), resolution_xlsx)
write_review_xlsx(list(title_dedup_table_actions = table_actions), table_actions_xlsx)
write_review_xlsx(list(screening_reconciliation_title_deduped = reconciliation_result$data), reconciliation_xlsx)
write_review_xlsx(list(screening_conflicts_title_deduped = conflicts_result$data), conflicts_xlsx)
write_review_xlsx(list(full_text_candidate_queue_title_deduped = full_text_queue_result$data), full_text_queue_xlsx)
write_screening_pre_adjudication_xlsx(
  data = pre_adjudication_result$data,
  codebook = screening_pre_adjudication_codebook(),
  path = pre_adjudication_xlsx
)

absorbed_records <- sum(resolution_log$action == "absorbed_title_duplicate")
removed_from_reconciliation <- nrow(reconciliation) - nrow(reconciliation_result$data)
removed_from_conflicts <- nrow(conflicts) - nrow(conflicts_result$data)
removed_from_full_text_queue <- nrow(full_text_queue) - nrow(full_text_queue_result$data)
removed_from_pre_adjudication <- nrow(pre_adjudication) - nrow(pre_adjudication_result$data)

checkpoint_warnings <- c(
  "Original CP10 outputs are preserved. Use the title-deduplicated CP10B outputs for continued pre-adjudication and full-text retrieval."
)

review_file <- write_checkpoint(
  id = "CP10B",
  name = "title_duplicate_resolution",
  what_ran = paste(
    "Resolved exact normalized title duplicates after CP10 without renumbering",
    "reviewer-facing record_id values. Created title-deduplicated reconciliation,",
    "conflict, full-text queue, and pre-adjudication workbooks plus an audit map."
  ),
  numbers = c(
    title_duplicate_groups_detected = length(unique(resolution_log$title_duplicate_group_id)),
    title_duplicate_records_absorbed_in_primary_map = absorbed_records,
    screening_reconciliation_rows_before = nrow(reconciliation),
    screening_reconciliation_rows_after = nrow(reconciliation_result$data),
    conflicts_rows_before = nrow(conflicts),
    conflicts_rows_after = nrow(conflicts_result$data),
    full_text_queue_rows_before = nrow(full_text_queue),
    full_text_queue_rows_after = nrow(full_text_queue_result$data),
    pre_adjudication_rows_before = nrow(pre_adjudication),
    pre_adjudication_rows_after = nrow(pre_adjudication_result$data)
  ),
  review_items = c(
    "Review title_duplicate_resolution.xlsx to confirm retained and absorbed records.",
    "Continue human pre-adjudication using title_abstract_pre_adjudication_template_title_deduped.xlsx.",
    "Use full_text_candidate_queue_title_deduped.xlsx for later full-text retrieval planning.",
    "Do not edit or delete original CP10 reviewer evidence files."
  ),
  outputs = c(
    resolution_csv,
    resolution_xlsx,
    table_actions_csv,
    table_actions_xlsx,
    reconciliation_csv,
    reconciliation_xlsx,
    conflicts_csv,
    conflicts_xlsx,
    full_text_queue_csv,
    full_text_queue_xlsx,
    pre_adjudication_csv,
    pre_adjudication_xlsx,
    file.path(CFG$checkpoints_dir, "CP10B_title_duplicate_resolution.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP10B only after the title-duplicate resolution map and deduplicated workbooks have been inspected."
)

cat("CP10B title duplicate resolution complete\n")
cat("Title duplicate groups detected:", length(unique(resolution_log$title_duplicate_group_id)), "\n")
cat("Primary duplicate records absorbed in map:", absorbed_records, "\n")
cat("Screening reconciliation rows:", nrow(reconciliation), "->", nrow(reconciliation_result$data), "\n")
cat("Conflict rows:", nrow(conflicts), "->", nrow(conflicts_result$data), "\n")
cat("Full-text queue rows:", nrow(full_text_queue), "->", nrow(full_text_queue_result$data), "\n")
cat("Pre-adjudication rows:", nrow(pre_adjudication), "->", nrow(pre_adjudication_result$data), "\n")
cat("Use pre-adjudication workbook:", project_relative_path(pre_adjudication_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
