#!/usr/bin/env Rscript

# Compara as respostas da triagem e separa os acordos das discordâncias.
# As decisões dos revisores permanecem identificáveis durante a reconciliação.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP09")

bind_frames <- function(frames, columns) {
  frames <- Filter(Negate(is.null), frames)
  frames <- lapply(frames, function(frame) {
    for (missing_col in setdiff(columns, names(frame))) {
      frame[[missing_col]] <- ""
    }
    frame[, columns, drop = FALSE]
  })
  if (length(frames) == 0L) {
    return(empty_named_frame(columns))
  }
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out[, columns, drop = FALSE]
}

value_by_record_id <- function(data, record_ids, column) {
  data <- as_character_frame(data)
  data$record_id <- trimws(data$record_id)
  values <- data[[column]][match(record_ids, data$record_id)]
  values[is.na(values)] <- ""
  values
}

prepare_reviewer_data <- function(read_result) {
  data <- as_character_frame(read_result$data)
  data$record_id <- trimws(data$record_id)
  data$decision <- normalize_screening_decision(data$decision)
  data <- data[nzchar(data$record_id), , drop = FALSE]
  data <- data[!duplicated(data$record_id), , drop = FALSE]
  data[, SCREENING_REVIEWER_COLUMNS, drop = FALSE]
}

reconciliation_reason <- function(r1_decision, r2_decision) {
  if (!nzchar(r1_decision) || !nzchar(r2_decision)) {
    return("")
  }
  if (identical(r1_decision, "exclude") && identical(r2_decision, "exclude")) {
    return("")
  }
  if (identical(r1_decision, r2_decision)) {
    return(paste0("Both reviewers selected ", r1_decision, "."))
  }
  "At least one reviewer selected include/uncertain; conflict remains flagged."
}

blinded_path <- file.path(CFG$screening_dir, "title_abstract_worksheet_blinded.csv")
if (!file.exists(blinded_path)) {
  stop("Missing CP09 blinded worksheet: ", project_relative_path(blinded_path), call. = FALSE)
}

blinded <- read_machine_csv(blinded_path)
validate_required_columns(blinded, SCREENING_BLINDED_COLUMNS, blinded_path)
blinded <- blinded[, SCREENING_BLINDED_COLUMNS, drop = FALSE]
blinded <- blinded[order(blinded$record_id, method = "radix"), , drop = FALSE]
expected_record_ids <- blinded$record_id
template <- screening_reviewer_template(blinded)

manifest <- bind_frames(
  lapply(SCREENING_REVIEWER_IDS, ensure_screening_reviewer_package, template = template),
  SCREENING_REVIEWER_MANIFEST_COLUMNS
)

read_results <- setNames(lapply(SCREENING_REVIEWER_IDS, read_screening_reviewer_workbook), SCREENING_REVIEWER_IDS)
validation_issues <- bind_frames(
  lapply(SCREENING_REVIEWER_IDS, function(reviewer_id) {
    screening_reviewer_validation_issues(
      reviewer_id = reviewer_id,
      data = read_results[[reviewer_id]]$data,
      expected_record_ids = expected_record_ids,
      read_issues = read_results[[reviewer_id]]$read_issues
    )
  }),
  SCREENING_VALIDATION_ISSUE_COLUMNS
)

r1 <- prepare_reviewer_data(read_results$reviewer_1)
r2 <- prepare_reviewer_data(read_results$reviewer_2)
r1_decision <- value_by_record_id(r1, expected_record_ids, "decision")
r2_decision <- value_by_record_id(r2, expected_record_ids, "decision")
r1_reason <- value_by_record_id(r1, expected_record_ids, "reason")
r2_reason <- value_by_record_id(r2, expected_record_ids, "reason")
r1_notes <- value_by_record_id(r1, expected_record_ids, "notes")
r2_notes <- value_by_record_id(r2, expected_record_ids, "notes")

allowed <- screening_allowed_decisions()
r1_valid <- nzchar(r1_decision) & r1_decision %in% allowed
r2_valid <- nzchar(r2_decision) & r2_decision %in% allowed
row_ready <- r1_valid & r2_valid
agreement_status <- ifelse(row_ready, ifelse(r1_decision == r2_decision, "agreement", "conflict"), "pending")
conflict_flag <- ifelse(agreement_status == "conflict", "yes", "no")
full_text_required_by_rule <- ifelse(
  row_ready,
  ifelse(r1_decision != "exclude" | r2_decision != "exclude", "yes", "no"),
  ""
)
reconciliation_status <- ifelse(
  row_ready,
  ifelse(agreement_status == "conflict", "ready_conflict_flagged", "ready_agreement"),
  "pending_human_input"
)

reconciliation <- data.frame(
  record_id = expected_record_ids,
  title = blinded$title,
  year = blinded$year,
  publication_type = blinded$publication_type,
  candidate_flags = blinded$candidate_flags,
  reviewer_1_decision = r1_decision,
  reviewer_1_reason = r1_reason,
  reviewer_1_notes = r1_notes,
  reviewer_2_decision = r2_decision,
  reviewer_2_reason = r2_reason,
  reviewer_2_notes = r2_notes,
  agreement_status = agreement_status,
  conflict_flag = conflict_flag,
  full_text_required_by_rule = full_text_required_by_rule,
  reconciliation_status = reconciliation_status,
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, SCREENING_RECONCILIATION_COLUMNS, drop = FALSE]

conflicts <- reconciliation[reconciliation$agreement_status == "conflict", , drop = FALSE]
all_records_ready <- all(row_ready) && !any(validation_issues$severity %in% c("BLOCKER", "PENDING"))

if (all_records_ready) {
  queue_rows <- reconciliation$full_text_required_by_rule == "yes"
  full_text_queue <- data.frame(
    record_id = reconciliation$record_id[queue_rows],
    title = reconciliation$title[queue_rows],
    year = reconciliation$year[queue_rows],
    publication_type = reconciliation$publication_type[queue_rows],
    candidate_flags = reconciliation$candidate_flags[queue_rows],
    reviewer_1_decision = reconciliation$reviewer_1_decision[queue_rows],
    reviewer_2_decision = reconciliation$reviewer_2_decision[queue_rows],
    reason_for_full_text = mapply(reconciliation_reason, reconciliation$reviewer_1_decision[queue_rows], reconciliation$reviewer_2_decision[queue_rows]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, FULL_TEXT_QUEUE_COLUMNS, drop = FALSE]
} else {
  full_text_queue <- empty_named_frame(FULL_TEXT_QUEUE_COLUMNS)
}

reconciliation_dir <- ensure_dir(file.path(CFG$screening_dir, "reconciliation"))

manifest_csv <- file.path(CFG$screening_reviewer_dir, "reviewer_package_manifest.csv")
manifest_xlsx <- file.path(CFG$screening_reviewer_dir, "reviewer_package_manifest.xlsx")
validation_csv <- file.path(reconciliation_dir, "screening_validation_issues.csv")
validation_xlsx <- file.path(reconciliation_dir, "screening_validation_issues.xlsx")
reconciliation_csv <- file.path(reconciliation_dir, "screening_reconciliation.csv")
reconciliation_xlsx <- file.path(reconciliation_dir, "screening_reconciliation.xlsx")
conflicts_csv <- file.path(reconciliation_dir, "screening_conflicts.csv")
conflicts_xlsx <- file.path(reconciliation_dir, "screening_conflicts.xlsx")
full_text_queue_csv <- file.path(reconciliation_dir, "full_text_candidate_queue.csv")
full_text_queue_xlsx <- file.path(reconciliation_dir, "full_text_candidate_queue.xlsx")

stable_write_csv(manifest, manifest_csv, SCREENING_REVIEWER_MANIFEST_COLUMNS)
stable_write_csv(validation_issues, validation_csv, SCREENING_VALIDATION_ISSUE_COLUMNS)
stable_write_csv(reconciliation, reconciliation_csv, SCREENING_RECONCILIATION_COLUMNS)
stable_write_csv(conflicts, conflicts_csv, SCREENING_RECONCILIATION_COLUMNS)
stable_write_csv(full_text_queue, full_text_queue_csv, FULL_TEXT_QUEUE_COLUMNS)

write_review_xlsx(list(reviewer_package_manifest = manifest), manifest_xlsx)
write_review_xlsx(list(screening_validation_issues = validation_issues), validation_xlsx)
write_review_xlsx(list(screening_reconciliation = reconciliation), reconciliation_xlsx)
write_review_xlsx(list(screening_conflicts = conflicts), conflicts_xlsx)
write_review_xlsx(list(full_text_candidate_queue = full_text_queue), full_text_queue_xlsx)

completed_by_reviewer <- vapply(SCREENING_REVIEWER_IDS, function(reviewer_id) {
  sum(nzchar(normalize_screening_decision(read_results[[reviewer_id]]$data$decision)))
}, integer(1L))
missing_decisions <- sum(validation_issues$issue_type == "missing_decision")
invalid_decisions <- sum(validation_issues$issue_type == "invalid_decision")
blocker_issues <- sum(validation_issues$severity == "BLOCKER")
pending_issues <- sum(validation_issues$severity == "PENDING")
records_requiring_full_text <- nrow(full_text_queue)
conflict_records <- nrow(conflicts)

checkpoint_warnings <- character()
if (any(manifest$workbook_created_this_run == "yes")) {
  checkpoint_warnings <- c(
    checkpoint_warnings,
    "Reviewer workbooks were created. CP10 awaits two independent human-completed XLSX files before reconciliation can be approved."
  )
}
if (pending_issues > 0L) {
  checkpoint_warnings <- c(checkpoint_warnings, paste0(pending_issues, " pending human-input issues remain, mostly blank reviewer decisions."))
}
if (blocker_issues > 0L) {
  checkpoint_warnings <- c(checkpoint_warnings, paste0(blocker_issues, " blocker validation issues must be corrected before CP10 can be approved."))
}
if (!all_records_ready) {
  checkpoint_warnings <- c(checkpoint_warnings, "Full-text queue is intentionally empty until both reviewer workbooks are complete and valid.")
}
if (all_records_ready && conflict_records > 0L) {
  checkpoint_warnings <- c(checkpoint_warnings, paste0(conflict_records, " title/abstract conflicts were flagged for human adjudication or protocol-consistent handling."))
}

review_file <- write_checkpoint(
  id = "CP10",
  name = "screening_reconciliation",
  what_ran = paste(
    "Created or validated independent blinded reviewer workbooks, ingested human",
    "title/abstract screening decisions when present, computed agreement/conflict",
    "flags, and generated the full-text queue only when both reviewer inputs were complete and valid."
  ),
  numbers = c(
    expected_screening_records = length(expected_record_ids),
    reviewer_1_completed_decisions = completed_by_reviewer[["reviewer_1"]],
    reviewer_2_completed_decisions = completed_by_reviewer[["reviewer_2"]],
    missing_decision_cells = missing_decisions,
    invalid_decision_cells = invalid_decisions,
    blocker_validation_issues = blocker_issues,
    pending_validation_issues = pending_issues,
    conflict_records = conflict_records,
    records_requiring_full_text = records_requiring_full_text,
    full_text_queue_ready = ifelse(all_records_ready, "yes", "no")
  ),
  review_items = c(
    "Send reviewer_inputs/reviewer_1/title_abstract_screening_reviewer_1.xlsx to reviewer 1 only.",
    "Send reviewer_inputs/reviewer_2/title_abstract_screening_reviewer_2.xlsx to reviewer 2 only.",
    "Review screening_validation_issues.xlsx after both workbooks are returned.",
    "Approve CP10 only when both reviewer workbooks are complete, valid, and conflicts/full-text queue have been inspected."
  ),
  outputs = c(
    manifest_csv,
    manifest_xlsx,
    screening_reviewer_paths("reviewer_1")$workbook,
    screening_reviewer_paths("reviewer_2")$workbook,
    validation_csv,
    validation_xlsx,
    reconciliation_csv,
    reconciliation_xlsx,
    conflicts_csv,
    conflicts_xlsx,
    full_text_queue_csv,
    full_text_queue_xlsx,
    file.path(CFG$checkpoints_dir, "CP10_screening_reconciliation.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = checkpoint_warnings,
  gate_question = "Approve CP10 only after two independent human reviewer workbooks are complete and validated."
)

cat("CP10 screening reconciliation complete\n")
cat("Expected screening records:", length(expected_record_ids), "\n")
cat("Reviewer 1 completed decisions:", completed_by_reviewer[["reviewer_1"]], "\n")
cat("Reviewer 2 completed decisions:", completed_by_reviewer[["reviewer_2"]], "\n")
cat("Pending validation issues:", pending_issues, "\n")
cat("Blocker validation issues:", blocker_issues, "\n")
cat("Conflict records:", conflict_records, "\n")
cat("Full-text queue ready:", ifelse(all_records_ready, "yes", "no"), "\n")
cat("Reviewer 1 workbook:", project_relative_path(screening_reviewer_paths("reviewer_1")$workbook), "\n")
cat("Reviewer 2 workbook:", project_relative_path(screening_reviewer_paths("reviewer_2")$workbook), "\n")
cat("Validation workbook:", project_relative_path(validation_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
