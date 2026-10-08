#!/usr/bin/env Rscript

# Aplica as decisões conjuntas fornecidas para as discordâncias da triagem.
# Confere as chaves e os valores permitidos antes de avançar.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP09")

FINAL_SCREENING_DECISION_COLUMNS <- c(
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
  "agreement_statuses",
  "pre_adjudicated_decisions",
  "pre_adjudication_reasons",
  "pre_adjudicator_notes",
  "final_title_abstract_decision",
  "full_text_required_final",
  "reason_for_final_decision",
  "decision_rule"
)

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

PRE_ADJUDICATION_APPLICATION_AUDIT_COLUMNS <- c(
  "record_id",
  "retained_record_id",
  "title_duplicate_group_id",
  "title_duplicate_action",
  "title",
  "reviewer_1_decision",
  "reviewer_2_decision",
  "agreement_status",
  "full_text_required_by_rule",
  "pre_adjudicated_decision",
  "pre_adjudication_reason",
  "group_final_title_abstract_decision",
  "group_full_text_required_final",
  "group_decision_rule"
)

FINAL_SCOPE_CORRECTION_COLUMNS <- c(
  "record_id",
  "corrected_final_title_abstract_decision",
  "corrected_reason_for_final_decision",
  "corrected_notes",
  "correction_date_local",
  "correction_source",
  "correction_rationale"
)

FINAL_SCOPE_CORRECTION_AUDIT_COLUMNS <- c(
  "record_id",
  "retained_record_id",
  "title_duplicate_group_id",
  "previous_final_title_abstract_decision",
  "previous_full_text_required_final",
  "previous_reason_for_final_decision",
  "previous_decision_rule",
  "corrected_final_title_abstract_decision",
  "corrected_full_text_required_final",
  "corrected_reason_for_final_decision",
  "corrected_notes",
  "correction_date_local",
  "correction_source",
  "correction_rationale",
  "correction_status"
)

SCOPE_PRECISION_REVIEW_COLUMNS <- c(
  "record_id",
  "title",
  "year",
  "publication_type",
  "candidate_flags",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "pre_adjudicated_decisions",
  "reason_for_full_text",
  "precision_review_flags",
  "recommended_action"
)

collapse_unique <- function(x, sep = "; ") {
  x <- trimws(norm_empty_if_na(x))
  x <- x[nzchar(x)]
  if (length(x) == 0L) {
    return("")
  }
  paste(unique(x), collapse = sep)
}

pre_adjudication_validation_issues <- function(data) {
  issues <- list()
  add_issue <- function(record_id, field, issue_type, observed_value, message) {
    issues[[length(issues) + 1L]] <<- data.frame(
      record_id = record_id,
      field = field,
      issue_type = issue_type,
      observed_value = observed_value,
      message = message,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  data <- as_character_frame(data)
  data$record_id <- trimws(data$record_id)
  decision <- normalize_pre_adjudication_decision(data$pre_adjudicated_decision)
  allowed <- pre_adjudication_allowed_decisions()

  duplicated_ids <- unique(data$record_id[nzchar(data$record_id) & duplicated(data$record_id)])
  for (record_id in duplicated_ids) {
    add_issue(record_id, "record_id", "duplicate_record_id", record_id, "Record appears more than once in the pre-adjudication workbook.")
  }

  missing <- which(!nzchar(decision))
  for (i in missing) {
    add_issue(data$record_id[[i]], "pre_adjudicated_decision", "missing_decision", "", "Pre-adjudication decision is blank.")
  }

  invalid <- which(nzchar(decision) & !(decision %in% allowed))
  for (i in invalid) {
    add_issue(
      data$record_id[[i]],
      "pre_adjudicated_decision",
      "invalid_decision",
      data$pre_adjudicated_decision[[i]],
      paste0("Decision must be one of: ", paste(allowed, collapse = ", "), ".")
    )
  }

  missing_reason <- which(nzchar(decision) & !nzchar(trimws(data$pre_adjudication_reason)))
  for (i in missing_reason) {
    add_issue(data$record_id[[i]], "pre_adjudication_reason", "missing_reason", "", "Decision has no supporting reason.")
  }

  if (length(issues) == 0L) {
    return(data.frame(
      record_id = character(),
      field = character(),
      issue_type = character(),
      observed_value = character(),
      message = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, issues)
}

read_validated_pre_adjudication <- function(xlsx_path) {
  data <- read_review_xlsx_sheet(xlsx_path, "pre_adjudication", SCREENING_PRE_ADJUDICATION_COLUMNS)
  validate_required_columns(data, SCREENING_PRE_ADJUDICATION_COLUMNS, xlsx_path)
  data <- as_character_frame(data[, SCREENING_PRE_ADJUDICATION_COLUMNS, drop = FALSE])
  data$pre_adjudicated_decision <- normalize_pre_adjudication_decision(data$pre_adjudicated_decision)
  issues <- pre_adjudication_validation_issues(data)
  if (nrow(issues) > 0L) {
    stop(
      "Pre-adjudication workbook is not ready. Found ",
      nrow(issues),
      " validation issue(s). Re-run the previous validation or inspect the workbook.",
      call. = FALSE
    )
  }
  data
}

record_value <- function(records, record_id, column) {
  index <- match(record_id, records$record_id)
  if (is.na(index) || !(column %in% names(records))) {
    return("")
  }
  norm_empty_if_na(records[[column]][[index]])
}

group_metadata <- function(records, rec_group, retained_id, column) {
  retained_value <- record_value(records, retained_id, column)
  if (nzchar(retained_value)) {
    return(retained_value)
  }
  indexes <- match(rec_group$record_id, records$record_id)
  indexes <- indexes[!is.na(indexes)]
  collapse_unique(records[[column]][indexes])
}

build_record_group_map <- function(reconciliation, resolution) {
  resolution <- as_character_frame(resolution)
  reconciliation <- as_character_frame(reconciliation)

  group_id_by_record <- stats::setNames(resolution$title_duplicate_group_id, resolution$record_id)
  retained_by_record <- stats::setNames(resolution$retained_record_id, resolution$record_id)
  action_by_record <- stats::setNames(resolution$action, resolution$record_id)

  group_id <- ifelse(
    reconciliation$record_id %in% names(group_id_by_record),
    group_id_by_record[reconciliation$record_id],
    paste0("SINGLE_", reconciliation$record_id)
  )
  retained_id <- ifelse(
    reconciliation$record_id %in% names(retained_by_record),
    retained_by_record[reconciliation$record_id],
    reconciliation$record_id
  )
  duplicate_action <- ifelse(
    reconciliation$record_id %in% names(action_by_record),
    action_by_record[reconciliation$record_id],
    "singleton"
  )

  data.frame(
    record_id = reconciliation$record_id,
    title_duplicate_group_id = unname(group_id),
    retained_record_id = unname(retained_id),
    title_duplicate_action = unname(duplicate_action),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

final_decision_allowed_values <- function() {
  c("full_text", "exclude")
}

read_final_scope_corrections <- function(path) {
  corrections <- read_csv_or_empty(path, FINAL_SCOPE_CORRECTION_COLUMNS)
  validate_required_columns(corrections, FINAL_SCOPE_CORRECTION_COLUMNS, path)
  corrections <- as_character_frame(corrections[, FINAL_SCOPE_CORRECTION_COLUMNS, drop = FALSE])
  corrections$record_id <- trimws(corrections$record_id)
  corrections$corrected_final_title_abstract_decision <- tolower(trimws(
    corrections$corrected_final_title_abstract_decision
  ))
  corrections <- corrections[nzchar(corrections$record_id), , drop = FALSE]
  if (nrow(corrections) == 0L) {
    return(corrections)
  }

  duplicated_records <- unique(corrections$record_id[duplicated(corrections$record_id)])
  if (length(duplicated_records) > 0L) {
    stop(
      "Final scope correction table contains duplicate record_id value(s): ",
      paste(duplicated_records, collapse = ", "),
      call. = FALSE
    )
  }

  invalid <- !(corrections$corrected_final_title_abstract_decision %in% final_decision_allowed_values())
  if (any(invalid)) {
    stop(
      "Invalid corrected_final_title_abstract_decision value(s): ",
      paste(unique(corrections$corrected_final_title_abstract_decision[invalid]), collapse = ", "),
      ". Allowed values: ",
      paste(final_decision_allowed_values(), collapse = ", "),
      call. = FALSE
    )
  }

  missing_reason <- !nzchar(trimws(corrections$corrected_reason_for_final_decision))
  if (any(missing_reason)) {
    stop(
      "Final scope correction table has missing corrected_reason_for_final_decision for record_id(s): ",
      paste(corrections$record_id[missing_reason], collapse = ", "),
      call. = FALSE
    )
  }

  corrections
}

apply_final_scope_corrections <- function(final_decisions, corrections, group_map) {
  if (nrow(corrections) == 0L) {
    audit <- empty_named_frame(FINAL_SCOPE_CORRECTION_AUDIT_COLUMNS)
    return(list(final_decisions = final_decisions, audit = audit))
  }

  correction_group_map <- group_map[match(corrections$record_id, group_map$record_id), , drop = FALSE]
  missing_records <- corrections$record_id[is.na(correction_group_map$retained_record_id)]
  if (length(missing_records) > 0L) {
    stop(
      "Final scope correction record_id(s) are absent from CP10 reconciliation: ",
      paste(missing_records, collapse = ", "),
      call. = FALSE
    )
  }

  corrections$retained_record_id <- correction_group_map$retained_record_id
  corrections$title_duplicate_group_id <- correction_group_map$title_duplicate_group_id

  duplicated_groups <- unique(corrections$retained_record_id[duplicated(corrections$retained_record_id)])
  if (length(duplicated_groups) > 0L) {
    stop(
      "Final scope correction table contains more than one correction for the same retained duplicate group: ",
      paste(duplicated_groups, collapse = ", "),
      call. = FALSE
    )
  }

  audit_rows <- vector("list", nrow(corrections))
  for (i in seq_len(nrow(corrections))) {
    retained_id <- corrections$retained_record_id[[i]]
    decision_index <- match(retained_id, final_decisions$record_id)
    if (is.na(decision_index)) {
      stop(
        "Retained record_id from final scope correction is absent from final decisions: ",
        retained_id,
        call. = FALSE
      )
    }

    previous <- final_decisions[decision_index, , drop = FALSE]
    corrected_decision <- corrections$corrected_final_title_abstract_decision[[i]]
    corrected_full_text <- ifelse(corrected_decision == "full_text", "yes", "no")
    corrected_rule <- paste0("post_review_scope_correction_", corrected_decision)

    final_decisions$final_title_abstract_decision[[decision_index]] <- corrected_decision
    final_decisions$full_text_required_final[[decision_index]] <- corrected_full_text
    final_decisions$reason_for_final_decision[[decision_index]] <- corrections$corrected_reason_for_final_decision[[i]]
    final_decisions$decision_rule[[decision_index]] <- corrected_rule

    audit_rows[[i]] <- data.frame(
      record_id = corrections$record_id[[i]],
      retained_record_id = retained_id,
      title_duplicate_group_id = corrections$title_duplicate_group_id[[i]],
      previous_final_title_abstract_decision = previous$final_title_abstract_decision,
      previous_full_text_required_final = previous$full_text_required_final,
      previous_reason_for_final_decision = previous$reason_for_final_decision,
      previous_decision_rule = previous$decision_rule,
      corrected_final_title_abstract_decision = corrected_decision,
      corrected_full_text_required_final = corrected_full_text,
      corrected_reason_for_final_decision = corrections$corrected_reason_for_final_decision[[i]],
      corrected_notes = corrections$corrected_notes[[i]],
      correction_date_local = corrections$correction_date_local[[i]],
      correction_source = corrections$correction_source[[i]],
      correction_rationale = corrections$correction_rationale[[i]],
      correction_status = "applied",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  audit <- do.call(rbind, audit_rows)
  audit <- audit[, FINAL_SCOPE_CORRECTION_AUDIT_COLUMNS, drop = FALSE]
  list(final_decisions = final_decisions, audit = audit)
}

build_scope_precision_review <- function(final_queue) {
  if (nrow(final_queue) == 0L) {
    return(empty_named_frame(SCOPE_PRECISION_REVIEW_COLUMNS))
  }

  final_queue <- as_character_frame(final_queue)
  flag_reason <- vapply(seq_len(nrow(final_queue)), function(i) {
    flags <- final_queue$candidate_flags[[i]]
    reasons <- character()
    if (!grepl("flag_has_ai_terms", flags, fixed = TRUE)) {
      reasons <- c(reasons, "missing_ai_flag")
    }
    if (grepl("flag_conference", flags, fixed = TRUE)) {
      reasons <- c(reasons, "conference_record")
    }
    if (grepl("flag_missing_abstract", flags, fixed = TRUE)) {
      reasons <- c(reasons, "missing_abstract")
    }
    if (grepl("flag_possible_therapy_not_imaging", flags, fixed = TRUE)) {
      reasons <- c(reasons, "possible_therapy_not_imaging")
    }
    paste(reasons, collapse = "; ")
  }, character(1L))

  keep <- nzchar(flag_reason)
  if (!any(keep)) {
    return(empty_named_frame(SCOPE_PRECISION_REVIEW_COLUMNS))
  }

  out <- data.frame(
    record_id = final_queue$record_id[keep],
    title = final_queue$title[keep],
    year = final_queue$year[keep],
    publication_type = final_queue$publication_type[keep],
    candidate_flags = final_queue$candidate_flags[keep],
    reviewer_1_decisions = final_queue$reviewer_1_decisions[keep],
    reviewer_2_decisions = final_queue$reviewer_2_decisions[keep],
    pre_adjudicated_decisions = final_queue$pre_adjudicated_decisions[keep],
    reason_for_full_text = final_queue$reason_for_full_text[keep],
    precision_review_flags = flag_reason[keep],
    recommended_action = "Review before CP11; if clearly outside scope, add a row to final_scope_corrections.csv and rerun B10c.",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[order(out$record_id, method = "radix"), SCOPE_PRECISION_REVIEW_COLUMNS, drop = FALSE]
}

validate_xlsx_record_ids <- function(xlsx_path, sheet_name, expected) {
  exported <- read_review_xlsx_sheet(xlsx_path, sheet_name, names(expected))
  exported_ids <- trimws(as.character(exported$record_id))
  expected_ids <- trimws(as.character(expected$record_id))

  if (!identical(exported_ids, expected_ids)) {
    stop(
      "XLSX export mismatch after writing ",
      project_relative_path(xlsx_path),
      ". Expected ",
      length(expected_ids),
      " record_id value(s), found ",
      length(exported_ids),
      ". Regenerate the checkpoint after closing any open workbook copies.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

records_path <- file.path(CFG$processed_dir, "records_after_dedup_primary.csv")
reconciliation_path <- file.path(CFG$screening_dir, "reconciliation", "screening_reconciliation.csv")
resolution_path <- file.path(CFG$screening_dir, "title_deduplication", "title_duplicate_resolution.csv")
pre_adjudication_xlsx_path <- file.path(CFG$screening_dir, "title_deduplication", "title_abstract_pre_adjudication_template_title_deduped.xlsx")
final_scope_corrections_path <- file.path(CFG$screening_dir, "title_deduplication", "final_scope_corrections.csv")

for (path in c(records_path, reconciliation_path, resolution_path, pre_adjudication_xlsx_path)) {
  if (!file.exists(path)) {
    stop("Required input is missing: ", project_relative_path(path), call. = FALSE)
  }
}

records <- read_machine_csv(records_path)
reconciliation <- read_machine_csv(reconciliation_path)
resolution <- read_machine_csv(resolution_path)
pre_adjudication <- read_validated_pre_adjudication(pre_adjudication_xlsx_path)
final_scope_corrections <- read_final_scope_corrections(final_scope_corrections_path)

validate_required_columns(records, c("record_id", "title", "abstract", "keywords", "authors", "year", "journal_or_source", "doi", "pmid", "url", "publication_type", "source_database", "source_file"), records_path)
validate_required_columns(reconciliation, SCREENING_RECONCILIATION_COLUMNS, reconciliation_path)
validate_required_columns(resolution, c("title_duplicate_group_id", "record_id", "action", "retained_record_id"), resolution_path)

group_map <- build_record_group_map(reconciliation, resolution)
reconciliation <- merge(reconciliation, group_map, by = "record_id", all.x = TRUE, sort = FALSE)

pre_group_map <- group_map[match(pre_adjudication$record_id, group_map$record_id), , drop = FALSE]
missing_pre_group <- is.na(pre_group_map$retained_record_id)
if (any(missing_pre_group)) {
  stop(
    "Pre-adjudication record_id(s) are absent from CP10 reconciliation: ",
    paste(pre_adjudication$record_id[missing_pre_group], collapse = ", "),
    call. = FALSE
  )
}
pre_adjudication$retained_record_id <- pre_group_map$retained_record_id
pre_adjudication$title_duplicate_group_id <- pre_group_map$title_duplicate_group_id

retained_ids <- sort(unique(reconciliation$retained_record_id), method = "radix")
decision_rows <- vector("list", length(retained_ids))

for (index in seq_along(retained_ids)) {
  retained_id <- retained_ids[[index]]
  rec_group <- reconciliation[reconciliation$retained_record_id == retained_id, , drop = FALSE]
  pre_group <- pre_adjudication[pre_adjudication$retained_record_id == retained_id, , drop = FALSE]

  group_id <- collapse_unique(rec_group$title_duplicate_group_id)
  group_record_ids <- collapse_unique(rec_group$record_id)
  absorbed_ids <- collapse_unique(rec_group$record_id[rec_group$record_id != retained_id])
  pre_decisions <- normalize_pre_adjudication_decision(pre_group$pre_adjudicated_decision)

  agreement_full_text <- any(rec_group$agreement_status == "agreement" & rec_group$full_text_required_by_rule == "yes")
  pre_full_text <- any(pre_decisions %in% c("full_text", "needs_discussion"))
  has_conflict <- any(rec_group$agreement_status == "conflict")

  final_decision <- ifelse(agreement_full_text || pre_full_text, "full_text", "exclude")
  full_text_required <- ifelse(identical(final_decision, "full_text"), "yes", "no")

  decision_rule <- if (agreement_full_text && pre_full_text) {
    "agreement_full_text_plus_pre_adjudication"
  } else if (agreement_full_text) {
    "agreement_full_text_rule"
  } else if (any(pre_decisions == "needs_discussion")) {
    "pre_adjudication_needs_discussion"
  } else if (any(pre_decisions == "full_text")) {
    "pre_adjudication_full_text"
  } else if (has_conflict) {
    "pre_adjudication_exclude"
  } else {
    "reviewer_agreement_exclude"
  }

  reason <- switch(
    decision_rule,
    agreement_full_text_plus_pre_adjudication = "At least one duplicate-group record required full text by reviewer agreement and human pre-adjudication also retained a conflict for full-text assessment.",
    agreement_full_text_rule = "At least one duplicate-group record required full text by reviewer agreement before pre-adjudication.",
    pre_adjudication_needs_discussion = "Human pre-adjudication marked at least one title/abstract conflict as needs_discussion; retained conservatively for full text/discussion.",
    pre_adjudication_full_text = "Human pre-adjudication retained at least one title/abstract conflict for full-text assessment.",
    pre_adjudication_exclude = "Human pre-adjudication excluded the title/abstract conflict(s), and no agreement row in the duplicate group required full text.",
    reviewer_agreement_exclude = "Both reviewers excluded the title/abstract record(s), and no conflict required human retention.",
    "Deterministic title/abstract rule applied."
  )

  decision_rows[[index]] <- data.frame(
    record_id = retained_id,
    title_duplicate_group_id = group_id,
    duplicate_group_record_ids = group_record_ids,
    absorbed_duplicate_record_ids = absorbed_ids,
    title = group_metadata(records, rec_group, retained_id, "title"),
    abstract = group_metadata(records, rec_group, retained_id, "abstract"),
    keywords = group_metadata(records, rec_group, retained_id, "keywords"),
    authors = group_metadata(records, rec_group, retained_id, "authors"),
    year = group_metadata(records, rec_group, retained_id, "year"),
    journal_or_source = group_metadata(records, rec_group, retained_id, "journal_or_source"),
    doi = group_metadata(records, rec_group, retained_id, "doi"),
    pmid = group_metadata(records, rec_group, retained_id, "pmid"),
    url = group_metadata(records, rec_group, retained_id, "url"),
    publication_type = collapse_unique(rec_group$publication_type),
    candidate_flags = collapse_unique(rec_group$candidate_flags),
    source_databases = collapse_unique(records$source_database[match(rec_group$record_id, records$record_id)]),
    source_files = collapse_unique(records$source_file[match(rec_group$record_id, records$record_id)]),
    reviewer_1_decisions = collapse_unique(rec_group$reviewer_1_decision),
    reviewer_2_decisions = collapse_unique(rec_group$reviewer_2_decision),
    agreement_statuses = collapse_unique(rec_group$agreement_status),
    pre_adjudicated_decisions = collapse_unique(pre_decisions),
    pre_adjudication_reasons = collapse_unique(pre_group$pre_adjudication_reason),
    pre_adjudicator_notes = collapse_unique(pre_group$pre_adjudicator_notes),
    final_title_abstract_decision = final_decision,
    full_text_required_final = full_text_required,
    reason_for_final_decision = reason,
    decision_rule = decision_rule,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

final_decisions <- do.call(rbind, decision_rows)
final_decisions <- final_decisions[order(final_decisions$final_title_abstract_decision != "full_text", final_decisions$record_id, method = "radix"), , drop = FALSE]
final_decisions <- final_decisions[, FINAL_SCREENING_DECISION_COLUMNS, drop = FALSE]

scope_correction_result <- apply_final_scope_corrections(final_decisions, final_scope_corrections, group_map)
final_decisions <- scope_correction_result$final_decisions
final_scope_correction_audit <- scope_correction_result$audit
final_decisions <- final_decisions[order(final_decisions$final_title_abstract_decision != "full_text", final_decisions$record_id, method = "radix"), , drop = FALSE]
final_decisions <- final_decisions[, FINAL_SCREENING_DECISION_COLUMNS, drop = FALSE]

final_queue <- final_decisions[final_decisions$full_text_required_final == "yes", , drop = FALSE]
final_queue <- data.frame(
  record_id = final_queue$record_id,
  title_duplicate_group_id = final_queue$title_duplicate_group_id,
  duplicate_group_record_ids = final_queue$duplicate_group_record_ids,
  absorbed_duplicate_record_ids = final_queue$absorbed_duplicate_record_ids,
  title = final_queue$title,
  abstract = final_queue$abstract,
  keywords = final_queue$keywords,
  authors = final_queue$authors,
  year = final_queue$year,
  journal_or_source = final_queue$journal_or_source,
  doi = final_queue$doi,
  pmid = final_queue$pmid,
  url = final_queue$url,
  publication_type = final_queue$publication_type,
  candidate_flags = final_queue$candidate_flags,
  source_databases = final_queue$source_databases,
  source_files = final_queue$source_files,
  reviewer_1_decisions = final_queue$reviewer_1_decisions,
  reviewer_2_decisions = final_queue$reviewer_2_decisions,
  pre_adjudicated_decisions = final_queue$pre_adjudicated_decisions,
  reason_for_full_text = final_queue$reason_for_final_decision,
  retrieval_status = "not_started",
  retrieval_notes = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, FINAL_FULL_TEXT_QUEUE_COLUMNS, drop = FALSE]

scope_precision_review <- build_scope_precision_review(final_queue)

audit_rows <- merge(
  reconciliation,
  pre_adjudication[, c("record_id", "pre_adjudicated_decision", "pre_adjudication_reason"), drop = FALSE],
  by = "record_id",
  all.x = TRUE,
  sort = FALSE
)
audit_rows$pre_adjudicated_decision[is.na(audit_rows$pre_adjudicated_decision)] <- ""
audit_rows$pre_adjudication_reason[is.na(audit_rows$pre_adjudication_reason)] <- ""

decision_lookup <- final_decisions[, c("record_id", "final_title_abstract_decision", "full_text_required_final", "decision_rule"), drop = FALSE]
names(decision_lookup) <- c("retained_record_id", "group_final_title_abstract_decision", "group_full_text_required_final", "group_decision_rule")
audit_rows <- merge(audit_rows, decision_lookup, by = "retained_record_id", all.x = TRUE, sort = FALSE)
audit_rows <- audit_rows[, c(
  "record_id",
  "retained_record_id",
  "title_duplicate_group_id",
  "title_duplicate_action",
  "title",
  "reviewer_1_decision",
  "reviewer_2_decision",
  "agreement_status",
  "full_text_required_by_rule",
  "pre_adjudicated_decision",
  "pre_adjudication_reason",
  "group_final_title_abstract_decision",
  "group_full_text_required_final",
  "group_decision_rule"
), drop = FALSE]
audit_rows <- audit_rows[order(audit_rows$retained_record_id, audit_rows$record_id, method = "radix"), , drop = FALSE]
audit_rows <- audit_rows[, PRE_ADJUDICATION_APPLICATION_AUDIT_COLUMNS, drop = FALSE]

full_text_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text"))
final_decisions_csv <- file.path(full_text_dir, "title_abstract_final_decisions.csv")
final_decisions_xlsx <- file.path(full_text_dir, "title_abstract_final_decisions.xlsx")
final_queue_csv <- file.path(full_text_dir, "final_full_text_retrieval_queue.csv")
final_queue_xlsx <- file.path(full_text_dir, "final_full_text_retrieval_queue.xlsx")
application_audit_csv <- file.path(full_text_dir, "pre_adjudication_application_audit.csv")
application_audit_xlsx <- file.path(full_text_dir, "pre_adjudication_application_audit.xlsx")
final_scope_correction_audit_csv <- file.path(full_text_dir, "final_scope_correction_audit.csv")
final_scope_correction_audit_xlsx <- file.path(full_text_dir, "final_scope_correction_audit.xlsx")
scope_precision_review_csv <- file.path(full_text_dir, "scope_precision_review_candidates.csv")
scope_precision_review_xlsx <- file.path(full_text_dir, "scope_precision_review_candidates.xlsx")

stable_write_csv(final_decisions, final_decisions_csv, FINAL_SCREENING_DECISION_COLUMNS)
stable_write_csv(final_queue, final_queue_csv, FINAL_FULL_TEXT_QUEUE_COLUMNS)
stable_write_csv(audit_rows, application_audit_csv, PRE_ADJUDICATION_APPLICATION_AUDIT_COLUMNS)
stable_write_csv(final_scope_correction_audit, final_scope_correction_audit_csv, FINAL_SCOPE_CORRECTION_AUDIT_COLUMNS)
stable_write_csv(scope_precision_review, scope_precision_review_csv, SCOPE_PRECISION_REVIEW_COLUMNS)

write_review_xlsx(list(title_abstract_final_decisions = final_decisions), final_decisions_xlsx)
write_review_xlsx(list(final_full_text_retrieval_queue = final_queue), final_queue_xlsx)
write_review_xlsx(list(pre_adjudication_application_audit = audit_rows), application_audit_xlsx)
write_review_xlsx(list(final_scope_correction_audit = final_scope_correction_audit), final_scope_correction_audit_xlsx)
write_review_xlsx(list(scope_precision_review = scope_precision_review), scope_precision_review_xlsx)

validate_xlsx_record_ids(final_queue_xlsx, "final_full_text_retrieval_queue", final_queue)

pre_decision <- normalize_pre_adjudication_decision(pre_adjudication$pre_adjudicated_decision)
final_full_text_records <- nrow(final_queue)
final_excluded_records <- sum(final_decisions$final_title_abstract_decision == "exclude")
agreement_full_text_groups <- sum(final_decisions$decision_rule %in% c("agreement_full_text_rule", "agreement_full_text_plus_pre_adjudication"))
pre_adjudication_only_full_text_groups <- sum(final_decisions$decision_rule == "pre_adjudication_full_text")
needs_discussion_groups <- sum(final_decisions$decision_rule == "pre_adjudication_needs_discussion")
final_scope_corrections_applied <- nrow(final_scope_correction_audit)
scope_precision_review_candidates <- nrow(scope_precision_review)
final_queue_without_ai_flag <- sum(!grepl("flag_has_ai_terms", final_queue$candidate_flags, fixed = TRUE))

review_file <- write_checkpoint(
  id = "CP10C",
  name = "apply_pre_adjudication",
  what_ran = paste(
    "Validated the human pre-adjudication workbook, applied those decisions to",
    "the title-deduplicated screening set, and generated final title/abstract",
    "decisions plus the full-text retrieval queue."
  ),
  numbers = c(
    title_deduplicated_screening_groups = nrow(final_decisions),
    pre_adjudicated_conflict_rows = nrow(pre_adjudication),
    pre_adjudication_full_text = sum(pre_decision == "full_text"),
    pre_adjudication_exclude = sum(pre_decision == "exclude"),
    pre_adjudication_needs_discussion = sum(pre_decision == "needs_discussion"),
    agreement_full_text_groups = agreement_full_text_groups,
    pre_adjudication_only_full_text_groups = pre_adjudication_only_full_text_groups,
    needs_discussion_groups = needs_discussion_groups,
    final_scope_corrections_applied = final_scope_corrections_applied,
    final_full_text_retrieval_records = final_full_text_records,
    final_title_abstract_exclusions = final_excluded_records,
    scope_precision_review_candidates = scope_precision_review_candidates,
    final_queue_without_ai_flag = final_queue_without_ai_flag
  ),
  review_items = c(
    "Review final_full_text_retrieval_queue.xlsx before retrieving PDFs or full-text files.",
    "Review title_abstract_final_decisions.xlsx to verify the final title/abstract disposition counts.",
    "Use pre_adjudication_application_audit.xlsx if any retained or excluded duplicate group needs tracing.",
    "Use final_scope_correction_audit.xlsx to verify post-review scope corrections applied after the initial deterministic rule.",
    "Review scope_precision_review_candidates.xlsx before CP11; add further clear exclusions to final_scope_corrections.csv and rerun CP10C.",
    "Do not treat CP10C as full-text eligibility assessment; it only closes title/abstract screening."
  ),
  outputs = c(
    final_decisions_csv,
    final_decisions_xlsx,
    final_queue_csv,
    final_queue_xlsx,
    application_audit_csv,
    application_audit_xlsx,
    final_scope_correction_audit_csv,
    final_scope_correction_audit_xlsx,
    scope_precision_review_csv,
    scope_precision_review_xlsx,
    file.path(CFG$checkpoints_dir, "CP10C_apply_pre_adjudication.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = "CP10C closes title/abstract screening only. Full-text eligibility, exclusion reasons, and quality assessment remain future human-review checkpoints.",
  gate_question = "Approve CP10C only after the final full-text retrieval queue and audit trail have been inspected."
)

cat("CP10C apply pre-adjudication complete\n")
cat("Title-deduplicated screening groups:", nrow(final_decisions), "\n")
cat("Pre-adjudicated conflict rows:", nrow(pre_adjudication), "\n")
cat("Pre-adjudication full_text:", sum(pre_decision == "full_text"), "\n")
cat("Pre-adjudication exclude:", sum(pre_decision == "exclude"), "\n")
cat("Final scope corrections applied:", final_scope_corrections_applied, "\n")
cat("Final full-text retrieval records:", final_full_text_records, "\n")
cat("Final title/abstract exclusions:", final_excluded_records, "\n")
cat("Scope precision review candidates:", scope_precision_review_candidates, "\n")
cat("Final queue records without AI flag:", final_queue_without_ai_flag, "\n")
cat("Final full-text queue:", project_relative_path(final_queue_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
