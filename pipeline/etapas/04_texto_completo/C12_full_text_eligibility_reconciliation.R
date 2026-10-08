#!/usr/bin/env Rscript

# Compara as avaliações dos textos completos e organiza as discordâncias.
# As exclusões precisam ter uma justificativa documentada e uma chave válida.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP11C")

require_checkpoint_approved <- function(checkpoint_id) {
  approvals <- read_csv_or_empty(checkpoint_approvals_path(), CHECKPOINT_APPROVAL_COLUMNS)
  checkpoint_id <- checkpoint_id_normalize(checkpoint_id)
  row <- approvals[approvals$checkpoint_id == checkpoint_id, , drop = FALSE]
  if (nrow(row) == 0L || !identical(tolower(trimws(row$approved[[1L]])), "yes")) {
    stop("CP12 requires ", checkpoint_id, " approval before full-text reconciliation.", call. = FALSE)
  }
  invisible(TRUE)
}

require_checkpoint_approved("CP11C")

FULL_TEXT_ELIGIBILITY_COLUMNS <- c(
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
  "reason_for_full_text",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "agreement_statuses",
  "pre_adjudicated_decisions",
  "pre_adjudication_reasons",
  "pre_adjudicator_notes",
  "decision_rule",
  "pdf_filename",
  "pdf_path",
  "full_text_decision",
  "primary_exclusion_reason",
  "secondary_exclusion_reason",
  "decision_notes",
  "reviewer",
  "decision_date"
)

FULL_TEXT_UNAVAILABLE_COLUMNS <- c(
  "record_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "url",
  "retrieval_status",
  "online_search_status",
  "author_contact_status",
  "retrieval_note",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "pre_adjudicated_decisions",
  "decision_rule"
)

FULL_TEXT_RECONCILIATION_COLUMNS <- c(
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
  "reason_for_full_text",
  "pdf_filename",
  "pdf_path",
  "reviewer_1_full_text_decision",
  "reviewer_1_primary_exclusion_reason",
  "reviewer_1_secondary_exclusion_reason",
  "reviewer_1_decision_notes",
  "reviewer_1_decision_date",
  "reviewer_2_full_text_decision",
  "reviewer_2_primary_exclusion_reason",
  "reviewer_2_secondary_exclusion_reason",
  "reviewer_2_decision_notes",
  "reviewer_2_decision_date",
  "decision_agreement_status",
  "exclusion_reason_agreement_status",
  "adjudication_required",
  "adjudication_priority",
  "adjudication_trigger",
  "preliminary_final_full_text_decision",
  "preliminary_primary_exclusion_reason",
  "preliminary_secondary_exclusion_reason",
  "preliminary_decision_rule",
  "next_action"
)

FULL_TEXT_ADJUDICATION_COLUMNS <- c(
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
  "reason_for_full_text",
  "pdf_filename",
  "pdf_path",
  "reviewer_1_full_text_decision",
  "reviewer_1_primary_exclusion_reason",
  "reviewer_1_secondary_exclusion_reason",
  "reviewer_1_decision_notes",
  "reviewer_2_full_text_decision",
  "reviewer_2_primary_exclusion_reason",
  "reviewer_2_secondary_exclusion_reason",
  "reviewer_2_decision_notes",
  "decision_agreement_status",
  "exclusion_reason_agreement_status",
  "adjudication_priority",
  "adjudication_trigger",
  "adjudicated_full_text_decision",
  "adjudicated_primary_exclusion_reason",
  "adjudicated_secondary_exclusion_reason",
  "adjudication_notes",
  "adjudicator",
  "adjudication_date"
)

FULL_TEXT_ADJUDICATION_CODEBOOK_COLUMNS <- c(
  "field",
  "allowed_value",
  "meaning",
  "required_when"
)

FULL_TEXT_RECONCILIATION_VALIDATION_COLUMNS <- c(
  "severity",
  "issue_type",
  "reviewer_id",
  "record_id",
  "field",
  "value",
  "details"
)

FULL_TEXT_RECONCILIATION_AUDIT_COLUMNS <- c(
  "audit_id",
  "record_id",
  "action",
  "from_value",
  "to_value",
  "reason",
  "script"
)

FULL_TEXT_RECONCILIATION_COUNT_COLUMNS <- c("metric", "value")

full_text_decision_values <- function() {
  c("include_for_extraction", "exclude_full_text", "unclear_needs_discussion")
}

full_text_exclusion_reason_values <- function() {
  c(
    "no_ai_model",
    "no_thermal_input",
    "no_pain_target",
    "not_human",
    "not_original",
    "abstract_only",
    "registry_no_results",
    "no_extractable_model_data",
    "wrong_population",
    "wrong_outcome",
    "duplicate",
    "language_unavailable",
    "other"
  )
}

full_text_adjudication_codebook <- function() {
  data.frame(
    field = c(
      "adjudicated_full_text_decision",
      "adjudicated_full_text_decision",
      rep("adjudicated_primary_exclusion_reason", length(full_text_exclusion_reason_values())),
      "adjudicated_secondary_exclusion_reason",
      "adjudication_notes",
      "adjudicator",
      "adjudication_date"
    ),
    allowed_value = c(
      "include_for_extraction",
      "exclude_full_text",
      full_text_exclusion_reason_values(),
      "same controlled values as adjudicated_primary_exclusion_reason; optional",
      "free text",
      "human adjudicator name or initials",
      "YYYY-MM-DD"
    ),
    meaning = c(
      "Final adjudicated decision: the full text meets review scope and proceeds to data extraction.",
      "Final adjudicated decision: the full text is excluded after full-text assessment.",
      "No eligible AI, machine learning, deep learning, computer vision, classifier, predictive model, or comparable computational method.",
      "No eligible infrared thermography, thermal imaging, thermal camera, or thermal-image-derived input.",
      "No eligible pain outcome, pain assessment, painful condition, analgesia/pain-control assessment, or pain-related clinical target.",
      "Animal, phantom, simulation-only, in vitro, or non-human-only study.",
      "Review, editorial, protocol, commentary, dataset note, or non-primary report.",
      "Conference abstract only or bibliographic abstract without a complete full paper.",
      "Protocol, trial registry, or registry-derived record without extractable results.",
      "No extractable model, validation, diagnostic, classification, segmentation, or performance information relevant to the review.",
      "Population or clinical setting is outside the review question.",
      "Outcome, target, or modeled construct is outside the review question.",
      "Same study/report superseded by another retained full text.",
      "Full text exists but cannot be assessed because language/translation is unavailable.",
      "Use only when no controlled reason fits; explain in adjudication_notes.",
      "Optional additional exclusion reason.",
      "Brief rationale resolving reviewer conflict, uncertainty, or exclusion-reason mismatch.",
      "Human accountability for final adjudication.",
      "Date of final joint decision."
    ),
    required_when = c(
      "always for rows in adjudication_required",
      "always for rows in adjudication_required",
      rep("adjudicated_full_text_decision == exclude_full_text", length(full_text_exclusion_reason_values())),
      "optional",
      "always",
      "always",
      "always"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, FULL_TEXT_ADJUDICATION_CODEBOOK_COLUMNS, drop = FALSE]
}

validation_issue <- function(severity, issue_type, reviewer_id, record_id, field, value, details) {
  data.frame(
    severity = severity,
    issue_type = issue_type,
    reviewer_id = reviewer_id,
    record_id = record_id,
    field = field,
    value = value,
    details = details,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, FULL_TEXT_RECONCILIATION_VALIDATION_COLUMNS, drop = FALSE]
}

bind_issue_rows <- function(rows) {
  rows <- rows[vapply(rows, function(x) !is.null(x) && nrow(x) > 0L, logical(1L))]
  if (length(rows) == 0L) {
    return(empty_named_frame(FULL_TEXT_RECONCILIATION_VALIDATION_COLUMNS))
  }
  do.call(rbind, rows)[, FULL_TEXT_RECONCILIATION_VALIDATION_COLUMNS, drop = FALSE]
}

audit_row <- function(record_id, action, from_value, to_value, reason) {
  data.frame(
    audit_id = "",
    record_id = record_id,
    action = action,
    from_value = from_value,
    to_value = to_value,
    reason = reason,
    script = "pipeline/scripts/C12_full_text_eligibility_reconciliation.R",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, FULL_TEXT_RECONCILIATION_AUDIT_COLUMNS, drop = FALSE]
}

bind_audit_rows <- function(rows) {
  rows <- rows[vapply(rows, function(x) !is.null(x) && nrow(x) > 0L, logical(1L))]
  if (length(rows) == 0L) {
    return(empty_named_frame(FULL_TEXT_RECONCILIATION_AUDIT_COLUMNS))
  }
  out <- do.call(rbind, rows)
  out$audit_id <- sprintf("CP12_AUDIT_%05d", seq_len(nrow(out)))
  out[, FULL_TEXT_RECONCILIATION_AUDIT_COLUMNS, drop = FALSE]
}

prepare_eligibility <- function(path, reviewer_id, reviewer_label) {
  if (!file.exists(path)) {
    stop("Missing normalized reviewer CSV: ", project_relative_path(path), call. = FALSE)
  }
  data <- read_machine_csv(path)
  validate_required_columns(data, FULL_TEXT_ELIGIBILITY_COLUMNS, path)
  data <- as_character_frame(data[, FULL_TEXT_ELIGIBILITY_COLUMNS, drop = FALSE])
  data$record_id <- trimws(data$record_id)
  data$full_text_decision <- trimws(data$full_text_decision)
  data$primary_exclusion_reason <- trimws(data$primary_exclusion_reason)
  data$secondary_exclusion_reason <- trimws(data$secondary_exclusion_reason)
  data$decision_date <- trimws(data$decision_date)
  data$reviewer <- trimws(data$reviewer)
  data <- data[nzchar(data$record_id), , drop = FALSE]
  data <- data[order(data$record_id, method = "radix"), , drop = FALSE]
  attr(data, "reviewer_id") <- reviewer_id
  attr(data, "reviewer_label") <- reviewer_label
  data
}

eligibility_validation_issues <- function(data) {
  reviewer_id <- attr(data, "reviewer_id")
  reviewer_label <- attr(data, "reviewer_label")
  issues <- list()
  allowed_decisions <- full_text_decision_values()
  allowed_reasons <- c(full_text_exclusion_reason_values(), "")

  duplicate_ids <- unique(data$record_id[duplicated(data$record_id)])
  if (length(duplicate_ids) > 0L) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(duplicate_ids, function(record_id) {
      validation_issue("BLOCKER", "duplicate_record_id", reviewer_id, record_id, "record_id", record_id, "Each reviewer input must contain each record_id once.")
    }))
  }

  invalid_decisions <- !(data$full_text_decision %in% allowed_decisions)
  if (any(invalid_decisions)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(invalid_decisions), function(i) {
      validation_issue("BLOCKER", "invalid_full_text_decision", reviewer_id, data$record_id[[i]], "full_text_decision", data$full_text_decision[[i]], "Value is not in the full-text eligibility codebook.")
    }))
  }

  invalid_primary <- !(data$primary_exclusion_reason %in% allowed_reasons)
  if (any(invalid_primary)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(invalid_primary), function(i) {
      validation_issue("BLOCKER", "invalid_primary_exclusion_reason", reviewer_id, data$record_id[[i]], "primary_exclusion_reason", data$primary_exclusion_reason[[i]], "Value is not in the full-text eligibility codebook.")
    }))
  }

  invalid_secondary <- !(data$secondary_exclusion_reason %in% allowed_reasons)
  if (any(invalid_secondary)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(invalid_secondary), function(i) {
      validation_issue("BLOCKER", "invalid_secondary_exclusion_reason", reviewer_id, data$record_id[[i]], "secondary_exclusion_reason", data$secondary_exclusion_reason[[i]], "Value is not in the full-text eligibility codebook.")
    }))
  }

  missing_reason <- data$full_text_decision == "exclude_full_text" & !nzchar(data$primary_exclusion_reason)
  if (any(missing_reason)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(missing_reason), function(i) {
      validation_issue("BLOCKER", "missing_primary_exclusion_reason", reviewer_id, data$record_id[[i]], "primary_exclusion_reason", "", "exclude_full_text requires a primary exclusion reason.")
    }))
  }

  include_with_reason <- data$full_text_decision == "include_for_extraction" &
    (nzchar(data$primary_exclusion_reason) | nzchar(data$secondary_exclusion_reason))
  if (any(include_with_reason)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(include_with_reason), function(i) {
      validation_issue("BLOCKER", "include_has_exclusion_reason", reviewer_id, data$record_id[[i]], "primary_exclusion_reason/secondary_exclusion_reason", paste(data$primary_exclusion_reason[[i]], data$secondary_exclusion_reason[[i]], sep = ";"), "Included records must not carry exclusion reason codes.")
    }))
  }

  unclear_without_notes <- data$full_text_decision == "unclear_needs_discussion" & !nzchar(data$decision_notes)
  if (any(unclear_without_notes)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(unclear_without_notes), function(i) {
      validation_issue("BLOCKER", "unclear_without_notes", reviewer_id, data$record_id[[i]], "decision_notes", "", "unclear_needs_discussion requires notes.")
    }))
  }

  bad_dates <- !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", data$decision_date)
  if (any(bad_dates)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(bad_dates), function(i) {
      validation_issue("BLOCKER", "invalid_decision_date", reviewer_id, data$record_id[[i]], "decision_date", data$decision_date[[i]], "Decision date must be ISO text YYYY-MM-DD.")
    }))
  }

  bad_reviewer <- data$reviewer != reviewer_label
  if (any(bad_reviewer)) {
    issues[[length(issues) + 1L]] <- do.call(rbind, lapply(which(bad_reviewer), function(i) {
      validation_issue("BLOCKER", "invalid_reviewer_label", reviewer_id, data$record_id[[i]], "reviewer", data$reviewer[[i]], paste0("Reviewer must be ", reviewer_label, "."))
    }))
  }

  bind_issue_rows(issues)
}

value_by_record <- function(data, record_ids, column) {
  values <- data[[column]][match(record_ids, data$record_id)]
  values[is.na(values)] <- ""
  values
}

reasons_match <- function(r1_primary, r1_secondary, r2_primary, r2_secondary) {
  identical(trimws(r1_primary), trimws(r2_primary)) &&
    identical(trimws(r1_secondary), trimws(r2_secondary))
}

classify_adjudication <- function(r1_decision, r1_primary, r1_secondary, r2_decision, r2_primary, r2_secondary) {
  if (!nzchar(r1_decision) || !nzchar(r2_decision)) {
    return(c("yes", "P0_input_validation", "missing_or_invalid_reviewer_decision"))
  }
  if (!identical(r1_decision, r2_decision)) {
    return(c("yes", "P1_decision_conflict", paste0(r1_decision, "_vs_", r2_decision)))
  }
  if (identical(r1_decision, "unclear_needs_discussion")) {
    return(c("yes", "P2_unclear_needs_discussion", "both_reviewers_unclear"))
  }
  if (identical(r1_decision, "exclude_full_text") && !reasons_match(r1_primary, r1_secondary, r2_primary, r2_secondary)) {
    return(c("yes", "P3_exclusion_reason_conflict", "exclude_agreement_but_reason_mismatch"))
  }
  c("no", "NONE", "reviewer_agreement")
}

preliminary_decision <- function(adjudication_required, decision, primary_reason, secondary_reason) {
  if (identical(adjudication_required, "yes")) {
    return(c("", "", "", "requires_human_adjudication"))
  }
  if (identical(decision, "include_for_extraction")) {
    return(c("include_for_extraction", "", "", "reviewer_agreement_include"))
  }
  if (identical(decision, "exclude_full_text")) {
    return(c("exclude_full_text", primary_reason, secondary_reason, "reviewer_agreement_exclude"))
  }
  c("", "", "", "requires_human_adjudication")
}

write_full_text_adjudication_xlsx <- function(adjudication_required, auto_resolved, unavailable, codebook, path) {
  require_openxlsx()
  ensure_parent_dir(path)

  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF7",
    border = "Bottom",
    halign = "left",
    valign = "top"
  )
  body_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  editable_style <- openxlsx::createStyle(fgFill = "#FFF3CD", wrapText = TRUE, valign = "top")

  add_sheet <- function(sheet_name, data, editable_cols = character()) {
    data <- xlsx_truncate_frame(data)
    if (ncol(data) == 0L) {
      data <- data.frame(note = character(), stringsAsFactors = FALSE)
    }
    openxlsx::addWorksheet(workbook, sheet_name, gridLines = TRUE)
    openxlsx::writeData(workbook, sheet_name, data, startRow = 1L, startCol = 1L, colNames = TRUE)
    openxlsx::addStyle(workbook, sheet_name, header_style, rows = 1L, cols = seq_len(ncol(data)), gridExpand = TRUE)
    if (nrow(data) > 0L) {
      rows <- seq_len(nrow(data)) + 1L
      openxlsx::addStyle(workbook, sheet_name, body_style, rows = rows, cols = seq_len(ncol(data)), gridExpand = TRUE)
      editable_index <- match(editable_cols, names(data))
      editable_index <- editable_index[!is.na(editable_index)]
      if (length(editable_index) > 0L) {
        openxlsx::addStyle(workbook, sheet_name, editable_style, rows = rows, cols = editable_index, gridExpand = TRUE, stack = TRUE)
      }
    }
    openxlsx::freezePane(workbook, sheet_name, firstActiveRow = 2L, firstActiveCol = 2L)
    openxlsx::addFilter(workbook, sheet_name, row = 1L, cols = seq_len(ncol(data)))
    widths <- pmin(pmax(nchar(names(data), type = "chars") + 2L, 12L), 46L)
    openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(ncol(data)), widths = widths)
  }

  add_sheet(
    "adjudication_required",
    adjudication_required,
    editable_cols = c(
      "adjudicated_full_text_decision",
      "adjudicated_primary_exclusion_reason",
      "adjudicated_secondary_exclusion_reason",
      "adjudication_notes",
      "adjudicator",
      "adjudication_date"
    )
  )
  add_sheet("auto_resolved_reference", auto_resolved)
  add_sheet("unavailable_reference", unavailable)
  add_sheet("codebook", codebook)

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  clean_generated_xlsx_structure(path)
  invisible(path)
}

eligibility_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text", "eligibility"))
reviewer_root <- file.path(eligibility_dir, "reviewer_inputs")
reconciliation_dir <- ensure_dir(file.path(eligibility_dir, "reconciliation"))

r1_path <- file.path(reviewer_root, "reviewer_1", "full_text_eligibility_reviewer_1.csv")
r2_path <- file.path(reviewer_root, "reviewer_2", "full_text_eligibility_reviewer_2.csv")
r1_unavailable_path <- file.path(reviewer_root, "reviewer_1", "full_text_eligibility_reviewer_1_unavailable_reference.csv")
r2_unavailable_path <- file.path(reviewer_root, "reviewer_2", "full_text_eligibility_reviewer_2_unavailable_reference.csv")

r1 <- prepare_eligibility(r1_path, "reviewer_1", "Revisor 1")
r2 <- prepare_eligibility(r2_path, "reviewer_2", "Revisor 2")

validation_issues <- bind_issue_rows(list(
  eligibility_validation_issues(r1),
  eligibility_validation_issues(r2)
))

r1_ids <- sort(unique(r1$record_id), method = "radix")
r2_ids <- sort(unique(r2$record_id), method = "radix")
only_r1 <- setdiff(r1_ids, r2_ids)
only_r2 <- setdiff(r2_ids, r1_ids)
if (length(only_r1) > 0L) {
  validation_issues <- rbind(
    validation_issues,
    do.call(rbind, lapply(only_r1, function(record_id) {
      validation_issue("BLOCKER", "record_missing_from_reviewer_2", "reviewer_2", record_id, "record_id", record_id, "Record is present in Revisor 1 but absent from Revisor 2.")
    }))
  )
}
if (length(only_r2) > 0L) {
  validation_issues <- rbind(
    validation_issues,
    do.call(rbind, lapply(only_r2, function(record_id) {
      validation_issue("BLOCKER", "record_missing_from_reviewer_1", "reviewer_1", record_id, "record_id", record_id, "Record is present in Revisor 2 but absent from Revisor 1.")
    }))
  )
}
validation_issues <- validation_issues[, FULL_TEXT_RECONCILIATION_VALIDATION_COLUMNS, drop = FALSE]

record_ids <- sort(intersect(r1_ids, r2_ids), method = "radix")
r1_match <- r1[match(record_ids, r1$record_id), , drop = FALSE]
r2_match <- r2[match(record_ids, r2$record_id), , drop = FALSE]

classifications <- t(mapply(
  classify_adjudication,
  r1_match$full_text_decision,
  r1_match$primary_exclusion_reason,
  r1_match$secondary_exclusion_reason,
  r2_match$full_text_decision,
  r2_match$primary_exclusion_reason,
  r2_match$secondary_exclusion_reason,
  USE.NAMES = FALSE
))
classifications <- as.data.frame(classifications, stringsAsFactors = FALSE)
names(classifications) <- c("adjudication_required", "adjudication_priority", "adjudication_trigger")

preliminary <- t(mapply(
  preliminary_decision,
  classifications$adjudication_required,
  r1_match$full_text_decision,
  r1_match$primary_exclusion_reason,
  r1_match$secondary_exclusion_reason,
  USE.NAMES = FALSE
))
preliminary <- as.data.frame(preliminary, stringsAsFactors = FALSE)
names(preliminary) <- c(
  "preliminary_final_full_text_decision",
  "preliminary_primary_exclusion_reason",
  "preliminary_secondary_exclusion_reason",
  "preliminary_decision_rule"
)

decision_agreement_status <- ifelse(
  r1_match$full_text_decision == r2_match$full_text_decision,
  "agreement",
  "conflict"
)
exclusion_reason_agreement_status <- ifelse(
  r1_match$full_text_decision == "exclude_full_text" & r2_match$full_text_decision == "exclude_full_text",
  ifelse(
    r1_match$primary_exclusion_reason == r2_match$primary_exclusion_reason &
      r1_match$secondary_exclusion_reason == r2_match$secondary_exclusion_reason,
    "agreement",
    "conflict"
  ),
  "not_applicable"
)

reconciliation <- data.frame(
  record_id = record_ids,
  title_duplicate_group_id = r1_match$title_duplicate_group_id,
  title = r1_match$title,
  authors = r1_match$authors,
  year = r1_match$year,
  journal_or_source = r1_match$journal_or_source,
  doi = r1_match$doi,
  pmid = r1_match$pmid,
  url = r1_match$url,
  publication_type = r1_match$publication_type,
  candidate_flags = r1_match$candidate_flags,
  source_databases = r1_match$source_databases,
  reason_for_full_text = r1_match$reason_for_full_text,
  pdf_filename = r1_match$pdf_filename,
  pdf_path = r1_match$pdf_path,
  reviewer_1_full_text_decision = r1_match$full_text_decision,
  reviewer_1_primary_exclusion_reason = r1_match$primary_exclusion_reason,
  reviewer_1_secondary_exclusion_reason = r1_match$secondary_exclusion_reason,
  reviewer_1_decision_notes = r1_match$decision_notes,
  reviewer_1_decision_date = r1_match$decision_date,
  reviewer_2_full_text_decision = r2_match$full_text_decision,
  reviewer_2_primary_exclusion_reason = r2_match$primary_exclusion_reason,
  reviewer_2_secondary_exclusion_reason = r2_match$secondary_exclusion_reason,
  reviewer_2_decision_notes = r2_match$decision_notes,
  reviewer_2_decision_date = r2_match$decision_date,
  decision_agreement_status = decision_agreement_status,
  exclusion_reason_agreement_status = exclusion_reason_agreement_status,
  adjudication_required = classifications$adjudication_required,
  adjudication_priority = classifications$adjudication_priority,
  adjudication_trigger = classifications$adjudication_trigger,
  preliminary_final_full_text_decision = preliminary$preliminary_final_full_text_decision,
  preliminary_primary_exclusion_reason = preliminary$preliminary_primary_exclusion_reason,
  preliminary_secondary_exclusion_reason = preliminary$preliminary_secondary_exclusion_reason,
  preliminary_decision_rule = preliminary$preliminary_decision_rule,
  next_action = ifelse(classifications$adjudication_required == "yes", "human_adjudication_required", "no_human_adjudication_required"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, FULL_TEXT_RECONCILIATION_COLUMNS, drop = FALSE]

priority_order <- c("P0_input_validation", "P1_decision_conflict", "P2_unclear_needs_discussion", "P3_exclusion_reason_conflict", "NONE")
reconciliation <- reconciliation[order(match(reconciliation$adjudication_priority, priority_order), reconciliation$record_id, method = "radix"), , drop = FALSE]

adjudication_required <- reconciliation[reconciliation$adjudication_required == "yes", , drop = FALSE]
adjudication_template <- data.frame(
  adjudication_required[, c(
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
    "reason_for_full_text",
    "pdf_filename",
    "pdf_path",
    "reviewer_1_full_text_decision",
    "reviewer_1_primary_exclusion_reason",
    "reviewer_1_secondary_exclusion_reason",
    "reviewer_1_decision_notes",
    "reviewer_2_full_text_decision",
    "reviewer_2_primary_exclusion_reason",
    "reviewer_2_secondary_exclusion_reason",
    "reviewer_2_decision_notes",
    "decision_agreement_status",
    "exclusion_reason_agreement_status",
    "adjudication_priority",
    "adjudication_trigger"
  ), drop = FALSE],
  adjudicated_full_text_decision = "",
  adjudicated_primary_exclusion_reason = "",
  adjudicated_secondary_exclusion_reason = "",
  adjudication_notes = "",
  adjudicator = "",
  adjudication_date = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, FULL_TEXT_ADJUDICATION_COLUMNS, drop = FALSE]

auto_resolved <- reconciliation[reconciliation$adjudication_required == "no", , drop = FALSE]
auto_resolved <- auto_resolved[order(auto_resolved$record_id, method = "radix"), , drop = FALSE]

read_unavailable <- function(path, reviewer_id) {
  if (!file.exists(path)) {
    return(empty_named_frame(FULL_TEXT_UNAVAILABLE_COLUMNS))
  }
  data <- read_machine_csv(path)
  validate_required_columns(data, FULL_TEXT_UNAVAILABLE_COLUMNS, path)
  data <- as_character_frame(data[, FULL_TEXT_UNAVAILABLE_COLUMNS, drop = FALSE])
  data$record_id <- trimws(data$record_id)
  data$reviewer_source <- reviewer_id
  data
}

unavailable <- rbind(
  read_unavailable(r1_unavailable_path, "reviewer_1"),
  read_unavailable(r2_unavailable_path, "reviewer_2")
)
if (nrow(unavailable) > 0L) {
  unavailable <- unavailable[!duplicated(unavailable$record_id), names(unavailable) != "reviewer_source", drop = FALSE]
  unavailable <- unavailable[order(unavailable$record_id, method = "radix"), , drop = FALSE]
} else {
  unavailable <- empty_named_frame(FULL_TEXT_UNAVAILABLE_COLUMNS)
}

audit <- bind_audit_rows(lapply(seq_len(nrow(reconciliation)), function(i) {
  row <- reconciliation[i, , drop = FALSE]
  if (identical(row$adjudication_required, "yes")) {
    return(audit_row(
      row$record_id,
      "flag_for_human_adjudication",
      paste(row$reviewer_1_full_text_decision, row$reviewer_2_full_text_decision, sep = "|"),
      row$adjudication_priority,
      paste("Reviewers did not produce a directly auto-resolvable full-text eligibility outcome:", row$adjudication_trigger)
    ))
  }
  audit_row(
    row$record_id,
    "derive_preliminary_final_decision_from_agreement",
    paste(row$reviewer_1_full_text_decision, row$reviewer_2_full_text_decision, sep = "|"),
    row$preliminary_final_full_text_decision,
    "Both reviewers agreed on the full-text eligibility decision and no exclusion-reason conflict was detected."
  )
}))

counts_values <- c(
  reviewer_1_eligibility_records = nrow(r1),
  reviewer_2_eligibility_records = nrow(r2),
  reconciled_records = nrow(reconciliation),
  unavailable_reference_records = nrow(unavailable),
  reviewer_decision_agreements = sum(reconciliation$decision_agreement_status == "agreement"),
  reviewer_decision_conflicts = sum(reconciliation$decision_agreement_status == "conflict"),
  exclusion_reason_conflicts = sum(reconciliation$exclusion_reason_agreement_status == "conflict"),
  records_with_any_unclear = sum(reconciliation$reviewer_1_full_text_decision == "unclear_needs_discussion" | reconciliation$reviewer_2_full_text_decision == "unclear_needs_discussion"),
  auto_resolved_records = nrow(auto_resolved),
  adjudication_required_records = nrow(adjudication_template),
  preliminary_include_for_extraction = sum(reconciliation$preliminary_final_full_text_decision == "include_for_extraction"),
  preliminary_exclude_full_text = sum(reconciliation$preliminary_final_full_text_decision == "exclude_full_text"),
  validation_blockers = sum(validation_issues$severity == "BLOCKER"),
  validation_warnings = sum(validation_issues$severity == "WARNING")
)
counts <- data.frame(
  metric = names(counts_values),
  value = as.character(unname(counts_values)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, FULL_TEXT_RECONCILIATION_COUNT_COLUMNS, drop = FALSE]

reconciliation_csv <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation.csv")
reconciliation_xlsx <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation.xlsx")
adjudication_csv <- file.path(reconciliation_dir, "full_text_eligibility_adjudication_template.csv")
adjudication_xlsx <- file.path(reconciliation_dir, "full_text_eligibility_adjudication_template.xlsx")
auto_resolved_csv <- file.path(reconciliation_dir, "full_text_eligibility_auto_resolved_reference.csv")
auto_resolved_xlsx <- file.path(reconciliation_dir, "full_text_eligibility_auto_resolved_reference.xlsx")
unavailable_csv <- file.path(reconciliation_dir, "full_text_eligibility_unavailable_reference_reconciled.csv")
unavailable_xlsx <- file.path(reconciliation_dir, "full_text_eligibility_unavailable_reference_reconciled.xlsx")
audit_csv <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation_audit.csv")
audit_xlsx <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation_audit.xlsx")
validation_csv <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation_validation_issues.csv")
validation_xlsx <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation_validation_issues.xlsx")
counts_csv <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation_counts.csv")
counts_xlsx <- file.path(reconciliation_dir, "full_text_eligibility_reconciliation_counts.xlsx")

stable_write_csv(reconciliation, reconciliation_csv, FULL_TEXT_RECONCILIATION_COLUMNS)
stable_write_csv(adjudication_template, adjudication_csv, FULL_TEXT_ADJUDICATION_COLUMNS)
stable_write_csv(auto_resolved, auto_resolved_csv, FULL_TEXT_RECONCILIATION_COLUMNS)
stable_write_csv(unavailable, unavailable_csv, FULL_TEXT_UNAVAILABLE_COLUMNS)
stable_write_csv(audit, audit_csv, FULL_TEXT_RECONCILIATION_AUDIT_COLUMNS)
stable_write_csv(validation_issues, validation_csv, FULL_TEXT_RECONCILIATION_VALIDATION_COLUMNS)
stable_write_csv(counts, counts_csv, FULL_TEXT_RECONCILIATION_COUNT_COLUMNS)

write_review_xlsx(list(full_text_eligibility_reconciliation = reconciliation), reconciliation_xlsx)
write_review_xlsx(list(full_text_eligibility_auto_resolved_reference = auto_resolved), auto_resolved_xlsx)
write_review_xlsx(list(full_text_eligibility_unavailable_reference_reconciled = unavailable), unavailable_xlsx)
write_review_xlsx(list(full_text_eligibility_reconciliation_audit = audit), audit_xlsx)
write_review_xlsx(list(full_text_eligibility_reconciliation_validation_issues = validation_issues), validation_xlsx)
write_review_xlsx(list(full_text_eligibility_reconciliation_counts = counts), counts_xlsx)
write_full_text_adjudication_xlsx(
  adjudication_template,
  auto_resolved,
  unavailable,
  full_text_adjudication_codebook(),
  adjudication_xlsx
)

warnings <- character()
if (sum(validation_issues$severity == "BLOCKER") > 0L) {
  warnings <- c(warnings, paste0(sum(validation_issues$severity == "BLOCKER"), " blocker validation issue(s) must be corrected before adjudication."))
}
if (nrow(adjudication_template) > 0L) {
  warnings <- c(warnings, paste0(nrow(adjudication_template), " full-text record(s) require joint decision before data extraction."))
}
if (nrow(unavailable) > 0L) {
  warnings <- c(warnings, paste0(nrow(unavailable), " full-text-stage record(s) remain in unavailable_reference and are not scientific exclusions."))
}

outputs <- c(
  reconciliation_csv,
  reconciliation_xlsx,
  adjudication_csv,
  adjudication_xlsx,
  auto_resolved_csv,
  auto_resolved_xlsx,
  unavailable_csv,
  unavailable_xlsx,
  audit_csv,
  audit_xlsx,
  validation_csv,
  validation_xlsx,
  counts_csv,
  counts_xlsx,
  file.path(CFG$checkpoints_dir, "CP12_full_text_eligibility_reconciliation.md")
)

review_file <- write_checkpoint(
  id = "CP12",
  name = "full_text_eligibility_reconciliation",
  what_ran = paste(
    "Compared the normalized Revisor 1 and Revisor 2 full-text eligibility",
    "outputs, validated controlled decisions and exclusion reasons, separated",
    "reviewer agreements from rows requiring joint decision, preserved",
    "unavailable references outside scientific exclusion decisions, and wrote",
    "CSV/XLSX outputs for audit and review."
  ),
  numbers = stats::setNames(counts$value, counts$metric),
  review_items = c(
    "Open full_text_eligibility_adjudication_template.xlsx and complete only the yellow adjudication fields for rows in adjudication_required.",
    "Use full_text_eligibility_reconciliation.xlsx to audit all 37 reviewer-compared records.",
    "Use full_text_eligibility_auto_resolved_reference.xlsx to inspect reviewer agreements that do not require adjudication.",
    "Do not proceed to data extraction until adjudication_required rows are resolved and a later checkpoint validates the adjudicated workbook."
  ),
  outputs = outputs,
  warnings = warnings,
  gate_question = "Approve CP12 only after the reconciliation counts, auto-resolved agreements, unavailable-reference handling, and adjudication template have been inspected."
)

cat("CP12 full-text eligibility reconciliation complete\n")
cat("Revisor 1 eligibility records:", nrow(r1), "\n")
cat("Revisor 2 eligibility records:", nrow(r2), "\n")
cat("Reconciled records:", nrow(reconciliation), "\n")
cat("Auto-resolved records:", nrow(auto_resolved), "\n")
cat("Adjudication-required records:", nrow(adjudication_template), "\n")
cat("Unavailable-reference records:", nrow(unavailable), "\n")
cat("Validation issues:", nrow(validation_issues), "\n")
cat("Adjudication workbook:", project_relative_path(adjudication_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
