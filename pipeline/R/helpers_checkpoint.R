# Registra os pontos de conferência e verifica as aprovações antes de avançar.
# Um arquivo de saída não significa, por si só, que a etapa foi aprovada.


CHECKPOINT_STATUS_COLUMNS <- c("checkpoint_id", "name", "n_warnings", "review_file", "status")
CHECKPOINT_APPROVAL_COLUMNS <- c("checkpoint_id", "approved", "reviewer", "date", "notes")

checkpoint_status_path <- function() {
  file.path(CFG$checkpoints_dir, "checkpoint_status.csv")
}

checkpoint_approvals_path <- function() {
  file.path(CFG$checkpoints_dir, "approvals.csv")
}

checkpoint_status_value <- function(checkpoint_id) {
  approvals <- read_csv_or_empty(checkpoint_approvals_path(), CHECKPOINT_APPROVAL_COLUMNS)
  checkpoint_id <- checkpoint_id_normalize(checkpoint_id)
  row <- approvals[approvals$checkpoint_id == checkpoint_id, , drop = FALSE]

  if (nrow(row) > 0L && identical(tolower(trimws(row$approved[[1L]])), "yes")) {
    return("APPROVED")
  }
  "AWAITING_HUMAN_REVIEW"
}

ensure_checkpoint_approval_row <- function(checkpoint_id) {
  approvals_path <- checkpoint_approvals_path()
  approvals <- read_csv_or_empty(approvals_path, CHECKPOINT_APPROVAL_COLUMNS)
  checkpoint_id <- checkpoint_id_normalize(checkpoint_id)

  if (!checkpoint_id %in% approvals$checkpoint_id) {
    approvals <- rbind(
      approvals,
      data.frame(
        checkpoint_id = checkpoint_id,
        approved = "no",
        reviewer = "",
        date = "",
        notes = "",
        stringsAsFactors = FALSE
      )
    )
    approvals <- approvals[order(approvals$checkpoint_id), , drop = FALSE]
    stable_write_csv(approvals, approvals_path, CHECKPOINT_APPROVAL_COLUMNS)
  }

  invisible(approvals_path)
}

upsert_checkpoint_status <- function(checkpoint_id, name, n_warnings, review_file) {
  status_path <- checkpoint_status_path()
  status <- read_csv_or_empty(status_path, CHECKPOINT_STATUS_COLUMNS)
  checkpoint_id <- checkpoint_id_normalize(checkpoint_id)

  status <- status[status$checkpoint_id != checkpoint_id, , drop = FALSE]
  status <- rbind(
    status,
    data.frame(
      checkpoint_id = checkpoint_id,
      name = name,
      n_warnings = as.character(n_warnings),
      review_file = project_relative_path(review_file),
      status = checkpoint_status_value(checkpoint_id),
      stringsAsFactors = FALSE
    )
  )
  status <- status[order(status$checkpoint_id), , drop = FALSE]
  stable_write_csv(status, status_path, CHECKPOINT_STATUS_COLUMNS)
  invisible(status_path)
}

markdown_bullets <- function(x) {
  x <- as.character(x)
  x <- x[nzchar(x)]
  if (length(x) == 0L) {
    return("- None")
  }
  paste0("- ", x, collapse = "\n")
}

markdown_numbers <- function(x) {
  if (length(x) == 0L) {
    return("- None")
  }
  paste0("- ", names(x), ": ", unname(vapply(x, as.character, character(1L))), collapse = "\n")
}

write_checkpoint <- function(id, name, what_ran, numbers, review_items, outputs, warnings, gate_question) {
  checkpoint_id <- checkpoint_id_normalize(id)
  safe_name <- gsub("[^A-Za-z0-9_]+", "_", tolower(name))
  safe_name <- gsub("^_|_$", "", safe_name)
  review_file <- file.path(CFG$checkpoints_dir, paste0(checkpoint_id, "_", safe_name, ".md"))

  warnings <- as.character(warnings)
  warnings <- warnings[nzchar(warnings)]

  ensure_checkpoint_approval_row(checkpoint_id)
  approvals <- read_csv_or_empty(checkpoint_approvals_path(), CHECKPOINT_APPROVAL_COLUMNS)
  approval <- approvals[approvals$checkpoint_id == checkpoint_id, , drop = FALSE]
  approval_value <- function(column) {
    value <- ""
    if (nrow(approval) > 0L && column %in% names(approval)) {
      value <- as.character(approval[[column]][[1L]])
    }
    if (is.na(value)) {
      return("")
    }
    value
  }

  lines <- c(
    paste0("# ", checkpoint_id, " - ", name),
    "",
    "## What This Step Did",
    "",
    as.character(what_ran),
    "",
    "## Key Numbers (Verify These)",
    "",
    markdown_numbers(numbers),
    "",
    "## What You Must Review Before Continuing",
    "",
    markdown_bullets(paste0("[ ] ", review_items)),
    "",
    "## Warnings Raised",
    "",
    markdown_bullets(warnings),
    "",
    "## Files Produced",
    "",
    markdown_bullets(project_relative_path(outputs)),
    "",
    "## Sign-Off",
    "",
    as.character(gate_question),
    "",
    "```text",
    paste0("checkpoint_id: ", checkpoint_id),
    paste0("approved: ", approval_value("approved")),
    paste0("reviewer: ", approval_value("reviewer")),
    paste0("date: ", approval_value("date")),
    paste0("notes: ", approval_value("notes")),
    "```",
    ""
  )

  ensure_parent_dir(review_file)
  writeLines(lines, review_file, useBytes = TRUE)

  upsert_checkpoint_status(checkpoint_id, name, length(warnings), review_file)
  invisible(review_file)
}

checkpoint_gate <- function(prior_id) {
  if (is.null(prior_id) || is.na(prior_id) || !nzchar(as.character(prior_id))) {
    return(invisible(TRUE))
  }
  if (!isTRUE(getOption("aithermo.strict", FALSE))) {
    return(invisible(TRUE))
  }

  prior_id <- checkpoint_id_normalize(prior_id)
  approvals <- read_csv_or_empty(checkpoint_approvals_path(), CHECKPOINT_APPROVAL_COLUMNS)
  row <- approvals[approvals$checkpoint_id == prior_id, , drop = FALSE]

  if (nrow(row) == 0L || !identical(tolower(trimws(row$approved[[1L]])), "yes")) {
    stop("Strict gate blocked execution: ", prior_id, " is not approved.", call. = FALSE)
  }

  invisible(TRUE)
}
