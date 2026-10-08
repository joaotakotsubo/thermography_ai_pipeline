#!/usr/bin/env Rscript

# Confere o inventário e a correspondência com os arquivos de busca.
# As divergências ficam registradas para revisão antes da importação.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP00")

INVENTORY_VERIFIED_COLUMNS <- c(
  "inventory_id",
  "item_type",
  "source_bucket",
  "source_bucket_normalized",
  "content_class",
  "role",
  "role_reason",
  "relative_path",
  "absolute_path",
  "resolved_absolute_path",
  "parent_relative_path",
  "file_name",
  "file_extension",
  "file_size_bytes",
  "file_size_mb",
  "modified_time",
  "permissions",
  "md5",
  "md5_recomputed",
  "md5_matches_inventory",
  "sha256",
  "path_exists",
  "actual_item_type"
)

INVENTORY_ISSUE_COLUMNS <- c(
  "issue_id",
  "severity",
  "issue_type",
  "inventory_id",
  "relative_path",
  "expected",
  "observed",
  "details"
)

required_inventory_columns <- c(
  "inventory_id",
  "item_type",
  "source_bucket",
  "content_class",
  "relative_path",
  "absolute_path",
  "file_name",
  "file_extension",
  "md5",
  "sha256"
)

resolve_inventory_path <- function(relative_path) {
  if (identical(relative_path, ".")) {
    return(normalizePath(CFG$buscas_dir, mustWork = FALSE))
  }
  normalizePath(file.path(CFG$buscas_dir, relative_path), mustWork = FALSE)
}

normalized_bucket <- function(source_bucket) {
  trimws(source_bucket)
}

classify_inventory_role <- function(item_type, content_class, relative_path, file_name, file_extension) {
  name_lower <- tolower(file_name)
  path_lower <- tolower(relative_path)
  ext <- tolower(file_extension)

  if (identical(item_type, "directory")) {
    return(c(role = "directory", reason = "Inventory item is a directory."))
  }
  if (identical(content_class, "os_metadata") || name_lower %in% c(".ds_store", "thumbs.db")) {
    return(c(role = "os_metadata", reason = "Operating-system metadata file."))
  }
  if (identical(content_class, "json_api_page") || grepl("/raw_api_json/", path_lower, fixed = TRUE)) {
    return(c(role = "api_provenance", reason = "Raw API JSON retained for provenance, not canonical import."))
  }
  if (grepl("combined|deduplicated|dedup", name_lower)) {
    return(c(role = "combined_crosscheck", reason = "Author-supplied combined or deduplicated file retained for reconciliation."))
  }
  if (
    identical(content_class, "log_or_text") ||
      grepl("log|manifest|readme|search_manager", name_lower) ||
      grepl("query_level", name_lower, fixed = TRUE)
  ) {
    return(c(role = "log", reason = "Search log, manifest, or audit text retained for traceability."))
  }
  if (identical(content_class, "script") || ext %in% c("r", "py", "sh")) {
    return(c(role = "script", reason = "Search or processing script retained as audit context."))
  }
  if (identical(content_class, "protocol_or_document") || ext %in% c("doc", "docx", "pdf")) {
    return(c(role = "doc", reason = "Protocol or document file retained as audit context."))
  }
  if (content_class %in% c("bibliographic_export", "tabular_export", "xml_export")) {
    return(c(role = "export", reason = "Structured search export eligible for later manifest routing."))
  }

  c(role = "unknown_pending_classification", reason = "No deterministic CP01 role rule matched this file.")
}

safe_file_md5 <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    return("")
  }
  tryCatch(file_md5(path), error = function(e) "")
}

issue_row <- function(severity, issue_type, inventory_id, relative_path, expected, observed, details) {
  data.frame(
    issue_id = "",
    severity = severity,
    issue_type = issue_type,
    inventory_id = inventory_id,
    relative_path = relative_path,
    expected = expected,
    observed = observed,
    details = details,
    stringsAsFactors = FALSE
  )
}

if (!file.exists(CFG$inventory_csv)) {
  stop("Inventory CSV is missing. Run pipeline/scripts/P00_create_buscas_inventory.R first.", call. = FALSE)
}

inventory <- utils::read.csv(CFG$inventory_csv, stringsAsFactors = FALSE, check.names = FALSE)
missing_columns <- setdiff(required_inventory_columns, names(inventory))
if (length(missing_columns) > 0L) {
  stop("Inventory CSV is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

inventory <- inventory[order(inventory$relative_path, method = "radix"), , drop = FALSE]
resolved_paths <- vapply(inventory$relative_path, resolve_inventory_path, character(1L))
path_exists <- file.exists(resolved_paths)
actual_item_type <- ifelse(path_exists & dir.exists(resolved_paths), "directory", ifelse(path_exists, "file", "missing"))
md5_recomputed <- ifelse(actual_item_type == "file", vapply(resolved_paths, safe_file_md5, character(1L)), "")
md5_matches <- ifelse(
  actual_item_type == "file",
  ifelse(nzchar(inventory$md5) & identical(length(md5_recomputed), length(inventory$md5)), md5_recomputed == inventory$md5, FALSE),
  "not_applicable"
)

role_matrix <- t(mapply(
  classify_inventory_role,
  item_type = inventory$item_type,
  content_class = inventory$content_class,
  relative_path = inventory$relative_path,
  file_name = inventory$file_name,
  file_extension = inventory$file_extension
))

verified <- inventory
verified$source_bucket_normalized <- vapply(verified$source_bucket, normalized_bucket, character(1L))
verified$role <- unname(role_matrix[, "role"])
verified$role_reason <- unname(role_matrix[, "reason"])
verified$resolved_absolute_path <- resolved_paths
verified$md5_recomputed <- md5_recomputed
verified$md5_matches_inventory <- as.character(md5_matches)
verified$path_exists <- ifelse(path_exists, "yes", "no")
verified$actual_item_type <- actual_item_type

issues <- list()

missing_path_rows <- which(!path_exists)
if (length(missing_path_rows) > 0L) {
  issues <- c(
    issues,
    lapply(missing_path_rows, function(i) {
      issue_row(
        severity = "error",
        issue_type = "path_missing",
        inventory_id = verified$inventory_id[[i]],
        relative_path = verified$relative_path[[i]],
        expected = "path exists",
        observed = "missing",
        details = "The inventory relative_path could not be resolved under BUSCAS."
      )
    })
  )
}

item_type_mismatch_rows <- which(path_exists & verified$item_type != actual_item_type)
if (length(item_type_mismatch_rows) > 0L) {
  issues <- c(
    issues,
    lapply(item_type_mismatch_rows, function(i) {
      issue_row(
        severity = "error",
        issue_type = "item_type_mismatch",
        inventory_id = verified$inventory_id[[i]],
        relative_path = verified$relative_path[[i]],
        expected = verified$item_type[[i]],
        observed = actual_item_type[[i]],
        details = "The live filesystem item type differs from the inventory item_type."
      )
    })
  )
}

missing_md5_rows <- which(actual_item_type == "file" & !nzchar(verified$md5))
if (length(missing_md5_rows) > 0L) {
  issues <- c(
    issues,
    lapply(missing_md5_rows, function(i) {
      issue_row(
        severity = "error",
        issue_type = "inventory_md5_missing",
        inventory_id = verified$inventory_id[[i]],
        relative_path = verified$relative_path[[i]],
        expected = "inventory MD5",
        observed = "",
        details = "A file row must include an inventory MD5 for reproducibility."
      )
    })
  )
}

md5_mismatch_rows <- which(actual_item_type == "file" & nzchar(verified$md5) & verified$md5_recomputed != verified$md5)
if (length(md5_mismatch_rows) > 0L) {
  issues <- c(
    issues,
    lapply(md5_mismatch_rows, function(i) {
      issue_row(
        severity = "error",
        issue_type = "md5_mismatch",
        inventory_id = verified$inventory_id[[i]],
        relative_path = verified$relative_path[[i]],
        expected = verified$md5[[i]],
        observed = verified$md5_recomputed[[i]],
        details = "The recomputed MD5 does not match the inventory MD5."
      )
    })
  )
}

unknown_role_rows <- which(verified$role == "unknown_pending_classification")
if (length(unknown_role_rows) > 0L) {
  issues <- c(
    issues,
    lapply(unknown_role_rows, function(i) {
      issue_row(
        severity = "warning",
        issue_type = "unknown_role",
        inventory_id = verified$inventory_id[[i]],
        relative_path = verified$relative_path[[i]],
        expected = "known CP01 role or explicit unknown",
        observed = verified$role[[i]],
        details = verified$role_reason[[i]]
      )
    })
  )
}

issue_log <- if (length(issues) == 0L) {
  as.data.frame(setNames(rep(list(character()), length(INVENTORY_ISSUE_COLUMNS)), INVENTORY_ISSUE_COLUMNS), stringsAsFactors = FALSE)
} else {
  do.call(rbind, issues)
}

if (nrow(issue_log) > 0L) {
  issue_log <- issue_log[order(issue_log$severity, issue_log$issue_type, issue_log$relative_path, method = "radix"), , drop = FALSE]
  issue_log$issue_id <- sprintf("INVISSUE_%04d", seq_len(nrow(issue_log)))
}

verified <- verified[order(verified$relative_path, method = "radix"), , drop = FALSE]

verified_path <- file.path(CFG$processed_dir, "inventory_verified.csv")
issues_path <- file.path(CFG$logs_dir, "inventory_verification_issues.csv")

stable_write_csv(verified, verified_path, INVENTORY_VERIFIED_COLUMNS)
stable_write_csv(issue_log, issues_path, INVENTORY_ISSUE_COLUMNS)

role_counts <- sort(table(verified$role), decreasing = TRUE)
error_count <- sum(issue_log$severity == "error")
warning_count <- sum(issue_log$severity == "warning")

review_file <- write_checkpoint(
  id = "CP01",
  name = "inventory_verification",
  what_ran = paste(
    "Resolved all inventory paths from BUSCAS-relative paths, recomputed MD5 for",
    "live files, compared hashes against the authoritative inventory, and classified",
    "each item into a broad audit role."
  ),
  numbers = c(
    total_items = nrow(verified),
    files = sum(verified$item_type == "file"),
    directories = sum(verified$item_type == "directory"),
    md5_matches = sum(verified$md5_matches_inventory == "TRUE"),
    errors = error_count,
    warnings = warning_count,
    unknown_pending_classification = sum(verified$role == "unknown_pending_classification")
  ),
  review_items = c(
    "Confirm there are zero path or MD5 errors before CP02.",
    "Review role counts, especially combined_crosscheck and api_provenance, because CP03 manifest routing depends on them.",
    "Confirm unknown_pending_classification is either zero or explicitly acceptable before progressing.",
    "Edit approvals.csv only after reviewing inventory_verified.csv and inventory_verification_issues.csv."
  ),
  outputs = c(
    verified_path,
    issues_path,
    file.path(CFG$checkpoints_dir, "CP01_inventory_verification.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = if (nrow(issue_log) == 0L) character() else paste(issue_log$issue_id, issue_log$issue_type, issue_log$relative_path),
  gate_question = "Approve CP01 only if all file paths resolve, all file MD5 values match, and all item roles are acceptable."
)

cat("CP01 inventory verification complete\n")
cat("Items:", nrow(verified), "\n")
cat("Errors:", error_count, "\n")
cat("Warnings:", warning_count, "\n")
cat("Review file:", project_relative_path(review_file), "\n")
