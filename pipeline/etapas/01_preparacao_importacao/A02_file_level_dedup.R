#!/usr/bin/env Rscript

# Identifica arquivos repetidos antes de importar os registros bibliográficos.
# Registra a escolha do arquivo de referência e preserva os arquivos de origem.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP01")

FILE_DEDUP_COLUMNS <- c(
  "inventory_id",
  "item_type",
  "source_bucket",
  "source_bucket_normalized",
  "content_class",
  "role",
  "relative_path",
  "absolute_path",
  "resolved_absolute_path",
  "file_name",
  "file_extension",
  "md5",
  "sha256",
  "dedup_candidate",
  "md5_group_size",
  "duplicate_group_id",
  "is_duplicate_md5_group",
  "canonical_inventory_id",
  "canonical_relative_path",
  "canonical_selection_rank",
  "is_canonical_file",
  "is_redundant_file_copy",
  "dedup_action",
  "decision_reason",
  "clinicaltrials_query_number",
  "clinicaltrials_empty_query_evidence_preserved"
)

SUPPRESSED_COLUMNS <- c(
  "duplicate_group_id",
  "md5",
  "redundant_inventory_id",
  "redundant_relative_path",
  "canonical_inventory_id",
  "canonical_relative_path",
  "role",
  "clinicaltrials_query_number",
  "clinicaltrials_empty_query_evidence_preserved",
  "decision_reason"
)

inventory_verified_path <- file.path(CFG$processed_dir, "inventory_verified.csv")
if (!file.exists(inventory_verified_path)) {
  stop("CP01 output is missing: ", inventory_verified_path, call. = FALSE)
}

inventory <- utils::read.csv(inventory_verified_path, stringsAsFactors = FALSE, check.names = FALSE)
inventory <- inventory[order(inventory$relative_path, method = "radix"), , drop = FALSE]

required_columns <- c(
  "inventory_id",
  "item_type",
  "source_bucket_normalized",
  "role",
  "relative_path",
  "file_name",
  "md5",
  "sha256"
)
missing_columns <- setdiff(required_columns, names(inventory))
if (length(missing_columns) > 0L) {
  stop("inventory_verified.csv is missing required columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
}

candidate <- inventory$item_type == "file" &
  inventory$role %in% c("export", "api_provenance") &
  nzchar(inventory$md5)

decisions <- inventory
decisions$dedup_candidate <- ifelse(candidate, "yes", "no")
decisions$md5_group_size <- ""
decisions$duplicate_group_id <- ""
decisions$is_duplicate_md5_group <- "no"
decisions$canonical_inventory_id <- ""
decisions$canonical_relative_path <- ""
decisions$canonical_selection_rank <- ""
decisions$is_canonical_file <- ifelse(candidate, "yes", "not_applicable")
decisions$is_redundant_file_copy <- ifelse(candidate, "no", "not_applicable")
decisions$dedup_action <- ifelse(candidate, "keep_unique", "not_dedup_candidate")
decisions$decision_reason <- ifelse(
  candidate,
  "Unique MD5 among export/provenance candidates unless later updated by duplicate-group logic.",
  "Role is outside CP02 export/provenance file deduplication."
)
decisions$clinicaltrials_query_number <- file_dedup_clinicaltrials_query_number(decisions$relative_path)
decisions$clinicaltrials_empty_query_evidence_preserved <- ifelse(
  file_dedup_is_ct_empty_query_evidence(
    decisions$source_bucket_normalized,
    decisions$relative_path,
    decisions$role
  ),
  "yes",
  "no"
)

candidate_rows <- which(candidate)
candidate_groups <- split(candidate_rows, decisions$md5[candidate_rows])
candidate_group_sizes <- vapply(candidate_groups, length, integer(1L))

for (group_md5 in names(candidate_groups)) {
  rows <- candidate_groups[[group_md5]]
  decisions$md5_group_size[rows] <- as.character(length(rows))
  decisions$canonical_inventory_id[rows] <- decisions$inventory_id[[rows[[1L]]]]
  decisions$canonical_relative_path[rows] <- decisions$relative_path[[rows[[1L]]]]
}

duplicate_groups <- candidate_groups[candidate_group_sizes > 1L]
duplicate_groups <- duplicate_groups[order(names(duplicate_groups), method = "radix")]

for (group_index in seq_along(duplicate_groups)) {
  rows <- duplicate_groups[[group_index]]
  group_id <- sprintf("FILEDUP_%04d", group_index)
  group_rows <- decisions[rows, , drop = FALSE]
  ranked_positions <- file_dedup_canonical_order(group_rows)
  ranked_rows <- rows[ranked_positions]
  canonical_row <- ranked_rows[[1L]]
  redundant_rows <- setdiff(ranked_rows, canonical_row)

  decisions$duplicate_group_id[rows] <- group_id
  decisions$is_duplicate_md5_group[rows] <- "yes"
  decisions$canonical_inventory_id[rows] <- decisions$inventory_id[[canonical_row]]
  decisions$canonical_relative_path[rows] <- decisions$relative_path[[canonical_row]]
  decisions$canonical_selection_rank[ranked_rows] <- as.character(seq_along(ranked_rows))

  decisions$is_canonical_file[canonical_row] <- "yes"
  decisions$is_redundant_file_copy[canonical_row] <- "no"
  decisions$dedup_action[canonical_row] <- "keep_canonical"
  decisions$decision_reason[canonical_row] <- paste(
    "Canonical selected by dated-prefix preference, non-BRUTO preference,",
    "then lexicographic relative path."
  )

  if (length(redundant_rows) > 0L) {
    decisions$is_canonical_file[redundant_rows] <- "no"
    decisions$is_redundant_file_copy[redundant_rows] <- "yes"
    decisions$dedup_action[redundant_rows] <- "suppress_redundant_copy"
    decisions$decision_reason[redundant_rows] <- paste(
      "Byte-identical redundant copy. File is not deleted; decision is recorded",
      "for downstream canonical import and audit."
    )
  }
}

ct_empty_rows <- decisions$clinicaltrials_empty_query_evidence_preserved == "yes"
decisions$decision_reason[ct_empty_rows & decisions$is_redundant_file_copy == "yes"] <- paste(
  decisions$decision_reason[ct_empty_rows & decisions$is_redundant_file_copy == "yes"],
  "ClinicalTrials query 08/15/18 evidence is preserved despite byte identity."
)

suppressed <- decisions[decisions$is_redundant_file_copy == "yes", , drop = FALSE]
suppressed_log <- data.frame(
  duplicate_group_id = suppressed$duplicate_group_id,
  md5 = suppressed$md5,
  redundant_inventory_id = suppressed$inventory_id,
  redundant_relative_path = suppressed$relative_path,
  canonical_inventory_id = suppressed$canonical_inventory_id,
  canonical_relative_path = suppressed$canonical_relative_path,
  role = suppressed$role,
  clinicaltrials_query_number = suppressed$clinicaltrials_query_number,
  clinicaltrials_empty_query_evidence_preserved = suppressed$clinicaltrials_empty_query_evidence_preserved,
  decision_reason = suppressed$decision_reason,
  stringsAsFactors = FALSE
)

suppressed_log <- suppressed_log[order(suppressed_log$duplicate_group_id, suppressed_log$redundant_relative_path, method = "radix"), , drop = FALSE]
decisions <- decisions[order(decisions$relative_path, method = "radix"), , drop = FALSE]

duplicate_group_count <- length(duplicate_groups)
suppressed_count <- nrow(suppressed_log)
ct_empty_result_exports <- sum(
  decisions$clinicaltrials_empty_query_evidence_preserved == "yes" &
    decisions$role == "export" &
    grepl("_results[.]csv$", decisions$file_name)
)
ct_empty_api_pages <- sum(
  decisions$clinicaltrials_empty_query_evidence_preserved == "yes" &
    decisions$role == "api_provenance" &
    grepl("_page_[0-9]{4}[.]json$", decisions$file_name)
)

warnings <- character()
if (duplicate_group_count != 18L) {
  warnings <- c(warnings, paste0("Expected 18 duplicate MD5 groups; observed ", duplicate_group_count, "."))
}
if (suppressed_count != 20L) {
  warnings <- c(warnings, paste0("Expected 20 redundant file copies; observed ", suppressed_count, "."))
}
if (ct_empty_result_exports != 3L) {
  warnings <- c(warnings, paste0("Expected 3 ClinicalTrials empty result exports preserved; observed ", ct_empty_result_exports, "."))
}
if (ct_empty_api_pages != 3L) {
  warnings <- c(warnings, paste0("Expected 3 ClinicalTrials empty API pages preserved; observed ", ct_empty_api_pages, "."))
}

decisions_path <- file.path(CFG$processed_dir, "file_dedup_decisions.csv")
suppressed_path <- file.path(CFG$logs_dir, "file_deduplication_suppressed.csv")

stable_write_csv(decisions, decisions_path, FILE_DEDUP_COLUMNS)
stable_write_csv(suppressed_log, suppressed_path, SUPPRESSED_COLUMNS)

review_file <- write_checkpoint(
  id = "CP02",
  name = "file_level_deduplication",
  what_ran = paste(
    "Grouped export/provenance files by MD5, selected one canonical file per",
    "duplicate group using dated-prefix, non-BRUTO, and lexicographic rules,",
    "and recorded redundant byte-identical copies without deleting raw evidence."
  ),
  numbers = c(
    dedup_candidates = sum(candidate),
    duplicate_md5_groups = duplicate_group_count,
    redundant_file_copies_suppressed = suppressed_count,
    clinicaltrials_empty_result_exports_preserved = ct_empty_result_exports,
    clinicaltrials_empty_api_pages_preserved = ct_empty_api_pages,
    warnings = length(warnings)
  ),
  review_items = c(
    "Confirm the acceptance counts are 18 duplicate MD5 groups and 20 redundant file copies.",
    "Confirm ClinicalTrials queries 08, 15, and 18 remain visible as executed empty-query evidence.",
    "Review file_deduplication_suppressed.csv before CP03 builds the import manifest.",
    "Confirm no physical file deletion occurred under BUSCAS."
  ),
  outputs = c(
    decisions_path,
    suppressed_path,
    file.path(CFG$checkpoints_dir, "CP02_file_level_deduplication.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = warnings,
  gate_question = "Approve CP02 only if canonical file choices and redundant-copy suppression are correct."
)

cat("CP02 file-level deduplication complete\n")
cat("Duplicate MD5 groups:", duplicate_group_count, "\n")
cat("Redundant file copies suppressed:", suppressed_count, "\n")
cat("Warnings:", length(warnings), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
