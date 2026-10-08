#!/usr/bin/env Rscript

# Prepara as pastas de trabalho e registra a configuração da execução.
# Aponta entradas ausentes e diferenças nas contagens antes da revisão inicial.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate(NULL)
invisible(vapply(PIPELINE_DIRS, ensure_dir, character(1L)))

expected_inventory <- c(files = 161L, directories = 31L, total_items = 192L)

inventory_counts <- c(files = 0L, directories = 0L, total_items = 0L)
if (file.exists(CFG$inventory_csv)) {
  inventory <- utils::read.csv(CFG$inventory_csv, stringsAsFactors = FALSE, check.names = FALSE)
  inventory_counts <- c(
    files = sum(inventory$item_type == "file"),
    directories = sum(inventory$item_type == "directory"),
    total_items = nrow(inventory)
  )
}

warnings <- character()
if (!dir.exists(CFG$buscas_dir)) {
  warnings <- c(warnings, paste0("BUSCAS directory is missing: ", CFG$buscas_dir))
}
if (!file.exists(CFG$inventory_csv)) {
  warnings <- c(warnings, "Inventory CSV is missing. Run pipeline/scripts/P00_create_buscas_inventory.R first.")
}
if (!file.exists(CFG$final_handoff)) {
  warnings <- c(warnings, paste0("Final governing handoff is missing: ", CFG$final_handoff))
}
if (file.exists(CFG$inventory_csv) && any(inventory_counts != expected_inventory)) {
  warnings <- c(
    warnings,
    paste0(
      "Inventory counts differ from final handoff facts. Expected files/directories/items = ",
      paste(expected_inventory, collapse = "/"),
      "; observed = ",
      paste(inventory_counts, collapse = "/"),
      "."
    )
  )
}
if (!identical(Sys.getenv("TZ", unset = ""), "UTC")) {
  warnings <- c(warnings, "Timezone is not UTC after bootstrap.")
}
if (!identical(CFG$locale$lc_collate, "C")) {
  warnings <- c(warnings, paste0("LC_COLLATE is not C: ", CFG$locale$lc_collate))
}
if (!grepl("UTF-8|UTF8", CFG$locale$lc_ctype, ignore.case = TRUE)) {
  warnings <- c(warnings, paste0("LC_CTYPE is not UTF-8 capable: ", CFG$locale$lc_ctype))
}

snapshot <- c(
  project_root = CFG$project_root,
  pipeline_dir = CFG$pipeline_dir,
  buscas_dir = CFG$buscas_dir,
  inventory_csv = CFG$inventory_csv,
  final_handoff = CFG$final_handoff,
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  r_platform = R.version$platform,
  seed = CFG$seed,
  strict_mode = CFG$strict,
  timezone = CFG$locale$timezone,
  lc_collate = CFG$locale$lc_collate,
  lc_ctype = CFG$locale$lc_ctype,
  buscas_dir_exists = yes_no_file(CFG$buscas_dir),
  inventory_csv_exists = yes_no_file(CFG$inventory_csv),
  final_handoff_exists = yes_no_file(CFG$final_handoff),
  inventory_files = inventory_counts[["files"]],
  inventory_directories = inventory_counts[["directories"]],
  inventory_total_items = inventory_counts[["total_items"]],
  final_handoff_md5 = file_md5(CFG$final_handoff)
)

snapshot_path <- file.path(CFG$processed_dir, "run_config_snapshot.csv")
write_key_value_csv(snapshot, snapshot_path)

review_file <- write_checkpoint(
  id = "CP00",
  name = "setup",
  what_ran = paste(
    "Validated deterministic configuration, project paths, final handoff presence,",
    "BUSCAS visibility, and the authoritative inventory counts before Phase A execution."
  ),
  numbers = c(
    inventory_files = inventory_counts[["files"]],
    inventory_directories = inventory_counts[["directories"]],
    inventory_total_items = inventory_counts[["total_items"]],
    warnings = length(warnings)
  ),
  review_items = c(
    "Confirm this run used the FINAL handoff, not an archived V2/V3/V4/V5 file.",
    "Confirm BUSCAS is visible and remains read-only for the pipeline.",
    "Confirm inventory counts match 161 files, 31 directories, and 192 total items.",
    "Edit approvals.csv manually only after reviewing this checkpoint."
  ),
  outputs = c(
    snapshot_path,
    file.path(CFG$checkpoints_dir, "CP00_setup.md"),
    checkpoint_status_path(),
    checkpoint_approvals_path()
  ),
  warnings = warnings,
  gate_question = "Approve CP00 only if the deterministic scaffold and inventory facts are correct."
)

cat("CP00 setup complete\n")
cat("Warnings:", length(warnings), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
