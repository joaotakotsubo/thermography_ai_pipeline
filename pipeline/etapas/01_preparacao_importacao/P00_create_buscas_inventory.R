#!/usr/bin/env Rscript

# Faz o inventário dos arquivos presentes na pasta de buscas.
# Registra os caminhos e as assinaturas dos arquivos sem modificar as fontes.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

INVENTORY_COLUMNS <- c(
  "inventory_id",
  "item_type",
  "source_bucket",
  "content_class",
  "relative_path",
  "absolute_path",
  "parent_relative_path",
  "file_name",
  "file_extension",
  "file_size_bytes",
  "file_size_mb",
  "modified_time",
  "permissions",
  "md5",
  "sha256"
)

relative_to_buscas <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = TRUE)
  if (identical(path, root)) {
    return(".")
  }
  substring(path, nchar(root) + 2L)
}

first_path_segment <- function(relative_path) {
  if (identical(relative_path, ".")) {
    return("BUSCAS_root")
  }
  parts <- strsplit(relative_path, .Platform$file.sep, fixed = TRUE)[[1L]]
  if (length(parts) == 1L) {
    return("BUSCAS_root_file")
  }
  parts[[1L]]
}

classify_inventory_item <- function(item_type, file_name, file_extension) {
  if (identical(item_type, "directory")) {
    return("directory")
  }

  name_lower <- tolower(file_name)
  ext <- tolower(file_extension)

  if (name_lower %in% c(".ds_store", "thumbs.db")) {
    return("os_metadata")
  }
  if (ext %in% c("ris", "nbib", "bib", "bibtex")) {
    return("bibliographic_export")
  }
  if (ext == "csv") {
    return("tabular_export")
  }
  if (ext == "xml") {
    return("xml_export")
  }
  if (ext == "json") {
    return("json_api_page")
  }
  if (ext %in% c("txt", "log") || grepl("log", name_lower, fixed = TRUE)) {
    return("log_or_text")
  }
  if (ext %in% c("doc", "docx", "pdf")) {
    return("protocol_or_document")
  }
  if (ext %in% c("r", "py", "sh")) {
    return("script")
  }
  "other"
}

sha256_file <- function(path) {
  if (dir.exists(path)) {
    return("")
  }
  if (!nzchar(Sys.which("shasum"))) {
    stop("The system command 'shasum' is required to compute SHA-256 checksums.", call. = FALSE)
  }

  result <- suppressWarnings(system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA-256 failed for: ", path, "\n", paste(result, collapse = "\n"), call. = FALSE)
  }
  sub("[[:space:]].*$", "", result[[1L]])
}

summarise_inventory <- function(inventory, group_column, include_directories = TRUE) {
  data <- inventory
  if (!include_directories) {
    data <- data[data$item_type == "file", , drop = FALSE]
  }

  group_values <- data[[group_column]]
  group_values[!nzchar(group_values)] <- "[none]"
  keys <- sort(unique(group_values))

  rows <- lapply(keys, function(key) {
    rows_for_key <- data[group_values == key, , drop = FALSE]
    sizes <- suppressWarnings(as.numeric(rows_for_key$file_size_bytes))
    sizes[is.na(sizes)] <- 0

    data.frame(
      group = key,
      n_items = nrow(rows_for_key),
      n_files = sum(rows_for_key$item_type == "file"),
      n_directories = sum(rows_for_key$item_type == "directory"),
      total_file_size_bytes = sum(sizes[rows_for_key$item_type == "file"]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

if (!dir.exists(CFG$buscas_dir)) {
  stop("BUSCAS directory was not found: ", CFG$buscas_dir, call. = FALSE)
}

buscas_root <- normalizePath(CFG$buscas_dir, mustWork = TRUE)
child_paths <- list.files(
  buscas_root,
  all.files = TRUE,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = TRUE,
  no.. = TRUE
)
all_paths <- unique(c(buscas_root, normalizePath(child_paths, mustWork = FALSE)))
relative_paths <- vapply(all_paths, relative_to_buscas, character(1L), root = buscas_root)
path_order <- order(relative_paths, method = "radix")
all_paths <- all_paths[path_order]
relative_paths <- relative_paths[path_order]

info <- file.info(all_paths, extra_cols = FALSE)
item_type <- ifelse(!is.na(info$isdir) & info$isdir, "directory", "file")
file_names <- ifelse(relative_paths == ".", basename(buscas_root), basename(all_paths))
file_extensions <- ifelse(item_type == "file", tolower(tools::file_ext(file_names)), "")
parent_relative_paths <- ifelse(relative_paths == ".", "", dirname(relative_paths))

inventory <- data.frame(
  inventory_id = sprintf("BUSCAS_%06d", seq_along(all_paths)),
  item_type = item_type,
  source_bucket = vapply(relative_paths, first_path_segment, character(1L)),
  content_class = mapply(classify_inventory_item, item_type, file_names, file_extensions, USE.NAMES = FALSE),
  relative_path = relative_paths,
  absolute_path = all_paths,
  parent_relative_path = parent_relative_paths,
  file_name = file_names,
  file_extension = file_extensions,
  file_size_bytes = as.character(ifelse(is.na(info$size), "", info$size)),
  file_size_mb = ifelse(is.na(info$size), "", sprintf("%.6f", as.numeric(info$size) / 1024^2)),
  modified_time = ifelse(
    is.na(info$mtime),
    "",
    format(as.POSIXct(info$mtime, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ),
  permissions = as.character(info$mode),
  md5 = ifelse(item_type == "file", vapply(all_paths, file_md5, character(1L)), ""),
  sha256 = ifelse(item_type == "file", vapply(all_paths, sha256_file, character(1L)), ""),
  stringsAsFactors = FALSE
)

stable_write_csv(inventory, CFG$inventory_csv, INVENTORY_COLUMNS)

summary_by_source <- summarise_inventory(inventory, "source_bucket", include_directories = TRUE)
stable_write_csv(
  summary_by_source,
  file.path(dirname(CFG$inventory_csv), "buscas_inventory_summary_by_source_bucket.csv"),
  columns = c("group", "n_items", "n_files", "n_directories", "total_file_size_bytes")
)

summary_by_extension <- summarise_inventory(inventory, "file_extension", include_directories = FALSE)
stable_write_csv(
  summary_by_extension,
  file.path(dirname(CFG$inventory_csv), "buscas_inventory_summary_by_extension.csv"),
  columns = c("group", "n_items", "n_files", "n_directories", "total_file_size_bytes")
)

log_path <- file.path(CFG$logs_dir, "preflight_buscas_inventory_generation_log.txt")
writeLines(
  c(
    "BUSCAS inventory preflight",
    paste0("generated_at_utc: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    paste0("buscas_dir: ", CFG$buscas_dir),
    paste0("inventory_csv: ", CFG$inventory_csv),
    paste0("files: ", sum(inventory$item_type == "file")),
    paste0("directories: ", sum(inventory$item_type == "directory")),
    paste0("total_items: ", nrow(inventory))
  ),
  con = log_path,
  useBytes = TRUE
)

cat("P00 BUSCAS inventory complete\n")
cat("Files:", sum(inventory$item_type == "file"), "\n")
cat("Directories:", sum(inventory$item_type == "directory"), "\n")
cat("Total items:", nrow(inventory), "\n")
cat("Inventory:", project_relative_path(CFG$inventory_csv), "\n")
