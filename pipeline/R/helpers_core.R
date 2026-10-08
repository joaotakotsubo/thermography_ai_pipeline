# Reúne as rotinas de criação de pastas, escrita de tabelas e identificação de arquivos.
# As saídas são organizadas para facilitar a conferência de cada etapa.


ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

ensure_parent_dir <- function(path) {
  ensure_dir(dirname(path))
}

stable_write_csv <- function(x, path, columns = NULL) {
  ensure_parent_dir(path)

  if (!is.null(columns)) {
    for (missing_col in setdiff(columns, names(x))) {
      x[[missing_col]] <- NA_character_
    }
    x <- x[, columns, drop = FALSE]
  }

  utils::write.csv(
    x,
    file = path,
    row.names = FALSE,
    na = "",
    quote = TRUE,
    fileEncoding = "UTF-8"
  )
  invisible(path)
}

read_csv_or_empty <- function(path, columns) {
  if (!file.exists(path)) {
    return(as.data.frame(setNames(rep(list(character()), length(columns)), columns), stringsAsFactors = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

checkpoint_id_normalize <- function(id) {
  id_chr <- toupper(trimws(as.character(id)))
  id_chr <- sub("^CP", "", id_chr)
  match <- regexec("^([0-9]+)([A-Z]*)$", id_chr)
  parts <- regmatches(id_chr, match)[[1L]]
  if (length(parts) == 0L) {
    stop("Invalid checkpoint id: ", id, call. = FALSE)
  }
  paste0("CP", sprintf("%02d", as.integer(parts[[2L]])), parts[[3L]])
}

project_relative_path <- function(path) {
  normalized <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(CFG$project_root, mustWork = TRUE)
  prefix <- paste0(root, .Platform$file.sep)
  ifelse(startsWith(normalized, prefix), substring(normalized, nchar(root) + 2L), normalized)
}

yes_no_file <- function(path) {
  ifelse(file.exists(path), "yes", "no")
}

file_md5 <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    return("")
  }
  unname(tools::md5sum(path))
}

write_key_value_csv <- function(values, path) {
  out <- data.frame(
    key = names(values),
    value = unname(vapply(values, as.character, character(1L))),
    stringsAsFactors = FALSE
  )
  stable_write_csv(out, path, columns = c("key", "value"))
}
