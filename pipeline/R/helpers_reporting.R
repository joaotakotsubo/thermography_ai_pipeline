# Reúne as rotinas de apresentação e exportação das tabelas e relatórios.
# Mantém os campos de identificação necessários para conferir os resultados.


XLSX_CELL_TEXT_LIMIT <- 30000L

require_openxlsx <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required to generate reviewer-facing XLSX files. ",
      "Install/pin it before running this checkpoint.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

xlsx_safe_sheet_name <- function(name, used = character()) {
  safe <- gsub("[\\[\\]\\*\\?/\\\\:]", "_", as.character(name), perl = TRUE)
  safe <- trimws(safe)
  if (!nzchar(safe)) {
    safe <- "Sheet"
  }
  safe <- substr(safe, 1L, 31L)

  candidate <- safe
  suffix <- 1L
  while (candidate %in% used) {
    suffix <- suffix + 1L
    suffix_text <- paste0("_", suffix)
    candidate <- paste0(substr(safe, 1L, 31L - nchar(suffix_text)), suffix_text)
  }
  candidate
}

as_character_frame <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  for (name in names(data)) {
    data[[name]] <- as.character(data[[name]])
    data[[name]][is.na(data[[name]])] <- ""
  }
  data
}

xlsx_truncate_frame <- function(data, limit = XLSX_CELL_TEXT_LIMIT) {
  marker <- " [TRUNCATED_FOR_XLSX_SEE_CSV]"
  max_chars <- limit - nchar(marker, type = "chars")
  data <- as_character_frame(data)
  for (name in names(data)) {
    too_long <- nchar(data[[name]], type = "chars", allowNA = FALSE, keepNA = FALSE) > limit
    if (any(too_long)) {
      data[[name]][too_long] <- paste0(substr(data[[name]][too_long], 1L, max_chars), marker)
    }
  }
  data
}

xlsx_column_to_index <- function(column) {
  letters <- strsplit(column, "", fixed = TRUE)[[1L]]
  value <- 0L
  for (letter in letters) {
    value <- value * 26L + match(letter, LETTERS)
  }
  value
}

xlsx_index_to_column <- function(index) {
  index <- as.integer(index)
  out <- character()
  while (index > 0L) {
    index <- index - 1L
    out <- c(LETTERS[(index %% 26L) + 1L], out)
    index <- index %/% 26L
  }
  paste(out, collapse = "")
}

xlsx_dimension_from_sheet_xml <- function(sheet_xml) {
  cell_tags <- regmatches(
    sheet_xml,
    gregexpr("<c\\s[^>]*r=\"[A-Z]+[0-9]+\"", sheet_xml, perl = TRUE)
  )[[1L]]

  if (length(cell_tags) == 0L || identical(cell_tags, character(0))) {
    return("A1")
  }

  refs <- sub(".*r=\"([A-Z]+)([0-9]+)\".*", "\\1|\\2", cell_tags, perl = TRUE)
  parts <- strsplit(refs, "|", fixed = TRUE)
  columns <- vapply(parts, function(x) x[[1L]], character(1L))
  rows <- as.integer(vapply(parts, function(x) x[[2L]], character(1L)))
  max_col <- max(vapply(columns, xlsx_column_to_index, integer(1L)), na.rm = TRUE)
  max_row <- max(rows, na.rm = TRUE)

  paste0("A1:", xlsx_index_to_column(max_col), max_row)
}

clean_generated_xlsx_structure <- function(path) {
  if (!file.exists(path)) {
    return(invisible(path))
  }

  work_dir <- tempfile("xlsx_clean_")
  ensure_dir(work_dir)
  utils::unzip(path, exdir = work_dir)

  worksheets_dir <- file.path(work_dir, "xl", "worksheets")
  sheet_files <- list.files(
    worksheets_dir,
    pattern = "^sheet[0-9]+[.]xml$",
    full.names = TRUE
  )
  for (sheet_file in sheet_files) {
    sheet_xml <- paste(readLines(sheet_file, warn = FALSE), collapse = "")
    dimension <- xlsx_dimension_from_sheet_xml(sheet_xml)
    if (grepl("<dimension\\s+ref=\"[^\"]*\"\\s*/>", sheet_xml, perl = TRUE)) {
      sheet_xml <- sub(
        "<dimension\\s+ref=\"[^\"]*\"\\s*/>",
        paste0("<dimension ref=\"", dimension, "\"/>"),
        sheet_xml,
        perl = TRUE
      )
    } else {
      sheet_xml <- sub(
        "(<worksheet[^>]*>)",
        paste0("\\1 <dimension ref=\"", dimension, "\"/>"),
        sheet_xml,
        perl = TRUE
      )
    }
    sheet_xml <- gsub("<drawing\\s+[^>]*/>", "", sheet_xml, perl = TRUE)
    sheet_xml <- gsub("<legacyDrawing\\s+[^>]*/>", "", sheet_xml, perl = TRUE)
    writeLines(sheet_xml, sheet_file, useBytes = TRUE)
  }

  rels_dir <- file.path(worksheets_dir, "_rels")
  if (dir.exists(rels_dir)) {
    rel_files <- list.files(rels_dir, pattern = "[.]xml[.]rels$", full.names = TRUE)
    for (rel_file in rel_files) {
      rel_xml <- paste(readLines(rel_file, warn = FALSE), collapse = "")
      rel_xml <- gsub(
        "<Relationship\\s+[^>]*Type=\"[^\"]*/drawing\"[^>]*/>",
        "",
        rel_xml,
        perl = TRUE
      )
      rel_xml <- gsub(
        "<Relationship\\s+[^>]*Type=\"[^\"]*/vmlDrawing\"[^>]*/>",
        "",
        rel_xml,
        perl = TRUE
      )
      if (!grepl("<Relationship\\s", rel_xml, perl = TRUE)) {
        unlink(rel_file)
      } else {
        writeLines(rel_xml, rel_file, useBytes = TRUE)
      }
    }
  }

  drawings_dir <- file.path(work_dir, "xl", "drawings")
  if (dir.exists(drawings_dir)) {
    unlink(drawings_dir, recursive = TRUE, force = TRUE)
  }

  content_types <- file.path(work_dir, "[Content_Types].xml")
  if (file.exists(content_types)) {
    content_xml <- paste(readLines(content_types, warn = FALSE), collapse = "")
    content_xml <- gsub("<Override\\s+[^>]*PartName=\"/xl/drawings/[^\"]+\"[^>]*/>", "", content_xml, perl = TRUE)
    content_xml <- gsub("<Default\\s+[^>]*Extension=\"vml\"[^>]*/>", "", content_xml, perl = TRUE)
    writeLines(content_xml, content_types, useBytes = TRUE)
  }

  fixed_path <- tempfile(fileext = ".xlsx")
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(work_dir)
  files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  utils::zip(fixed_path, files = files, flags = "-q -r9X")
  setwd(old_wd)

  ok <- file.copy(fixed_path, path, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Could not write cleaned XLSX structure: ", path, call. = FALSE)
  }
  invisible(path)
}

xlsx_has_drawing_artifacts <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  listing <- utils::unzip(path, list = TRUE)$Name
  if (any(startsWith(listing, "xl/drawings/"))) {
    return(TRUE)
  }
  rel_files <- listing[startsWith(listing, "xl/worksheets/_rels/")]
  for (rel_file in rel_files) {
    rel_xml <- paste(readLines(unz(path, rel_file), warn = FALSE), collapse = "")
    if (grepl("/drawing\"|/vmlDrawing\"", rel_xml, perl = TRUE)) {
      return(TRUE)
    }
  }
  FALSE
}

write_review_xlsx <- function(sheets, path) {
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
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

  used_names <- character()
  for (sheet_name in names(sheets)) {
    safe_name <- xlsx_safe_sheet_name(sheet_name, used_names)
    used_names <- c(used_names, safe_name)
    data <- xlsx_truncate_frame(sheets[[sheet_name]])

    openxlsx::addWorksheet(workbook, safe_name, gridLines = TRUE)
    if (ncol(data) == 0L) {
      data <- data.frame(note = character(), stringsAsFactors = FALSE)
    }
    openxlsx::writeData(workbook, safe_name, data, startRow = 1L, startCol = 1L, colNames = TRUE)
    openxlsx::addStyle(workbook, safe_name, header_style, rows = 1L, cols = seq_len(ncol(data)), gridExpand = TRUE)
    if (nrow(data) > 0L) {
      openxlsx::addStyle(workbook, safe_name, wrap_style, rows = seq_len(nrow(data)) + 1L, cols = seq_len(ncol(data)), gridExpand = TRUE)
    }
    openxlsx::freezePane(workbook, safe_name, firstActiveRow = 2L)
    openxlsx::addFilter(workbook, safe_name, row = 1L, cols = seq_len(ncol(data)))
    widths <- pmin(pmax(nchar(names(data), type = "chars") + 2L, 12L), 42L)
    openxlsx::setColWidths(workbook, safe_name, cols = seq_len(ncol(data)), widths = widths)
  }

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  clean_generated_xlsx_structure(path)
  invisible(path)
}

read_review_xlsx_sheet <- function(path, sheet, columns) {
  require_openxlsx()
  if (!file.exists(path)) {
    return(empty_named_frame(columns))
  }

  sheet_names <- openxlsx::getSheetNames(path)
  if (!(sheet %in% sheet_names)) {
    stop("Workbook is missing required sheet '", sheet, "': ", project_relative_path(path), call. = FALSE)
  }

  data <- openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE, detectDates = FALSE)
  if (is.null(data) || ncol(data) == 0L) {
    data <- empty_named_frame(columns)
  }
  data <- as_character_frame(data)
  for (missing_col in setdiff(columns, names(data))) {
    data[[missing_col]] <- ""
  }
  data[, columns, drop = FALSE]
}
