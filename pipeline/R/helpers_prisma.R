# Organiza as contagens de seleção e os registros de buscas complementares.
# Os números devem ser conferidos com as decisões documentadas da revisão.


SNOWBALLING_RECORD_COLUMNS <- c(
  "snowballing_id",
  "snowballing_method",
  "seed_record_id",
  "seed_record_source_id",
  "seed_title",
  "discovery_source",
  "discovery_date",
  "title",
  "authors",
  "year",
  "doi",
  "pmid",
  "arxiv_id",
  "url",
  "journal_or_source",
  "abstract",
  "candidate_status",
  "added_by",
  "reviewer",
  "review_date",
  "notes"
)

SNOWBALLING_LOG_COLUMNS <- c(
  "log_id",
  "snowballing_method",
  "seed_record_id",
  "seed_record_source_id",
  "seed_title",
  "source_platform",
  "search_date",
  "query_or_scope",
  "records_found_reported",
  "records_added_to_template",
  "reviewer",
  "notes"
)

PRISMA_RECONCILIATION_COLUMNS <- c(
  "check_id",
  "check_name",
  "scope",
  "expected_value",
  "observed_value",
  "difference",
  "status",
  "notes"
)

empty_named_frame <- function(columns) {
  as.data.frame(
    setNames(rep(list(character()), length(columns)), columns),
    stringsAsFactors = FALSE
  )
}

snowballingo_records_path <- function() {
  file.path(CFG$snowballingo_dir, "snowballing_records.csv")
}

snowballingo_log_path <- function() {
  file.path(CFG$snowballingo_dir, "snowballing_log.csv")
}

snowballingo_workbook_path <- function() {
  file.path(CFG$snowballingo_dir, "snowballing_input.xlsx")
}

ensure_snowballingo_templates <- function() {
  ensure_dir(CFG$snowballingo_dir)

  records_path <- snowballingo_records_path()
  log_path <- snowballingo_log_path()
  workbook_path <- snowballingo_workbook_path()

  if (!file.exists(records_path)) {
    stable_write_csv(empty_named_frame(SNOWBALLING_RECORD_COLUMNS), records_path, SNOWBALLING_RECORD_COLUMNS)
  }
  if (!file.exists(log_path)) {
    stable_write_csv(empty_named_frame(SNOWBALLING_LOG_COLUMNS), log_path, SNOWBALLING_LOG_COLUMNS)
  }
  if (!file.exists(workbook_path)) {
    write_review_xlsx(
      list(
        snowballing_records = empty_named_frame(SNOWBALLING_RECORD_COLUMNS),
        snowballing_log = empty_named_frame(SNOWBALLING_LOG_COLUMNS)
      ),
      workbook_path
    )
  }

  invisible(c(records = records_path, log = log_path, workbook = workbook_path))
}

read_machine_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character"
  )
}

validate_required_columns <- function(data, columns, path) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      "Required columns are missing from ",
      project_relative_path(path),
      ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

nonempty_row <- function(data) {
  if (nrow(data) == 0L) {
    return(logical())
  }
  rowSums(vapply(data, function(column) nzchar(trimws(as.character(column))), logical(nrow(data)))) > 0L
}

read_snowballingo_inputs <- function() {
  ensure_snowballingo_templates()
  records_path <- snowballingo_records_path()
  log_path <- snowballingo_log_path()
  workbook_path <- snowballingo_workbook_path()

  if (file.exists(workbook_path)) {
    records <- read_review_xlsx_sheet(workbook_path, "snowballing_records", SNOWBALLING_RECORD_COLUMNS)
    log <- read_review_xlsx_sheet(workbook_path, "snowballing_log", SNOWBALLING_LOG_COLUMNS)
  } else {
    records <- read_machine_csv(records_path)
    log <- read_machine_csv(log_path)
  }
  validate_required_columns(records, SNOWBALLING_RECORD_COLUMNS, records_path)
  validate_required_columns(log, SNOWBALLING_LOG_COLUMNS, log_path)

  records <- records[, SNOWBALLING_RECORD_COLUMNS, drop = FALSE]
  log <- log[, SNOWBALLING_LOG_COLUMNS, drop = FALSE]

  record_rows <- records[nonempty_row(records), , drop = FALSE]
  log_rows <- log[nonempty_row(log), , drop = FALSE]

  status <- tolower(trimws(record_rows$candidate_status))
  excluded_status <- status %in% c("exclude_from_prisma", "do_not_count", "excluded")
  has_minimum_identity <- nzchar(trimws(record_rows$title)) |
    nzchar(trimws(record_rows$doi)) |
    nzchar(trimws(record_rows$pmid)) |
    nzchar(trimws(record_rows$arxiv_id)) |
    nzchar(trimws(record_rows$url))

  eligible_records <- record_rows[!excluded_status & has_minimum_identity, , drop = FALSE]
  invalid_records <- record_rows[!has_minimum_identity, , drop = FALSE]

  duplicate_ids <- character()
  ids <- trimws(record_rows$snowballing_id)
  ids <- ids[nzchar(ids)]
  if (length(ids) > 0L) {
    duplicate_ids <- sort(unique(ids[duplicated(ids)]), method = "radix")
  }

  list(
    records_path = records_path,
    log_path = log_path,
    workbook_path = workbook_path,
    records = record_rows,
    log = log_rows,
    eligible_records = eligible_records,
    invalid_records = invalid_records,
    duplicate_ids = duplicate_ids
  )
}

prisma_reconciliation_row <- function(check_id, check_name, scope, expected_value, observed_value, status, notes = "") {
  expected_chr <- as.character(expected_value)
  observed_chr <- as.character(observed_value)
  expected_num <- suppressWarnings(as.numeric(expected_value))
  observed_num <- suppressWarnings(as.numeric(observed_value))
  difference <- ""
  if (!is.na(expected_num) && !is.na(observed_num)) {
    difference <- as.character(observed_num - expected_num)
  }

  data.frame(
    check_id = check_id,
    check_name = check_name,
    scope = scope,
    expected_value = expected_chr,
    observed_value = observed_chr,
    difference = difference,
    status = status,
    notes = notes,
    stringsAsFactors = FALSE
  )
}
