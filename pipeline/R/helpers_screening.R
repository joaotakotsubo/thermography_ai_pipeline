# Prepara os materiais de triagem e organiza o acompanhamento dos registros.
# As respostas e justificativas continuam sendo fornecidas pelos revisores.


SENTINEL_COLUMNS <- c(
  "sentinel_id",
  "sentinel_label",
  "title",
  "doi",
  "pmid",
  "arxiv_id",
  "notes"
)

SENTINEL_REPORT_COLUMNS <- c(
  "sentinel_id",
  "sentinel_label",
  "title",
  "doi",
  "pmid",
  "arxiv_id",
  "doi_normalized",
  "pmid_normalized",
  "arxiv_id_normalized",
  "title_normalized",
  "match_key",
  "recovered_in_primary",
  "recovered_record_id",
  "recovered_record_source_id",
  "recovered_source_database",
  "recovered_title",
  "found_in_normalized_primary",
  "found_in_imported_primary",
  "found_in_any_processed_stream",
  "status",
  "notes"
)

sentinels_csv_path <- function() {
  file.path(CFG$tests_dir, "sentinels.csv")
}

sentinels_workbook_path <- function() {
  file.path(CFG$tests_dir, "sentinels.xlsx")
}

ensure_sentinel_templates <- function() {
  ensure_dir(CFG$tests_dir)
  csv_path <- sentinels_csv_path()
  workbook_path <- sentinels_workbook_path()

  if (!file.exists(csv_path)) {
    stable_write_csv(empty_named_frame(SENTINEL_COLUMNS), csv_path, SENTINEL_COLUMNS)
  }
  if (!file.exists(workbook_path)) {
    csv_data <- read_csv_or_empty(csv_path, SENTINEL_COLUMNS)
    write_review_xlsx(list(sentinels = csv_data[, SENTINEL_COLUMNS, drop = FALSE]), workbook_path)
  }

  invisible(c(csv = csv_path, workbook = workbook_path))
}

read_sentinel_inputs <- function() {
  ensure_sentinel_templates()
  csv_path <- sentinels_csv_path()
  workbook_path <- sentinels_workbook_path()

  csv_sentinels <- read_machine_csv(csv_path)
  validate_required_columns(csv_sentinels, SENTINEL_COLUMNS, csv_path)
  csv_sentinels <- csv_sentinels[, SENTINEL_COLUMNS, drop = FALSE]
  csv_sentinels <- csv_sentinels[nonempty_row(csv_sentinels), , drop = FALSE]

  xlsx_sentinels <- empty_named_frame(SENTINEL_COLUMNS)
  if (file.exists(workbook_path)) {
    xlsx_sentinels <- read_review_xlsx_sheet(workbook_path, "sentinels", SENTINEL_COLUMNS)
    validate_required_columns(xlsx_sentinels, SENTINEL_COLUMNS, workbook_path)
    xlsx_sentinels <- xlsx_sentinels[, SENTINEL_COLUMNS, drop = FALSE]
    xlsx_sentinels <- xlsx_sentinels[nonempty_row(xlsx_sentinels), , drop = FALSE]
  }

  sentinels <- if (nrow(xlsx_sentinels) > 0L) xlsx_sentinels else csv_sentinels

  duplicate_ids <- character()
  ids <- trimws(sentinels$sentinel_id)
  ids <- ids[nzchar(ids)]
  if (length(ids) > 0L) {
    duplicate_ids <- sort(unique(ids[duplicated(ids)]), method = "radix")
  }

  list(
    csv_path = csv_path,
    workbook_path = workbook_path,
    sentinels = sentinels,
    duplicate_ids = duplicate_ids
  )
}

sentinel_match_key <- function(doi_normalized, pmid_normalized, arxiv_id_normalized, title_normalized) {
  ifelse(
    nzchar(doi_normalized),
    paste0("doi:", doi_normalized),
    ifelse(
      nzchar(pmid_normalized),
      paste0("pmid:", pmid_normalized),
      ifelse(
        nzchar(arxiv_id_normalized),
        paste0("arxiv:", arxiv_id_normalized),
        ifelse(nzchar(title_normalized), paste0("title:", title_normalized), "")
      )
    )
  )
}

sentinel_record_keys <- function(records) {
  cbind(
    doi = ifelse(nzchar(records$doi_normalized), paste0("doi:", records$doi_normalized), ""),
    pmid = ifelse(nzchar(records$pmid_normalized), paste0("pmid:", records$pmid_normalized), ""),
    arxiv = ifelse(nzchar(records$arxiv_id_normalized), paste0("arxiv:", records$arxiv_id_normalized), ""),
    title = ifelse(nzchar(records$title_normalized), paste0("title:", records$title_normalized), "")
  )
}

sentinel_find_first_match <- function(match_key, records) {
  if (!nzchar(match_key) || nrow(records) == 0L) {
    return(NA_integer_)
  }
  keys <- sentinel_record_keys(records)
  hits <- which(rowSums(keys == match_key) > 0L)
  if (length(hits) == 0L) {
    return(NA_integer_)
  }
  hits[[1L]]
}

SCREENING_WORKSHEET_COLUMNS <- c(
  "record_id",
  "source_databases",
  "source_files",
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
  "reviewer_1_decision",
  "reviewer_1_reason",
  "reviewer_2_decision",
  "reviewer_2_reason",
  "conflict_flag",
  "final_decision",
  "final_reason",
  "notes"
)

SCREENING_BLINDED_COLUMNS <- c(
  "record_id",
  "title",
  "abstract",
  "keywords",
  "year",
  "publication_type",
  "candidate_flags",
  "reviewer_1_decision",
  "reviewer_1_reason",
  "reviewer_2_decision",
  "reviewer_2_reason",
  "conflict_flag",
  "final_decision",
  "final_reason",
  "notes"
)

SCREENING_CODEBOOK_COLUMNS <- c(
  "field",
  "allowed_values",
  "definition",
  "human_editable",
  "notes"
)

text_contains_any <- function(text, pattern) {
  grepl(pattern, tolower(norm_empty_if_na(text)), perl = TRUE)
}

screening_candidate_flags <- function(records) {
  text <- paste(records$title, records$abstract, records$keywords, records$publication_type, records$document_type)
  flags <- vector("list", nrow(records))

  flag_specs <- list(
    flag_has_thermal_terms = "thermograph|thermal imaging|thermal image|thermal camera|thermal video|infrared|ir camera|temperature map",
    flag_has_ai_terms = "artificial intelligence|machine learning|deep learning|neural network|computer vision|radiomics|algorithm|classification|prediction|automated|xgboost|support vector|random forest",
    flag_has_pain_terms = "pain|nocicep|analgesi|hyperalgesi|allodyni|ache|dolor|neuropath",
    flag_possible_animal = "\\brat\\b|\\brats\\b|\\bmouse\\b|\\bmice\\b|\\banimal\\b|\\brabbit\\b|\\bporcine\\b|\\bswine\\b|\\bcanine\\b",
    flag_possible_therapy_not_imaging = "treatment|therapy|therapeutic|intervention|rehabilitation|cooling|heating|laser therapy",
    flag_possible_fever_only = "fever|febrile|temperature screening|covid|sars-cov-2",
    flag_possible_breast_cancer_only = "breast cancer|breast neoplasm|mammograph",
    flag_possible_diabetic_foot_only = "diabetic foot|diabetes|neuropathy|plantar|ulcer",
    flag_possible_review_article = "systematic review|meta-analysis|scoping review|narrative review|literature review",
    flag_possible_conference_review = "conference review|conference abstract",
    flag_missing_abstract = "^$",
    flag_preprint = "preprint|arxiv|medrxiv|biorxiv",
    flag_conference = "conference|proceedings|inproceedings|cpaper|\\bconf\\b",
    flag_dissertation = "dissertation|thesis|proquest"
  )

  for (i in seq_len(nrow(records))) {
    row_flags <- character()
    for (flag_name in names(flag_specs)) {
      if (identical(flag_name, "flag_missing_abstract")) {
        matched <- !nzchar(trimws(records$abstract[[i]]))
      } else {
        matched <- text_contains_any(text[[i]], flag_specs[[flag_name]])
      }
      if (isTRUE(matched)) {
        row_flags <- c(row_flags, flag_name)
      }
    }
    flags[[i]] <- paste(unique(row_flags), collapse = ";")
  }

  unlist(flags, use.names = FALSE)
}

screening_codebook <- function() {
  rows <- list(
    c("record_id", "", "Stable post-deduplication primary record identifier.", "no", "Generated by CP06."),
    c("source_databases", "", "Semicolon-separated source databases represented by the survivor and absorbed exact duplicates.", "no", "Useful for audit, not eligibility."),
    c("source_files", "", "Semicolon-separated source files represented by the survivor and absorbed exact duplicates.", "no", "Useful for audit, not eligibility."),
    c("title", "", "Record title.", "no", ""),
    c("abstract", "", "Record abstract or summary when available.", "no", ""),
    c("keywords", "", "Record keywords when available.", "no", ""),
    c("authors", "", "Record authors as parsed from source export.", "no", ""),
    c("year", "", "Publication year parsed during CP05.", "no", ""),
    c("journal_or_source", "", "Journal, proceedings, preprint server, thesis source, or source venue.", "no", ""),
    c("doi", "", "Raw DOI value when available.", "no", ""),
    c("pmid", "", "Raw PMID value when available.", "no", ""),
    c("url", "", "Source URL when available.", "no", ""),
    c("publication_type", "", "Publication type as parsed from source export.", "no", ""),
    c("candidate_flags", "semicolon-separated machine flags", "Machine-generated logistics flags only. These are not eligibility decisions.", "no", "Allowed flags are listed in the final handoff."),
    c("reviewer_1_decision", "include;exclude;uncertain", "Reviewer 1 title/abstract decision.", "yes", "Do not pre-fill by machine."),
    c("reviewer_1_reason", "free text", "Reviewer 1 reason or short note.", "yes", ""),
    c("reviewer_2_decision", "include;exclude;uncertain", "Reviewer 2 title/abstract decision.", "yes", "Do not pre-fill by machine."),
    c("reviewer_2_reason", "free text", "Reviewer 2 reason or short note.", "yes", ""),
    c("conflict_flag", "", "Reserved for later reconciliation script.", "no", "Left blank in CP09."),
    c("final_decision", "include;exclude;uncertain", "Final adjudicated screening decision.", "yes", "Do not complete before reviewer reconciliation."),
    c("final_reason", "free text", "Final exclusion/adjudication reason when applicable.", "yes", ""),
    c("notes", "free text", "Reviewer notes.", "yes", "")
  )

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- SCREENING_CODEBOOK_COLUMNS
  out
}

SCREENING_REVIEWER_IDS <- c("reviewer_1", "reviewer_2")

SCREENING_REVIEWER_COLUMNS <- c(
  "record_id",
  "title",
  "abstract",
  "keywords",
  "year",
  "publication_type",
  "candidate_flags",
  "decision",
  "reason",
  "notes"
)

SCREENING_REVIEWER_CODEBOOK_COLUMNS <- c(
  "field",
  "allowed_values",
  "definition",
  "human_editable",
  "notes"
)

SCREENING_REVIEWER_MANIFEST_COLUMNS <- c(
  "reviewer_id",
  "workbook_path",
  "csv_template_path",
  "sheet_name",
  "records",
  "workbook_created_this_run",
  "csv_template_created_this_run",
  "status"
)

SCREENING_VALIDATION_ISSUE_COLUMNS <- c(
  "reviewer_id",
  "record_id",
  "field",
  "issue_type",
  "severity",
  "observed_value",
  "message"
)

SCREENING_RECONCILIATION_COLUMNS <- c(
  "record_id",
  "title",
  "year",
  "publication_type",
  "candidate_flags",
  "reviewer_1_decision",
  "reviewer_1_reason",
  "reviewer_1_notes",
  "reviewer_2_decision",
  "reviewer_2_reason",
  "reviewer_2_notes",
  "agreement_status",
  "conflict_flag",
  "full_text_required_by_rule",
  "reconciliation_status"
)

FULL_TEXT_QUEUE_COLUMNS <- c(
  "record_id",
  "title",
  "year",
  "publication_type",
  "candidate_flags",
  "reviewer_1_decision",
  "reviewer_2_decision",
  "reason_for_full_text"
)

SCREENING_PRE_ADJUDICATION_COLUMNS <- c(
  "record_id",
  "title",
  "abstract",
  "keywords",
  "year",
  "publication_type",
  "candidate_flags",
  "reviewer_1_decision",
  "reviewer_1_reason",
  "reviewer_1_notes",
  "reviewer_2_decision",
  "reviewer_2_reason",
  "reviewer_2_notes",
  "decision_pattern",
  "pre_adjudication_priority",
  "pre_adjudicated_decision",
  "pre_adjudication_reason",
  "pre_adjudicator_notes"
)

SCREENING_PRE_ADJUDICATION_CODEBOOK_COLUMNS <- c(
  "field",
  "allowed_values",
  "definition",
  "human_editable",
  "notes"
)

screening_allowed_decisions <- function() {
  c("include", "exclude", "uncertain")
}

pre_adjudication_allowed_decisions <- function() {
  c("full_text", "exclude", "needs_discussion")
}

normalize_screening_decision <- function(x) {
  tolower(trimws(norm_empty_if_na(x)))
}

normalize_pre_adjudication_decision <- function(x) {
  tolower(trimws(norm_empty_if_na(x)))
}

screening_pre_adjudication_priority <- function(reviewer_1_decision, reviewer_2_decision) {
  r1 <- normalize_screening_decision(reviewer_1_decision)
  r2 <- normalize_screening_decision(reviewer_2_decision)

  ifelse(
    r1 == "include" | r2 == "include",
    "P1_any_include_conflict",
    ifelse(
      (r1 == "exclude" & r2 == "uncertain") | (r1 == "uncertain" & r2 == "exclude"),
      "P2_exclude_uncertain_conflict",
      "P3_other_conflict"
    )
  )
}

screening_pre_adjudication_codebook <- function() {
  rows <- list(
    c("record_id", "", "Stable post-deduplication primary record identifier.", "no", "Do not edit."),
    c("title", "", "Record title used for title/abstract screening.", "no", "Do not edit."),
    c("abstract", "", "Record abstract or summary when available.", "no", "Do not edit."),
    c("keywords", "", "Record keywords when available.", "no", "Do not edit."),
    c("year", "", "Publication year parsed during CP05.", "no", "Do not edit."),
    c("publication_type", "", "Publication type as parsed from source export.", "no", "Do not edit."),
    c("candidate_flags", "semicolon-separated machine flags", "Machine-generated logistics flags only. These are not eligibility decisions.", "no", "Use only as attention flags."),
    c("reviewer_1_decision", "include;exclude;uncertain", "Reviewer 1 original title/abstract decision.", "no", "Preserve as observed input."),
    c("reviewer_1_reason", "free text", "Reviewer 1 original reason.", "no", "Preserve as observed input."),
    c("reviewer_1_notes", "free text", "Reviewer 1 original optional notes.", "no", "Preserve as observed input."),
    c("reviewer_2_decision", "include;exclude;uncertain", "Reviewer 2 original title/abstract decision.", "no", "Preserve as observed input."),
    c("reviewer_2_reason", "free text", "Reviewer 2 original reason.", "no", "Preserve as observed input."),
    c("reviewer_2_notes", "free text", "Reviewer 2 original optional notes.", "no", "Preserve as observed input."),
    c("decision_pattern", "", "Reviewer decision contrast for sorting and audit.", "no", "Generated from reviewer decisions."),
    c("pre_adjudication_priority", "P1_any_include_conflict;P2_exclude_uncertain_conflict;P3_other_conflict", "Operational priority for reviewing title/abstract conflicts.", "no", "Start with P1 records."),
    c("pre_adjudicated_decision", "full_text;exclude;needs_discussion", "Human pre-adjudication outcome for title/abstract conflicts only.", "yes", "full_text keeps the record for full-text assessment; exclude removes only if clearly outside scope; needs_discussion marks unresolved cases."),
    c("pre_adjudication_reason", "free text", "Short reason supporting the pre-adjudication decision.", "yes", "Use Portuguese if possible and cite title/abstract logic only."),
    c("pre_adjudicator_notes", "free text", "Optional notes for the later full-text or discussion stage.", "yes", "Do not rewrite reviewer decisions here.")
  )

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- SCREENING_PRE_ADJUDICATION_CODEBOOK_COLUMNS
  out
}

screening_pre_adjudication_template <- function(conflicts, blinded_records) {
  validate_required_columns(conflicts, SCREENING_RECONCILIATION_COLUMNS, "CP10 screening conflicts")
  validate_required_columns(blinded_records, SCREENING_BLINDED_COLUMNS, "CP09 blinded screening worksheet")

  conflicts <- as_character_frame(conflicts)
  blinded_records <- as_character_frame(blinded_records)
  conflicts$record_id <- trimws(conflicts$record_id)
  blinded_records$record_id <- trimws(blinded_records$record_id)

  match_index <- match(conflicts$record_id, blinded_records$record_id)
  from_blinded <- function(column) {
    values <- rep("", nrow(conflicts))
    found <- !is.na(match_index)
    values[found] <- blinded_records[[column]][match_index[found]]
    values
  }

  title <- conflicts$title
  missing_title <- !nzchar(trimws(title))
  if (any(missing_title)) {
    title[missing_title] <- from_blinded("title")[missing_title]
  }

  out <- data.frame(
    record_id = conflicts$record_id,
    title = title,
    abstract = from_blinded("abstract"),
    keywords = from_blinded("keywords"),
    year = conflicts$year,
    publication_type = conflicts$publication_type,
    candidate_flags = conflicts$candidate_flags,
    reviewer_1_decision = conflicts$reviewer_1_decision,
    reviewer_1_reason = conflicts$reviewer_1_reason,
    reviewer_1_notes = conflicts$reviewer_1_notes,
    reviewer_2_decision = conflicts$reviewer_2_decision,
    reviewer_2_reason = conflicts$reviewer_2_reason,
    reviewer_2_notes = conflicts$reviewer_2_notes,
    decision_pattern = paste0(conflicts$reviewer_1_decision, "_vs_", conflicts$reviewer_2_decision),
    pre_adjudication_priority = screening_pre_adjudication_priority(conflicts$reviewer_1_decision, conflicts$reviewer_2_decision),
    pre_adjudicated_decision = "",
    pre_adjudication_reason = "",
    pre_adjudicator_notes = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, SCREENING_PRE_ADJUDICATION_COLUMNS, drop = FALSE]

  out <- out[order(out$pre_adjudication_priority, out$record_id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

write_screening_pre_adjudication_xlsx <- function(data, codebook, path) {
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

  data <- xlsx_truncate_frame(data[, SCREENING_PRE_ADJUDICATION_COLUMNS, drop = FALSE])
  codebook <- xlsx_truncate_frame(codebook[, SCREENING_PRE_ADJUDICATION_CODEBOOK_COLUMNS, drop = FALSE])

  openxlsx::addWorksheet(workbook, "pre_adjudication", gridLines = TRUE)
  openxlsx::writeData(workbook, "pre_adjudication", data, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::addStyle(workbook, "pre_adjudication", header_style, rows = 1L, cols = seq_len(ncol(data)), gridExpand = TRUE)
  if (nrow(data) > 0L) {
    data_rows <- seq_len(nrow(data)) + 1L
    openxlsx::addStyle(workbook, "pre_adjudication", body_style, rows = data_rows, cols = seq_len(ncol(data)), gridExpand = TRUE)
    editable_cols <- match(c("pre_adjudicated_decision", "pre_adjudication_reason", "pre_adjudicator_notes"), names(data))
    openxlsx::addStyle(workbook, "pre_adjudication", editable_style, rows = data_rows, cols = editable_cols, gridExpand = TRUE, stack = TRUE)
    decision_col <- match("pre_adjudicated_decision", names(data))
    suppressWarnings(openxlsx::dataValidation(
      workbook,
      "pre_adjudication",
      cols = decision_col,
      rows = data_rows,
      type = "list",
      value = paste0('"', paste(pre_adjudication_allowed_decisions(), collapse = ","), '"'),
      allowBlank = TRUE
    ))
  }
  openxlsx::freezePane(workbook, "pre_adjudication", firstActiveRow = 2L, firstActiveCol = 2L)
  openxlsx::addFilter(workbook, "pre_adjudication", row = 1L, cols = seq_len(ncol(data)))
  openxlsx::setColWidths(
    workbook,
    "pre_adjudication",
    cols = seq_len(ncol(data)),
    widths = c(18, 55, 80, 28, 10, 18, 38, 16, 36, 26, 16, 36, 26, 22, 30, 24, 42, 42)
  )

  openxlsx::addWorksheet(workbook, "codebook", gridLines = TRUE)
  openxlsx::writeData(workbook, "codebook", codebook, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::addStyle(workbook, "codebook", header_style, rows = 1L, cols = seq_len(ncol(codebook)), gridExpand = TRUE)
  if (nrow(codebook) > 0L) {
    openxlsx::addStyle(workbook, "codebook", body_style, rows = seq_len(nrow(codebook)) + 1L, cols = seq_len(ncol(codebook)), gridExpand = TRUE)
  }
  openxlsx::freezePane(workbook, "codebook", firstActiveRow = 2L)
  openxlsx::addFilter(workbook, "codebook", row = 1L, cols = seq_len(ncol(codebook)))
  openxlsx::setColWidths(workbook, "codebook", cols = seq_len(ncol(codebook)), widths = c(26, 32, 80, 16, 60))

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

screening_reviewer_sheet_name <- function(reviewer_id) {
  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  paste0("screening_r", sub("^reviewer_", "", reviewer_id))
}

screening_reviewer_dir <- function(reviewer_id) {
  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  file.path(CFG$screening_reviewer_dir, reviewer_id)
}

screening_reviewer_paths <- function(reviewer_id) {
  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  reviewer_dir <- screening_reviewer_dir(reviewer_id)
  list(
    reviewer_id = reviewer_id,
    dir = reviewer_dir,
    workbook = file.path(reviewer_dir, paste0("title_abstract_screening_", reviewer_id, ".xlsx")),
    csv_template = file.path(reviewer_dir, paste0("title_abstract_screening_", reviewer_id, "_template.csv")),
    sheet = screening_reviewer_sheet_name(reviewer_id)
  )
}

screening_reviewer_template <- function(blinded_records) {
  validate_required_columns(blinded_records, SCREENING_BLINDED_COLUMNS, "CP09 blinded screening worksheet")
  data.frame(
    record_id = blinded_records$record_id,
    title = blinded_records$title,
    abstract = blinded_records$abstract,
    keywords = blinded_records$keywords,
    year = blinded_records$year,
    publication_type = blinded_records$publication_type,
    candidate_flags = blinded_records$candidate_flags,
    decision = "",
    reason = "",
    notes = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, SCREENING_REVIEWER_COLUMNS, drop = FALSE]
}

screening_reviewer_codebook <- function(reviewer_id) {
  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  rows <- list(
    c("record_id", "", "Stable post-deduplication primary record identifier.", "no", "Do not edit."),
    c("title", "", "Record title.", "no", "Blinded title/abstract screening field."),
    c("abstract", "", "Record abstract or summary when available.", "no", "Blank abstracts are flagged in candidate_flags."),
    c("keywords", "", "Record keywords when available.", "no", ""),
    c("year", "", "Publication year parsed during CP05.", "no", ""),
    c("publication_type", "", "Publication type as parsed from source export.", "no", ""),
    c("candidate_flags", "semicolon-separated machine flags", "Machine-generated logistics flags only. These are not eligibility decisions.", "no", "Flags can guide attention but must not replace human judgment."),
    c("decision", "include;exclude;uncertain", paste0(reviewer_id, " title/abstract decision."), "yes", "Use exclude only when an exclusion criterion is clearly met; keep uncertain records."),
    c("reason", "free text", "Short reason for the decision, especially for exclusions or uncertainty.", "yes", ""),
    c("notes", "free text", "Optional reviewer notes.", "yes", "Do not enter another reviewer's decision here.")
  )

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- SCREENING_REVIEWER_CODEBOOK_COLUMNS
  out
}

write_screening_reviewer_xlsx <- function(data, codebook, path, reviewer_id) {
  require_openxlsx()
  ensure_parent_dir(path)

  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  sheet <- screening_reviewer_sheet_name(reviewer_id)
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

  data <- xlsx_truncate_frame(data[, SCREENING_REVIEWER_COLUMNS, drop = FALSE])
  codebook <- xlsx_truncate_frame(codebook[, SCREENING_REVIEWER_CODEBOOK_COLUMNS, drop = FALSE])

  openxlsx::addWorksheet(workbook, sheet, gridLines = TRUE)
  openxlsx::writeData(workbook, sheet, data, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::addStyle(workbook, sheet, header_style, rows = 1L, cols = seq_len(ncol(data)), gridExpand = TRUE)
  if (nrow(data) > 0L) {
    openxlsx::addStyle(workbook, sheet, body_style, rows = seq_len(nrow(data)) + 1L, cols = seq_len(ncol(data)), gridExpand = TRUE)
    editable_cols <- match(c("decision", "reason", "notes"), names(data))
    openxlsx::addStyle(workbook, sheet, editable_style, rows = seq_len(nrow(data)) + 1L, cols = editable_cols, gridExpand = TRUE, stack = TRUE)
    decision_col <- match("decision", names(data))
    suppressWarnings(openxlsx::dataValidation(
      workbook,
      sheet,
      cols = decision_col,
      rows = seq_len(nrow(data)) + 1L,
      type = "list",
      value = paste0('"', paste(screening_allowed_decisions(), collapse = ","), '"'),
      allowBlank = TRUE
    ))
  }
  openxlsx::freezePane(workbook, sheet, firstActiveRow = 2L, firstActiveCol = 2L)
  openxlsx::addFilter(workbook, sheet, row = 1L, cols = seq_len(ncol(data)))
  openxlsx::setColWidths(
    workbook,
    sheet,
    cols = seq_len(ncol(data)),
    widths = c(18, 55, 80, 28, 10, 18, 38, 16, 32, 32)
  )

  openxlsx::addWorksheet(workbook, "codebook", gridLines = TRUE)
  openxlsx::writeData(workbook, "codebook", codebook, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::addStyle(workbook, "codebook", header_style, rows = 1L, cols = seq_len(ncol(codebook)), gridExpand = TRUE)
  if (nrow(codebook) > 0L) {
    openxlsx::addStyle(workbook, "codebook", body_style, rows = seq_len(nrow(codebook)) + 1L, cols = seq_len(ncol(codebook)), gridExpand = TRUE)
  }
  openxlsx::freezePane(workbook, "codebook", firstActiveRow = 2L)
  openxlsx::addFilter(workbook, "codebook", row = 1L, cols = seq_len(ncol(codebook)))
  openxlsx::setColWidths(workbook, "codebook", cols = seq_len(ncol(codebook)), widths = c(22, 26, 70, 16, 55))

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

ensure_screening_reviewer_package <- function(reviewer_id, template) {
  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  paths <- screening_reviewer_paths(reviewer_id)
  ensure_dir(paths$dir)

  workbook_created <- "no"
  csv_created <- "no"
  if (!file.exists(paths$csv_template)) {
    stable_write_csv(template, paths$csv_template, SCREENING_REVIEWER_COLUMNS)
    csv_created <- "yes"
  }
  if (!file.exists(paths$workbook)) {
    write_screening_reviewer_xlsx(
      data = template,
      codebook = screening_reviewer_codebook(reviewer_id),
      path = paths$workbook,
      reviewer_id = reviewer_id
    )
    workbook_created <- "yes"
  }

  data.frame(
    reviewer_id = reviewer_id,
    workbook_path = project_relative_path(paths$workbook),
    csv_template_path = project_relative_path(paths$csv_template),
    sheet_name = paths$sheet,
    records = as.character(nrow(template)),
    workbook_created_this_run = workbook_created,
    csv_template_created_this_run = csv_created,
    status = ifelse(file.exists(paths$workbook), "available", "missing"),
    stringsAsFactors = FALSE
  )
}

read_screening_reviewer_workbook <- function(reviewer_id) {
  require_openxlsx()
  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  paths <- screening_reviewer_paths(reviewer_id)

  if (!file.exists(paths$workbook)) {
    return(list(
      data = empty_named_frame(SCREENING_REVIEWER_COLUMNS),
      read_issues = data.frame(
        reviewer_id = reviewer_id,
        record_id = "",
        field = "workbook",
        issue_type = "missing_workbook",
        severity = "PENDING",
        observed_value = "",
        message = paste0("Reviewer workbook is missing: ", project_relative_path(paths$workbook)),
        stringsAsFactors = FALSE
      )
    ))
  }

  sheet_names <- openxlsx::getSheetNames(paths$workbook)
  if (!(paths$sheet %in% sheet_names)) {
    return(list(
      data = empty_named_frame(SCREENING_REVIEWER_COLUMNS),
      read_issues = data.frame(
        reviewer_id = reviewer_id,
        record_id = "",
        field = "sheet",
        issue_type = "missing_sheet",
        severity = "BLOCKER",
        observed_value = paste(sheet_names, collapse = ";"),
        message = paste0("Workbook is missing required sheet '", paths$sheet, "'."),
        stringsAsFactors = FALSE
      )
    ))
  }

  raw <- openxlsx::read.xlsx(paths$workbook, sheet = paths$sheet, colNames = TRUE, detectDates = FALSE)
  if (is.null(raw) || ncol(raw) == 0L) {
    raw <- empty_named_frame(SCREENING_REVIEWER_COLUMNS)
  }
  raw <- as_character_frame(raw)
  for (column in names(raw)) {
    raw[[column]] <- norm_decode_html_entities(raw[[column]])
  }

  missing_columns <- setdiff(SCREENING_REVIEWER_COLUMNS, names(raw))
  read_issues <- empty_named_frame(SCREENING_VALIDATION_ISSUE_COLUMNS)
  if (length(missing_columns) > 0L) {
    read_issues <- data.frame(
      reviewer_id = reviewer_id,
      record_id = "",
      field = missing_columns,
      issue_type = "missing_required_column",
      severity = "BLOCKER",
      observed_value = "",
      message = paste0("Required column is missing from reviewer workbook: ", missing_columns),
      stringsAsFactors = FALSE
    )
  }

  for (missing_col in missing_columns) {
    raw[[missing_col]] <- ""
  }

  list(
    data = raw[, SCREENING_REVIEWER_COLUMNS, drop = FALSE],
    read_issues = read_issues
  )
}

screening_reviewer_validation_issues <- function(reviewer_id, data, expected_record_ids, read_issues = empty_named_frame(SCREENING_VALIDATION_ISSUE_COLUMNS)) {
  reviewer_id <- match.arg(reviewer_id, SCREENING_REVIEWER_IDS)
  issues <- read_issues[, SCREENING_VALIDATION_ISSUE_COLUMNS, drop = FALSE]
  add_issue <- function(record_id, field, issue_type, severity, observed_value, message) {
    data.frame(
      reviewer_id = reviewer_id,
      record_id = record_id,
      field = field,
      issue_type = issue_type,
      severity = severity,
      observed_value = observed_value,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  data <- as_character_frame(data)
  data$record_id <- trimws(data$record_id)
  data$decision <- normalize_screening_decision(data$decision)
  expected_record_ids <- sort(unique(trimws(expected_record_ids)), method = "radix")

  if (nrow(data) == 0L) {
    issues <- rbind(issues, add_issue("", "workbook", "empty_workbook", "PENDING", "", "Reviewer workbook has no screening rows."))
    return(issues[, SCREENING_VALIDATION_ISSUE_COLUMNS, drop = FALSE])
  }

  duplicated_ids <- sort(unique(data$record_id[nzchar(data$record_id) & duplicated(data$record_id)]), method = "radix")
  if (length(duplicated_ids) > 0L) {
    issues <- rbind(
      issues,
      do.call(rbind, lapply(duplicated_ids, function(record_id) {
        add_issue(record_id, "record_id", "duplicate_record_id", "BLOCKER", record_id, "Record appears more than once in reviewer workbook.")
      }))
    )
  }

  missing_ids <- setdiff(expected_record_ids, data$record_id)
  if (length(missing_ids) > 0L) {
    issues <- rbind(
      issues,
      do.call(rbind, lapply(missing_ids, function(record_id) {
        add_issue(record_id, "record_id", "missing_record_id", "BLOCKER", "", "Expected record is missing from reviewer workbook.")
      }))
    )
  }

  extra_ids <- setdiff(data$record_id[nzchar(data$record_id)], expected_record_ids)
  if (length(extra_ids) > 0L) {
    issues <- rbind(
      issues,
      do.call(rbind, lapply(sort(unique(extra_ids), method = "radix"), function(record_id) {
        add_issue(record_id, "record_id", "unexpected_record_id", "BLOCKER", record_id, "Reviewer workbook contains a record_id not present in CP09 screening set.")
      }))
    )
  }

  allowed <- screening_allowed_decisions()
  invalid_rows <- which(nzchar(data$decision) & !(data$decision %in% allowed))
  if (length(invalid_rows) > 0L) {
    issues <- rbind(
      issues,
      do.call(rbind, lapply(invalid_rows, function(i) {
        add_issue(
          data$record_id[[i]],
          "decision",
          "invalid_decision",
          "BLOCKER",
          data$decision[[i]],
          paste0("Decision must be one of: ", paste(allowed, collapse = ", "), ".")
        )
      }))
    )
  }

  expected_rows <- data$record_id %in% expected_record_ids
  missing_decision_rows <- which(expected_rows & !nzchar(data$decision))
  if (length(missing_decision_rows) > 0L) {
    issues <- rbind(
      issues,
      do.call(rbind, lapply(missing_decision_rows, function(i) {
        add_issue(data$record_id[[i]], "decision", "missing_decision", "PENDING", "", "Human reviewer decision is still blank.")
      }))
    )
  }

  issues[, SCREENING_VALIDATION_ISSUE_COLUMNS, drop = FALSE]
}
