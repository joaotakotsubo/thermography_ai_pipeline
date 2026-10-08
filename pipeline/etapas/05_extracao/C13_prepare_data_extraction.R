#!/usr/bin/env Rscript

# Prepara a estrutura de extração para os estudos incluídos na revisão.
# Separa os campos de identificação dos campos que precisam de preenchimento humano.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP12B")

require_checkpoint_approved <- function(checkpoint_id) {
  approvals <- read_csv_or_empty(checkpoint_approvals_path(), CHECKPOINT_APPROVAL_COLUMNS)
  checkpoint_id <- checkpoint_id_normalize(checkpoint_id)
  row <- approvals[approvals$checkpoint_id == checkpoint_id, , drop = FALSE]
  if (nrow(row) == 0L || !identical(tolower(trimws(row$approved[[1L]])), "yes")) {
    stop("CP13 requires ", checkpoint_id, " approval before preparing data extraction.", call. = FALSE)
  }
  invisible(TRUE)
}

require_checkpoint_approved("CP12B")

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

STUDY_LEVEL_COLUMNS <- c(
  "extraction_id",
  "record_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "url",
  "pdf_filename",
  "pdf_path",
  "eligibility_decision_notes",
  "extraction_status",
  "extractor",
  "extraction_date",
  "checker",
  "check_date",
  "study_country",
  "clinical_setting",
  "study_design",
  "study_objective",
  "population_condition",
  "pain_target_category",
  "pain_measure_or_reference",
  "sample_size_total",
  "sample_size_cases",
  "sample_size_controls",
  "age_summary",
  "sex_summary",
  "inclusion_criteria",
  "exclusion_criteria",
  "dataset_name_or_source",
  "funding_statement",
  "conflict_of_interest_statement",
  "pages_or_sections_checked",
  "study_level_notes"
)

THERMAL_PROTOCOL_COLUMNS <- c(
  "thermal_row_id",
  "record_id",
  "title",
  "body_region",
  "camera_model",
  "spectral_range",
  "thermal_image_type",
  "acquisition_setting",
  "room_temperature",
  "humidity",
  "acclimatization_time",
  "camera_distance",
  "emissivity",
  "roi_definition_method",
  "segmentation_method",
  "preprocessing_steps",
  "thermal_features_extracted",
  "thermal_protocol_notes"
)

MODEL_PERFORMANCE_COLUMNS <- c(
  "model_row_id",
  "record_id",
  "title",
  "analysis_unit",
  "model_family",
  "model_name_or_architecture",
  "model_task_type",
  "input_modalities",
  "thermal_input_role",
  "target_label_or_outcome",
  "reference_standard",
  "train_sample_size",
  "validation_sample_size",
  "test_sample_size",
  "external_validation_sample_size",
  "split_or_cross_validation",
  "comparator_models",
  "primary_metric_name",
  "primary_metric_value",
  "sensitivity",
  "specificity",
  "auc",
  "accuracy",
  "f1_score",
  "mae_or_rmse",
  "calibration_metric",
  "confidence_interval",
  "explainability_method",
  "software_or_framework",
  "code_availability",
  "data_availability",
  "performance_notes"
)

OUTCOME_COLUMNS <- c(
  "outcome_row_id",
  "record_id",
  "title",
  "outcome_domain",
  "pain_measure",
  "pain_timepoint",
  "target_construct",
  "ground_truth_source",
  "main_result_text",
  "statistical_association",
  "clinical_interpretation_notes"
)

QA_ROUTING_COLUMNS <- c(
  "qa_row_id",
  "record_id",
  "title",
  "study_design_after_extraction",
  "model_purpose",
  "qa_primary_tool",
  "qa_secondary_tool",
  "reporting_checklist",
  "qa_routing_status",
  "qa_rationale",
  "qa_reviewer",
  "qa_date"
)

PDF_INVENTORY_COLUMNS <- c(
  "record_id",
  "pdf_filename",
  "pdf_path",
  "pdf_absolute_path",
  "pdf_exists",
  "pdf_size_bytes",
  "pdf_md5"
)

CODEBOOK_COLUMNS <- c("sheet", "field", "allowed_value", "meaning", "required_when")
COUNT_COLUMNS <- c("metric", "value", "note")
VALIDATION_COLUMNS <- c("severity", "issue_type", "record_id", "field", "value", "details")

data_extraction_codebook <- function() {
  rows <- list(
    c("study_level_extraction", "extraction_status", "not_started", "Extraction has not begun.", "always"),
    c("study_level_extraction", "extraction_status", "in_progress", "Extraction has begun but is not ready for checking.", "always"),
    c("study_level_extraction", "extraction_status", "complete", "Primary extractor completed the row.", "always"),
    c("study_level_extraction", "extraction_status", "needs_query", "Extractor needs discussion or source clarification.", "always"),
    c("study_level_extraction", "extraction_status", "cannot_extract", "The full text is available but this field group cannot be extracted.", "always"),
    c("study_level_extraction", "study_design", "diagnostic_accuracy", "Diagnostic or classification accuracy study.", "when extractable"),
    c("study_level_extraction", "study_design", "prediction_model", "Prediction or prognostic model study.", "when extractable"),
    c("study_level_extraction", "study_design", "classification_model_development", "Model development/classification study without full diagnostic-accuracy framing.", "when extractable"),
    c("study_level_extraction", "study_design", "external_validation", "External validation of a prior model or index.", "when extractable"),
    c("study_level_extraction", "study_design", "experimental_pain", "Experimental pain induction or controlled pain-assessment study.", "when extractable"),
    c("study_level_extraction", "study_design", "observational", "Observational study not otherwise classified.", "when extractable"),
    c("study_level_extraction", "pain_target_category", "direct_pain_intensity", "Model targets pain intensity or pain level directly.", "when extractable"),
    c("study_level_extraction", "pain_target_category", "pain_presence_detection", "Model targets pain presence/absence directly.", "when extractable"),
    c("study_level_extraction", "pain_target_category", "painful_condition_diagnosis", "Model targets a painful clinical condition rather than pain score itself.", "when extractable"),
    c("study_level_extraction", "pain_target_category", "analgesia_or_pain_control_response", "Model relates to analgesia, block success, or pain-control response.", "when extractable"),
    c("study_level_extraction", "pain_target_category", "pain_related_thermal_dysfunction", "Model targets pain-related thermal dysfunction or compensatory thermal pattern.", "when extractable"),
    c("model_performance_extraction", "model_family", "classical_ml", "Traditional supervised machine-learning model.", "when model row is used"),
    c("model_performance_extraction", "model_family", "deep_learning", "Deep learning model such as CNN, transformer, MLP, or related architecture.", "when model row is used"),
    c("model_performance_extraction", "model_family", "computer_vision_segmentation", "Computer-vision segmentation or localization method.", "when model row is used"),
    c("model_performance_extraction", "model_family", "statistical_classifier", "Classifying statistical model such as discriminant analysis or classification tree.", "when model row is used"),
    c("model_performance_extraction", "model_family", "radiomics_texture", "Radiomics, texture, or hand-engineered image feature model.", "when model row is used"),
    c("model_performance_extraction", "model_task_type", "classification", "Categorical model output.", "when model row is used"),
    c("model_performance_extraction", "model_task_type", "regression", "Continuous model output.", "when model row is used"),
    c("model_performance_extraction", "model_task_type", "segmentation", "Pixel/region segmentation output.", "when model row is used"),
    c("model_performance_extraction", "model_task_type", "detection_localization", "Detection or localization output.", "when model row is used"),
    c("model_performance_extraction", "thermal_input_role", "primary_input", "Thermal image or thermal feature is the main model input.", "when model row is used"),
    c("model_performance_extraction", "thermal_input_role", "multimodal_input", "Thermal data are used with RGB, depth, clinical, or other modalities.", "when model row is used"),
    c("model_performance_extraction", "thermal_input_role", "feature_source", "Thermal data are converted to extracted variables/features before modeling.", "when model row is used"),
    c("qa_routing", "qa_primary_tool", "QUADAS_2", "Candidate tool for diagnostic accuracy or classification studies with a clinical reference standard.", "after extraction"),
    c("qa_routing", "qa_primary_tool", "PROBAST", "Candidate tool for prediction/prognostic model studies.", "after extraction"),
    c("qa_routing", "qa_primary_tool", "ROBINS_I", "Candidate tool for non-randomized intervention/observational causal questions if applicable.", "after extraction"),
    c("qa_routing", "qa_primary_tool", "needs_methodologist_review", "Use when no single quality-assessment tool is clearly appropriate.", "after extraction"),
    c("qa_routing", "reporting_checklist", "CLAIM", "Candidate reporting checklist for AI medical imaging studies.", "optional"),
    c("qa_routing", "reporting_checklist", "TRIPOD_AI", "Candidate reporting checklist for prediction model studies involving AI.", "optional"),
    c("qa_routing", "qa_routing_status", "not_started", "Quality-assessment routing has not begun.", "always"),
    c("qa_routing", "qa_routing_status", "routed", "Quality-assessment tool route selected.", "after extraction"),
    c("qa_routing", "qa_routing_status", "completed", "Quality assessment completed in a later checkpoint.", "after QA"),
    c("all_extraction_sheets", "blank_cell", "", "Leave blank only when not yet extracted; use explicit not_reported/not_applicable/unclear in notes when completing a field.", "all human-entered fields")
  )

  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- CODEBOOK_COLUMNS
  out
}

validation_issue <- function(severity, issue_type, record_id, field, value, details) {
  data.frame(
    severity = severity,
    issue_type = issue_type,
    record_id = record_id,
    field = field,
    value = value,
    details = details,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, VALIDATION_COLUMNS, drop = FALSE]
}

bind_validation_issues <- function(rows) {
  rows <- rows[vapply(rows, function(x) !is.null(x) && nrow(x) > 0L, logical(1L))]
  if (length(rows) == 0L) {
    return(empty_named_frame(VALIDATION_COLUMNS))
  }
  do.call(rbind, rows)[, VALIDATION_COLUMNS, drop = FALSE]
}

resolve_project_path <- function(path) {
  if (!nzchar(path)) {
    return("")
  }
  if (grepl("^/", path)) {
    return(normalizePath(path, mustWork = FALSE))
  }
  normalizePath(file.path(CFG$project_root, path), mustWork = FALSE)
}

template_base <- function(queue) {
  queue[, c(
    "record_id",
    "title",
    "authors",
    "year",
    "journal_or_source",
    "doi",
    "url",
    "pdf_filename",
    "pdf_path",
    "decision_notes"
  ), drop = FALSE]
}

prepare_study_level <- function(queue) {
  base <- template_base(queue)
  out <- data.frame(
    extraction_id = paste0("EXT_", base$record_id),
    record_id = base$record_id,
    title = base$title,
    authors = base$authors,
    year = base$year,
    journal_or_source = base$journal_or_source,
    doi = base$doi,
    url = base$url,
    pdf_filename = base$pdf_filename,
    pdf_path = base$pdf_path,
    eligibility_decision_notes = base$decision_notes,
    extraction_status = "not_started",
    extractor = "",
    extraction_date = "",
    checker = "",
    check_date = "",
    study_country = "",
    clinical_setting = "",
    study_design = "",
    study_objective = "",
    population_condition = "",
    pain_target_category = "",
    pain_measure_or_reference = "",
    sample_size_total = "",
    sample_size_cases = "",
    sample_size_controls = "",
    age_summary = "",
    sex_summary = "",
    inclusion_criteria = "",
    exclusion_criteria = "",
    dataset_name_or_source = "",
    funding_statement = "",
    conflict_of_interest_statement = "",
    pages_or_sections_checked = "",
    study_level_notes = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[, STUDY_LEVEL_COLUMNS, drop = FALSE]
}

prepare_repeated_rows <- function(queue, rows_per_record, prefix, row_id_column, columns, extra_defaults = list()) {
  repeated <- queue[rep(seq_len(nrow(queue)), each = rows_per_record), , drop = FALSE]
  sequence <- rep(seq_len(rows_per_record), times = nrow(queue))
  out <- data.frame(
    row_id = sprintf("%s_%s_%02d", prefix, repeated$record_id, sequence),
    record_id = repeated$record_id,
    title = repeated$title,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out)[names(out) == "row_id"] <- row_id_column

  for (column in setdiff(columns, names(out))) {
    out[[column]] <- ""
  }
  for (name in names(extra_defaults)) {
    out[[name]] <- extra_defaults[[name]]
  }
  out[, columns, drop = FALSE]
}

prepare_qa_routing <- function(queue) {
  out <- data.frame(
    qa_row_id = paste0("QA_", queue$record_id),
    record_id = queue$record_id,
    title = queue$title,
    study_design_after_extraction = "",
    model_purpose = "",
    qa_primary_tool = "",
    qa_secondary_tool = "",
    reporting_checklist = "",
    qa_routing_status = "not_started",
    qa_rationale = "",
    qa_reviewer = "",
    qa_date = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out[, QA_ROUTING_COLUMNS, drop = FALSE]
}

prepare_pdf_inventory <- function(queue) {
  absolute_paths <- vapply(queue$pdf_path, resolve_project_path, character(1L))
  exists <- file.exists(absolute_paths)
  sizes <- ifelse(exists, as.character(file.info(absolute_paths)$size), "")
  md5 <- vapply(absolute_paths, file_md5, character(1L))
  data.frame(
    record_id = queue$record_id,
    pdf_filename = queue$pdf_filename,
    pdf_path = queue$pdf_path,
    pdf_absolute_path = absolute_paths,
    pdf_exists = ifelse(exists, "yes", "no"),
    pdf_size_bytes = sizes,
    pdf_md5 = md5,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, PDF_INVENTORY_COLUMNS, drop = FALSE]
}

validate_queue <- function(queue, pdf_inventory) {
  rows <- list()

  duplicate_ids <- unique(queue$record_id[duplicated(queue$record_id)])
  if (length(duplicate_ids) > 0L) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(duplicate_ids, function(record_id) {
      validation_issue("BLOCKER", "duplicate_record_id", record_id, "record_id", record_id, "Each included record must appear once in the extraction queue.")
    }))
  }

  not_included <- queue$full_text_decision != "include_for_extraction"
  if (any(not_included)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(not_included), function(i) {
      validation_issue("BLOCKER", "non_included_record_in_extraction_queue", queue$record_id[[i]], "full_text_decision", queue$full_text_decision[[i]], "CP13 can only prepare extraction for include_for_extraction records.")
    }))
  }

  missing_pdf <- pdf_inventory$pdf_exists != "yes"
  if (any(missing_pdf)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(missing_pdf), function(i) {
      validation_issue("BLOCKER", "missing_pdf_for_included_record", pdf_inventory$record_id[[i]], "pdf_path", pdf_inventory$pdf_path[[i]], "Included records require a retrievable local PDF before extraction.")
    }))
  }

  missing_notes <- !nzchar(queue$decision_notes)
  if (any(missing_notes)) {
    rows[[length(rows) + 1L]] <- do.call(rbind, lapply(which(missing_notes), function(i) {
      validation_issue("WARNING", "missing_eligibility_decision_notes", queue$record_id[[i]], "decision_notes", "", "Eligibility decision notes are useful for extraction orientation.")
    }))
  }

  bind_validation_issues(rows)
}

write_data_extraction_xlsx <- function(sheets, editable_columns, path) {
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
  locked_style <- openxlsx::createStyle(fgFill = "#F2F2F2", wrapText = TRUE, valign = "top")

  used_names <- character()
  for (sheet_name in names(sheets)) {
    safe_name <- xlsx_safe_sheet_name(sheet_name, used_names)
    used_names <- c(used_names, safe_name)
    data <- xlsx_truncate_frame(sheets[[sheet_name]])
    if (ncol(data) == 0L) {
      data <- data.frame(note = character(), stringsAsFactors = FALSE)
    }

    openxlsx::addWorksheet(workbook, safe_name, gridLines = TRUE)
    openxlsx::writeData(workbook, safe_name, data, startRow = 1L, startCol = 1L, colNames = TRUE)
    openxlsx::addStyle(workbook, safe_name, header_style, rows = 1L, cols = seq_len(ncol(data)), gridExpand = TRUE)

    if (nrow(data) > 0L) {
      rows <- seq_len(nrow(data)) + 1L
      openxlsx::addStyle(workbook, safe_name, body_style, rows = rows, cols = seq_len(ncol(data)), gridExpand = TRUE)

      editable <- match(editable_columns[[sheet_name]], names(data))
      editable <- editable[!is.na(editable)]
      locked <- setdiff(seq_len(ncol(data)), editable)
      if (length(locked) > 0L) {
        openxlsx::addStyle(workbook, safe_name, locked_style, rows = rows, cols = locked, gridExpand = TRUE, stack = TRUE)
      }
      if (length(editable) > 0L) {
        openxlsx::addStyle(workbook, safe_name, editable_style, rows = rows, cols = editable, gridExpand = TRUE, stack = TRUE)
      }
    }

    openxlsx::freezePane(workbook, safe_name, firstActiveRow = 2L, firstActiveCol = 2L)
    openxlsx::addFilter(workbook, safe_name, row = 1L, cols = seq_len(ncol(data)))
    widths <- pmin(pmax(nchar(names(data), type = "chars") + 2L, 12L), 48L)
    openxlsx::setColWidths(workbook, safe_name, cols = seq_len(ncol(data)), widths = widths)
  }

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  clean_generated_xlsx_structure(path)
  invisible(path)
}

input_queue_path <- file.path(CFG$pipeline_dir, "full_text", "eligibility", "final_adjudication", "full_text_eligibility_extraction_queue.csv")
if (!file.exists(input_queue_path)) {
  stop("Missing CP12B extraction queue: ", project_relative_path(input_queue_path), call. = FALSE)
}

queue <- read_machine_csv(input_queue_path)
validate_required_columns(queue, FULL_TEXT_ELIGIBILITY_COLUMNS, input_queue_path)
queue <- as_character_frame(queue[, FULL_TEXT_ELIGIBILITY_COLUMNS, drop = FALSE])
queue$record_id <- trimws(queue$record_id)
queue <- queue[nzchar(queue$record_id), , drop = FALSE]
queue <- queue[order(queue$record_id, method = "radix"), , drop = FALSE]

data_extraction_dir <- ensure_dir(file.path(CFG$pipeline_dir, "data_extraction"))

study_level <- prepare_study_level(queue)
thermal_protocol <- prepare_repeated_rows(
  queue,
  rows_per_record = 2L,
  prefix = "THERM",
  row_id_column = "thermal_row_id",
  columns = THERMAL_PROTOCOL_COLUMNS
)
model_performance <- prepare_repeated_rows(
  queue,
  rows_per_record = 4L,
  prefix = "MODEL",
  row_id_column = "model_row_id",
  columns = MODEL_PERFORMANCE_COLUMNS
)
outcomes <- prepare_repeated_rows(
  queue,
  rows_per_record = 2L,
  prefix = "OUTCOME",
  row_id_column = "outcome_row_id",
  columns = OUTCOME_COLUMNS
)
qa_routing <- prepare_qa_routing(queue)
pdf_inventory <- prepare_pdf_inventory(queue)
codebook <- data_extraction_codebook()
validation_issues <- validate_queue(queue, pdf_inventory)

counts_values <- list(
  included_records_ready_for_extraction = nrow(queue),
  study_level_template_rows = nrow(study_level),
  thermal_protocol_template_rows = nrow(thermal_protocol),
  model_performance_template_rows = nrow(model_performance),
  outcome_template_rows = nrow(outcomes),
  qa_routing_rows = nrow(qa_routing),
  pdfs_present = sum(pdf_inventory$pdf_exists == "yes"),
  pdfs_missing = sum(pdf_inventory$pdf_exists != "yes"),
  validation_blockers = sum(validation_issues$severity == "BLOCKER"),
  validation_warnings = sum(validation_issues$severity == "WARNING")
)

counts <- data.frame(
  metric = names(counts_values),
  value = as.character(unlist(counts_values, use.names = FALSE)),
  note = c(
    "Number of CP12B-included records used to prepare extraction.",
    "One row per included study.",
    "Two rows per included study for multiple thermal acquisition/protocol descriptions.",
    "Four rows per included study for multiple models, comparators, or metrics.",
    "Two rows per included study for multiple pain/clinical outcomes.",
    "One row per included study for later quality-assessment routing.",
    "Included records with local PDF present.",
    "Included records missing local PDF.",
    "Blocking validation issues in CP13.",
    "Non-blocking validation warnings in CP13."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)[, COUNT_COLUMNS, drop = FALSE]

instructions <- data.frame(
  topic = c(
    "Scope",
    "Editable fields",
    "Study-level sheet",
    "Model-performance sheet",
    "Thermal-protocol sheet",
    "Outcome sheet",
    "Quality routing",
    "Audit rule"
  ),
  instruction = c(
    "This package covers the 33 CP12B-approved studies included for data extraction.",
    "Yellow cells are intended for human extraction. Grey cells are identifiers or audit context generated by the R pipeline.",
    "Use study_level_extraction for study design, population, pain target, sample and study-level notes.",
    "Use model_performance_extraction for every AI/ML/computer-vision model, comparator, validation split and performance metric that should be extracted.",
    "Use thermal_protocol_extraction for camera, acquisition conditions, body region, ROI, segmentation and thermal features.",
    "Use outcome_extraction for pain or clinical outcomes, reference standards and interpretive notes.",
    "Use qa_routing after extraction to choose the most appropriate quality-assessment or reporting framework.",
    "Do not edit raw search exports or CP12B eligibility outputs; returned extraction workbooks will be imported by a later checkpoint."
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

study_csv <- file.path(data_extraction_dir, "data_extraction_study_level_template.csv")
thermal_csv <- file.path(data_extraction_dir, "data_extraction_thermal_protocol_template.csv")
model_csv <- file.path(data_extraction_dir, "data_extraction_model_performance_template.csv")
outcome_csv <- file.path(data_extraction_dir, "data_extraction_outcome_template.csv")
qa_csv <- file.path(data_extraction_dir, "data_extraction_quality_routing_template.csv")
reference_csv <- file.path(data_extraction_dir, "data_extraction_included_reference.csv")
pdf_inventory_csv <- file.path(data_extraction_dir, "data_extraction_pdf_inventory.csv")
codebook_csv <- file.path(data_extraction_dir, "data_extraction_codebook.csv")
counts_csv <- file.path(data_extraction_dir, "data_extraction_counts.csv")
validation_csv <- file.path(data_extraction_dir, "data_extraction_validation_issues.csv")
package_xlsx <- file.path(data_extraction_dir, "data_extraction_package.xlsx")

stable_write_csv(study_level, study_csv, STUDY_LEVEL_COLUMNS)
stable_write_csv(thermal_protocol, thermal_csv, THERMAL_PROTOCOL_COLUMNS)
stable_write_csv(model_performance, model_csv, MODEL_PERFORMANCE_COLUMNS)
stable_write_csv(outcomes, outcome_csv, OUTCOME_COLUMNS)
stable_write_csv(qa_routing, qa_csv, QA_ROUTING_COLUMNS)
stable_write_csv(queue, reference_csv, FULL_TEXT_ELIGIBILITY_COLUMNS)
stable_write_csv(pdf_inventory, pdf_inventory_csv, PDF_INVENTORY_COLUMNS)
stable_write_csv(codebook, codebook_csv, CODEBOOK_COLUMNS)
stable_write_csv(counts, counts_csv, COUNT_COLUMNS)
stable_write_csv(validation_issues, validation_csv, VALIDATION_COLUMNS)

write_data_extraction_xlsx(
  sheets = list(
    instructions = instructions,
    study_level_extraction = study_level,
    thermal_protocol_extraction = thermal_protocol,
    model_performance_extraction = model_performance,
    outcome_extraction = outcomes,
    qa_routing = qa_routing,
    included_reference = queue,
    pdf_inventory = pdf_inventory,
    codebook = codebook,
    counts = counts,
    validation_issues = validation_issues
  ),
  editable_columns = list(
    instructions = character(),
    study_level_extraction = setdiff(STUDY_LEVEL_COLUMNS, c(
      "extraction_id", "record_id", "title", "authors", "year", "journal_or_source",
      "doi", "url", "pdf_filename", "pdf_path", "eligibility_decision_notes"
    )),
    thermal_protocol_extraction = setdiff(THERMAL_PROTOCOL_COLUMNS, c("thermal_row_id", "record_id", "title")),
    model_performance_extraction = setdiff(MODEL_PERFORMANCE_COLUMNS, c("model_row_id", "record_id", "title")),
    outcome_extraction = setdiff(OUTCOME_COLUMNS, c("outcome_row_id", "record_id", "title")),
    qa_routing = setdiff(QA_ROUTING_COLUMNS, c("qa_row_id", "record_id", "title")),
    included_reference = character(),
    pdf_inventory = character(),
    codebook = character(),
    counts = character(),
    validation_issues = character()
  ),
  path = package_xlsx
)

warnings <- character()
if (sum(validation_issues$severity == "BLOCKER") > 0L) {
  warnings <- c(warnings, paste0(sum(validation_issues$severity == "BLOCKER"), " blocker validation issue(s) must be corrected before extraction."))
}
if (sum(validation_issues$severity == "WARNING") > 0L) {
  warnings <- c(warnings, paste0(sum(validation_issues$severity == "WARNING"), " non-blocking validation warning(s) were recorded."))
}
if (nrow(model_performance) > nrow(queue)) {
  warnings <- c(warnings, "Model-performance sheet intentionally provides multiple rows per study; unused rows may remain blank.")
}

outputs <- c(
  study_csv,
  thermal_csv,
  model_csv,
  outcome_csv,
  qa_csv,
  reference_csv,
  pdf_inventory_csv,
  codebook_csv,
  counts_csv,
  validation_csv,
  package_xlsx,
  file.path(CFG$checkpoints_dir, "CP13_prepare_data_extraction.md")
)

review_file <- write_checkpoint(
  id = "CP13",
  name = "prepare_data_extraction",
  what_ran = paste(
    "Prepared the human data-extraction package from the CP12B-approved",
    "include_for_extraction records. The script validated record uniqueness,",
    "local PDF availability, and final eligibility status, then wrote structured",
    "study-level, thermal-protocol, model-performance, outcome, quality-routing,",
    "reference, codebook, validation, and count outputs."
  ),
  numbers = stats::setNames(counts$value, counts$metric),
  review_items = c(
    "Open data_extraction_package.xlsx and confirm the 33 included records.",
    "Confirm every included record has a local PDF in pdf_inventory.",
    "Use only yellow cells for human extraction fields.",
    "Do not begin import of completed extraction until returned workbook structure is validated in a later checkpoint."
  ),
  outputs = outputs,
  warnings = warnings,
  gate_question = "Approve CP13 only after the extraction workbook, codebook, PDF inventory, and validation issues have been inspected."
)

cat("CP13 data-extraction package prepared\n")
cat("Included records:", nrow(queue), "\n")
cat("Study-level rows:", nrow(study_level), "\n")
cat("Model-performance rows:", nrow(model_performance), "\n")
cat("PDFs present:", sum(pdf_inventory$pdf_exists == "yes"), "\n")
cat("Validation issues:", nrow(validation_issues), "\n")
cat("Extraction workbook:", project_relative_path(package_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
