#!/usr/bin/env Rscript

# Confere as camadas finais e organiza as informações de relato da revisão.
# As verificações ajudam a localizar diferenças de contagem e de documentação.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required for CP16.", call. = FALSE)
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("The readxl package is required for CP16.", call. = FALSE)
}

checkpoint_gate("CP15")

cp16_dir <- file.path(PROJECT_ROOT, "pipeline", "reporting", "cp16")
tables_dir <- file.path(cp16_dir, "tables")
figure_data_dir <- file.path(cp16_dir, "figure_data")
qa_dir <- file.path(cp16_dir, "qa")
final_dir <- file.path(cp16_dir, "final")
invisible(vapply(
  c(cp16_dir, tables_dir, figure_data_dir, qa_dir, final_dir),
  ensure_dir,
  character(1L)
))

paths <- list(
  cp13 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "data_extraction",
    "data_extraction_package_CP13_COMPLETE_33.xlsx"
  ),
  cp14 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "methodological_appraisal",
    "cp14",
    "final",
    "CP14_AVALIACAO_METODOLOGICA_FINAL_33_ESTUDOS.xlsx"
  ),
  cp15 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "clinical_readiness",
    "cp15",
    "final",
    "CP15_PRONTIDAO_CLINICA_FINAL_33_ESTUDOS.xlsx"
  ),
  prisma_counts = file.path(
    PROJECT_ROOT,
    "pipeline",
    "outputs",
    "prisma_flow_counts.csv"
  ),
  title_duplicate_resolution = file.path(
    PROJECT_ROOT,
    "pipeline",
    "screening",
    "title_deduplication",
    "title_duplicate_resolution.csv"
  ),
  title_abstract_final = file.path(
    PROJECT_ROOT,
    "pipeline",
    "full_text",
    "title_abstract_final_decisions.csv"
  ),
  retrieval_tracking = file.path(
    PROJECT_ROOT,
    "pipeline",
    "full_text",
    "retrieval_tracking",
    "full_text_retrieval_tracking.csv"
  ),
  eligibility_final = file.path(
    PROJECT_ROOT,
    "pipeline",
    "full_text",
    "eligibility",
    "final_adjudication",
    "full_text_eligibility_final_decisions.csv"
  ),
  full_text_exclusions = file.path(
    PROJECT_ROOT,
    "pipeline",
    "full_text",
    "eligibility",
    "final_adjudication",
    "full_text_eligibility_full_text_exclusions.csv"
  ),
  unavailable = file.path(
    PROJECT_ROOT,
    "pipeline",
    "full_text",
    "eligibility",
    "final_adjudication",
    "full_text_eligibility_unavailable_reference_final.csv"
  ),
  workbook = file.path(
    final_dir,
    "CP16_VERIFICACAO_DE_RELATO_33_ESTUDOS.xlsx"
  )
)

missing_inputs <- unlist(paths[names(paths) != "workbook"])
missing_inputs <- missing_inputs[!file.exists(missing_inputs)]
if (length(missing_inputs) > 0L) {
  stop(
    "Missing CP16 inputs: ",
    paste(project_relative_path(missing_inputs), collapse = ", "),
    call. = FALSE
  )
}

read_frozen_csv <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    encoding = "UTF-8"
  )
}

read_sheet <- function(path, sheet) {
  data <- suppressMessages(readxl::read_excel(path, sheet = sheet))
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  for (column in names(data)) {
    if (is.character(data[[column]])) {
      data[[column]][is.na(data[[column]])] <- ""
    }
  }
  data
}

sha256_file <- function(path) {
  result <- system2(
    "shasum",
    c("-a", "256", shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) {
    stop("SHA-256 calculation failed for ", path, call. = FALSE)
  }
  strsplit(result[[1L]], "[[:space:]]+", perl = TRUE)[[1L]][[1L]]
}

normalize_value <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  trimws(value)
}

safe_numeric <- function(value) {
  suppressWarnings(as.numeric(normalize_value(value)))
}

select_existing <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      "Missing required reporting columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  data[, columns, drop = FALSE]
}

left_join_key <- function(left, right, key) {
  left$.cp16_order <- seq_len(nrow(left))
  joined <- merge(
    left,
    right,
    by = key,
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", "_joined")
  )
  joined <- joined[order(joined$.cp16_order), , drop = FALSE]
  joined$.cp16_order <- NULL
  rownames(joined) <- NULL
  joined
}

count_by <- function(data, grouping_columns, count_name = "n") {
  if (nrow(data) == 0L) {
    empty <- data.frame(stringsAsFactors = FALSE)
    for (column in grouping_columns) empty[[column]] <- character()
    empty[[count_name]] <- integer()
    return(empty)
  }
  result <- aggregate(
    rep(1L, nrow(data)),
    by = data[grouping_columns],
    FUN = sum
  )
  names(result)[ncol(result)] <- count_name
  result
}

write_table <- function(data, path, order_columns) {
  if (nrow(data) > 0L && length(order_columns) > 0L) {
    ordering <- do.call(
      order,
      c(
        lapply(order_columns, function(column) {
          value <- data[[column]]
          if (is.character(value)) normalize_value(value) else value
        }),
        list(method = "radix", na.last = TRUE)
      )
    )
    data <- data[ordering, , drop = FALSE]
  }
  rownames(data) <- NULL
  stable_write_csv(data, path, columns = names(data))
  invisible(data)
}

study <- read_sheet(paths$cp13, "study_level_extraction")
thermal <- read_sheet(paths$cp13, "thermal_acquisition")
dataset <- read_sheet(paths$cp13, "dataset_partition")
models <- read_sheet(paths$cp13, "model_definition")
metrics <- read_sheet(paths$cp13, "model_metric")
overlap <- read_sheet(paths$cp13, "publication_overlap")

units <- read_sheet(paths$cp14, "appraisal_units")
applicability <- read_sheet(paths$cp14, "applicability_final")
items <- read_sheet(paths$cp14, "item_final")
domains <- read_sheet(paths$cp14, "domain_final")

readiness <- read_sheet(paths$cp15, "final_readiness")

prisma_counts <- read_frozen_csv(paths$prisma_counts)
title_duplicates <- read_frozen_csv(paths$title_duplicate_resolution)
title_abstract <- read_frozen_csv(paths$title_abstract_final)
retrieval <- read_frozen_csv(paths$retrieval_tracking)
eligibility <- read_frozen_csv(paths$eligibility_final)
exclusions <- read_frozen_csv(paths$full_text_exclusions)
unavailable <- read_frozen_csv(paths$unavailable)

study_columns <- c(
  "record_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "country",
  "clinical_setting",
  "centers_n",
  "study_design",
  "prospective_retrospective",
  "population_condition",
  "pain_scope_relation",
  "pain_target_category",
  "primary_clinical_target",
  "direct_pain_measure_used",
  "pain_measure_name",
  "participants_n_total",
  "participants_n_cases",
  "participants_n_controls",
  "publication_status",
  "peer_review_status",
  "row_status",
  "extraction_certainty",
  "extraction_notes"
)
study_characteristics <- select_existing(study, study_columns)
study_characteristics$year <- safe_numeric(study_characteristics$year)
study_characteristics$centers_n <- safe_numeric(
  study_characteristics$centers_n
)
study_characteristics$participants_n_total <- safe_numeric(
  study_characteristics$participants_n_total
)
study_characteristics$participants_n_cases <- safe_numeric(
  study_characteristics$participants_n_cases
)
study_characteristics$participants_n_controls <- safe_numeric(
  study_characteristics$participants_n_controls
)

thermo_items <- items[
  items$instrument == "Thermography reporting",
  ,
  drop = FALSE
]
thermo_domains <- domains[
  domains$instrument == "Thermography reporting",
  ,
  drop = FALSE
]
thermo_applicability <- applicability[
  applicability$instrument == "Thermography reporting",
  c(
    "appraisal_unit_id",
    "record_id",
    "final_decision",
    "final_rationale"
  ),
  drop = FALSE
]
names(thermo_applicability)[
  names(thermo_applicability) == "final_decision"
] <- "instrument_applicability"
names(thermo_applicability)[
  names(thermo_applicability) == "final_rationale"
] <- "applicability_rationale"
thermo_reporting_rows <- lapply(
  seq_len(nrow(thermo_applicability)),
  function(position) {
    unit_id <- thermo_applicability$appraisal_unit_id[[position]]
    unit_items <- thermo_items[
      thermo_items$appraisal_unit_id == unit_id,
      ,
      drop = FALSE
    ]
    unit_domains <- thermo_domains[
      thermo_domains$appraisal_unit_id == unit_id,
      ,
      drop = FALSE
    ]
    responses <- normalize_value(unit_items$response)
    applicable_responses <- responses[
      responses != "not_applicable" & nzchar(responses)
    ]
    overall <- unit_domains$domain_judgment[
      unit_domains$domain == "overall"
    ]
    if (length(overall) == 0L) overall <- ""
    data.frame(
      appraisal_unit_id = unit_id,
      record_id = thermo_applicability$record_id[[position]],
      instrument_applicability =
        thermo_applicability$instrument_applicability[[position]],
      items_total = length(responses),
      items_applicable = length(applicable_responses),
      response_yes = sum(responses == "yes"),
      response_partial = sum(responses == "partial"),
      response_no = sum(responses == "no"),
      response_not_applicable = sum(responses == "not_applicable"),
      fully_reported_fraction = if (length(applicable_responses) == 0L) {
        NA_real_
      } else {
        sum(applicable_responses == "yes") / length(applicable_responses)
      },
      at_least_partial_fraction = if (
        length(applicable_responses) == 0L
      ) {
        NA_real_
      } else {
        sum(applicable_responses %in% c("yes", "partial")) /
          length(applicable_responses)
      },
      overall_reporting_judgment = overall[[1L]],
      applicability_rationale =
        thermo_applicability$applicability_rationale[[position]],
      stringsAsFactors = FALSE
    )
  }
)
thermography_reporting <- do.call(rbind, thermo_reporting_rows)
unit_context <- units[
  ,
  c(
    "appraisal_unit_id",
    "title",
    "dataset_id",
    "task_type",
    "target"
  ),
  drop = FALSE
]
thermography_reporting <- left_join_key(
  thermography_reporting,
  unit_context,
  "appraisal_unit_id"
)
thermography_reporting <- thermography_reporting[
  ,
  c(
    "appraisal_unit_id",
    "record_id",
    "title",
    "dataset_id",
    "task_type",
    "target",
    "instrument_applicability",
    "items_total",
    "items_applicable",
    "response_yes",
    "response_partial",
    "response_no",
    "response_not_applicable",
    "fully_reported_fraction",
    "at_least_partial_fraction",
    "overall_reporting_judgment",
    "applicability_rationale"
  ),
  drop = FALSE
]

model_validation_columns <- c(
  "lot_id",
  "model_task_id",
  "study_id",
  "record_id",
  "title",
  "dataset_id",
  "task_type",
  "target",
  "readiness_applicability",
  "readiness_level",
  "internal_validation_present",
  "external_validation_present",
  "temporal_validation_present",
  "multicentre_validation_present",
  "prospective_clinical_workflow_evaluation",
  "clinical_utility_demonstrated",
  "model_calibration_reported",
  "model_explainability_reported",
  "uncertainty_estimation_reported",
  "code_available",
  "data_available",
  "model_available",
  "fairness_or_subgroup_bias_evaluated",
  "regulatory_or_deployment_discussed",
  "thermography_protocol_replicable",
  "risk_of_bias_summary",
  "reporting_quality_summary",
  "readiness_limitations",
  "supporting_quote_or_location",
  "consistency_flag"
)
model_validation <- select_existing(readiness, model_validation_columns)
model_validation$readiness_level <- safe_numeric(
  model_validation$readiness_level
)

appraisal_rows <- lapply(
  seq_len(nrow(applicability)),
  function(position) {
    unit_id <- applicability$appraisal_unit_id[[position]]
    instrument <- applicability$instrument[[position]]
    unit_items <- items[
      items$appraisal_unit_id == unit_id &
        items$instrument == instrument,
      ,
      drop = FALSE
    ]
    unit_domains <- domains[
      domains$appraisal_unit_id == unit_id &
        domains$instrument == instrument,
      ,
      drop = FALSE
    ]
    response_values <- normalize_value(unit_items$response)
    judgment_values <- normalize_value(unit_domains$domain_judgment)
    data.frame(
      appraisal_unit_id = unit_id,
      record_id = applicability$record_id[[position]],
      instrument = instrument,
      instrument_applicability =
        applicability$final_decision[[position]],
      item_rows = nrow(unit_items),
      item_yes_or_low = sum(response_values %in% c("yes", "low")),
      item_partial_or_probably_yes = sum(
        response_values %in% c("partial", "probably_yes")
      ),
      item_no_or_high = sum(response_values %in% c("no", "high")),
      item_probably_no = sum(response_values == "probably_no"),
      item_unclear_or_no_information = sum(
        response_values %in% c("unclear", "no_information")
      ),
      item_not_applicable = sum(response_values == "not_applicable"),
      domain_rows = nrow(unit_domains),
      domain_low_or_complete = sum(
        judgment_values %in% c("low", "complete")
      ),
      domain_high_or_incomplete = sum(
        judgment_values %in% c("high", "incomplete")
      ),
      domain_unclear = sum(judgment_values == "unclear"),
      domain_not_applicable = sum(judgment_values == "not_applicable"),
      applicability_rationale = applicability$final_rationale[[position]],
      stringsAsFactors = FALSE
    )
  }
)
methodological_appraisal <- do.call(rbind, appraisal_rows)
methodological_appraisal <- left_join_key(
  methodological_appraisal,
  unit_context,
  "appraisal_unit_id"
)
methodological_appraisal <- methodological_appraisal[
  ,
  c(
    "appraisal_unit_id",
    "record_id",
    "title",
    "dataset_id",
    "task_type",
    "target",
    "instrument",
    "instrument_applicability",
    "item_rows",
    "item_yes_or_low",
    "item_partial_or_probably_yes",
    "item_no_or_high",
    "item_probably_no",
    "item_unclear_or_no_information",
    "item_not_applicable",
    "domain_rows",
    "domain_low_or_complete",
    "domain_high_or_incomplete",
    "domain_unclear",
    "domain_not_applicable",
    "applicability_rationale"
  ),
  drop = FALSE
]

clinical_readiness_columns <- c(
  "model_task_id",
  "record_id",
  "title",
  "dataset_id",
  "task_type",
  "target",
  "readiness_applicability",
  "applicability_basis",
  "readiness_level",
  "readiness_basis",
  "internal_validation_present",
  "external_validation_present",
  "temporal_validation_present",
  "multicentre_validation_present",
  "prospective_clinical_workflow_evaluation",
  "clinical_utility_demonstrated",
  "impact_on_decision_making_reported",
  "impact_on_patient_outcomes_reported",
  "workflow_or_cost_effectiveness_reported",
  "code_available",
  "data_available",
  "model_available",
  "fairness_or_subgroup_bias_evaluated",
  "regulatory_or_deployment_discussed",
  "thermography_protocol_replicable",
  "risk_of_bias_summary",
  "reporting_quality_summary",
  "readiness_limitations",
  "supporting_quote_or_location",
  "reviewer_notes",
  "completion_status",
  "consistency_flag"
)
clinical_readiness <- select_existing(readiness, clinical_readiness_columns)
clinical_readiness$readiness_level <- safe_numeric(
  clinical_readiness$readiness_level
)

metric_value <- function(metric) {
  rows <- prisma_counts[prisma_counts$metric == metric, , drop = FALSE]
  if (nrow(rows) != 1L) {
    stop("Missing or duplicated PRISMA metric: ", metric, call. = FALSE)
  }
  as.integer(rows$value[[1L]])
}
exact_duplicates <- metric_value("duplicates_removed_exact")
primary_imported <- metric_value("records_imported_primary")
screening_ready <- metric_value("records_screening_ready_primary")
late_title_duplicates <- sum(
  title_duplicates$action == "absorbed_title_duplicate"
)
records_excluded <- sum(
  title_abstract$final_title_abstract_decision == "exclude"
)
reports_sought <- sum(
  title_abstract$final_title_abstract_decision == "full_text"
)
reports_not_retrieved <- nrow(unavailable)
reports_assessed <- nrow(eligibility)
reports_excluded <- nrow(exclusions)
studies_included <- sum(
  eligibility$full_text_decision == "include_for_extraction"
)

prisma_flow <- data.frame(
  display_order = 1:10,
  node_id = c(
    "records_identified_primary",
    "exact_duplicates_removed_before_screening",
    "records_screened_initially",
    "records_excluded_title_abstract",
    "title_duplicates_removed_after_initial_screening",
    "reports_sought_for_retrieval",
    "reports_not_retrieved",
    "reports_assessed_for_eligibility",
    "reports_excluded_full_text",
    "studies_included"
  ),
  stage = c(
    "identification",
    "before_screening",
    "screening",
    "screening",
    "post_screening_cleanup",
    "retrieval",
    "retrieval",
    "eligibility",
    "eligibility",
    "included"
  ),
  label = c(
    "Registros primários identificados/importados",
    "Duplicatas exatas removidas antes da triagem",
    "Registros inicialmente triados",
    "Registros excluídos por título/resumo",
    "Duplicatas exatas de título removidas após a triagem inicial",
    "Relatórios buscados para recuperação",
    "Relatórios não recuperados",
    "Relatórios avaliados em texto completo",
    "Relatórios excluídos após texto completo",
    "Estudos incluídos na revisão"
  ),
  n = c(
    primary_imported,
    exact_duplicates,
    screening_ready,
    records_excluded,
    late_title_duplicates,
    reports_sought,
    reports_not_retrieved,
    reports_assessed,
    reports_excluded,
    studies_included
  ),
  source = c(
    project_relative_path(paths$prisma_counts),
    project_relative_path(paths$prisma_counts),
    project_relative_path(paths$prisma_counts),
    project_relative_path(paths$title_abstract_final),
    project_relative_path(paths$title_duplicate_resolution),
    project_relative_path(paths$title_abstract_final),
    project_relative_path(paths$unavailable),
    project_relative_path(paths$eligibility_final),
    project_relative_path(paths$full_text_exclusions),
    project_relative_path(paths$eligibility_final)
  ),
  reporting_note = c(
    "Fluxo primário; registros de ensaios e protocolos foram mantidos como fluxos suplementares.",
    "Deduplicação exata determinística do CP06.",
    "Conjunto inicialmente disponibilizado aos dois revisores.",
    "Contagem final após resolução e consolidação por título.",
    "Duplicatas detectadas e documentadas após o trabalho inicial dos revisores, antes da fila final de textos completos.",
    "Aritmética: 1609 - 1452 - 115 = 42.",
    "Inclui quatro buscas sem texto recuperado e um PDF incorreto/não verificável reclassificado como indisponível.",
    "Aritmética: 42 - 5 = 37.",
    "Três sem modelo de IA elegível e um com desfecho incorreto.",
    "Todos os 33 estudos incluídos têm extração CP13."
  ),
  stringsAsFactors = FALSE
)

year_data <- study_characteristics[
  nzchar(normalize_value(study_characteristics$year)),
  c("year"),
  drop = FALSE
]
year_data <- count_by(year_data, "year", "studies")
year_data$year <- as.integer(year_data$year)

study_pain <- study_characteristics[
  ,
  c("record_id", "pain_target_category"),
  drop = FALSE
]
task_pain_source <- readiness[
  readiness$readiness_applicability == "yes",
  c("model_task_id", "record_id", "task_type"),
  drop = FALSE
]
task_pain_source <- left_join_key(
  task_pain_source,
  study_pain,
  "record_id"
)
task_pain_data <- count_by(
  task_pain_source,
  c("pain_target_category", "task_type"),
  "model_task_units"
)

thermography_response_data <- count_by(
  thermo_items,
  "response",
  "item_rows"
)

appraisal_judgment_data <- count_by(
  domains,
  c("instrument", "domain_judgment"),
  "domain_rows"
)

readiness_data <- data.frame(
  category = c(
    "Não classificada",
    "Nível 0",
    "Nível 1",
    "Nível 2",
    "Nível 3",
    "Nível 4"
  ),
  display_order = 0:5,
  model_task_units = c(
    sum(readiness$readiness_applicability != "yes"),
    sum(
      readiness$readiness_applicability == "yes" &
        safe_numeric(readiness$readiness_level) == 0,
      na.rm = TRUE
    ),
    sum(
      readiness$readiness_applicability == "yes" &
        safe_numeric(readiness$readiness_level) == 1,
      na.rm = TRUE
    ),
    sum(
      readiness$readiness_applicability == "yes" &
        safe_numeric(readiness$readiness_level) == 2,
      na.rm = TRUE
    ),
    sum(
      readiness$readiness_applicability == "yes" &
        safe_numeric(readiness$readiness_level) == 3,
      na.rm = TRUE
    ),
    sum(
      readiness$readiness_applicability == "yes" &
        safe_numeric(readiness$readiness_level) == 4,
      na.rm = TRUE
    )
  ),
  stringsAsFactors = FALSE
)

summary_metrics <- data.frame(
  metric = c(
    "included_studies",
    "appraisal_units",
    "clinical_readiness_applicable_units",
    "thermography_reporting_applicable_units",
    "cp13_thermal_acquisition_rows",
    "cp13_dataset_rows",
    "cp13_model_rows",
    "cp13_metric_rows",
    "cp14_item_rows",
    "cp14_domain_rows",
    "readiness_level_0",
    "readiness_level_1",
    "readiness_level_2",
    "readiness_level_3",
    "readiness_level_4",
    "prisma_records_initially_screened",
    "prisma_reports_sought",
    "prisma_reports_assessed",
    "prisma_studies_included"
  ),
  value = c(
    nrow(study_characteristics),
    nrow(units),
    sum(readiness$readiness_applicability == "yes"),
    sum(thermography_reporting$instrument_applicability == "yes"),
    nrow(thermal),
    nrow(dataset),
    nrow(models),
    nrow(metrics),
    nrow(items),
    nrow(domains),
    readiness_data$model_task_units[readiness_data$category == "Nível 0"],
    readiness_data$model_task_units[readiness_data$category == "Nível 1"],
    readiness_data$model_task_units[readiness_data$category == "Nível 2"],
    readiness_data$model_task_units[readiness_data$category == "Nível 3"],
    readiness_data$model_task_units[readiness_data$category == "Nível 4"],
    screening_ready,
    reports_sought,
    reports_assessed,
    studies_included
  ),
  source_phase = c(
    rep("CP13", 1),
    "CP14",
    "CP15",
    "CP14",
    rep("CP13", 4),
    rep("CP14", 2),
    rep("CP15", 5),
    rep("PRISMA", 4)
  ),
  stringsAsFactors = FALSE
)

output_paths <- list(
  study_characteristics = file.path(
    tables_dir,
    "CP16_TABLE_1_STUDY_CHARACTERISTICS.csv"
  ),
  thermography_reporting = file.path(
    tables_dir,
    "CP16_TABLE_2_THERMOGRAPHY_REPORTING.csv"
  ),
  model_validation = file.path(
    tables_dir,
    "CP16_TABLE_3_MODEL_VALIDATION.csv"
  ),
  methodological_appraisal = file.path(
    tables_dir,
    "CP16_TABLE_4_METHODOLOGICAL_APPRAISAL.csv"
  ),
  clinical_readiness = file.path(
    tables_dir,
    "CP16_TABLE_5_CLINICAL_READINESS.csv"
  ),
  prisma_flow = file.path(
    tables_dir,
    "CP16_PRISMA_FLOW_VERIFIED.csv"
  ),
  year_data = file.path(
    figure_data_dir,
    "CP16_FIG_DATA_PUBLICATION_YEAR.csv"
  ),
  task_pain_data = file.path(
    figure_data_dir,
    "CP16_FIG_DATA_PAIN_DOMAIN_BY_TASK.csv"
  ),
  thermography_response_data = file.path(
    figure_data_dir,
    "CP16_FIG_DATA_THERMOGRAPHY_REPORTING.csv"
  ),
  appraisal_judgment_data = file.path(
    figure_data_dir,
    "CP16_FIG_DATA_APPRAISAL_JUDGMENTS.csv"
  ),
  readiness_data = file.path(
    figure_data_dir,
    "CP16_FIG_DATA_CLINICAL_READINESS.csv"
  ),
  summary_metrics = file.path(
    cp16_dir,
    "CP16_REPORTING_SUMMARY.csv"
  )
)

study_characteristics <- write_table(
  study_characteristics,
  output_paths$study_characteristics,
  "record_id"
)
thermography_reporting <- write_table(
  thermography_reporting,
  output_paths$thermography_reporting,
  "appraisal_unit_id"
)
model_validation <- write_table(
  model_validation,
  output_paths$model_validation,
  c("lot_id", "model_task_id")
)
methodological_appraisal <- write_table(
  methodological_appraisal,
  output_paths$methodological_appraisal,
  c("appraisal_unit_id", "instrument")
)
clinical_readiness <- write_table(
  clinical_readiness,
  output_paths$clinical_readiness,
  "model_task_id"
)
prisma_flow <- write_table(
  prisma_flow,
  output_paths$prisma_flow,
  "display_order"
)
year_data <- write_table(year_data, output_paths$year_data, "year")
task_pain_data <- write_table(
  task_pain_data,
  output_paths$task_pain_data,
  c("pain_target_category", "task_type")
)
thermography_response_data <- write_table(
  thermography_response_data,
  output_paths$thermography_response_data,
  "response"
)
appraisal_judgment_data <- write_table(
  appraisal_judgment_data,
  output_paths$appraisal_judgment_data,
  c("instrument", "domain_judgment")
)
readiness_data <- write_table(
  readiness_data,
  output_paths$readiness_data,
  "display_order"
)
summary_metrics <- write_table(
  summary_metrics,
  output_paths$summary_metrics,
  "metric"
)

source_manifest <- data.frame(
  source_phase = c(
    "CP13",
    "CP14",
    "CP15",
    "PRISMA",
    "Screening",
    "Full text"
  ),
  source_file = project_relative_path(c(
    paths$cp13,
    paths$cp14,
    paths$cp15,
    paths$prisma_counts,
    paths$title_abstract_final,
    paths$eligibility_final
  )),
  sha256 = vapply(
    c(
      paths$cp13,
      paths$cp14,
      paths$cp15,
      paths$prisma_counts,
      paths$title_abstract_final,
      paths$eligibility_final
    ),
    sha256_file,
    character(1L)
  ),
  role = c(
    "Características dos estudos e extração normalizada",
    "Avaliação metodológica final",
    "Prontidão clínica final aprovada",
    "Identificação e deduplicação inicial",
    "Triagem final consolidada e fila de textos completos",
    "Elegibilidade final em texto completo"
  ),
  status = c(
    "APPROVED",
    "APPROVED",
    "APPROVED",
    "FROZEN",
    "FROZEN",
    "FROZEN"
  ),
  stringsAsFactors = FALSE
)
source_manifest_path <- file.path(
  cp16_dir,
  "CP16_SOURCE_MANIFEST.csv"
)
source_manifest <- write_table(
  source_manifest,
  source_manifest_path,
  "source_phase"
)

data_checks <- list()
add_data_check <- function(check, observed, expected, pass, note = "") {
  data_checks[[length(data_checks) + 1L]] <<- data.frame(
    check = check,
    observed = paste(observed, collapse = "; "),
    expected = paste(expected, collapse = "; "),
    status = if (isTRUE(pass)) "PASS" else "FAIL",
    note = note,
    stringsAsFactors = FALSE
  )
}

approvals <- read_frozen_csv(checkpoint_approvals_path())
cp15_approval <- approvals[
  approvals$checkpoint_id == "CP15",
  ,
  drop = FALSE
]
add_data_check(
  "cp15_approved",
  if (nrow(cp15_approval) == 1L) cp15_approval$approved[[1L]] else "missing",
  "yes",
  nrow(cp15_approval) == 1L &&
    tolower(trimws(cp15_approval$approved[[1L]])) == "yes"
)
add_data_check(
  "study_characteristics_rows",
  nrow(study_characteristics),
  33,
  nrow(study_characteristics) == 33L
)
add_data_check(
  "study_characteristics_unique_records",
  length(unique(study_characteristics$record_id)),
  33,
  length(unique(study_characteristics$record_id)) == 33L &&
    !anyDuplicated(study_characteristics$record_id)
)
add_data_check(
  "cp13_cp14_cp15_record_alignment",
  paste(
    length(unique(units$record_id)),
    length(unique(readiness$record_id)),
    sep = "/"
  ),
  "33/33",
  setequal(study_characteristics$record_id, units$record_id) &&
    setequal(study_characteristics$record_id, readiness$record_id)
)
add_data_check(
  "appraisal_units",
  nrow(units),
  50,
  nrow(units) == 50L && !anyDuplicated(units$appraisal_unit_id)
)
add_data_check(
  "thermography_reporting_units",
  nrow(thermography_reporting),
  50,
  nrow(thermography_reporting) == 50L
)
add_data_check(
  "thermography_reporting_applicable_units",
  sum(thermography_reporting$instrument_applicability == "yes"),
  47,
  sum(thermography_reporting$instrument_applicability == "yes") == 47L
)
add_data_check(
  "thermography_reporting_item_reconciliation",
  sum(thermography_reporting$items_total),
  nrow(thermo_items),
  sum(thermography_reporting$items_total) == nrow(thermo_items) &&
    nrow(thermo_items) == 1128L
)
add_data_check(
  "model_validation_rows",
  nrow(model_validation),
  50,
  nrow(model_validation) == 50L
)
add_data_check(
  "methodological_appraisal_rows",
  nrow(methodological_appraisal),
  310,
  nrow(methodological_appraisal) == 310L
)
add_data_check(
  "methodological_item_reconciliation",
  sum(methodological_appraisal$item_rows),
  nrow(items),
  sum(methodological_appraisal$item_rows) == nrow(items) &&
    nrow(items) == 7106L
)
add_data_check(
  "methodological_domain_reconciliation",
  sum(methodological_appraisal$domain_rows),
  nrow(domains),
  sum(methodological_appraisal$domain_rows) == nrow(domains) &&
    nrow(domains) == 893L
)
add_data_check(
  "clinical_readiness_rows",
  nrow(clinical_readiness),
  50,
  nrow(clinical_readiness) == 50L
)
add_data_check(
  "clinical_readiness_complete",
  sum(clinical_readiness$completion_status == "complete"),
  50,
  all(clinical_readiness$completion_status == "complete")
)
add_data_check(
  "clinical_readiness_consistent",
  paste(sort(unique(clinical_readiness$consistency_flag)), collapse = "; "),
  "NOT_CLASSIFIED; OK",
  setequal(
    unique(clinical_readiness$consistency_flag),
    c("NOT_CLASSIFIED", "OK")
  )
)
add_data_check(
  "readiness_distribution",
  paste(readiness_data$model_task_units, collapse = "; "),
  "12; 11; 24; 3; 0; 0",
  identical(
    as.integer(readiness_data$model_task_units),
    c(12L, 11L, 24L, 3L, 0L, 0L)
  )
)
add_data_check(
  "prisma_initial_arithmetic",
  primary_imported - exact_duplicates,
  screening_ready,
  primary_imported - exact_duplicates == screening_ready
)
add_data_check(
  "prisma_screening_arithmetic",
  screening_ready - records_excluded - late_title_duplicates,
  reports_sought,
  screening_ready - records_excluded - late_title_duplicates ==
    reports_sought
)
add_data_check(
  "prisma_retrieval_arithmetic",
  reports_sought - reports_not_retrieved,
  reports_assessed,
  reports_sought - reports_not_retrieved == reports_assessed
)
add_data_check(
  "prisma_eligibility_arithmetic",
  reports_assessed - reports_excluded,
  studies_included,
  reports_assessed - reports_excluded == studies_included &&
    studies_included == 33L
)
add_data_check(
  "late_title_duplicates_documented",
  late_title_duplicates,
  115,
  late_title_duplicates == 115L,
  "Detected after initial reviewer work and before the final full-text queue."
)
add_data_check(
  "source_manifest_hashes",
  nrow(source_manifest),
  6,
  nrow(source_manifest) == 6L &&
    all(nchar(source_manifest$sha256) == 64L)
)

provider_product_names <- paste0(
  c("chat", "cl", "anthro", "open", "co"),
  c("gpt", "aude", "pic", "ai", "dex")
)
provider_pattern <- paste0(
  "\\b(",
  paste(provider_product_names, collapse = "|"),
  ")\\b"
)
public_objects <- list(
  study_characteristics,
  thermography_reporting,
  model_validation,
  methodological_appraisal,
  clinical_readiness,
  prisma_flow,
  source_manifest
)
public_text <- paste(
  unlist(
    lapply(public_objects, function(data) {
      unlist(data, use.names = FALSE)
    }),
    use.names = FALSE
  ),
  collapse = "\n"
)
provider_locations <- gregexpr(
  provider_pattern,
  public_text,
  ignore.case = TRUE,
  perl = TRUE
)[[1L]]
provider_hits <- if (
  length(provider_locations) == 1L &&
    provider_locations[[1L]] == -1L
) {
  0L
} else {
  length(provider_locations)
}
add_data_check(
  "provider_names_in_public_cp16_data",
  provider_hits,
  0,
  provider_hits == 0L
)

data_qa <- do.call(rbind, data_checks)
data_qa_path <- file.path(qa_dir, "CP16_R_DATA_QA.csv")
stable_write_csv(data_qa, data_qa_path, columns = names(data_qa))
failed_data_checks <- data_qa[data_qa$status == "FAIL", , drop = FALSE]
if (nrow(failed_data_checks) > 0L) {
  stop(
    "CP16 data QA failed: ",
    paste(failed_data_checks$check, collapse = ", "),
    call. = FALSE
  )
}

if (!file.exists(paths$workbook)) {
  message(
    "CP16 reporting tables generated and ",
    nrow(data_qa),
    "/",
    nrow(data_qa),
    " data checks passed. Build the workbook, then rerun this script."
  )
  quit(save = "no", status = 0L)
}

final_checks <- list()
add_final_check <- function(check, observed, expected, pass, note = "") {
  final_checks[[length(final_checks) + 1L]] <<- data.frame(
    check = check,
    observed = paste(observed, collapse = "; "),
    expected = paste(expected, collapse = "; "),
    status = if (isTRUE(pass)) "PASS" else "FAIL",
    note = note,
    stringsAsFactors = FALSE
  )
}

add_final_check(
  "data_qa",
  paste0(sum(data_qa$status == "PASS"), "/", nrow(data_qa)),
  paste0(nrow(data_qa), "/", nrow(data_qa)),
  all(data_qa$status == "PASS")
)
expected_sheets <- c(
  "summary",
  "study_characteristics",
  "thermography_reporting",
  "model_validation",
  "methodological_appraisal",
  "clinical_readiness",
  "prisma_flow",
  "fig_publication_year",
  "fig_task_pain",
  "fig_thermography",
  "fig_appraisal",
  "fig_readiness",
  "source_manifest",
  "qa_checks"
)
observed_sheets <- readxl::excel_sheets(paths$workbook)
add_final_check(
  "workbook_sheets",
  paste(observed_sheets, collapse = "; "),
  paste(expected_sheets, collapse = "; "),
  identical(observed_sheets, expected_sheets)
)

workbook_sheet_rows <- c(
  study_characteristics = 33L,
  thermography_reporting = 50L,
  model_validation = 50L,
  methodological_appraisal = 310L,
  clinical_readiness = 50L,
  prisma_flow = 10L,
  fig_publication_year = nrow(year_data),
  fig_task_pain = nrow(task_pain_data),
  fig_thermography = nrow(thermography_response_data),
  fig_appraisal = nrow(appraisal_judgment_data),
  fig_readiness = nrow(readiness_data),
  source_manifest = 6L,
  qa_checks = nrow(data_qa)
)
observed_workbook_rows <- vapply(
  names(workbook_sheet_rows),
  function(sheet) {
    nrow(
      suppressMessages(
        readxl::read_excel(paths$workbook, sheet = sheet)
      )
    )
  },
  integer(1L)
)
add_final_check(
  "workbook_table_rows",
  paste(observed_workbook_rows, collapse = "; "),
  paste(workbook_sheet_rows, collapse = "; "),
  identical(
    unname(observed_workbook_rows),
    unname(as.integer(workbook_sheet_rows))
  )
)

numeric_column_spec <- list(
  study_characteristics = c(
    "year",
    "centers_n",
    "participants_n_total",
    "participants_n_cases",
    "participants_n_controls"
  ),
  thermography_reporting = c(
    "items_total",
    "items_applicable",
    "response_yes",
    "response_partial",
    "response_no",
    "response_not_applicable",
    "fully_reported_fraction",
    "at_least_partial_fraction"
  ),
  model_validation = "readiness_level",
  methodological_appraisal = c(
    "item_rows",
    "item_yes_or_low",
    "item_partial_or_probably_yes",
    "item_no_or_high",
    "item_probably_no",
    "item_unclear_or_no_information",
    "item_not_applicable",
    "domain_rows",
    "domain_low_or_complete",
    "domain_high_or_incomplete",
    "domain_unclear",
    "domain_not_applicable"
  ),
  clinical_readiness = "readiness_level",
  prisma_flow = c("display_order", "n"),
  fig_publication_year = c("year", "studies"),
  fig_task_pain = "model_task_units",
  fig_thermography = "item_rows",
  fig_appraisal = "domain_rows",
  fig_readiness = c("display_order", "model_task_units")
)
numeric_type_status <- unlist(
  lapply(
    names(numeric_column_spec),
    function(sheet) {
      workbook_data <- suppressMessages(
        readxl::read_excel(paths$workbook, sheet = sheet)
      )
      columns <- numeric_column_spec[[sheet]]
      status <- vapply(
        columns,
        function(column) {
          column %in% names(workbook_data) &&
            is.numeric(workbook_data[[column]])
        },
        logical(1L)
      )
      names(status) <- paste(sheet, columns, sep = ".")
      status
    }
  ),
  use.names = TRUE
)
add_final_check(
  "workbook_numeric_cell_types",
  paste0(sum(numeric_type_status), "/", length(numeric_type_status)),
  paste0(length(numeric_type_status), "/", length(numeric_type_status)),
  all(numeric_type_status),
  if (all(numeric_type_status)) {
    ""
  } else {
    paste(names(numeric_type_status)[!numeric_type_status], collapse = "; ")
  }
)

summary_prisma_values <- suppressMessages(
  readxl::read_excel(
    paths$workbook,
    sheet = "summary",
    range = "B14:B16",
    col_names = FALSE
  )
)[[1L]]
expected_summary_prisma <- c(
  screening_ready,
  reports_sought,
  reports_assessed
)
add_final_check(
  "workbook_summary_prisma_values",
  summary_prisma_values,
  expected_summary_prisma,
  is.numeric(summary_prisma_values) &&
    identical(
      as.integer(summary_prisma_values),
      as.integer(expected_summary_prisma)
    )
)

readiness_helper_values <- suppressMessages(
  readxl::read_excel(
    paths$workbook,
    sheet = "summary",
    range = "L4:L9",
    col_names = FALSE
  )
)[[1L]]
year_helper_values <- suppressMessages(
  readxl::read_excel(
    paths$workbook,
    sheet = "summary",
    range = "L13:L26",
    col_names = FALSE
  )
)[[1L]]
chart_helper_values <- c(readiness_helper_values, year_helper_values)
expected_chart_helper_values <- c(
  readiness_data$model_task_units,
  year_data$studies
)
add_final_check(
  "workbook_chart_helper_values",
  chart_helper_values,
  expected_chart_helper_values,
  is.numeric(chart_helper_values) &&
    identical(
      as.integer(chart_helper_values),
      as.integer(expected_chart_helper_values)
    )
)

zip_listing <- unzip(paths$workbook, list = TRUE)
worksheet_xml <- grep(
  "^xl/worksheets/sheet[0-9]+[.]xml$",
  zip_listing$Name,
  value = TRUE
)
chart_xml <- grep(
  "^xl/(charts|drawings/charts)/chart[0-9]+[.]xml$",
  zip_listing$Name,
  value = TRUE
)
add_final_check(
  "workbook_xml_sheet_count",
  length(worksheet_xml),
  14,
  length(worksheet_xml) == 14L
)
add_final_check(
  "workbook_chart_count",
  length(chart_xml),
  2,
  length(chart_xml) == 2L
)

workbook_extract <- tempfile("cp16_xlsx_")
dir.create(workbook_extract, recursive = TRUE)
on.exit(unlink(workbook_extract, recursive = TRUE, force = TRUE), add = TRUE)
unzip(paths$workbook, exdir = workbook_extract)
chart_text <- paste(
  unlist(
    lapply(
      file.path(workbook_extract, chart_xml),
      readLines,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    use.names = FALSE
  ),
  collapse = ""
)
expected_chart_references <- c(
  "'summary'!$K$4:$K$9",
  "'summary'!$L$4:$L$9",
  "'summary'!$K$13:$K$26",
  "'summary'!$L$13:$L$26"
)
chart_reference_status <- vapply(
  expected_chart_references,
  function(reference) grepl(reference, chart_text, fixed = TRUE),
  logical(1L)
)
add_final_check(
  "workbook_chart_series_references",
  paste0(sum(chart_reference_status), "/", length(chart_reference_status)),
  paste0(length(chart_reference_status), "/", length(chart_reference_status)),
  all(chart_reference_status),
  if (all(chart_reference_status)) {
    ""
  } else {
    paste(
      expected_chart_references[!chart_reference_status],
      collapse = "; "
    )
  }
)

worksheet_text <- paste(
  unlist(
    lapply(
      file.path(workbook_extract, worksheet_xml),
      readLines,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    use.names = FALSE
  ),
  collapse = ""
)
formula_error_cells <- gregexpr('t="e"', worksheet_text, fixed = TRUE)[[1L]]
formula_error_count <- if (
  length(formula_error_cells) == 1L &&
    formula_error_cells[[1L]] == -1L
) {
  0L
} else {
  length(formula_error_cells)
}
add_final_check(
  "workbook_formula_errors",
  formula_error_count,
  0,
  formula_error_count == 0L
)

workbook_hash <- sha256_file(paths$workbook)
add_final_check(
  "workbook_sha256",
  nchar(workbook_hash),
  64,
  nchar(workbook_hash) == 64L
)

final_validation <- do.call(rbind, final_checks)
final_validation_path <- file.path(
  final_dir,
  "CP16_R_FINAL_VALIDATION.csv"
)
stable_write_csv(
  final_validation,
  final_validation_path,
  columns = names(final_validation)
)
failed_final_checks <- final_validation[
  final_validation$status == "FAIL",
  ,
  drop = FALSE
]
if (nrow(failed_final_checks) > 0L) {
  stop(
    "CP16 final validation failed: ",
    paste(failed_final_checks$check, collapse = ", "),
    call. = FALSE
  )
}

verification <- list(
  checkpoint = "CP16",
  status = "TECHNICALLY_CLOSED_AWAITING_HUMAN_APPROVAL",
  workbook = project_relative_path(paths$workbook),
  workbook_sha256 = workbook_hash,
  counts = list(
    studies = 33L,
    appraisal_units = 50L,
    thermography_reporting_applicable_units = 47L,
    clinical_readiness_applicable_units = 38L,
    methodological_appraisal_rows = 310L,
    prisma_records_initially_screened = screening_ready,
    prisma_reports_sought = reports_sought,
    prisma_reports_assessed = reports_assessed,
    prisma_studies_included = studies_included,
    data_checks = nrow(data_qa),
    final_checks = nrow(final_validation),
    failures = 0L
  ),
  reporting_notes = list(
    late_title_duplicates = late_title_duplicates,
    late_title_duplicate_note = paste(
      "Exact title duplicates were documented after initial reviewer work and",
      "before the final full-text queue; the PRISMA table preserves this as a",
      "separate post-screening cleanup node."
    ),
    meta_analysis = "Not assessed in CP16; quantitative-synthesis gating belongs to CP17."
  )
)
verification_path <- file.path(
  final_dir,
  "CP16_REPORTING_VERIFICATION.json"
)
writeLines(
  jsonlite::toJSON(
    verification,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ),
  verification_path,
  useBytes = TRUE
)

report <- c(
  "# CP16 — relatório de verificação de relato",
  "",
  "## Resultado",
  "",
  paste(
    "**FECHAMENTO TÉCNICO CONCLUÍDO; APROVAÇÃO HUMANA PENDENTE.**",
    "As tabelas de relato foram derivadas deterministicamente das fontes",
    "aprovadas de CP13, CP14 e CP15, sem reabrir julgamentos científicos."
  ),
  "",
  "## Produtos verificados",
  "",
  "- Características dos 33 estudos.",
  "- Relato termográfico para 50 unidades; 47 com instrumento aplicável.",
  "- Modelos e estágio de validação para 50 unidades.",
  "- Síntese de 310 decisões instrumento–unidade da avaliação metodológica.",
  "- Prontidão clínica de 50 unidades, sendo 38 aplicáveis.",
  "- Fluxo PRISMA reconciliado até 33 estudos incluídos.",
  "- Cinco bases de dados para figuras reproduzíveis.",
  "- Tipos numéricos, totais da aba-resumo e fontes dos dois gráficos verificados no XLSX.",
  "",
  "## Nota PRISMA",
  "",
  paste(
    "O fluxo mantém 1.609 registros inicialmente triados, 1.452 excluídos por",
    "título/resumo e 115 duplicatas exatas de título identificadas após o",
    "trabalho inicial dos revisores e antes da fila final de textos completos.",
    "A aritmética preservada é 1.609 − 1.452 − 115 = 42 relatórios buscados."
  ),
  "",
  "## Limites desta fase",
  "",
  paste(
    "O CP16 verifica relato e coerência tabular. Não decide homogeneidade para",
    "meta-análise, não combina métricas incompatíveis e não altera as",
    "classificações aprovadas de risco de viés ou prontidão clínica."
  ),
  ""
)
report_path <- file.path(final_dir, "CP16_REPORTING_REPORT.md")
writeLines(report, report_path, useBytes = TRUE)

checkpoint_path <- write_checkpoint(
  id = "CP16",
  name = "reporting_verification",
  what_ran = paste(
    "Generated and reconciled the reporting tables and figure-data layers from",
    "the approved CP13 extraction, CP14 methodological appraisal and CP15",
    "clinical-readiness outputs. The checkpoint verifies reporting structure",
    "only and does not reopen scientific judgments or perform meta-analysis.",
    "Numeric storage, summary totals and chart-source references were also",
    "validated after correction of the spreadsheet export layer."
  ),
  numbers = c(
    included_studies = 33L,
    appraisal_units = 50L,
    thermography_reporting_applicable_units = 47L,
    clinical_readiness_applicable_units = 38L,
    methodological_appraisal_rows = 310L,
    prisma_records_initially_screened = screening_ready,
    prisma_title_abstract_exclusions = records_excluded,
    prisma_late_title_duplicates = late_title_duplicates,
    prisma_reports_sought = reports_sought,
    prisma_reports_not_retrieved = reports_not_retrieved,
    prisma_reports_assessed = reports_assessed,
    prisma_full_text_exclusions = reports_excluded,
    prisma_studies_included = studies_included,
    data_checks = nrow(data_qa),
    final_checks = nrow(final_validation),
    validation_failures = 0L
  ),
  review_items = c(
    "Confirm that Table 1 correctly represents the 33 included studies and retains CP13 uncertainty/status fields.",
    "Confirm that thermography reporting uses item counts and transparent fractions without inventing a weighted quality score.",
    "Confirm that methodological-appraisal and readiness tables reproduce the approved CP14 and CP15 decisions.",
    "Confirm the PRISMA presentation of 115 late exact-title duplicates as a documented post-screening cleanup step.",
    "Confirm that both summary charts display nonzero values and match their source-data sheets.",
    "Approve CP16 before assessing homogeneous subsets and narrative/quantitative synthesis in CP17."
  ),
  outputs = c(
    unlist(output_paths, use.names = FALSE),
    source_manifest_path,
    data_qa_path,
    paths$workbook,
    final_validation_path,
    verification_path,
    report_path
  ),
  warnings = c(
    "The PRISMA flow requires a footnote because 115 exact-title duplicates were detected after initial reviewer work and before the final full-text queue.",
    "Four CP13 study-level rows retain the accepted needs_query status; the reporting tables preserve these uncertainty fields rather than silently normalizing them.",
    "No unit reached readiness level 3 or 4; do not imply prospective workflow evaluation or demonstrated clinical utility in narrative reporting."
  ),
  gate_question = paste(
    "Are the CP16 reporting tables, figure data and PRISMA reconciliation",
    "accurate enough to serve as the locked reporting source for CP17 synthesis",
    "and reproducible packaging?"
  )
)

message(
  "CP16 reporting verification complete: ",
  nrow(data_qa),
  "/",
  nrow(data_qa),
  " data checks and ",
  nrow(final_validation),
  "/",
  nrow(final_validation),
  " final checks passed."
)
message("Workbook: ", project_relative_path(paths$workbook))
message("Checkpoint: ", project_relative_path(checkpoint_path))
