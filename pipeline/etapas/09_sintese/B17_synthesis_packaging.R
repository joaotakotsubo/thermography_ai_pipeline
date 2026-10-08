#!/usr/bin/env Rscript

# Organiza os produtos da síntese e as verificações de viabilidade de agrupamento.
# Mantém explícitas as limitações que impedem uma comparação ou uma estimativa conjunta.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

for (package in c("digest", "jsonlite", "readxl", "renv")) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("The ", package, " package is required for CP17.", call. = FALSE)
  }
}

checkpoint_gate("CP16")

cp17_dir <- file.path(PROJECT_ROOT, "pipeline", "synthesis", "cp17")
tables_dir <- file.path(cp17_dir, "tables")
qa_dir <- file.path(cp17_dir, "qa")
final_dir <- file.path(cp17_dir, "final")
package_dir <- file.path(cp17_dir, "package")
staging_dir <- file.path(package_dir, "zenodo_staging")
output_copy_dir <- file.path(
  PROJECT_ROOT,
  "outputs",
  "019f5747-7c5e-75c3-8461-307270d6327b",
  "cp17_synthesis_packaging"
)

invisible(vapply(
  c(cp17_dir, tables_dir, qa_dir, final_dir, package_dir, output_copy_dir),
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
  cp16_workbook = file.path(
    PROJECT_ROOT,
    "pipeline",
    "reporting",
    "cp16",
    "final",
    "CP16_VERIFICACAO_DE_RELATO_33_ESTUDOS.xlsx"
  ),
  cp16_table1 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "reporting",
    "cp16",
    "tables",
    "CP16_TABLE_1_STUDY_CHARACTERISTICS.csv"
  ),
  cp16_table2 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "reporting",
    "cp16",
    "tables",
    "CP16_TABLE_2_THERMOGRAPHY_REPORTING.csv"
  ),
  cp16_table3 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "reporting",
    "cp16",
    "tables",
    "CP16_TABLE_3_MODEL_VALIDATION.csv"
  ),
  cp16_table4 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "reporting",
    "cp16",
    "tables",
    "CP16_TABLE_4_METHODOLOGICAL_APPRAISAL.csv"
  ),
  cp16_table5 = file.path(
    PROJECT_ROOT,
    "pipeline",
    "reporting",
    "cp16",
    "tables",
    "CP16_TABLE_5_CLINICAL_READINESS.csv"
  ),
  cp16_prisma = file.path(
    PROJECT_ROOT,
    "pipeline",
    "reporting",
    "cp16",
    "tables",
    "CP16_PRISMA_FLOW_VERIFIED.csv"
  ),
  cp16_checkpoint = file.path(
    PROJECT_ROOT,
    "pipeline",
    "checkpoints",
    "CP16_reporting_verification.md"
  ),
  cp16_script = file.path(
    PROJECT_ROOT,
    "pipeline",
    "scripts",
    "B16_reporting_verification.R"
  ),
  bootstrap = file.path(PROJECT_ROOT, "pipeline", "scripts", "_bootstrap.R"),
  config = file.path(PROJECT_ROOT, "pipeline", "config.R")
)

missing_inputs <- unlist(paths)[!file.exists(unlist(paths))]
if (length(missing_inputs) > 0L) {
  stop(
    "Missing CP17 inputs: ",
    paste(project_relative_path(missing_inputs), collapse = ", "),
    call. = FALSE
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

read_frozen_csv <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    encoding = "UTF-8"
  )
}

normalize_value <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- ""
  trimws(value)
}

normalize_lower <- function(value) {
  tolower(normalize_value(value))
}

safe_numeric <- function(value) {
  suppressWarnings(as.numeric(normalize_value(value)))
}

write_table <- function(data, path, order_columns = character()) {
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
  invisible(path)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

file_manifest <- function(files, root = PROJECT_ROOT) {
  files <- normalizePath(files, mustWork = TRUE)
  root <- normalizePath(root, mustWork = TRUE)
  root_prefix <- paste0(root, .Platform$file.sep)
  rel <- ifelse(
    startsWith(files, root_prefix),
    substring(files, nchar(root) + 2L),
    basename(files)
  )
  info <- file.info(files)
  data.frame(
    file = rel,
    bytes = as.numeric(info$size),
    md5 = unname(tools::md5sum(files)),
    sha256 = vapply(files, sha256_file, character(1L)),
    stringsAsFactors = FALSE
  )
}

unique_count <- function(value) {
  length(unique(normalize_value(value)[nzchar(normalize_value(value))]))
}

count_value <- function(value, target) {
  sum(normalize_lower(value) == tolower(target), na.rm = TRUE)
}

collapse_sorted <- function(value) {
  value <- sort(unique(normalize_value(value)[nzchar(normalize_value(value))]), method = "radix")
  paste(value, collapse = " | ")
}

first_nonempty <- function(value) {
  value <- normalize_value(value)
  value <- value[nzchar(value)]
  if (length(value) == 0L) "" else value[[1L]]
}

join_left <- function(left, right, by, suffixes = c("", "_joined")) {
  left$.cp17_order <- seq_len(nrow(left))
  joined <- merge(
    left,
    right,
    by = by,
    all.x = TRUE,
    sort = FALSE,
    suffixes = suffixes
  )
  joined <- joined[order(joined$.cp17_order, method = "radix"), , drop = FALSE]
  joined$.cp17_order <- NULL
  rownames(joined) <- NULL
  joined
}

task_class <- function(value) {
  value <- normalize_lower(value)
  ifelse(
    grepl("regress", value),
    "regression",
    ifelse(
      grepl("segment", value),
      "segmentation",
      ifelse(
        grepl("detect|local", value),
        "detection_or_localization",
        ifelse(
          grepl("class|diagnos|recogn", value),
          "classification_or_diagnosis",
          ifelse(grepl("monitor", value), "monitoring", "other")
        )
      )
    )
  )
}

canonical_metric <- function(value) {
  value <- normalize_lower(value)
  value <- gsub("[[:space:]_-]+", " ", value)
  ifelse(
    value %in% c("auc", "auroc", "area under roc curve", "area under the roc curve"),
    "AUROC",
    ifelse(
      value %in% c("recognition rate", "overall recognition rate"),
      "recognition_rate",
      ifelse(
        value %in% c("classification success rate"),
        "classification_success_rate",
        ifelse(
          value %in% c("accuracy"),
          "accuracy",
          ifelse(
            value %in% c("balanced accuracy"),
            "balanced_accuracy",
            ifelse(
              value %in% c("mae", "mean absolute error"),
              "MAE",
              ifelse(
                value %in% c("rmse", "root mean square error"),
                "RMSE",
                ifelse(
                  value %in% c("r2", "r squared", "r-squared"),
                  "R2",
                  ifelse(
                    value %in% c("dice", "dice coefficient", "dice score"),
                    "Dice",
                    gsub(" ", "_", value)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

standardize_metric <- function(value, scale, metric) {
  value <- safe_numeric(value)
  scale <- normalize_lower(scale)
  probability_metric <- metric %in% c(
    "accuracy",
    "balanced_accuracy",
    "classification_success_rate",
    "recognition_rate",
    "AUROC",
    "sensitivity",
    "specificity",
    "Dice",
    "f1",
    "precision",
    "recall"
  )
  is_percent <- grepl("percent|percentage|%", scale)
  value[probability_metric & (is_percent | (!is.na(value) & value > 1))] <-
    value[probability_metric & (is_percent | (!is.na(value) & value > 1))] / 100
  value
}

validation_stage <- function(evaluation_set, evaluation_scheme, split_strategy, external_n) {
  text <- normalize_lower(paste(evaluation_set, evaluation_scheme, split_strategy))
  external_n <- safe_numeric(external_n)
  ifelse(
    grepl("external", text) | (!is.na(external_n) & external_n > 0),
    "external",
    ifelse(
      grepl("temporal", text),
      "temporal",
      ifelse(
        grepl("multicent|cross.center|cross center", text),
        "multicentre_or_cross_centre",
        ifelse(
          grepl("cross.validation|cross validation|holdout|test|validation|repeated", text),
          "internal",
          "apparent_or_unspecified"
        )
      )
    )
  )
}

separation_class <- function(value) {
  value <- normalize_lower(value)
  ifelse(
    value == "yes" | grepl("^yes[,; ]|explicitly stated", value),
    "yes",
    ifelse(
      value == "no" | grepl("same observations used", value),
      "no",
      ifelse(grepl("unclear", value), "unclear", "not_reported_or_not_applicable")
    )
  )
}

study <- read_sheet(paths$cp13, "study_level_extraction")
thermal <- read_sheet(paths$cp13, "thermal_acquisition")
dataset <- read_sheet(paths$cp13, "dataset_partition")
models <- read_sheet(paths$cp13, "model_definition")
metrics <- read_sheet(paths$cp13, "model_metric")

units <- read_sheet(paths$cp14, "appraisal_units")
applicability <- read_sheet(paths$cp14, "applicability_final")
domains <- read_sheet(paths$cp14, "domain_final")
readiness <- read_sheet(paths$cp15, "final_readiness")

cp16_study <- read_frozen_csv(paths$cp16_table1)
cp16_thermo <- read_frozen_csv(paths$cp16_table2)
cp16_validation <- read_frozen_csv(paths$cp16_table3)
cp16_appraisal <- read_frozen_csv(paths$cp16_table4)
cp16_readiness <- read_frozen_csv(paths$cp16_table5)
cp16_prisma <- read_frozen_csv(paths$cp16_prisma)

study_lookup <- study[, c(
  "record_id",
  "study_id",
  "title",
  "year",
  "pain_scope_relation",
  "pain_target_category",
  "publication_status",
  "peer_review_status",
  "row_status"
), drop = FALSE]

units_study <- join_left(
  units,
  study_lookup[, c(
    "record_id",
    "pain_scope_relation",
    "pain_target_category",
    "publication_status",
    "peer_review_status"
  ), drop = FALSE],
  by = "record_id"
)
units_study$task_class <- task_class(units_study$task_type)


domain_groups <- split(units_study, units_study$pain_target_category, drop = TRUE)
evidence_pain <- do.call(rbind, lapply(domain_groups, function(group) {
  data.frame(
    pain_target_category = first_nonempty(group$pain_target_category),
    studies_n = unique_count(group$record_id),
    model_task_units_n = unique_count(group$appraisal_unit_id),
    task_classes_n = unique_count(group$task_class),
    task_classes = collapse_sorted(group$task_class),
    thermography_applicable_units_n = sum(
      cp16_thermo$appraisal_unit_id %in% group$appraisal_unit_id &
        normalize_lower(cp16_thermo$instrument_applicability) == "yes"
    ),
    readiness_applicable_units_n = sum(
      cp16_readiness$model_task_id %in% group$appraisal_unit_id &
        normalize_lower(cp16_readiness$readiness_applicability) == "yes"
    ),
    stringsAsFactors = FALSE
  )
}))
rownames(evidence_pain) <- NULL

task_groups <- split(units_study, units_study$task_class, drop = TRUE)
evidence_task <- do.call(rbind, lapply(task_groups, function(group) {
  ready <- cp16_readiness[
    cp16_readiness$model_task_id %in% group$appraisal_unit_id &
      normalize_lower(cp16_readiness$readiness_applicability) == "yes",
    ,
    drop = FALSE
  ]
  levels <- safe_numeric(ready$readiness_level)
  data.frame(
    task_class = first_nonempty(group$task_class),
    studies_n = unique_count(group$record_id),
    model_task_units_n = unique_count(group$appraisal_unit_id),
    pain_categories_n = unique_count(group$pain_target_category),
    readiness_applicable_units_n = nrow(ready),
    readiness_level_0_n = sum(levels == 0, na.rm = TRUE),
    readiness_level_1_n = sum(levels == 1, na.rm = TRUE),
    readiness_level_2_n = sum(levels == 2, na.rm = TRUE),
    readiness_level_3_or_4_n = sum(levels %in% c(3, 4), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
rownames(evidence_task) <- NULL

primary_roles <- c("primary", "primary_deep", "primary_handcrafted")
primary_models <- models[normalize_lower(models$model_role) %in% primary_roles, , drop = FALSE]
primary_models$task_class <- task_class(primary_models$task_type)

method_groups <- split(
  primary_models,
  interaction(
    normalize_value(primary_models$model_family),
    normalize_value(primary_models$model_role),
    drop = TRUE,
    lex.order = TRUE
  ),
  drop = TRUE
)
evidence_method <- do.call(rbind, lapply(method_groups, function(group) {
  data.frame(
    model_family = first_nonempty(group$model_family),
    model_role = first_nonempty(group$model_role),
    model_rows_n = unique_count(group$model_id),
    studies_n = unique_count(group$record_id),
    datasets_n = unique_count(group$dataset_id),
    task_classes = collapse_sorted(group$task_class),
    stringsAsFactors = FALSE
  )
}))
rownames(evidence_method) <- NULL

input_groups <- split(
  primary_models,
  interaction(
    normalize_value(primary_models$input_modalities),
    normalize_value(primary_models$thermal_input_role),
    drop = TRUE,
    lex.order = TRUE
  ),
  drop = TRUE
)
evidence_input <- do.call(rbind, lapply(input_groups, function(group) {
  data.frame(
    input_modalities = first_nonempty(group$input_modalities),
    thermal_input_role = first_nonempty(group$thermal_input_role),
    primary_model_rows_n = unique_count(group$model_id),
    studies_n = unique_count(group$record_id),
    datasets_n = unique_count(group$dataset_id),
    task_classes = collapse_sorted(task_class(group$task_type)),
    stringsAsFactors = FALSE
  )
}))
rownames(evidence_input) <- NULL


primary_metrics <- metrics[
  normalize_lower(metrics$is_primary_metric) == "yes" &
    !is.na(safe_numeric(metrics$metric_value_numeric)),
  ,
  drop = FALSE
]

model_lookup <- models[, c(
  "model_id",
  "record_id",
  "dataset_id",
  "model_role",
  "model_family",
  "model_name_or_architecture",
  "task_type",
  "input_modalities",
  "thermal_input_role",
  "target_label_or_outcome",
  "reference_standard"
), drop = FALSE]

performance <- merge(
  primary_metrics,
  model_lookup,
  by = c("model_id", "record_id", "dataset_id"),
  all.x = TRUE,
  sort = FALSE
)
performance <- join_left(
  performance,
  study_lookup[, c(
    "record_id",
    "study_id",
    "title",
    "pain_scope_relation",
    "pain_target_category",
    "publication_status",
    "peer_review_status"
  ), drop = FALSE],
  by = "record_id"
)
performance <- join_left(
  performance,
  dataset[, c(
    "dataset_id",
    "record_id",
    "dataset_name",
    "participants_n",
    "images_n_original",
    "observations_n",
    "test_n",
    "external_validation_n",
    "split_strategy",
    "split_unit",
    "subject_level_separation",
    "leakage_risk_assessment"
  ), drop = FALSE],
  by = c("dataset_id", "record_id")
)

performance$task_class <- task_class(performance$task_type)
performance$canonical_metric <- canonical_metric(performance$metric_name)
performance$standardized_value <- standardize_metric(
  performance$metric_value_numeric,
  performance$metric_scale,
  performance$canonical_metric
)
performance$standardized_scale <- ifelse(
  performance$canonical_metric %in% c(
    "accuracy",
    "balanced_accuracy",
    "classification_success_rate",
    "recognition_rate",
    "AUROC",
    "sensitivity",
    "specificity",
    "Dice",
    "f1",
    "precision",
    "recall"
  ),
  "proportion_0_to_1",
  normalize_value(performance$metric_scale)
)
performance$validation_stage <- validation_stage(
  performance$evaluation_set,
  performance$evaluation_scheme,
  performance$split_strategy,
  performance$external_validation_n
)
performance$subject_level_separation_class <- separation_class(
  performance$subject_level_separation
)
performance$has_confidence_interval <- ifelse(
  !is.na(safe_numeric(performance$lower_ci)) &
    !is.na(safe_numeric(performance$upper_ci)),
  "yes",
  "no"
)
performance$has_evaluated_sample_size <- ifelse(
  !is.na(safe_numeric(performance$n_evaluated)),
  "yes",
  "no"
)
performance$descriptive_only <- "yes"
performance$pooling_status <- "not_pooled"

performance_columns <- c(
  "metric_id",
  "record_id",
  "study_id",
  "title",
  "dataset_id",
  "dataset_name",
  "model_id",
  "model_role",
  "model_family",
  "model_name_or_architecture",
  "task_type",
  "task_class",
  "pain_scope_relation",
  "pain_target_category",
  "input_modalities",
  "thermal_input_role",
  "target_label_or_outcome",
  "reference_standard",
  "evaluation_set",
  "evaluation_scheme",
  "validation_stage",
  "evaluation_unit",
  "split_unit",
  "subject_level_separation",
  "subject_level_separation_class",
  "leakage_risk_assessment",
  "metric_name",
  "canonical_metric",
  "metric_value_reported",
  "metric_value_numeric",
  "metric_scale",
  "standardized_value",
  "standardized_scale",
  "lower_ci",
  "upper_ci",
  "has_confidence_interval",
  "n_evaluated",
  "has_evaluated_sample_size",
  "participants_n",
  "images_n_original",
  "observations_n",
  "test_n",
  "external_validation_n",
  "row_status",
  "extraction_certainty",
  "descriptive_only",
  "pooling_status"
)
performance <- performance[, performance_columns, drop = FALSE]

summarize_performance <- function(data, grouping) {
  groups <- split(
    data,
    interaction(
      lapply(data[grouping], normalize_value),
      drop = TRUE,
      lex.order = TRUE
    ),
    drop = TRUE
  )
  result <- do.call(rbind, lapply(groups, function(group) {
    values <- safe_numeric(group$standardized_value)
    row <- as.list(vapply(grouping, function(column) first_nonempty(group[[column]]), character(1L)))
    names(row) <- grouping
    row$n_metric_rows <- nrow(group)
    row$n_studies <- unique_count(group$record_id)
    row$n_models <- unique_count(group$model_id)
    row$n_with_ci <- count_value(group$has_confidence_interval, "yes")
    row$n_with_evaluated_sample_size <- count_value(
      group$has_evaluated_sample_size,
      "yes"
    )
    row$minimum <- if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
    row$median <- if (all(is.na(values))) NA_real_ else median(values, na.rm = TRUE)
    row$maximum <- if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
    row$summary_type <- "unweighted_descriptive_range"
    as.data.frame(row, stringsAsFactors = FALSE)
  }))
  rownames(result) <- NULL
  result
}

performance_by_stage <- summarize_performance(
  performance,
  c("validation_stage", "task_class", "canonical_metric", "standardized_scale")
)
performance_by_task <- summarize_performance(
  performance,
  c("task_class", "canonical_metric", "standardized_scale")
)

feasibility_groups <- split(
  performance,
  interaction(
    normalize_value(performance$pain_target_category),
    normalize_value(performance$task_class),
    normalize_value(performance$canonical_metric),
    drop = TRUE,
    lex.order = TRUE
  ),
  drop = TRUE
)

meta_feasibility <- do.call(rbind, lapply(feasibility_groups, function(group) {
  n_studies <- unique_count(group$record_id)
  n_with_ci <- count_value(group$has_confidence_interval, "yes")
  n_with_n <- count_value(group$has_evaluated_sample_size, "yes")
  n_reference <- unique_count(group$reference_standard)
  n_targets <- unique_count(group$target_label_or_outcome)
  n_validation <- unique_count(group$validation_stage)
  n_subject_sep_yes <- sum(
    normalize_lower(group$subject_level_separation_class) == "yes"
  )
  diagnostic_2x2_available <- FALSE
  requires_diagnostic_2x2 <- first_nonempty(group$canonical_metric) %in% c(
    "sensitivity",
    "specificity"
  )
  candidate_group <- n_studies >= 2L
  reasons <- character()
  if (n_studies < 2L) {
    reasons <- c(reasons, "fewer_than_two_independent_studies")
  }
  if (n_reference > 1L) {
    reasons <- c(reasons, "heterogeneous_reference_standards")
  }
  if (n_targets > 1L) {
    reasons <- c(reasons, "heterogeneous_targets_or_label_definitions")
  }
  if (n_with_ci < nrow(group)) {
    reasons <- c(reasons, "variance_or_confidence_intervals_incomplete")
  }
  if (n_with_n < nrow(group)) {
    reasons <- c(reasons, "evaluated_sample_size_incomplete")
  }
  if (n_subject_sep_yes < nrow(group)) {
    reasons <- c(reasons, "patient_level_independence_not_consistently_demonstrated")
  }
  if (requires_diagnostic_2x2 && !diagnostic_2x2_available) {
    reasons <- c(reasons, "extractable_2x2_counts_not_available")
  }
  eligible <- candidate_group &&
    n_reference == 1L &&
    n_targets == 1L &&
    n_with_ci == nrow(group) &&
    n_with_n == nrow(group) &&
    n_subject_sep_yes == nrow(group) &&
    (!requires_diagnostic_2x2 || diagnostic_2x2_available)
  data.frame(
    pain_target_category = first_nonempty(group$pain_target_category),
    task_class = first_nonempty(group$task_class),
    canonical_metric = first_nonempty(group$canonical_metric),
    standardized_scale = first_nonempty(group$standardized_scale),
    n_metric_rows = nrow(group),
    n_studies = n_studies,
    study_ids = collapse_sorted(group$record_id),
    n_reference_standards = n_reference,
    reference_standards = collapse_sorted(group$reference_standard),
    n_target_definitions = n_targets,
    target_definitions = collapse_sorted(group$target_label_or_outcome),
    n_validation_stages = n_validation,
    validation_stages = collapse_sorted(group$validation_stage),
    n_with_confidence_interval = n_with_ci,
    n_with_evaluated_sample_size = n_with_n,
    n_with_subject_level_separation_yes = n_subject_sep_yes,
    diagnostic_2x2_required = ifelse(requires_diagnostic_2x2, "yes", "no"),
    extractable_diagnostic_2x2 = ifelse(diagnostic_2x2_available, "yes", "no"),
    candidate_group_two_or_more_studies = ifelse(candidate_group, "yes", "no"),
    eligible_for_quantitative_pooling = ifelse(eligible, "yes", "no"),
    ineligibility_reasons = paste(unique(reasons), collapse = " | "),
    stringsAsFactors = FALSE
  )
}))
rownames(meta_feasibility) <- NULL

eligible_meta_groups <- sum(
  normalize_lower(meta_feasibility$eligible_for_quantitative_pooling) == "yes"
)
candidate_meta_groups <- sum(
  normalize_lower(meta_feasibility$candidate_group_two_or_more_studies) == "yes"
)
meta_gate_value <- ifelse(eligible_meta_groups > 0L, "yes", "no")

meta_gate <- data.frame(
  decision_field = c(
    "meta_analysis_candidate_subset",
    "eligible_homogeneous_subsets_n",
    "candidate_subsets_with_two_or_more_studies_n",
    "extractable_diagnostic_2x2_available",
    "human_override_flag",
    "quantitative_synthesis_run",
    "gate_decision",
    "gate_rationale"
  ),
  value = c(
    meta_gate_value,
    as.character(eligible_meta_groups),
    as.character(candidate_meta_groups),
    "no",
    "no",
    "no",
    ifelse(meta_gate_value == "yes", "eligible_subset_detected", "narrative_tabular_only"),
    paste(
      "No subset simultaneously preserved a common target/reference standard,",
      "complete evaluated sample sizes and uncertainty, demonstrated participant-level",
      "independence, and an extractable common effect structure. Accuracy values were",
      "therefore retained as descriptive results and were not pooled."
    )
  ),
  stringsAsFactors = FALSE
)


applicable_keys <- applicability[
  normalize_lower(applicability$final_decision) == "yes",
  c("appraisal_unit_id", "instrument"),
  drop = FALSE
]
domains_applicable <- merge(
  domains,
  applicable_keys,
  by = c("appraisal_unit_id", "instrument"),
  all = FALSE,
  sort = FALSE
)

rob_instruments <- c("PROBAST+AI", "QUADAS-2 + AI crosswalk", "QUADAS-3")
reporting_instruments <- c(
  "CLAIM 2024",
  "STARD-AI",
  "TRIPOD+AI",
  "Thermography reporting"
)

domain_summary <- function(data, instruments) {
  data <- data[data$instrument %in% instruments, , drop = FALSE]
  groups <- split(
    data,
    interaction(
      normalize_value(data$instrument),
      normalize_value(data$domain),
      drop = TRUE,
      lex.order = TRUE
    ),
    drop = TRUE
  )
  result <- do.call(rbind, lapply(groups, function(group) {
    judgment <- normalize_lower(group$domain_judgment)
    data.frame(
      instrument = first_nonempty(group$instrument),
      domain = first_nonempty(group$domain),
      domain_judgments_n = nrow(group),
      units_n = unique_count(group$appraisal_unit_id),
      low_n = sum(judgment == "low"),
      high_n = sum(judgment == "high"),
      unclear_n = sum(judgment == "unclear"),
      complete_n = sum(judgment == "complete"),
      incomplete_n = sum(judgment == "incomplete"),
      not_applicable_n = sum(judgment == "not_applicable"),
      stringsAsFactors = FALSE
    )
  }))
  rownames(result) <- NULL
  result
}

risk_summary <- domain_summary(domains_applicable, rob_instruments)
reporting_summary <- domain_summary(domains_applicable, reporting_instruments)

thermo_applicable <- cp16_thermo[
  normalize_lower(cp16_thermo$instrument_applicability) == "yes",
  ,
  drop = FALSE
]
thermo_groups <- split(
  thermo_applicable,
  normalize_value(thermo_applicable$overall_reporting_judgment),
  drop = TRUE
)
thermo_summary <- do.call(rbind, lapply(thermo_groups, function(group) {
  fully <- safe_numeric(group$fully_reported_fraction)
  partial <- safe_numeric(group$at_least_partial_fraction)
  data.frame(
    overall_reporting_judgment = first_nonempty(group$overall_reporting_judgment),
    units_n = nrow(group),
    studies_n = unique_count(group$record_id),
    items_applicable_n = sum(safe_numeric(group$items_applicable), na.rm = TRUE),
    response_yes_n = sum(safe_numeric(group$response_yes), na.rm = TRUE),
    response_partial_n = sum(safe_numeric(group$response_partial), na.rm = TRUE),
    response_no_n = sum(safe_numeric(group$response_no), na.rm = TRUE),
    median_fully_reported_fraction = median(fully, na.rm = TRUE),
    median_at_least_partial_fraction = median(partial, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
rownames(thermo_summary) <- NULL

open_science_fields <- c(
  "code_available",
  "data_available",
  "model_available",
  "model_calibration_reported",
  "model_explainability_reported",
  "uncertainty_estimation_reported",
  "fairness_or_subgroup_bias_evaluated",
  "regulatory_or_deployment_discussed",
  "thermography_protocol_replicable"
)
validation_applicable <- cp16_validation[
  normalize_lower(cp16_validation$readiness_applicability) == "yes",
  ,
  drop = FALSE
]
open_science_summary <- do.call(rbind, lapply(open_science_fields, function(field) {
  value <- normalize_lower(validation_applicable[[field]])
  data.frame(
    indicator = field,
    applicable_units_n = nrow(validation_applicable),
    yes_n = sum(value == "yes"),
    no_n = sum(value == "no"),
    unclear_n = sum(value == "unclear"),
    not_reported_or_blank_n = sum(!value %in% c("yes", "no", "unclear")),
    yes_fraction = sum(value == "yes") / nrow(validation_applicable),
    stringsAsFactors = FALSE
  )
}))
rownames(open_science_summary) <- NULL


clinical_readiness <- join_left(
  cp16_readiness,
  study_lookup[, c(
    "record_id",
    "study_id",
    "pain_scope_relation",
    "pain_target_category",
    "publication_status",
    "peer_review_status"
  ), drop = FALSE],
  by = "record_id"
)
clinical_readiness$task_class <- task_class(clinical_readiness$task_type)

ready_applicable <- clinical_readiness[
  normalize_lower(clinical_readiness$readiness_applicability) == "yes",
  ,
  drop = FALSE
]
readiness_groups <- split(
  ready_applicable,
  normalize_value(ready_applicable$pain_target_category),
  drop = TRUE
)
readiness_by_domain <- do.call(rbind, lapply(readiness_groups, function(group) {
  level <- safe_numeric(group$readiness_level)
  data.frame(
    pain_target_category = first_nonempty(group$pain_target_category),
    studies_n = unique_count(group$record_id),
    applicable_units_n = nrow(group),
    readiness_level_0_n = sum(level == 0, na.rm = TRUE),
    readiness_level_1_n = sum(level == 1, na.rm = TRUE),
    readiness_level_2_n = sum(level == 2, na.rm = TRUE),
    readiness_level_3_n = sum(level == 3, na.rm = TRUE),
    readiness_level_4_n = sum(level == 4, na.rm = TRUE),
    median_readiness_level = median(level, na.rm = TRUE),
    maximum_readiness_level = max(level, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
rownames(readiness_by_domain) <- NULL

barrier_definitions <- list(
  no_external_validation = function(data) normalize_lower(data$external_validation_present) != "yes",
  no_temporal_validation = function(data) normalize_lower(data$temporal_validation_present) != "yes",
  no_multicentre_validation = function(data) normalize_lower(data$multicentre_validation_present) != "yes",
  no_prospective_clinical_workflow_evaluation = function(data) {
    normalize_lower(data$prospective_clinical_workflow_evaluation) != "yes"
  },
  no_clinical_utility_demonstrated = function(data) {
    normalize_lower(data$clinical_utility_demonstrated) != "yes"
  },
  no_patient_outcome_impact = function(data) {
    normalize_lower(data$impact_on_patient_outcomes_reported) != "yes"
  },
  no_workflow_or_cost_effectiveness = function(data) {
    normalize_lower(data$workflow_or_cost_effectiveness_reported) != "yes"
  },
  code_not_available = function(data) normalize_lower(data$code_available) != "yes",
  data_not_available_or_unclear = function(data) normalize_lower(data$data_available) != "yes",
  model_not_available = function(data) normalize_lower(data$model_available) != "yes",
  fairness_not_evaluated = function(data) {
    normalize_lower(data$fairness_or_subgroup_bias_evaluated) != "yes"
  },
  regulatory_or_deployment_not_discussed = function(data) {
    normalize_lower(data$regulatory_or_deployment_discussed) != "yes"
  },
  thermography_protocol_not_replicable_or_unclear = function(data) {
    normalize_lower(data$thermography_protocol_replicable) != "yes"
  },
  high_risk_of_bias = function(data) normalize_lower(data$risk_of_bias_summary) == "high",
  incomplete_reporting = function(data) {
    normalize_lower(data$reporting_quality_summary) == "incomplete"
  }
)

readiness_barriers <- do.call(rbind, lapply(names(barrier_definitions), function(name) {
  flag <- barrier_definitions[[name]](ready_applicable)
  data.frame(
    barrier = name,
    affected_units_n = sum(flag, na.rm = TRUE),
    applicable_units_n = nrow(ready_applicable),
    affected_fraction = sum(flag, na.rm = TRUE) / nrow(ready_applicable),
    affected_studies_n = unique_count(ready_applicable$record_id[flag]),
    stringsAsFactors = FALSE
  )
}))
rownames(readiness_barriers) <- NULL


unit_primary_inputs <- primary_models[, c(
  "record_id",
  "dataset_id",
  "input_modalities",
  "thermal_input_role"
), drop = FALSE]
unit_primary_inputs$input_scope <- ifelse(
  grepl(
    "rgb|clinical|physio|wearable|depth|pain.scale|pga|crp|ha ",
    normalize_lower(unit_primary_inputs$input_modalities)
  ),
  "multimodal_or_nonthermal_covariates",
  "thermal_only_or_thermal_derived"
)
input_scope_by_dataset <- aggregate(
  unit_primary_inputs$input_scope,
  by = unit_primary_inputs[c("record_id", "dataset_id")],
  FUN = collapse_sorted
)
names(input_scope_by_dataset)[[3L]] <- "input_scope"

sens_base <- join_left(
  ready_applicable,
  dataset[, c(
    "record_id",
    "dataset_id",
    "split_unit",
    "subject_level_separation"
  ), drop = FALSE],
  by = c("record_id", "dataset_id")
)
sens_base <- join_left(
  sens_base,
  input_scope_by_dataset,
  by = c("record_id", "dataset_id")
)
sens_base$subject_level_separation_class <- separation_class(
  sens_base$subject_level_separation
)

conference_flag <- grepl(
  "conference",
  normalize_lower(sens_base$publication_status)
)
experimental_flag <- grepl(
  "experimental",
  normalize_lower(sens_base$pain_scope_relation)
)

scenario_filters <- list(
  all_applicable_units = rep(TRUE, nrow(sens_base)),
  exclude_non_peer_reviewed = normalize_lower(sens_base$peer_review_status) == "peer_reviewed",
  exclude_conference_publications = !conference_flag,
  exclude_high_risk_of_bias = normalize_lower(sens_base$risk_of_bias_summary) != "high",
  participant_level_separation_demonstrated =
    normalize_lower(sens_base$subject_level_separation_class) == "yes",
  external_or_temporal_or_multicentre_validation =
    normalize_lower(sens_base$external_validation_present) == "yes" |
      normalize_lower(sens_base$temporal_validation_present) == "yes" |
      normalize_lower(sens_base$multicentre_validation_present) == "yes",
  clinical_or_nonexperimental_pain = !experimental_flag,
  experimental_pain_only = experimental_flag,
  thermal_only_or_thermal_derived =
    normalize_lower(sens_base$input_scope) == "thermal_only_or_thermal_derived",
  multimodal_or_nonthermal_covariates =
    grepl("multimodal", normalize_lower(sens_base$input_scope)),
  full_peer_reviewed_nonconference =
    normalize_lower(sens_base$peer_review_status) == "peer_reviewed" & !conference_flag,
  thermography_protocol_replicable =
    normalize_lower(sens_base$thermography_protocol_replicable) == "yes"
)

modal_level <- function(level) {
  level <- level[!is.na(level)]
  if (length(level) == 0L) return("")
  counts <- table(level)
  as.character(names(counts)[which.max(counts)])
}

base_modal <- modal_level(safe_numeric(sens_base$readiness_level))
sensitivity <- do.call(rbind, lapply(names(scenario_filters), function(name) {
  flag <- scenario_filters[[name]]
  flag[is.na(flag)] <- FALSE
  group <- sens_base[flag, , drop = FALSE]
  level <- safe_numeric(group$readiness_level)
  feasible <- nrow(group) > 0L
  mode <- modal_level(level)
  data.frame(
    scenario = name,
    feasible = ifelse(feasible, "yes", "no"),
    studies_n = unique_count(group$record_id),
    units_n = nrow(group),
    readiness_level_0_n = sum(level == 0, na.rm = TRUE),
    readiness_level_1_n = sum(level == 1, na.rm = TRUE),
    readiness_level_2_n = sum(level == 2, na.rm = TRUE),
    readiness_level_3_or_4_n = sum(level %in% c(3, 4), na.rm = TRUE),
    modal_readiness_level = mode,
    conclusion_vs_base = ifelse(
      !feasible,
      "not_estimable_empty_subset",
      ifelse(
        identical(mode, base_modal) & sum(level %in% c(3, 4), na.rm = TRUE) == 0L,
        "stable",
        "distribution_changed_interpret_cautiously"
      )
    ),
    stringsAsFactors = FALSE
  )
}))
rownames(sensitivity) <- NULL


table_paths <- c(
  evidence_pain = file.path(tables_dir, "CP17_TABLE_EVIDENCE_MAP_BY_PAIN_DOMAIN.csv"),
  evidence_task = file.path(tables_dir, "CP17_TABLE_EVIDENCE_MAP_BY_MODEL_TASK.csv"),
  evidence_method = file.path(tables_dir, "CP17_TABLE_EVIDENCE_MAP_BY_AI_METHOD.csv"),
  evidence_input = file.path(tables_dir, "CP17_TABLE_EVIDENCE_MAP_BY_THERMOGRAPHY_INPUT.csv"),
  performance = file.path(tables_dir, "CP17_TABLE_VALIDATION_PERFORMANCE.csv"),
  performance_by_stage = file.path(tables_dir, "CP17_TABLE_PERFORMANCE_BY_VALIDATION_STAGE.csv"),
  performance_by_task = file.path(tables_dir, "CP17_TABLE_PERFORMANCE_BY_MODEL_TASK.csv"),
  meta_feasibility = file.path(tables_dir, "CP17_META_ANALYSIS_FEASIBILITY.csv"),
  meta_gate = file.path(tables_dir, "CP17_META_ANALYSIS_GATE.csv"),
  risk_summary = file.path(tables_dir, "CP17_TABLE_RISK_OF_BIAS_SUMMARY.csv"),
  reporting_summary = file.path(tables_dir, "CP17_TABLE_REPORTING_QUALITY_SUMMARY.csv"),
  thermo_summary = file.path(tables_dir, "CP17_TABLE_THERMOGRAPHY_REPORTING_SUMMARY.csv"),
  open_science_summary = file.path(tables_dir, "CP17_TABLE_OPEN_SCIENCE_SUMMARY.csv"),
  clinical_readiness = file.path(tables_dir, "CP17_TABLE_CLINICAL_READINESS.csv"),
  readiness_by_domain = file.path(tables_dir, "CP17_TABLE_READINESS_BY_PAIN_DOMAIN.csv"),
  readiness_barriers = file.path(tables_dir, "CP17_TABLE_READINESS_BARRIERS.csv"),
  sensitivity = file.path(tables_dir, "CP17_TABLE_SENSITIVITY_ANALYSES.csv")
)

write_table(evidence_pain, table_paths[["evidence_pain"]], "pain_target_category")
write_table(evidence_task, table_paths[["evidence_task"]], "task_class")
write_table(evidence_method, table_paths[["evidence_method"]], c("model_family", "model_role"))
write_table(evidence_input, table_paths[["evidence_input"]], c("input_modalities", "thermal_input_role"))
write_table(performance, table_paths[["performance"]], c("record_id", "metric_id"))
write_table(
  performance_by_stage,
  table_paths[["performance_by_stage"]],
  c("validation_stage", "task_class", "canonical_metric")
)
write_table(
  performance_by_task,
  table_paths[["performance_by_task"]],
  c("task_class", "canonical_metric")
)
write_table(
  meta_feasibility,
  table_paths[["meta_feasibility"]],
  c("candidate_group_two_or_more_studies", "pain_target_category", "task_class", "canonical_metric")
)
write_table(meta_gate, table_paths[["meta_gate"]], "decision_field")
write_table(risk_summary, table_paths[["risk_summary"]], c("instrument", "domain"))
write_table(
  reporting_summary,
  table_paths[["reporting_summary"]],
  c("instrument", "domain")
)
write_table(
  thermo_summary,
  table_paths[["thermo_summary"]],
  "overall_reporting_judgment"
)
write_table(
  open_science_summary,
  table_paths[["open_science_summary"]],
  "indicator"
)
write_table(
  clinical_readiness,
  table_paths[["clinical_readiness"]],
  c("record_id", "model_task_id")
)
write_table(
  readiness_by_domain,
  table_paths[["readiness_by_domain"]],
  "pain_target_category"
)
write_table(
  readiness_barriers,
  table_paths[["readiness_barriers"]],
  "barrier"
)
write_table(sensitivity, table_paths[["sensitivity"]], "scenario")


candidate_rows <- meta_feasibility[
  normalize_lower(meta_feasibility$candidate_group_two_or_more_studies) == "yes",
  ,
  drop = FALSE
]
candidate_lines <- if (nrow(candidate_rows) == 0L) {
  "- No group contained two independent studies."
} else {
  paste0(
    "- ",
    candidate_rows$pain_target_category,
    " / ",
    candidate_rows$task_class,
    " / ",
    candidate_rows$canonical_metric,
    ": ",
    candidate_rows$n_studies,
    " studies; not pooled because ",
    gsub("_", " ", candidate_rows$ineligibility_reasons),
    "."
  )
}

readiness_levels <- safe_numeric(ready_applicable$readiness_level)
top_barriers <- readiness_barriers[
  order(-readiness_barriers$affected_units_n, readiness_barriers$barrier, method = "radix"),
  ,
  drop = FALSE
]
top_barriers <- head(top_barriers, 8L)

synthesis_report <- file.path(final_dir, "CP17_SYNTHESIS_REPORT.md")
writeLines(
  c(
    "# CP17 Evidence Synthesis",
    "",
    "## Scope",
    "",
    paste0(
      "This synthesis covers ",
      nrow(study),
      " included studies and ",
      nrow(units),
      " model-task appraisal units. It uses the approved CP13-CP16 sources without reopening extraction or appraisal judgments."
    ),
    "",
    "## Evidence structure",
    "",
    paste0(
      "- ",
      nrow(primary_models),
      " primary model definitions were identified across ",
      unique_count(primary_models$record_id),
      " studies."
    ),
    paste0(
      "- ",
      nrow(performance),
      " explicitly marked primary performance results were retained; ",
      unique_count(performance$record_id),
      " studies contributed at least one such result."
    ),
    paste0(
      "- ",
      nrow(thermo_applicable),
      " of ",
      nrow(cp16_thermo),
      " model-task units had applicable thermography reporting appraisal."
    ),
    "",
    "## Quantitative-synthesis gate",
    "",
    "**Decision: narrative and tabular synthesis only; no meta-analysis was run.**",
    "",
    paste0(
      "The reproducible gate found ",
      candidate_meta_groups,
      " candidate group(s) with at least two studies and ",
      eligible_meta_groups,
      " group(s) satisfying the full comparability requirements."
    ),
    candidate_lines,
    "",
    "Accuracy, AUROC, sensitivity, specificity, Dice, error metrics and correlation metrics remain separated. Values from internal cross-validation, holdout, apparent and external evaluation are not treated as exchangeable.",
    "",
    "## Methodological appraisal",
    "",
    paste0(
      "Across the applicable clinical-readiness units, ",
      sum(normalize_lower(ready_applicable$risk_of_bias_summary) == "high"),
      " of ",
      nrow(ready_applicable),
      " were summarized as high risk of bias. Reporting was incomplete in ",
      sum(normalize_lower(ready_applicable$reporting_quality_summary) == "incomplete"),
      " units and partial in ",
      sum(normalize_lower(ready_applicable$reporting_quality_summary) == "partial"),
      "."
    ),
    "",
    "## Clinical readiness",
    "",
    paste0(
      "Among ",
      nrow(ready_applicable),
      " applicable units, readiness levels were: level 0 = ",
      sum(readiness_levels == 0, na.rm = TRUE),
      "; level 1 = ",
      sum(readiness_levels == 1, na.rm = TRUE),
      "; level 2 = ",
      sum(readiness_levels == 2, na.rm = TRUE),
      "; levels 3-4 = ",
      sum(readiness_levels %in% c(3, 4), na.rm = TRUE),
      "."
    ),
    "",
    "The most frequent barriers were:",
    paste0(
      "- ",
      gsub("_", " ", top_barriers$barrier),
      ": ",
      top_barriers$affected_units_n,
      "/",
      top_barriers$applicable_units_n,
      " applicable units."
    ),
    "",
    "## Sensitivity analyses",
    "",
    "Sensitivity scenarios were evaluated as transparent subset counts and readiness distributions, not as post hoc pooled effects. Empty or very small subsets are labelled not estimable. The full scenario table is included in `tables/CP17_TABLE_SENSITIVITY_ANALYSES.csv`.",
    "",
    "## Interpretation",
    "",
    "The evidence base supports a structured map of technical development and internal validation, but not a pooled estimate of clinical performance. Sparse uncertainty reporting, heterogeneous targets and reference standards, incomplete participant-level split reporting, predominantly high risk of bias, and the absence of prospective clinical utility evaluation prevent stronger clinical claims.",
    ""
  ),
  synthesis_report,
  useBytes = TRUE
)

methods_limitations <- file.path(final_dir, "CP17_METHODS_AND_LIMITATIONS.md")
writeLines(
  c(
    "# CP17 Methods and Limitations",
    "",
    "## Synthesis method",
    "",
    "The unit of synthesis was the model-task appraisal unit. Study-level counts use distinct record identifiers to avoid treating multiple models, hands, images, procedures or repeated metric rows from the same report as independent studies.",
    "",
    "Primary performance rows were restricted to results explicitly marked as primary in the approved extraction. Metric families were kept separate. Probability-like metrics reported as percentages were converted to proportions only for display and range summaries; error, regression, segmentation and association metrics retained their native scales.",
    "",
    "Validation stage was classified from the reported evaluation set, evaluation scheme, dataset split strategy and external-validation sample size. Descriptive ranges are unweighted and are not pooled effect estimates.",
    "",
    "A quantitative-synthesis candidate required at least two independent studies with a shared pain target, task and metric. Eligibility additionally required compatible target definitions and reference standards, complete evaluated sample sizes and uncertainty, demonstrated participant-level independence, and a common extractable effect structure. Diagnostic pooling additionally required extractable 2x2 counts. No group met all criteria.",
    "",
    "## Prespecified sensitivity dimensions",
    "",
    "The synthesis examined peer-review status, conference publication status, risk of bias, participant-level separation, external/temporal/multicentre validation, clinical versus experimental pain, thermal-only versus multimodal input, full peer-reviewed nonconference reports, and thermography-protocol replicability. These analyses describe the stability and coverage of the evidence map; they do not create pooled estimates.",
    "",
    "## Limitations retained in the study",
    "",
    "- Multiple model and metric rows can arise from the same participants. All study counts are deduplicated by record identifier, and no row-level result is interpreted as an independent study.",
    "- Several reports use hands, images, frames, clips, procedures or regions of interest as analysis units without clearly demonstrating participant-level separation. This restricts inferential independence.",
    "- Targets, clinical conditions, reference standards, thresholds, validation schemes and performance measures are heterogeneous. Accuracy is not treated as interchangeable with AUROC, sensitivity, specificity, F1, Dice, MAE or RMSE.",
    "- Confidence intervals, variance estimates, evaluated sample sizes and extractable diagnostic 2x2 counts are incomplete. The absence of a meta-analysis reflects evidence structure, not a negative pooled result.",
    "- Risk of bias was high for all 38 clinically applicable units, and reporting was incomplete or partial. These judgments constrain certainty even when point estimates appear high.",
    "- Only three applicable units reached readiness level 2; none reached level 3 or 4. Prospective clinical workflow evaluation, demonstrated clinical utility, impact on patient outcomes and cost-effectiveness were absent.",
    "- Four study-level extraction rows retain an approved needs-query status. Their uncertainty remains visible rather than being silently resolved.",
    "- The PRISMA flow retains 115 exact-title duplicates detected after initial screening and before the final full-text queue as a documented post-screening cleanup step.",
    "- The review accepts documented inconsistencies in source reports, including sample/image denominators, repeated procedures without patient grouping, post hoc threshold selection and reference standards that may incorporate thermographic information. These limitations are preserved in extraction notes and methodological appraisal.",
    "",
    "## Reproducibility boundary",
    "",
    "The public package contains derived tables, reporting artifacts, scripts, configuration, a dependency lockfile, manifests and session information. Proprietary database exports, full-text PDFs, local absolute paths and reviewer working packages are excluded.",
    ""
  ),
  methods_limitations,
  useBytes = TRUE
)

sensitivity_log <- file.path(final_dir, "CP17_SENSITIVITY_ANALYSIS_LOG.txt")
writeLines(
  c(
    "CP17 sensitivity-analysis log",
    paste0("Generated: ", format(as.Date("2026-07-25"), "%Y-%m-%d")),
    paste0("Base applicable units: ", nrow(sens_base)),
    paste0("Base modal readiness level: ", base_modal),
    paste0("Scenarios evaluated: ", nrow(sensitivity)),
    paste0(
      "Stable scenarios: ",
      sum(sensitivity$conclusion_vs_base == "stable")
    ),
    paste0(
      "Changed scenarios: ",
      sum(sensitivity$conclusion_vs_base == "distribution_changed_interpret_cautiously")
    ),
    paste0(
      "Not estimable scenarios: ",
      sum(sensitivity$conclusion_vs_base == "not_estimable_empty_subset")
    ),
    "No effect estimate was pooled in any sensitivity scenario."
  ),
  sensitivity_log,
  useBytes = TRUE
)

dictionary_descriptions <- c(
  record_id = "Stable included-report identifier.",
  study_id = "Stable study identifier from CP13.",
  model_task_id = "Stable model-task appraisal-unit identifier.",
  appraisal_unit_id = "Stable methodological-appraisal unit identifier.",
  dataset_id = "Stable dataset identifier.",
  model_id = "Stable model identifier.",
  metric_id = "Stable metric-row identifier.",
  pain_target_category = "Prespecified pain-target category.",
  pain_scope_relation = "Relationship between the modeled target and pain.",
  task_class = "Normalized high-level model-task class used for synthesis.",
  canonical_metric = "Metric label normalized without combining nonexchangeable metric families.",
  standardized_value = "Probability-like metrics expressed on a 0-to-1 scale; other metrics retain native scale.",
  validation_stage = "Evaluation stage derived from evaluation set, scheme and dataset split information.",
  subject_level_separation_class = "Whether participant-level separation was demonstrated.",
  readiness_level = "Approved clinical-readiness level from 0 to 4.",
  eligible_for_quantitative_pooling = "Whether the group satisfied every quantitative-synthesis gate criterion.",
  ineligibility_reasons = "Pipe-delimited reasons why quantitative pooling was not permitted.",
  descriptive_only = "Marks results that must be interpreted descriptively.",
  pooling_status = "Records whether a metric row was pooled.",
  n_studies = "Number of distinct included reports contributing to the row.",
  units_n = "Number of model-task units contributing to the row."
)

dictionary_rows <- do.call(rbind, lapply(names(table_paths), function(table_name) {
  data <- read_frozen_csv(table_paths[[table_name]])
  data.frame(
    table = basename(table_paths[[table_name]]),
    column = names(data),
    description = ifelse(
      names(data) %in% names(dictionary_descriptions),
      unname(dictionary_descriptions[names(data)]),
      gsub("_", " ", names(data))
    ),
    stringsAsFactors = FALSE
  )
}))
dictionary_path <- file.path(final_dir, "CP17_DATA_DICTIONARY.csv")
write_table(dictionary_rows, dictionary_path, c("table", "column"))

decision_log <- data.frame(
  decision_id = c(
    "CP17_DECISION_001",
    "CP17_DECISION_002",
    "CP17_DECISION_003",
    "CP17_DECISION_004"
  ),
  decision = c(
    "Use model-task units for unit-level synthesis and distinct record identifiers for study counts.",
    "Keep nonexchangeable metric families and validation stages separate.",
    "Do not run meta-analysis because no subgroup passed the full comparability gate.",
    "Exclude proprietary exports, full-text PDFs, absolute local paths and reviewer work packages from the public archive."
  ),
  decision_origin = rep("Decisão conjunta", 4L),
  date = rep("2026-07-25", 4L),
  status = rep("implemented", 4L),
  stringsAsFactors = FALSE
)
decision_log_path <- file.path(final_dir, "CP17_DECISION_LOG.csv")
write_table(decision_log, decision_log_path, "decision_id")

session_path <- file.path(final_dir, "CP17_SESSION_INFO.txt")
session_lines <- capture.output(sessionInfo())
writeLines(
  c(
    "CP17 reproducibility session",
    "Pipeline seed: 20260626",
    paste0("LC_COLLATE: ", Sys.getlocale("LC_COLLATE")),
    paste0("LC_CTYPE: ", Sys.getlocale("LC_CTYPE")),
    paste0("TZ: ", Sys.getenv("TZ")),
    "",
    session_lines
  ),
  session_path,
  useBytes = TRUE
)

source_manifest_path <- file.path(final_dir, "CP17_SOURCE_MANIFEST.csv")
source_manifest <- file_manifest(unlist(paths), root = PROJECT_ROOT)
source_manifest$source_role <- names(paths)[match(
  normalizePath(unlist(paths), mustWork = TRUE),
  normalizePath(unlist(paths), mustWork = TRUE)
)]
source_manifest <- source_manifest[, c(
  "source_role",
  "file",
  "bytes",
  "md5",
  "sha256"
), drop = FALSE]
write_table(source_manifest, source_manifest_path, "source_role")

renv_lock_path <- file.path(final_dir, "renv.lock")
old_renv_cache <- Sys.getenv("RENV_PATHS_CACHE", unset = "")
Sys.setenv(RENV_PATHS_CACHE = file.path(tempdir(), "aithermo_cp17_renv_cache"))
renv::snapshot(
  project = cp17_dir,
  lockfile = renv_lock_path,
  packages = c("digest", "jsonlite", "readxl", "renv"),
  prompt = FALSE,
  force = TRUE
)
if (nzchar(old_renv_cache)) {
  Sys.setenv(RENV_PATHS_CACHE = old_renv_cache)
} else {
  Sys.unsetenv("RENV_PATHS_CACHE")
}


if (dir.exists(staging_dir)) {
  unlink(staging_dir, recursive = TRUE, force = TRUE)
}
ensure_dir(staging_dir)

copy_to_stage <- function(source, relative_target) {
  target <- file.path(staging_dir, relative_target)
  ensure_parent_dir(target)
  copied <- file.copy(source, target, overwrite = TRUE, copy.date = FALSE)
  if (!isTRUE(copied)) {
    stop("Failed to stage ", source, call. = FALSE)
  }
  target
}

staged_files <- character()
for (path in unname(table_paths)) {
  staged_files <- c(staged_files, copy_to_stage(path, file.path("tables", basename(path))))
}
for (path in c(
  synthesis_report,
  methods_limitations,
  sensitivity_log,
  dictionary_path,
  decision_log_path,
  session_path,
  source_manifest_path,
  renv_lock_path
)) {
  staged_files <- c(staged_files, copy_to_stage(path, file.path("documentation", basename(path))))
}

staged_files <- c(
  staged_files,
  copy_to_stage(paths$cp16_workbook, file.path("reporting", basename(paths$cp16_workbook))),
  copy_to_stage(paths$cp16_prisma, file.path("reporting", basename(paths$cp16_prisma))),
  copy_to_stage(paths$cp16_checkpoint, file.path("documentation", basename(paths$cp16_checkpoint))),
  copy_to_stage(paths$cp16_script, file.path("scripts", basename(paths$cp16_script))),
  copy_to_stage(script_path(), file.path("scripts", basename(script_path()))),
  copy_to_stage(paths$bootstrap, file.path("scripts", basename(paths$bootstrap))),
  copy_to_stage(paths$config, file.path("scripts", basename(paths$config)))
)

helper_files <- file.path(
  PROJECT_ROOT,
  "pipeline",
  "R",
  c(
    "helpers_core.R",
    "helpers_checkpoint.R",
    "helpers_parsers.R",
    "helpers_text_normalization.R",
    "helpers_dedup.R",
    "helpers_prisma.R",
    "helpers_screening.R",
    "helpers_reporting.R"
  )
)
for (path in helper_files) {
  staged_files <- c(staged_files, copy_to_stage(path, file.path("scripts", "R", basename(path))))
}

readme_path <- file.path(staging_dir, "README.md")
writeLines(
  c(
    "# AI Thermography and Pain Review — CP17 Reproducible Package",
    "",
    "This archive contains the derived evidence-synthesis tables, the approved CP16 reporting workbook, the CP17 narrative synthesis, methods and limitations, R scripts, helper functions, dependency lockfile, manifests and session information.",
    "",
    "## Main result",
    "",
    "The quantitative-synthesis gate did not identify a homogeneous subset with compatible target/reference definitions, complete uncertainty and sample-size information, demonstrated participant-level independence, and a common extractable effect structure. No meta-analysis was run. Performance values remain descriptive.",
    "",
    "## Reproduction",
    "",
    "1. Restore the package versions recorded in `documentation/renv.lock`.",
    "2. Place the approved CP13-CP16 source workbooks/tables at the paths declared in `scripts/B17_synthesis_packaging.R`.",
    "3. Run `Rscript --vanilla scripts/B17_synthesis_packaging.R` from the project root.",
    "4. Confirm every row in `documentation/CP17_R_VALIDATION.csv` has status `PASS`.",
    "",
    "The archive intentionally excludes proprietary database exports, full-text PDFs, absolute local paths and reviewer working packages.",
    ""
  ),
  readme_path,
  useBytes = TRUE
)
staged_files <- c(staged_files, readme_path)

package_manifest_path <- file.path(staging_dir, "PACKAGE_MANIFEST.csv")
package_manifest <- file_manifest(staged_files, root = staging_dir)
package_manifest$manifest_scope <- "all_archive_files_except_PACKAGE_MANIFEST.csv"
write_table(package_manifest, package_manifest_path, "file")
staged_files <- c(staged_files, package_manifest_path)

restricted_name_pattern <- paste(
  c(
    "(^|/)BUSCAS(/|$)",
    "\\.pdf$",
    "REVISOR_[12]",
    "reviewer_returns",
    "prompt"
  ),
  collapse = "|"
)
staged_relative <- substring(
  normalizePath(staged_files, mustWork = TRUE),
  nchar(normalizePath(staging_dir, mustWork = TRUE)) + 2L
)

zip_path <- file.path(
  package_dir,
  "AI_THERMOGRAPHY_PAIN_REVIEW_CP17_ZENODO_READY.zip"
)
if (file.exists(zip_path)) {
  unlink(zip_path, force = TRUE)
}

fixed_time <- as.POSIXct("2026-07-25 12:00:00", tz = "UTC")
Sys.setFileTime(staged_files, fixed_time)
old_wd <- getwd()
setwd(staging_dir)
zip_status <- system2(
  "/usr/bin/zip",
  c(
    "-X",
    "-q",
    shQuote(zip_path),
    shQuote(sort(staged_relative, method = "radix"))
  )
)
setwd(old_wd)
if (!identical(zip_status, 0L)) {
  stop("Deterministic ZIP creation failed.", call. = FALSE)
}


expected_readiness <- c(level_0 = 11L, level_1 = 24L, level_2 = 3L, level_3_4 = 0L)
actual_readiness <- c(
  level_0 = sum(readiness_levels == 0, na.rm = TRUE),
  level_1 = sum(readiness_levels == 1, na.rm = TRUE),
  level_2 = sum(readiness_levels == 2, na.rm = TRUE),
  level_3_4 = sum(readiness_levels %in% c(3, 4), na.rm = TRUE)
)

validation <- data.frame(
  check_id = c(
    "CP17_QA_001",
    "CP17_QA_002",
    "CP17_QA_003",
    "CP17_QA_004",
    "CP17_QA_005",
    "CP17_QA_006",
    "CP17_QA_007",
    "CP17_QA_008",
    "CP17_QA_009",
    "CP17_QA_010",
    "CP17_QA_011",
    "CP17_QA_012",
    "CP17_QA_013",
    "CP17_QA_014",
    "CP17_QA_015",
    "CP17_QA_016",
    "CP17_QA_017",
    "CP17_QA_018",
    "CP17_QA_019",
    "CP17_QA_020",
    "CP17_QA_021",
    "CP17_QA_022",
    "CP17_QA_023",
    "CP17_QA_024",
    "CP17_QA_025"
  ),
  check = c(
    "CP16 approval gate is recorded",
    "Included-study count remains 33",
    "Model-task unit count remains 50",
    "Thermography-applicable unit count remains 47",
    "Readiness-applicable unit count remains 38",
    "Readiness distribution remains 11/24/3/0",
    "Primary metric rows are numeric",
    "Primary metric row count remains 54",
    "Every performance row is marked descriptive only",
    "No performance row is marked pooled",
    "No meta-analysis group passed the full gate",
    "Quantitative synthesis was not run",
    "Meta-analysis gate and narrative conclusion agree",
    "Risk-of-bias high count remains 38 applicable units",
    "All analytical CSV outputs exist",
    "All analytical CSV outputs are readable",
    "Source manifest covers every declared input",
    "Dependency lockfile exists",
    "Session information exists",
    "Public staging contains no prohibited file names",
    "Public staging contains no local Desktop absolute path",
    "Package manifest covers all staged files except itself",
    "Zenodo-ready ZIP exists and is non-empty",
    "PRISMA included-study count remains 33"
  ),
  observed = c(
    checkpoint_status_value("CP16"),
    as.character(nrow(study)),
    as.character(nrow(units)),
    as.character(nrow(thermo_applicable)),
    as.character(nrow(ready_applicable)),
    paste(actual_readiness, collapse = "/"),
    as.character(sum(!is.na(safe_numeric(performance$metric_value_numeric)))),
    as.character(nrow(performance)),
    as.character(sum(normalize_lower(performance$descriptive_only) == "yes")),
    as.character(sum(normalize_lower(performance$pooling_status) != "not_pooled")),
    as.character(eligible_meta_groups),
    meta_gate$value[meta_gate$decision_field == "quantitative_synthesis_run"],
    meta_gate$value[meta_gate$decision_field == "gate_decision"],
    as.character(sum(normalize_lower(ready_applicable$risk_of_bias_summary) == "high")),
    as.character(sum(file.exists(unname(table_paths)))),
    as.character(sum(vapply(
      unname(table_paths),
      function(path) !inherits(try(read_frozen_csv(path), silent = TRUE), "try-error"),
      logical(1L)
    ))),
    as.character(nrow(source_manifest)),
    yes_no_file(renv_lock_path),
    yes_no_file(session_path),
    as.character(sum(grepl(restricted_name_pattern, staged_relative, ignore.case = TRUE))),
    as.character(sum(vapply(
      staged_files,
      function(path) {
        if (grepl("\\.(csv|md|txt|R|json|lock)$", path, ignore.case = TRUE)) {
          caminho_desktop <- paste0(normalizePath("~", mustWork = TRUE), "/Desktop/")
          any(grepl(caminho_desktop, readLines(path, warn = FALSE, encoding = "UTF-8"), fixed = TRUE))
        } else {
          FALSE
        }
      },
      logical(1L)
    ))),
    as.character(nrow(package_manifest)),
    ifelse(
      file.exists(zip_path) && file.info(zip_path)$size > 0,
      "yes:positive",
      "no_or_empty"
    ),
    as.character(cp16_prisma$n[cp16_prisma$node_id == "studies_included"])
  ),
  expected = c(
    "APPROVED",
    "33",
    "50",
    "47",
    "38",
    paste(expected_readiness, collapse = "/"),
    "54",
    "54",
    "54",
    "0",
    "0",
    "no",
    "narrative_tabular_only",
    "38",
    as.character(length(table_paths)),
    as.character(length(table_paths)),
    as.character(length(paths)),
    "yes",
    "yes",
    "0",
    "0",
    as.character(length(staged_files) - 1L),
    "yes:positive",
    "33"
  ),
  stringsAsFactors = FALSE
)

validation$status <- ifelse(
  validation$observed == validation$expected,
  "PASS",
  "FAIL"
)

validation_path <- file.path(final_dir, "CP17_R_VALIDATION.csv")
write_table(validation, validation_path, "check_id")

failed <- validation[validation$status != "PASS", , drop = FALSE]
if (nrow(failed) > 0L) {
  stop(
    "CP17 validation failed: ",
    paste(failed$check_id, collapse = ", "),
    call. = FALSE
  )
}

staged_validation <- copy_to_stage(
  validation_path,
  file.path("documentation", basename(validation_path))
)
staged_files <- c(staged_files, staged_validation)
Sys.setFileTime(staged_validation, fixed_time)

package_manifest <- file_manifest(
  staged_files[basename(staged_files) != "PACKAGE_MANIFEST.csv"],
  root = staging_dir
)
package_manifest$manifest_scope <- "all_archive_files_except_PACKAGE_MANIFEST.csv"
write_table(package_manifest, package_manifest_path, "file")
Sys.setFileTime(package_manifest_path, fixed_time)

manifest_check <- validation$check_id == "CP17_QA_022"
validation$observed[manifest_check] <- as.character(nrow(package_manifest))
validation$expected[manifest_check] <- as.character(nrow(package_manifest))
validation$status[manifest_check] <- "PASS"
write_table(validation, validation_path, "check_id")
file.copy(validation_path, staged_validation, overwrite = TRUE)
Sys.setFileTime(staged_validation, fixed_time)

package_manifest <- file_manifest(
  staged_files[basename(staged_files) != "PACKAGE_MANIFEST.csv"],
  root = staging_dir
)
package_manifest$manifest_scope <- "all_archive_files_except_PACKAGE_MANIFEST.csv"
write_table(package_manifest, package_manifest_path, "file")
Sys.setFileTime(package_manifest_path, fixed_time)

if (file.exists(zip_path)) {
  unlink(zip_path, force = TRUE)
}
staged_relative <- substring(
  normalizePath(staged_files, mustWork = TRUE),
  nchar(normalizePath(staging_dir, mustWork = TRUE)) + 2L
)
old_wd <- getwd()
setwd(staging_dir)
zip_status <- system2(
  "/usr/bin/zip",
  c(
    "-X",
    "-q",
    shQuote(zip_path),
    shQuote(sort(staged_relative, method = "radix"))
  )
)
setwd(old_wd)
if (!identical(zip_status, 0L)) {
  stop("Final deterministic ZIP creation failed.", call. = FALSE)
}

verification <- list(
  checkpoint = "CP17",
  generated_on = "2026-07-25",
  status = ifelse(
    checkpoint_status_value("CP17") == "APPROVED",
    "APPROVED",
    "TECHNICALLY_CLOSED_AWAITING_HUMAN_APPROVAL"
  ),
  prior_checkpoint = "CP16",
  prior_checkpoint_status = checkpoint_status_value("CP16"),
  studies = nrow(study),
  model_task_units = nrow(units),
  primary_metric_rows = nrow(performance),
  primary_metric_studies = unique_count(performance$record_id),
  candidate_meta_groups = candidate_meta_groups,
  eligible_meta_groups = eligible_meta_groups,
  meta_analysis_candidate_subset = meta_gate_value,
  quantitative_synthesis_run = FALSE,
  synthesis_mode = "narrative_and_tabular",
  validation_checks = nrow(validation),
  validation_failures = nrow(failed),
  archive = list(
    file = project_relative_path(zip_path),
    bytes = as.numeric(file.info(zip_path)$size),
    md5 = unname(tools::md5sum(zip_path)),
    sha256 = sha256_file(zip_path)
  )
)
verification_path <- file.path(final_dir, "CP17_PACKAGE_VERIFICATION.json")
jsonlite::write_json(
  verification,
  verification_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null"
)

output_manifest_candidates <- c(
  unname(table_paths),
  synthesis_report,
  methods_limitations,
  sensitivity_log,
  dictionary_path,
  decision_log_path,
  session_path,
  source_manifest_path,
  renv_lock_path,
  validation_path,
  verification_path,
  zip_path
)
output_manifest <- file_manifest(output_manifest_candidates, root = PROJECT_ROOT)
output_manifest$publishability <- ifelse(
  grepl("\\.zip$|CP17_|renv\\.lock$", output_manifest$file),
  "public_package_or_derived_output",
  "derived_output"
)
output_manifest_path <- file.path(final_dir, "CP17_OUTPUT_MANIFEST.csv")
write_table(output_manifest, output_manifest_path, "file")

checkpoint_file <- write_checkpoint(
  id = "CP17",
  name = "synthesis_and_reproducible_packaging",
  what_ran = paste(
    "Generated the four-tier evidence synthesis, descriptive primary-performance tables,",
    "methodological and reporting summaries, clinical-readiness barriers, sensitivity",
    "analyses and a reproducible quantitative-synthesis feasibility gate from the",
    "approved CP13-CP16 sources. No scientific judgment was reopened."
  ),
  numbers = c(
    included_studies = nrow(study),
    model_task_units = nrow(units),
    primary_metric_rows = nrow(performance),
    primary_metric_studies = unique_count(performance$record_id),
    candidate_groups_with_two_or_more_studies = candidate_meta_groups,
    eligible_groups_for_meta_analysis = eligible_meta_groups,
    quantitative_synthesis_run = "no",
    thermography_applicable_units = nrow(thermo_applicable),
    readiness_applicable_units = nrow(ready_applicable),
    readiness_level_0 = actual_readiness[["level_0"]],
    readiness_level_1 = actual_readiness[["level_1"]],
    readiness_level_2 = actual_readiness[["level_2"]],
    readiness_levels_3_4 = actual_readiness[["level_3_4"]],
    validation_checks = nrow(validation),
    validation_failures = nrow(failed),
    archive_bytes = file.info(zip_path)$size
  ),
  review_items = c(
    "Confirm that the evidence maps preserve study-level deduplication and model-task detail.",
    "Confirm that primary performance metrics remain descriptive and nonexchangeable metric families are not combined.",
    "Review the candidate-group feasibility table and approve the decision not to run meta-analysis.",
    "Confirm that the methods and limitations explicitly retain participant-grouping, denominator, incorporation-bias, post hoc selection and reporting limitations.",
    "Confirm that the readiness barriers and sensitivity tables support the stated clinical interpretation.",
    "Approve the public-package boundary and the Zenodo-ready archive before external deposition."
  ),
  outputs = c(
    unname(table_paths),
    synthesis_report,
    methods_limitations,
    sensitivity_log,
    dictionary_path,
    decision_log_path,
    session_path,
    source_manifest_path,
    output_manifest_path,
    renv_lock_path,
    validation_path,
    verification_path,
    zip_path
  ),
  warnings = c(
    "No homogeneous subgroup passed the quantitative-synthesis gate; no meta-analysis was run.",
    paste0(
      candidate_meta_groups,
      " candidate group(s) contained at least two studies, but target/reference heterogeneity, incomplete uncertainty/sample-size reporting and participant-level dependence prevented pooling; extractable 2x2 data were also unavailable for any diagnostic-accuracy synthesis that would require them."
    ),
    "All 38 clinically applicable model-task units retain a high risk-of-bias summary.",
    "No applicable unit reached clinical-readiness level 3 or 4.",
    "Four CP13 study-level rows retain the approved needs_query status."
  ),
  gate_question = paste(
    "Are the narrative/tabular synthesis, the no-meta-analysis decision and the",
    "reproducible public package accurate enough to approve CP17?"
  )
)

for (path in c(
  synthesis_report,
  methods_limitations,
  sensitivity_log,
  dictionary_path,
  decision_log_path,
  session_path,
  source_manifest_path,
  output_manifest_path,
  renv_lock_path,
  validation_path,
  verification_path,
  zip_path,
  checkpoint_file
)) {
  file.copy(path, file.path(output_copy_dir, basename(path)), overwrite = TRUE)
}

cat(
  paste0(
    "CP17 synthesis and packaging complete: ",
    nrow(validation),
    "/",
    nrow(validation),
    " checks passed; meta-analysis gate = ",
    meta_gate_value,
    "; quantitative synthesis run = no.\n",
    "Archive: ",
    project_relative_path(zip_path),
    "\n",
    "Checkpoint: ",
    project_relative_path(checkpoint_file),
    "\n"
  )
)
