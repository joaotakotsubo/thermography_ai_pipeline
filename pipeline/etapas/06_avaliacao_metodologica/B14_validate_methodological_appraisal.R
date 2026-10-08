#!/usr/bin/env Rscript

# Confere a estrutura e a consistência das avaliações metodológicas consolidadas.
# Não transforma informação ausente em evidência favorável ao estudo.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP13")

require_checkpoint_approved <- function(checkpoint_id) {
  approvals <- read_csv_or_empty(
    checkpoint_approvals_path(),
    CHECKPOINT_APPROVAL_COLUMNS
  )
  checkpoint_id <- checkpoint_id_normalize(checkpoint_id)
  row <- approvals[approvals$checkpoint_id == checkpoint_id, , drop = FALSE]
  if (
    nrow(row) == 0L ||
      !identical(tolower(trimws(row$approved[[1L]])), "yes")
  ) {
    stop(
      "CP14 closure requires ",
      checkpoint_id,
      " approval.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

require_checkpoint_approved("CP13")

cp14_dir <- file.path(
  CFG$pipeline_dir,
  "methodological_appraisal",
  "cp14",
  "final"
)

paths <- c(
  studies = file.path(cp14_dir, "CP14_STUDY_INDEX_FINAL.csv"),
  units = file.path(cp14_dir, "CP14_APPRAISAL_UNITS_FINAL.csv"),
  applicability = file.path(cp14_dir, "CP14_APPLICABILITY_FINAL.csv"),
  items = file.path(cp14_dir, "CP14_ITEM_JUDGMENTS_FINAL.csv"),
  domains = file.path(cp14_dir, "CP14_DOMAIN_JUDGMENTS_FINAL.csv"),
  agreement = file.path(cp14_dir, "CP14_AGREEMENT_PAIRS_PRE_ADJUDICATION.csv"),
  batches = file.path(cp14_dir, "CP14_BATCH_MANIFEST.csv"),
  qa = file.path(cp14_dir, "CP14_QA_CHECKS.csv"),
  workbook = file.path(cp14_dir, "CP14_AVALIACAO_METODOLOGICA_FINAL_33_ESTUDOS.xlsx"),
  closure_report = file.path(cp14_dir, "CP14_CLOSURE_REPORT.md"),
  closure_verification = file.path(cp14_dir, "CP14_CLOSURE_VERIFICATION.json")
)

missing_paths <- paths[!file.exists(paths)]
if (length(missing_paths) > 0L) {
  stop(
    "CP14 closure cannot be validated because required files are missing: ",
    paste(project_relative_path(missing_paths), collapse = ", "),
    call. = FALSE
  )
}

read_cp14_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
}

studies <- read_cp14_csv(paths[["studies"]])
units <- read_cp14_csv(paths[["units"]])
applicability <- read_cp14_csv(paths[["applicability"]])
items <- read_cp14_csv(paths[["items"]])
domains <- read_cp14_csv(paths[["domains"]])
agreement <- read_cp14_csv(paths[["agreement"]])
batches <- read_cp14_csv(paths[["batches"]])
qa <- read_cp14_csv(paths[["qa"]])

required_columns <- list(
  studies = c("record_id", "title", "appraisal_units", "batches", "active_instruments"),
  units = c("batch", "appraisal_unit_id", "record_id", "active_instrument_count"),
  applicability = c(
    "batch", "appraisal_unit_id", "record_id", "instrument",
    "reviewer_1_response", "reviewer_2_response", "final_decision",
    "decision_origin", "final_rationale", "decision_date"
  ),
  items = c(
    "batch", "appraisal_unit_id", "record_id", "instrument", "phase",
    "domain", "item_id", "instrument_applicability", "response",
    "rationale", "reviewer", "review_date", "status"
  ),
  domains = c(
    "batch", "appraisal_unit_id", "record_id", "instrument", "phase",
    "domain", "domain_judgment", "rationale", "reviewer",
    "review_date", "status"
  ),
  agreement = c("assessment_type", "reviewer_1_response", "reviewer_2_response"),
  batches = c(
    "batch", "final_workbook", "final_sha256", "reviewer_1_archive",
    "reviewer_1_sha256", "reviewer_2_archive", "reviewer_2_sha256",
    "adjudication_date", "verification_file", "status"
  ),
  qa = c("check", "observed", "expected", "status", "note")
)

tables <- list(
  studies = studies,
  units = units,
  applicability = applicability,
  items = items,
  domains = domains,
  agreement = agreement,
  batches = batches,
  qa = qa
)

missing_columns <- unlist(
  lapply(
    names(required_columns),
    function(table_name) {
      missing <- setdiff(required_columns[[table_name]], names(tables[[table_name]]))
      if (length(missing) == 0L) {
        return(character())
      }
      paste0(table_name, ":", missing)
    }
  ),
  use.names = FALSE
)
if (length(missing_columns) > 0L) {
  stop(
    "CP14 closure tables are missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

nonblank <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

key_is_unique <- function(data, columns) {
  key <- do.call(paste, c(data[columns], sep = "\u001f"))
  !anyDuplicated(key)
}

checks <- list()
add_check <- function(check, observed, expected, pass, note = "") {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = as.character(check),
    observed = as.character(observed),
    expected = as.character(expected),
    status = if (isTRUE(pass)) "PASS" else "FAIL",
    note = as.character(note),
    stringsAsFactors = FALSE
  )
}

add_check("required_files_present", length(paths), length(paths), TRUE)
add_check("included_studies", nrow(studies), 33L, nrow(studies) == 33L)
add_check("appraisal_units", nrow(units), 50L, nrow(units) == 50L)
add_check("applicability_rows", nrow(applicability), 310L, nrow(applicability) == 310L)
add_check("applicable_item_rows", nrow(items), 7106L, nrow(items) == 7106L)
add_check("applicable_domain_rows", nrow(domains), 893L, nrow(domains) == 893L)
add_check("agreement_pairs", nrow(agreement), 8309L, nrow(agreement) == 8309L)
add_check("canonical_batches", nrow(batches), 32L, nrow(batches) == 32L)

expected_batches <- sprintf("B%02d", seq_len(32L))
observed_batches <- sort(unique(as.character(batches$batch)))
add_check(
  "batch_sequence_B01_B32",
  paste(observed_batches, collapse = ";"),
  paste(expected_batches, collapse = ";"),
  identical(observed_batches, expected_batches)
)

add_check(
  "unique_study_ids",
  length(unique(studies$record_id)),
  nrow(studies),
  !anyDuplicated(studies$record_id)
)
add_check(
  "unique_appraisal_unit_ids",
  length(unique(units$appraisal_unit_id)),
  nrow(units),
  !anyDuplicated(units$appraisal_unit_id)
)
add_check(
  "unique_applicability_keys",
  "evaluated",
  "unique",
  key_is_unique(applicability, c("appraisal_unit_id", "instrument"))
)
add_check(
  "unique_item_keys",
  "evaluated",
  "unique",
  key_is_unique(items, c("appraisal_unit_id", "instrument", "phase", "domain", "item_id"))
)
add_check(
  "unique_domain_keys",
  "evaluated",
  "unique",
  key_is_unique(domains, c("appraisal_unit_id", "instrument", "phase", "domain"))
)

unit_records_in_studies <- all(units$record_id %in% studies$record_id)
applicability_units_in_master <- all(applicability$appraisal_unit_id %in% units$appraisal_unit_id)
item_units_in_master <- all(items$appraisal_unit_id %in% units$appraisal_unit_id)
domain_units_in_master <- all(domains$appraisal_unit_id %in% units$appraisal_unit_id)
add_check("unit_record_linkage", sum(units$record_id %in% studies$record_id), nrow(units), unit_records_in_studies)
add_check(
  "applicability_unit_linkage",
  sum(applicability$appraisal_unit_id %in% units$appraisal_unit_id),
  nrow(applicability),
  applicability_units_in_master
)
add_check(
  "item_unit_linkage",
  sum(items$appraisal_unit_id %in% units$appraisal_unit_id),
  nrow(items),
  item_units_in_master
)
add_check(
  "domain_unit_linkage",
  sum(domains$appraisal_unit_id %in% units$appraisal_unit_id),
  nrow(domains),
  domain_units_in_master
)

applicability_allowed <- c("yes", "no", "unclear")
item_allowed <- c(
  "yes", "probably_yes", "probably_no", "no", "not_applicable",
  "unclear", "low", "high", "partial", "no_information", "complete", "incomplete"
)
domain_allowed <- c("low", "high", "unclear", "complete", "partial", "incomplete", "not_applicable")

add_check(
  "final_applicability_vocabulary",
  paste(sort(unique(applicability$final_decision)), collapse = ";"),
  paste(applicability_allowed, collapse = ";"),
  all(nonblank(applicability$final_decision)) &&
    all(applicability$final_decision %in% applicability_allowed)
)
add_check(
  "final_item_vocabulary",
  paste(sort(unique(items$response)), collapse = ";"),
  "controlled item vocabulary",
  all(nonblank(items$response)) && all(items$response %in% item_allowed)
)
add_check(
  "final_domain_vocabulary",
  paste(sort(unique(domains$domain_judgment)), collapse = ";"),
  "controlled domain vocabulary",
  all(nonblank(domains$domain_judgment)) &&
    all(domains$domain_judgment %in% domain_allowed)
)

add_check(
  "final_item_completion",
  sum(items$status == "complete" & items$reviewer == "Decisão conjunta"),
  nrow(items),
  all(items$status == "complete") &&
    all(items$reviewer == "Decisão conjunta") &&
    all(nonblank(items$rationale)) &&
    all(nonblank(items$review_date))
)
add_check(
  "final_domain_completion",
  sum(domains$status == "complete" & domains$reviewer == "Decisão conjunta"),
  nrow(domains),
  all(domains$status == "complete") &&
    all(domains$reviewer == "Decisão conjunta") &&
    all(nonblank(domains$rationale)) &&
    all(nonblank(domains$review_date))
)
add_check(
  "final_applicability_completion",
  sum(nonblank(applicability$final_decision)),
  nrow(applicability),
  all(nonblank(applicability$final_decision)) &&
    all(nonblank(applicability$final_rationale)) &&
    all(nonblank(applicability$decision_date))
)

add_check(
  "batch_hashes_and_archives",
  sum(
    nonblank(batches$final_sha256) &
      nonblank(batches$reviewer_1_sha256) &
      nonblank(batches$reviewer_2_sha256) &
      nonblank(batches$reviewer_1_archive) &
      nonblank(batches$reviewer_2_archive)
  ),
  nrow(batches),
  all(
    nonblank(batches$final_sha256) &
      nonblank(batches$reviewer_1_sha256) &
      nonblank(batches$reviewer_2_sha256) &
      nonblank(batches$reviewer_1_archive) &
      nonblank(batches$reviewer_2_archive)
  )
)
add_check(
  "batch_completion",
  sum(batches$status == "complete" & nonblank(batches$adjudication_date)),
  nrow(batches),
  all(batches$status == "complete") && all(nonblank(batches$adjudication_date))
)

qa_pass <- all(qa$status == "PASS")
add_check("consolidation_qa", sum(qa$status == "PASS"), nrow(qa), qa_pass)

validation <- do.call(rbind, checks)
validation_path <- file.path(cp14_dir, "CP14_R_VALIDATION.csv")
stable_write_csv(
  validation,
  validation_path,
  columns = c("check", "observed", "expected", "status", "note")
)

failed <- validation[validation$status == "FAIL", , drop = FALSE]

checkpoint_path <- write_checkpoint(
  id = "CP14",
  name = "methodological_appraisal_closed",
  what_ran = paste(
    "Validated the final CP14 consolidation in R without recoding scientific judgments.",
    "The validation checked file presence, exact row counts, key uniqueness, linkage",
    "across studies and appraisal units, controlled vocabularies, completion status,",
    "batch hashes and reviewer-return archival fields."
  ),
  numbers = c(
    included_studies = nrow(studies),
    appraisal_units = nrow(units),
    applicability_decisions = nrow(applicability),
    applicable_item_judgments = nrow(items),
    applicable_domain_judgments = nrow(domains),
    pre_adjudication_pairs = nrow(agreement),
    batches_closed = nrow(batches),
    independent_returns_archived = 64L,
    r_validation_checks = nrow(validation),
    r_validation_failures = nrow(failed)
  ),
  review_items = c(
    "Confirm that all 33 included studies and 50 appraisal units are represented.",
    "Confirm that the 310 applicability decisions, 7,106 item judgments, and 893 domain judgments are complete.",
    "Confirm that B01-B32 have both independent returns archived and a joint decision.",
    "Read the documented B03 source-label exception and the applicability sensitivity analysis."
  ),
  outputs = c(
    paths,
    validation_path
  ),
  warnings = c(
    "Four raw independent applicability responses used the legacy label not_applicable. They remain unchanged in the audit trail; the final decisions use only yes/no/unclear, and a not_applicable-to-no sensitivity analysis is reported.",
    "In B03, both source workbooks carried the internal label Revisor 2. Reviewer roles were recovered from the immutable SHA-256 hashes frozen in the B03 verification record; the source files were not edited."
  ),
  gate_question = "The CP14 methodological-appraisal package is complete and reproducibly validated. Approve CP14 to permit the next pipeline phase?"
)

if (nrow(failed) > 0L) {
  stop(
    "CP14 R validation failed: ",
    paste(failed$check, collapse = ", "),
    ". See ",
    project_relative_path(validation_path),
    call. = FALSE
  )
}

message("CP14 R validation passed: ", nrow(validation), " checks, 0 failures.")
message("Validation: ", project_relative_path(validation_path))
message("Checkpoint: ", project_relative_path(checkpoint_path))
