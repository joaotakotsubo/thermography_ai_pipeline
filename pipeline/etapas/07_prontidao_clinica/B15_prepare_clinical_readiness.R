#!/usr/bin/env Rscript

# Organiza as unidades e as evidências para a avaliação de prontidão clínica.
# A preparação não atribui níveis de evidência em nome dos revisores.

arquivo_ativo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(arquivo_ativo)) stop("Execute este arquivo com Rscript.", call. = FALSE)
p <- dirname(normalizePath(arquivo_ativo[[1L]], mustWork = TRUE))
repeat {
  if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break
  anterior <- dirname(p)
  if (identical(anterior, p)) stop("Bootstrap não localizado.", call. = FALSE)
  p <- anterior
}
source(file.path(p, "pipeline", "_bootstrap.R"))

if (!requireNamespace("readxl", quietly = TRUE)) stop("O pacote readxl é necessário.", call. = FALSE)
if (!requireNamespace("digest", quietly = TRUE)) stop("O pacote digest é necessário.", call. = FALSE)

cp13 <- file.path(CFG$pipeline_dir, "data_extraction", "data_extraction_package_CP13_COMPLETE_33.xlsx")
cp14 <- file.path(CFG$pipeline_dir, "methodological_appraisal", "cp14", "final", "CP14_AVALIACAO_METODOLOGICA_FINAL_33_ESTUDOS.xlsx")
if (!file.exists(cp13) || !file.exists(cp14)) stop("CP13 e CP14 aprovados são obrigatórios para preparar CP15.", call. = FALSE)

cp15 <- file.path(CFG$pipeline_dir, "clinical_readiness", "cp15")
source_dir <- file.path(cp15, "source_data")
qa_dir <- file.path(cp15, "qa")
ensure_dir(source_dir)
ensure_dir(qa_dir)

unidades <- suppressMessages(readxl::read_excel(cp14, sheet = "appraisal_units", .name_repair = "minimal"))
itens <- suppressMessages(readxl::read_excel(cp14, sheet = "item_final", .name_repair = "minimal"))
dominios <- suppressMessages(readxl::read_excel(cp14, sheet = "domain_final", .name_repair = "minimal"))

validate_required_columns(unidades, c("appraisal_unit_id", "record_id", "title", "dataset_id", "task_type", "target", "reference_standard"), cp14)
if (nrow(unidades) != 50L || length(unique(unidades$record_id)) != 33L || anyDuplicated(unidades$appraisal_unit_id)) {
  stop("O universo CP14 deve conter 50 unidades únicas de 33 estudos.", call. = FALSE)
}

universe <- as.data.frame(unidades, stringsAsFactors = FALSE)
universe$model_task_id <- as.character(universe$appraisal_unit_id)
primeiras <- c("model_task_id", setdiff(names(universe), "model_task_id"))
universe <- universe[, primeiras, drop = FALSE]
universe <- universe[order(universe$model_task_id, method = "radix"), , drop = FALSE]
stable_write_csv(universe, file.path(source_dir, "CP15_READINESS_UNIVERSE.csv"))

texto_evidencia <- function(id, df, campos) {
  parte <- df[as.character(df$appraisal_unit_id) == id, intersect(campos, names(df)), drop = FALSE]
  if (!nrow(parte)) return("")
  linhas <- apply(as.data.frame(lapply(parte, function(x) trimws(as.character(x))), stringsAsFactors = FALSE), 1L, function(x) {
    x <- x[nzchar(x)]
    paste(x, collapse = " | ")
  })
  paste(unique(linhas[nzchar(linhas)]), collapse = " || ")
}

evidence <- data.frame(
  model_task_id = universe$model_task_id,
  record_id = universe$record_id,
  title = universe$title,
  dataset_id = universe$dataset_id,
  task_type = universe$task_type,
  target = universe$target,
  reference_standard = universe$reference_standard,
  item_level_evidence = vapply(
    universe$model_task_id,
    texto_evidencia,
    character(1L),
    df = itens,
    campos = c("instrument", "phase", "domain", "item_id", "response", "supporting_page_or_location", "rationale")
  ),
  domain_level_evidence = vapply(
    universe$model_task_id,
    texto_evidencia,
    character(1L),
    df = dominios,
    campos = c("instrument", "phase", "domain", "domain_judgment", "rationale")
  ),
  stringsAsFactors = FALSE
)
stable_write_csv(evidence, file.path(source_dir, "CP15_EVIDENCE_PROFILE.csv"))

manifesto <- data.frame(
  source = c("CP13", "CP14"),
  file = project_relative_path(c(cp13, cp14)),
  sha256 = vapply(c(cp13, cp14), digest::digest, character(1L), file = TRUE, algo = "sha256"),
  role = c("extração aprovada", "avaliação metodológica aprovada"),
  stringsAsFactors = FALSE
)
stable_write_csv(manifesto, file.path(source_dir, "CP15_SOURCE_MANIFEST.csv"))

qa <- data.frame(
  check = c("model_task_units", "included_studies", "unique_unit_ids", "item_rows_available", "domain_rows_available"),
  observed = c(nrow(universe), length(unique(universe$record_id)), length(unique(universe$model_task_id)), nrow(itens), nrow(dominios)),
  expected = c(50L, 33L, 50L, 7106L, 893L),
  stringsAsFactors = FALSE
)
qa$status <- ifelse(qa$observed == qa$expected, "PASS", "FAIL")
stable_write_csv(qa, file.path(qa_dir, "CP15_PREPARATION_QA.csv"))
if (any(qa$status != "PASS")) stop("A preparação CP15 falhou nos controles de denominador.", call. = FALSE)

writeLines(
  c(
    "# Entradas humanas da prontidão clínica", "",
    "As planilhas independentes preenchidas devem ser mantidas em duas pastas separadas.",
    "Informe essas pastas por AITHERMO_CP15_REVIEWER_1_DIR e AITHERMO_CP15_REVIEWER_2_DIR.",
    "Cada arquivo deve conter a aba readiness_form e preservar model_task_id, record_id e lot_id."
  ),
  file.path(cp15, "ENTRADAS_REVISORES.md"),
  useBytes = TRUE
)
cat("Universo CP15 preparado: 50 unidades de 33 estudos.\n")
