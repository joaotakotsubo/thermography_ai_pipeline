#!/usr/bin/env Rscript

# Recebe a planilha de extração concluída e confere sua estrutura.
# A planilha original é preservada, e as contagens são verificadas antes da cópia de referência.

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

entrada <- Sys.getenv(
  "AITHERMO_CP13_COMPLETED_WORKBOOK",
  unset = file.path(PROJECT_ROOT, "dados_humanos", "cp13", "data_extraction_package_CP13_COMPLETE_33.xlsx")
)
saida_dir <- file.path(CFG$pipeline_dir, "data_extraction")
saida <- file.path(saida_dir, "data_extraction_package_CP13_COMPLETE_33.xlsx")
ensure_dir(saida_dir)

if (!file.exists(entrada)) {
  stop(
    "Planilha CP13 concluída não localizada. Informe AITHERMO_CP13_COMPLETED_WORKBOOK.",
    call. = FALSE
  )
}
if (identical(normalizePath(entrada), normalizePath(saida, mustWork = FALSE))) {
  stop("A entrada CP13 deve ficar fora da camada derivada do fluxo.", call. = FALSE)
}

abas_obrigatorias <- c(
  "study_level_extraction", "thermal_acquisition", "dataset_partition",
  "model_definition", "model_metric", "clinical_outcome",
  "publication_overlap", "qa_routing", "included_reference"
)
abas <- readxl::excel_sheets(entrada)
faltantes <- setdiff(abas_obrigatorias, abas)
if (length(faltantes)) {
  stop("Abas obrigatórias ausentes na extração: ", paste(faltantes, collapse = ", "), call. = FALSE)
}

esperados <- c(
  study_level_extraction = 33L,
  thermal_acquisition = 50L,
  dataset_partition = 46L,
  model_definition = 237L,
  model_metric = 1156L,
  clinical_outcome = 43L,
  publication_overlap = 33L,
  qa_routing = 33L,
  included_reference = 33L
)

contagens <- lapply(names(esperados), function(aba) {
  dados <- suppressMessages(readxl::read_excel(entrada, sheet = aba, .name_repair = "minimal"))
  if (!"record_id" %in% names(dados)) stop("A aba ", aba, " não possui record_id.", call. = FALSE)
  ids <- trimws(as.character(dados$record_id))
  if (any(!nzchar(ids))) stop("A aba ", aba, " possui record_id vazio.", call. = FALSE)
  data.frame(
    aba = aba,
    linhas_observadas = nrow(dados),
    linhas_esperadas = unname(esperados[[aba]]),
    registros_unicos = length(unique(ids)),
    status = if (nrow(dados) == esperados[[aba]]) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  )
})
contagens <- do.call(rbind, contagens)

estudos <- suppressMessages(readxl::read_excel(entrada, sheet = "study_level_extraction", .name_repair = "minimal"))
referencias <- suppressMessages(readxl::read_excel(entrada, sheet = "included_reference", .name_repair = "minimal"))
ids_estudos <- sort(unique(trimws(as.character(estudos$record_id))), method = "radix")
ids_referencias <- sort(unique(trimws(as.character(referencias$record_id))), method = "radix")
conjuntos_iguais <- identical(ids_estudos, ids_referencias) && length(ids_estudos) == 33L
contagens <- rbind(
  contagens,
  data.frame(
    aba = "record_id_crosswalk",
    linhas_observadas = length(ids_estudos),
    linhas_esperadas = 33L,
    registros_unicos = length(ids_estudos),
    status = if (conjuntos_iguais) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  )
)
if (any(contagens$status != "PASS")) {
  stable_write_csv(contagens, file.path(saida_dir, "CP13_IMPORT_VALIDATION.csv"))
  stop("A planilha CP13 falhou nos controles de estrutura ou denominador.", call. = FALSE)
}

hash_entrada <- digest::digest(entrada, file = TRUE, algo = "sha256")
if (!isTRUE(file.copy(entrada, saida, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE))) {
  stop("Não foi possível copiar a planilha CP13 validada.", call. = FALSE)
}
hash_saida <- digest::digest(saida, file = TRUE, algo = "sha256")
if (!identical(hash_entrada, hash_saida)) stop("A cópia CP13 não preservou o SHA-256.", call. = FALSE)

stable_write_csv(contagens, file.path(saida_dir, "CP13_IMPORT_VALIDATION.csv"))
manifesto <- data.frame(
  papel = c("entrada_humana_imutavel", "copia_canonica_derivada"),
  arquivo = c(normalizePath(entrada), project_relative_path(saida)),
  sha256 = c(hash_entrada, hash_saida),
  status = c("preservada", "validada"),
  stringsAsFactors = FALSE
)
stable_write_csv(manifesto, file.path(saida_dir, "CP13_IMPORT_MANIFEST.csv"))
cat("Extração CP13 validada e importada com SHA-256 preservado.\n")
