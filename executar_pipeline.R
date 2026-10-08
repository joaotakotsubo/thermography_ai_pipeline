#!/usr/bin/env Rscript

# Executa as etapas na ordem escolhida e registra o andamento em um arquivo de log.
# Se uma etapa falhar ou faltar uma aprovação, o fluxo para naquele ponto.


argumentos <- commandArgs(trailingOnly = TRUE)
valor_argumento <- function(nome, padrao = "") {
  achado <- grep(paste0("^--", nome, "="), argumentos, value = TRUE)
  if (!length(achado)) return(padrao)
  sub(paste0("^--", nome, "="), "", achado[[1L]])
}

arquivo_ativo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(arquivo_ativo)) stop("Execute este arquivo com Rscript.", call. = FALSE)
raiz <- dirname(normalizePath(arquivo_ativo[[1L]], mustWork = TRUE))
source(file.path(raiz, "pipeline", "_bootstrap.R"))

etapas <- c(
  P00 = "01_preparacao_importacao/P00_create_buscas_inventory.R",
  A00 = "01_preparacao_importacao/A00_setup.R",
  A01 = "01_preparacao_importacao/A01_verify_inventory.R",
  A02 = "01_preparacao_importacao/A02_file_level_dedup.R",
  A03 = "01_preparacao_importacao/A03_build_search_manifest.R",
  A04 = "01_preparacao_importacao/A04_import_records.R",
  A05 = "01_preparacao_importacao/A05_normalize_records.R",
  A06 = "02_deduplicacao/A06_deduplicate.R",
  A07 = "02_deduplicacao/A07_prisma_counts.R",
  A08 = "02_deduplicacao/A08_sentinel_recovery_check.R",
  A09 = "03_triagem/A09_prepare_screening.R",
  B10 = "03_triagem/B10_screening_reconciliation.R",
  B10a = "03_triagem/B10a_prepare_pre_adjudication.R",
  B10b = "03_triagem/B10b_title_duplicate_resolution.R",
  B10c = "03_triagem/B10c_apply_pre_adjudication.R",
  B10d = "03_triagem/B10d_apply_pre_adjudication_corrections.R",
  C11 = "04_texto_completo/C11_full_text_retrieval_tracking.R",
  C11a = "04_texto_completo/C11a_import_adjudication_pdfs.R",
  C11b = "04_texto_completo/C11b_prepare_full_text_eligibility.R",
  C11c = "04_texto_completo/C11c_import_full_text_eligibility_returns.R",
  C12 = "04_texto_completo/C12_full_text_eligibility_reconciliation.R",
  C12b = "04_texto_completo/C12b_import_full_text_adjudication_final.R",
  C13 = "05_extracao/C13_prepare_data_extraction.R",
  C13b = "05_extracao/C13b_importar_extracao_concluida.R",
  CP14 = "06_avaliacao_metodologica/01_reconciliar_avaliacao_metodologica.R",
  B14 = "06_avaliacao_metodologica/B14_validate_methodological_appraisal.R",
  B15 = "07_prontidao_clinica/B15_prepare_clinical_readiness.R",
  B15a = "07_prontidao_clinica/B15a_import_clinical_readiness_returns.R",
  B15b = "07_prontidao_clinica/B15b_adjudicate_clinical_readiness.R",
  B15c = "07_prontidao_clinica/B15c_finalize_clinical_readiness.R",
  B16 = "09_sintese/B16_reporting_verification.R",
  B17 = "09_sintese/B17_synthesis_packaging.R",
  TEHAI_1 = "08_tehai/01_reconciliar_tehai.R",
  TEHAI_2 = "08_tehai/02_sintese_descritiva_tehai.R",
  VIS_DADOS = "09_sintese/03_preparar_dados_visualizacao.R",
  FIG_1 = "10_figuras/08_figura_1_prisma.R",
  FIG_2_6 = "10_figuras/09_figuras_2_a_6.R",
  FIG_SUP = "10_figuras/10_figuras_suplementares.R",
  MANIFESTO = "11_integridade/11_manifesto_reproducibilidade.R"
)

de <- valor_argumento("de", names(etapas)[[1L]])
ate <- valor_argumento("ate", names(etapas)[[length(etapas)]])
if (!de %in% names(etapas) || !ate %in% names(etapas)) {
  stop("Use nomes de etapa válidos em --de e --ate. Consulte --listar.", call. = FALSE)
}
if ("--listar" %in% argumentos) {
  cat(paste(names(etapas), etapas, sep = "\t"), sep = "\n")
  quit(save = "no", status = 0L)
}
intervalo <- seq.int(match(de, names(etapas)), match(ate, names(etapas)))
if (length(intervalo) && intervalo[[1L]] > intervalo[[length(intervalo)]]) stop("A etapa inicial vem depois da etapa final.", call. = FALSE)

ensure_dir(CFG$logs_dir)
log_path <- file.path(CFG$logs_dir, paste0("execucao_", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), ".log"))
for (indice in intervalo) {
  nome <- names(etapas)[[indice]]
  script <- file.path(CFG$scripts_dir, etapas[[indice]])
  inicio <- Sys.time()
  cat(sprintf("[%s] Iniciando %s\n", format(inicio, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), nome), file = log_path, append = TRUE)
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)), stdout = log_path, stderr = log_path)
  fim <- Sys.time()
  cat(sprintf("[%s] Encerrando %s com status %d\n", format(fim, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), nome, status), file = log_path, append = TRUE)
  if (!identical(status, 0L)) stop("A etapa ", nome, " foi interrompida. Consulte ", log_path, call. = FALSE)
}
cat("Fluxo concluído no intervalo ", de, "–", ate, ". Log: ", log_path, "\n", sep = "")
