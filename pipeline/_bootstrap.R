# Localiza a configuração e carrega as funções usadas pelas etapas.
# Também cria as pastas de trabalho na raiz de dados escolhida para a execução.


arquivo_bootstrap <- function() {
  argumento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(argumento)) return(NA_character_)
  normalizePath(sub("^--file=", "", argumento[[1L]]), mustWork = FALSE)
}

localizar_configuracao <- function() {
  candidatos <- c(getwd())
  ativo <- arquivo_bootstrap()
  if (!is.na(ativo)) candidatos <- c(candidatos, dirname(ativo))
  candidatos <- unique(normalizePath(candidatos, mustWork = FALSE))
  for (candidato in candidatos) {
    atual <- candidato
    repeat {
      arquivo <- file.path(atual, "pipeline", "config.R")
      if (file.exists(arquivo)) return(arquivo)
      arquivo <- file.path(atual, "config.R")
      if (file.exists(arquivo) && basename(atual) == "pipeline") return(arquivo)
      anterior <- dirname(atual)
      if (identical(anterior, atual)) break
      atual <- anterior
    }
  }
  stop("Não foi possível localizar pipeline/config.R.", call. = FALSE)
}

source(localizar_configuracao(), chdir = TRUE)

auxiliares <- c(
  "helpers_core.R",
  "helpers_checkpoint.R",
  "helpers_parsers.R",
  "helpers_text_normalization.R",
  "helpers_dedup.R",
  "helpers_prisma.R",
  "helpers_screening.R",
  "helpers_reporting.R",
  "helpers_reviewer_reconciliation.R",
  "helpers_input_contracts.R"
)

for (nome in auxiliares) {
  caminho_auxiliar <- file.path(CFG$r_dir, nome)
  if (!file.exists(caminho_auxiliar)) stop("Módulo auxiliar ausente: ", caminho_auxiliar, call. = FALSE)
  source(caminho_auxiliar, chdir = TRUE)
}

invisible(vapply(PIPELINE_DIRS, ensure_dir, character(1L)))
