# Registra as assinaturas SHA-256 das saídas e as informações da sessão R.
# O manifesto permite conferir a identidade dos arquivos, não a validade científica das decisões.


inicio_etapa <- Sys.time()
argumento_arquivo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(argumento_arquivo)) stop("Execute este arquivo com Rscript.", call. = FALSE)
p <- dirname(normalizePath(argumento_arquivo[[1L]], mustWork = TRUE))
repeat {
  if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break
  anterior <- dirname(p)
  if (identical(anterior, p)) stop("Bootstrap não localizado.", call. = FALSE)
  p <- anterior
}
source(file.path(p, "pipeline", "_bootstrap.R"))
if (!requireNamespace("digest", quietly = TRUE)) stop("O pacote digest é necessário.", call. = FALSE)

PASTA_RAIZ <- PROJECT_ROOT
caminho <- function(...) file.path(PROJECT_ROOT, ...)
gravar_csv_estavel <- function(x, path) stable_write_csv(x, path)
registrar_execucao <- function(etapa, inicio, arquivos = character()) {
  ensure_dir(CFG$logs_dir)
  destino <- file.path(CFG$logs_dir, "execucoes_integridade.csv")
  linha <- data.frame(
    etapa = etapa,
    inicio_utc = format(inicio, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    fim_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    arquivos = paste(project_relative_path(arquivos), collapse = ";"),
    stringsAsFactors = FALSE
  )
  anterior <- if (file.exists(destino)) read_machine_csv(destino) else linha[FALSE, ]
  stable_write_csv(rbind(anterior, linha), destino)
}

ensure_dir(caminho("documentacao"))
writeLines(capture.output(sessionInfo()), caminho("documentacao", "AMBIENTE_DE_EXECUCAO.txt"), useBytes = TRUE)
arquivos <- list.files(PASTA_RAIZ, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                       no.. = TRUE, include.dirs = FALSE)
arquivos <- arquivos[!grepl("(^|/)MANIFESTO_SHA256\\.csv$", arquivos)]
arquivos <- arquivos[!grepl("(^|/)\\.git(/|$)", arquivos)]
arquivos <- arquivos[!grepl("(^|/)logs(/|$)", arquivos)]
relativos <- substring(arquivos, nchar(PASTA_RAIZ) + 2L)
manifesto <- data.frame(
  arquivo = relativos,
  bytes = file.info(arquivos)$size,
  sha256 = vapply(arquivos, digest::digest, character(1L), file = TRUE, algo = "sha256"),
  stringsAsFactors = FALSE
)
manifesto <- manifesto[order(manifesto$arquivo, method = "radix"), ]
gravar_csv_estavel(manifesto, caminho("MANIFESTO_SHA256.csv"))
registrar_execucao("11_manifesto_reproducibilidade", inicio_etapa,
                   c(caminho("MANIFESTO_SHA256.csv"), caminho("documentacao", "AMBIENTE_DE_EXECUCAO.txt")))
