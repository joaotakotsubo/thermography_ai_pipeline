# Prepara os caminhos e as funções compartilhadas pela geração de figuras.
# Os gráficos usam as tabelas e classificações já aprovadas.


argumento_arquivo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(argumento_arquivo)) stop("Execute esta etapa com Rscript.", call. = FALSE)

p <- dirname(normalizePath(argumento_arquivo[[1L]], mustWork = TRUE))
repeat {
  if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break
  anterior <- dirname(p)
  if (identical(anterior, p)) stop("Bootstrap não localizado.", call. = FALSE)
  p <- anterior
}
source(file.path(p, "pipeline", "_bootstrap.R"))

PASTA_RAIZ <- PROJECT_ROOT
caminho <- function(...) file.path(PROJECT_ROOT, ...)
criar_pasta <- function(path) ensure_dir(path)
ler_csv_texto <- function(path) read_machine_csv(path)
gravar_csv_estavel <- function(x, path) stable_write_csv(x, path)
exigir_colunas <- function(x, cols, nome = "tabela") validate_required_columns(x, cols, nome)
verificar_igual <- function(observado, esperado, mensagem) {
  if (!identical(observado, esperado)) stop(mensagem, call. = FALSE)
  invisible(TRUE)
}
registrar_execucao <- function(etapa, inicio, arquivos = character()) {
  ensure_dir(CFG$logs_dir)
  linha <- data.frame(
    etapa = etapa,
    inicio_utc = format(inicio, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    fim_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    arquivos = paste(project_relative_path(arquivos), collapse = ";"),
    stringsAsFactors = FALSE
  )
  destino <- file.path(CFG$logs_dir, "execucoes_figuras.csv")
  anterior <- if (file.exists(destino)) read_machine_csv(destino) else linha[FALSE, ]
  stable_write_csv(rbind(anterior, linha), destino)
  invisible(linha)
}
