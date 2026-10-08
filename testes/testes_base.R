# Confere as regras centrais do código com exemplos sintéticos.
# Cada teste informa se a regra passou; uma falha interrompe a verificação.

raiz_codigo_teste <- normalizePath(Sys.getenv("AITHERMO_CODE_ROOT"), mustWork = TRUE)
dir.create(Sys.getenv("AITHERMO_DATA_ROOT"), recursive = TRUE, showWarnings = FALSE)
source(file.path(raiz_codigo_teste, "pipeline", "_bootstrap.R"))

resultados <- list()
verificar <- function(nome, expressao) {
  erro <- try(force(expressao), silent = TRUE)
  passou <- !inherits(erro, "try-error") && isTRUE(erro)
  resultados[[length(resultados) + 1L]] <<- data.frame(teste = nome, status = ifelse(passou, "PASS", "FAIL"), stringsAsFactors = FALSE)
  if (!passou) stop("Teste falhou: ", nome, if (inherits(erro, "try-error")) paste0(" — ", as.character(erro)) else "", call. = FALSE)
  invisible(TRUE)
}

verificar("sintaxe de todos os scripts R", {
  arquivos <- list.files(raiz_codigo_teste, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  !any(vapply(arquivos, function(f) inherits(try(parse(file = f), silent = TRUE), "try-error"), logical(1L)))
})

verificar("identificadores independentes da ordem", {
  base <- data.frame(record_source_id = c("fonte:c", "fonte:a", "fonte:b"), titulo = c("C", "A", "B"), stringsAsFactors = FALSE)
  a <- assign_stable_record_ids(base, "REC_TESTE")
  b <- assign_stable_record_ids(base[c(2, 3, 1), ], "REC_TESTE")
  identical(a, b) && identical(a$record_id, sprintf("REC_TESTE_%05d", 1:3))
})

verificar("chaves inválidas bloqueiam a numeração", {
  repetida <- inherits(try(assign_stable_record_ids(data.frame(record_source_id = c("a", "a")), "REC"), silent = TRUE), "try-error")
  vazia <- inherits(try(assign_stable_record_ids(data.frame(record_source_id = c("a", "")), "REC"), silent = TRUE), "try-error")
  repetida && vazia
})

verificar("normalização determinística de DOI", {
  entradas <- c("https://doi.org/10.1000/ABC.1", "doi:10.1000/abc.1", " 10.1000/abc.1 ")
  identical(unique(normalize_doi(entradas)), "10.1000/abc.1")
})

verificar("limites de similaridade de título", {
  igual <- record_jaro_winkler_similarity("thermography pain assessment", "thermography pain assessment")
  diferente <- record_jaro_winkler_similarity("thermography pain assessment", "unrelated registry protocol")
  identical(igual, 1) && diferente < CFG$fuzzy_review_threshold
})

verificar("revisores alinhados por chave", {
  r1 <- data.frame(id = c("u1", "u2"), resposta = c("yes", "no"), stringsAsFactors = FALSE)
  r2 <- data.frame(id = c("u2", "u1"), resposta = c("no", "unclear"), stringsAsFactors = FALSE)
  cmp <- comparar_revisores(r1, r2, "id", "resposta", nome = "teste")
  identical(cmp$id, c("u1", "u2")) && identical(cmp$agreement, c(FALSE, TRUE)) && identical(cmp$final_decision, c("", "no"))
})

verificar("consenso corresponde exatamente à fila", {
  r1 <- data.frame(id = "u1", resposta = "yes", stringsAsFactors = FALSE)
  r2 <- data.frame(id = "u1", resposta = "no", stringsAsFactors = FALSE)
  cmp <- comparar_revisores(r1, r2, "id", "resposta", nome = "teste")
  decisao <- data.frame(
    assessment_type = "item", comparison_key = cmp$comparison_key, final_decision = "yes",
    final_rationale = "Decisão documentada", supporting_page_or_location = "p. 1",
    supporting_evidence_note = "Trecho verificado", decision_role = "Decisão conjunta",
    decision_date = "2026-01-01", stringsAsFactors = FALSE
  )
  final <- aplicar_decisoes_conjuntas(cmp, decisao, "item", c("yes", "no"))
  bloqueia_vazia <- inherits(try(aplicar_decisoes_conjuntas(cmp, decisao[FALSE, ], "item"), silent = TRUE), "try-error")
  identical(final$final_decision, "yes") && identical(final$decision_origin, "joint_decision") && bloqueia_vazia
})

verificar("estados controlados permanecem distintos", {
  valores <- c("", "NA", "no", "not_applicable")
  length(unique(valores)) == 4L
})

verificar("árvore pública sem formatos de dados protegidos", {
  arquivos <- list.files(raiz_codigo_teste, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  arquivos <- arquivos[!grepl("(^|/)[.]git(/|$)", arquivos) & !file.info(arquivos)$isdir]
  !any(grepl("[.](pdf|xlsx?|docx?|ris|nbib|bib|zip|png|jpe?g|tiff?)$", arquivos, ignore.case = TRUE))
})

verificar("árvore pública sem caminho pessoal incorporado", {
  arquivos <- list.files(raiz_codigo_teste, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  arquivos <- arquivos[!file.info(arquivos)$isdir & grepl("[.](R|md|csv|lock|gitignore|Rprofile)$", arquivos, ignore.case = TRUE)]
  texto <- paste(unlist(lapply(arquivos, readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n")
  !grepl(normalizePath("~", mustWork = TRUE), texto, fixed = TRUE)
})

verificar("modelo TEHAI não incorpora decisões", {
  contrato <- contrato_decisoes_tehai()
  identical(
    contrato,
    c(
      "tehai_unit_id", "subcomponent_id", "consensus_score",
      "consensus_evidence_location", "consensus_rationale", "decision_role",
      "decision_date", "approval_status"
    )
  ) && !any(grepl("REC_PRIMARY", contrato))
})

verificar("árvore pública sem arquivos tabulares", {
  arquivos <- list.files(raiz_codigo_teste, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  arquivos <- arquivos[!grepl("(^|/)[.]git(/|$)", arquivos) & !file.info(arquivos)$isdir]
  !any(grepl("[.](csv|tsv|xlsx?|ods|parquet|feather)$", arquivos, ignore.case = TRUE))
})

resultado <- do.call(rbind, resultados)
print(resultado, row.names = FALSE)
cat("Todos os ", nrow(resultado), " testes passaram.\n", sep = "")
