# Compara as avaliações dos revisores usando as mesmas chaves de identificação.
# Os acordos são preservados; as discordâncias exigem uma decisão conjunta documentada.


texto_limpo <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

chave_estavel <- function(dados, colunas) {
  validate_required_columns(dados, colunas, "dados para chave estável")
  partes <- lapply(dados[colunas], texto_limpo)
  do.call(paste, c(partes, sep = "\u001f"))
}

validar_chave_unica <- function(dados, colunas, nome) {
  chave <- chave_estavel(dados, colunas)
  if (any(!nzchar(chave))) stop(nome, ": há chave vazia.", call. = FALSE)
  if (anyDuplicated(chave)) {
    repetidas <- unique(chave[duplicated(chave)])
    stop(nome, ": há chaves duplicadas: ", paste(utils::head(repetidas, 10L), collapse = "; "), call. = FALSE)
  }
  invisible(chave)
}

ler_tabela_revisor <- function(caminho, aba = NULL, pular = 0L) {
  if (!file.exists(caminho)) stop("Arquivo de revisor ausente: ", caminho, call. = FALSE)
  extensao <- tolower(tools::file_ext(caminho))
  if (extensao == "csv") return(read_machine_csv(caminho))
  if (!extensao %in% c("xlsx", "xls")) stop("Formato de revisor não suportado: ", extensao, call. = FALSE)
  if (!requireNamespace("readxl", quietly = TRUE)) stop("O pacote readxl é necessário para planilhas.", call. = FALSE)
  as.data.frame(
    readxl::read_excel(caminho, sheet = aba, skip = pular, col_types = "text"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

comparar_revisores <- function(revisor_1, revisor_2, colunas_chave, coluna_resposta,
                               colunas_contexto = character(), nome = "avaliação") {
  validate_required_columns(revisor_1, c(colunas_chave, coluna_resposta), paste(nome, "Revisor 1"))
  validate_required_columns(revisor_2, c(colunas_chave, coluna_resposta), paste(nome, "Revisor 2"))
  chave_1 <- validar_chave_unica(revisor_1, colunas_chave, paste(nome, "Revisor 1"))
  chave_2 <- validar_chave_unica(revisor_2, colunas_chave, paste(nome, "Revisor 2"))
  if (!setequal(chave_1, chave_2)) {
    faltam_1 <- setdiff(chave_2, chave_1)
    faltam_2 <- setdiff(chave_1, chave_2)
    stop(
      nome, ": os revisores não avaliaram exatamente as mesmas chaves. ",
      "Ausentes no Revisor 1: ", length(faltam_1),
      "; ausentes no Revisor 2: ", length(faltam_2), ".",
      call. = FALSE
    )
  }

  ordem <- order(chave_1, method = "radix")
  revisor_1 <- revisor_1[ordem, , drop = FALSE]
  chave_1 <- chave_1[ordem]
  revisor_2 <- revisor_2[match(chave_1, chave_2), , drop = FALSE]

  resposta_1 <- texto_limpo(revisor_1[[coluna_resposta]])
  resposta_2 <- texto_limpo(revisor_2[[coluna_resposta]])
  if (any(!nzchar(resposta_1)) || any(!nzchar(resposta_2))) {
    stop(nome, ": há respostas vazias; o fluxo foi colocado em espera.", call. = FALSE)
  }

  contexto <- unique(c(colunas_chave, intersect(colunas_contexto, names(revisor_1))))
  saida <- revisor_1[, contexto, drop = FALSE]
  saida$comparison_key <- chave_1
  saida$reviewer_1_response <- resposta_1
  saida$reviewer_2_response <- resposta_2
  saida$agreement <- resposta_1 == resposta_2
  saida$final_decision <- ifelse(saida$agreement, resposta_1, "")
  saida$decision_origin <- ifelse(saida$agreement, "exact_agreement", "pending_joint_decision")
  saida
}

aplicar_decisoes_conjuntas <- function(comparacao, decisoes, tipo_avaliacao,
                                       valores_permitidos = NULL) {
  validate_required_columns(
    comparacao,
    c("comparison_key", "reviewer_1_response", "reviewer_2_response", "agreement", "final_decision"),
    paste(tipo_avaliacao, "comparação")
  )
  colunas_decisao <- c(
    "assessment_type", "comparison_key", "final_decision", "final_rationale",
    "supporting_page_or_location", "supporting_evidence_note", "decision_role", "decision_date"
  )
  validate_required_columns(decisoes, colunas_decisao, paste(tipo_avaliacao, "decisões conjuntas"))
  decisoes <- as_character_frame(decisoes[, colunas_decisao, drop = FALSE])
  decisoes <- decisoes[texto_limpo(decisoes$assessment_type) == tipo_avaliacao, , drop = FALSE]
  pendentes <- comparacao$comparison_key[!comparacao$agreement]

  if (anyDuplicated(decisoes$comparison_key)) stop(tipo_avaliacao, ": decisão conjunta duplicada.", call. = FALSE)
  faltantes <- setdiff(pendentes, decisoes$comparison_key)
  extras <- setdiff(decisoes$comparison_key, pendentes)
  if (length(faltantes) || length(extras)) {
    stop(
      tipo_avaliacao, ": a tabela conjunta não corresponde à fila congelada. ",
      "Faltantes: ", length(faltantes), "; extras: ", length(extras), ".",
      call. = FALSE
    )
  }
  campos_obrigatorios <- c("final_decision", "final_rationale", "decision_role", "decision_date")
  for (campo in campos_obrigatorios) {
    if (any(!nzchar(texto_limpo(decisoes[[campo]])))) stop(tipo_avaliacao, ": campo conjunto vazio: ", campo, call. = FALSE)
  }
  if (any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", decisoes$decision_date))) {
    stop(tipo_avaliacao, ": decision_date deve usar AAAA-MM-DD.", call. = FALSE)
  }
  if (!is.null(valores_permitidos) && any(!decisoes$final_decision %in% valores_permitidos)) {
    stop(tipo_avaliacao, ": decisão fora do vocabulário aprovado.", call. = FALSE)
  }

  indice <- match(comparacao$comparison_key, decisoes$comparison_key)
  discordante <- !comparacao$agreement
  comparacao$final_decision[discordante] <- decisoes$final_decision[indice[discordante]]
  comparacao$decision_origin[discordante] <- "joint_decision"
  comparacao$final_rationale <- ""
  comparacao$supporting_page_or_location <- ""
  comparacao$supporting_evidence_note <- ""
  comparacao$decision_role <- ifelse(comparacao$agreement, "Revisores 1 e 2", "")
  comparacao$decision_date <- ""
  comparacao$final_rationale[discordante] <- decisoes$final_rationale[indice[discordante]]
  comparacao$supporting_page_or_location[discordante] <- decisoes$supporting_page_or_location[indice[discordante]]
  comparacao$supporting_evidence_note[discordante] <- decisoes$supporting_evidence_note[indice[discordante]]
  comparacao$decision_role[discordante] <- decisoes$decision_role[indice[discordante]]
  comparacao$decision_date[discordante] <- decisoes$decision_date[indice[discordante]]
  comparacao
}

gravar_fila_em_espera <- function(comparacao, destino, tipo_avaliacao) {
  fila <- comparacao[!comparacao$agreement, , drop = FALSE]
  stable_write_csv(fila, destino)
  if (nrow(fila)) {
    stop(
      tipo_avaliacao, ": há ", nrow(fila),
      " discordância(s). Preencha a tabela de decisão conjunta e execute novamente.",
      call. = FALSE
    )
  }
  invisible(fila)
}
