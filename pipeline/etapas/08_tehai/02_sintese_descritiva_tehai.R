#!/usr/bin/env Rscript

# Resume as avaliações TEHAI consolidadas em contagens e distribuições.
# A síntese descreve os julgamentos existentes, sem produzir novos escores.


script_path <- function() {
  argumento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  normalizePath(sub("^--file=", "", argumento[[1L]]), mustWork = FALSE)
}
raiz_codigo <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; p <- dirname(p) }; p })
source(file.path(raiz_codigo, "pipeline", "_bootstrap.R"))

entrada <- file.path(CFG$pipeline_dir, "tehai", "final", "TEHAI_ADJUDICATION_FINAL.csv")
if (!file.exists(entrada)) stop("Base TEHAI final ausente.", call. = FALSE)
dados <- read_machine_csv(entrada)
validate_required_columns(dados, c("tehai_unit_id", "component", "subcomponent_id", "consensus_score", "approval_status", "status"), entrada)
if (!all(dados$approval_status == "human_approved" & dados$status == "complete")) stop("A base TEHAI não está integralmente aprovada.", call. = FALSE)

component_order <- c("Capability", "Utility", "Adoption")
scores <- as.character(0:3)
grid_component <- expand.grid(component = component_order, score = scores, stringsAsFactors = FALSE)
counts_component <- aggregate(list(n = dados$consensus_score), list(component = dados$component, score = dados$consensus_score), length)
component <- merge(grid_component, counts_component, by = c("component", "score"), all.x = TRUE, sort = FALSE)
component$n[is.na(component$n)] <- 0L
component$denominator <- ave(component$n, component$component, FUN = sum)

subcomponents <- unique(dados[, c("component", "subcomponent_id")])
grid_sub <- merge(subcomponents, data.frame(score = scores), by = NULL)
counts_sub <- aggregate(list(n = dados$consensus_score), list(component = dados$component, subcomponent_id = dados$subcomponent_id, score = dados$consensus_score), length)
subcomponent <- merge(grid_sub, counts_sub, by = c("component", "subcomponent_id", "score"), all.x = TRUE, sort = FALSE)
subcomponent$n[is.na(subcomponent$n)] <- 0L
subcomponent$denominator <- ave(subcomponent$n, interaction(subcomponent$component, subcomponent$subcomponent_id), FUN = sum)

esperados <- c(Capability = 174L, Utility = 145L, Adoption = 116L)
observados <- tapply(component$n, component$component, sum)
if (!identical(as.integer(observados[names(esperados)]), as.integer(esperados))) stop("Denominadores TEHAI divergentes.", call. = FALSE)
if (sum(component$n) != 435L || length(unique(dados$tehai_unit_id)) != 29L) stop("Base TEHAI incompleta.", call. = FALSE)
saida <- file.path(CFG$pipeline_dir, "tehai", "synthesis")
ensure_dir(saida)
stable_write_csv(component, file.path(saida, "TEHAI_COMPONENT_SCORE_DISTRIBUTION.csv"))
stable_write_csv(subcomponent, file.path(saida, "TEHAI_SUBCOMPONENT_SCORE_DISTRIBUTION.csv"))
stable_write_csv(data.frame(
  check = c("systems", "assessments", "component_denominators", "global_score_absent"),
  observed = c(29L, 435L, "174/145/116", "yes"),
  expected = c(29L, 435L, "174/145/116", "yes"),
  status = "PASS",
  stringsAsFactors = FALSE
), file.path(saida, "TEHAI_SYNTHESIS_VALIDATION.csv"))
cat("Síntese TEHAI concluída sem escore global ou ranking.\n")
