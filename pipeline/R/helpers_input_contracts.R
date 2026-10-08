# Define as colunas que precisam estar presentes nas entradas dos revisores.
# As funções descrevem a estrutura esperada, sem preencher respostas ou decisões.

contrato_decisoes_cp14 <- function() {
  c(
    "assessment_type", "comparison_key", "final_decision", "final_rationale",
    "supporting_page_or_location", "supporting_evidence_note",
    "decision_role", "decision_date"
  )
}

contrato_decisoes_prontidao_clinica <- function() {
  c(
    "model_task_id", "field", "joint_value", "adjudication_rationale",
    "decision_role", "decision_date"
  )
}

contrato_decisoes_tehai <- function() {
  c(
    "tehai_unit_id", "subcomponent_id", "consensus_score",
    "consensus_evidence_location", "consensus_rationale", "decision_role",
    "decision_date", "approval_status"
  )
}

contrato_conformidade_texto_completo <- function() {
  c(
    "record_id", "field", "authorized_value", "authorization_basis",
    "decision_role", "decision_date"
  )
}

contrato_classificacao_visual <- function() {
  c(
    "model_task_id", "construct", "reference_standard_independence",
    "validation_category", "input_modality", "approval_status",
    "decision_role", "decision_date"
  )
}
