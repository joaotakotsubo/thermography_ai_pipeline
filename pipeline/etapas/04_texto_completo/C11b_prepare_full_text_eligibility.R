#!/usr/bin/env Rscript

# Prepara os materiais para avaliar a elegibilidade dos textos completos.
# A decisão e a justificativa de exclusão continuam a cargo dos revisores.


script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("Run this script with Rscript so its path can be resolved.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = FALSE)
}

source(file.path(CODE_ROOT <- local({ p <- dirname(script_path()); repeat { if (file.exists(file.path(p, "pipeline", "_bootstrap.R"))) break; q <- dirname(p); if (identical(q, p)) stop("Bootstrap não localizado.", call. = FALSE); p <- q }; p }), "pipeline", "_bootstrap.R"))

checkpoint_gate("CP11")

RETRIEVAL_TRACKING_COLUMNS <- c(
  "record_id",
  "title_duplicate_group_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "pmid",
  "url",
  "publication_type",
  "candidate_flags",
  "source_databases",
  "source_files",
  "reason_for_full_text",
  "expected_pdf_filename",
  "expected_pdf_path",
  "pdf_present",
  "retrieval_status",
  "retrieval_attempt_date_local",
  "online_search_status",
  "author_contact_status",
  "library_request_status",
  "retrieval_sources_checked",
  "retrieval_note",
  "updated_by",
  "updated_at_local"
)

FINAL_DECISION_PROVENANCE_COLUMNS <- c(
  "record_id",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "agreement_statuses",
  "pre_adjudicated_decisions",
  "pre_adjudication_reasons",
  "pre_adjudicator_notes",
  "decision_rule"
)

FULL_TEXT_ELIGIBILITY_COLUMNS <- c(
  "record_id",
  "title_duplicate_group_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "pmid",
  "url",
  "publication_type",
  "candidate_flags",
  "source_databases",
  "reason_for_full_text",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "agreement_statuses",
  "pre_adjudicated_decisions",
  "pre_adjudication_reasons",
  "pre_adjudicator_notes",
  "decision_rule",
  "pdf_filename",
  "pdf_path",
  "full_text_decision",
  "primary_exclusion_reason",
  "secondary_exclusion_reason",
  "decision_notes",
  "reviewer",
  "decision_date"
)

FULL_TEXT_UNAVAILABLE_COLUMNS <- c(
  "record_id",
  "title",
  "authors",
  "year",
  "journal_or_source",
  "doi",
  "url",
  "retrieval_status",
  "online_search_status",
  "author_contact_status",
  "retrieval_note",
  "reviewer_1_decisions",
  "reviewer_2_decisions",
  "pre_adjudicated_decisions",
  "decision_rule"
)

FULL_TEXT_ELIGIBILITY_CODEBOOK_COLUMNS <- c(
  "field",
  "allowed_value",
  "meaning",
  "required_when"
)

FULL_TEXT_REVIEWER_PACKAGE_COLUMNS <- c(
  "reviewer_id",
  "reviewer_label",
  "reviewer_role",
  "package_dir",
  "workbook_path",
  "csv_path",
  "handoff_path",
  "prompt_path",
  "workbook_status",
  "csv_status"
)

full_text_decision_values <- function() {
  c("include_for_extraction", "exclude_full_text", "unclear_needs_discussion")
}

full_text_exclusion_reason_values <- function() {
  c(
    "no_ai_model",
    "no_thermal_input",
    "no_pain_target",
    "not_human",
    "not_original",
    "abstract_only",
    "registry_no_results",
    "no_extractable_model_data",
    "wrong_population",
    "wrong_outcome",
    "duplicate",
    "language_unavailable",
    "other"
  )
}

build_eligibility_template <- function(tracking) {
  retrieved <- tracking[tracking$retrieval_status == "retrieved" & tracking$pdf_present == "yes", , drop = FALSE]
  retrieved <- retrieved[order(retrieved$record_id, method = "radix"), , drop = FALSE]

  out <- data.frame(
    record_id = retrieved$record_id,
    title_duplicate_group_id = retrieved$title_duplicate_group_id,
    title = retrieved$title,
    authors = retrieved$authors,
    year = retrieved$year,
    journal_or_source = retrieved$journal_or_source,
    doi = retrieved$doi,
    pmid = retrieved$pmid,
    url = retrieved$url,
    publication_type = retrieved$publication_type,
    candidate_flags = retrieved$candidate_flags,
    source_databases = retrieved$source_databases,
    reason_for_full_text = retrieved$reason_for_full_text,
    reviewer_1_decisions = retrieved$reviewer_1_decisions,
    reviewer_2_decisions = retrieved$reviewer_2_decisions,
    agreement_statuses = retrieved$agreement_statuses,
    pre_adjudicated_decisions = retrieved$pre_adjudicated_decisions,
    pre_adjudication_reasons = retrieved$pre_adjudication_reasons,
    pre_adjudicator_notes = retrieved$pre_adjudicator_notes,
    decision_rule = retrieved$decision_rule,
    pdf_filename = retrieved$expected_pdf_filename,
    pdf_path = retrieved$expected_pdf_path,
    full_text_decision = "",
    primary_exclusion_reason = "",
    secondary_exclusion_reason = "",
    decision_notes = "",
    reviewer = "",
    decision_date = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out[, FULL_TEXT_ELIGIBILITY_COLUMNS, drop = FALSE]
}

build_unavailable_reference <- function(tracking) {
  unavailable <- tracking[tracking$retrieval_status != "retrieved" | tracking$pdf_present != "yes", , drop = FALSE]
  unavailable <- unavailable[order(unavailable$record_id, method = "radix"), , drop = FALSE]

  out <- data.frame(
    record_id = unavailable$record_id,
    title = unavailable$title,
    authors = unavailable$authors,
    year = unavailable$year,
    journal_or_source = unavailable$journal_or_source,
    doi = unavailable$doi,
    url = unavailable$url,
    retrieval_status = unavailable$retrieval_status,
    online_search_status = unavailable$online_search_status,
    author_contact_status = unavailable$author_contact_status,
    retrieval_note = unavailable$retrieval_note,
    reviewer_1_decisions = unavailable$reviewer_1_decisions,
    reviewer_2_decisions = unavailable$reviewer_2_decisions,
    pre_adjudicated_decisions = unavailable$pre_adjudicated_decisions,
    decision_rule = unavailable$decision_rule,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out[, FULL_TEXT_UNAVAILABLE_COLUMNS, drop = FALSE]
}

build_codebook <- function() {
  data.frame(
    field = c(
      rep("full_text_decision", length(full_text_decision_values())),
      rep("primary_exclusion_reason", length(full_text_exclusion_reason_values())),
      "secondary_exclusion_reason",
      "decision_notes",
      "reviewer",
      "decision_date"
    ),
    allowed_value = c(
      "include_for_extraction",
      "exclude_full_text",
      "unclear_needs_discussion",
      full_text_exclusion_reason_values(),
      "same controlled values as primary_exclusion_reason; optional",
      "free text",
      "reviewer name or initials",
      "YYYY-MM-DD"
    ),
    meaning = c(
      "Full text meets the review scope and should proceed to data extraction.",
      "Full text does not meet one or more eligibility criteria.",
      "Reviewer cannot decide from the full text without discussion or second opinion.",
      "No eligible AI, machine learning, deep learning, computer vision, classifier, predictive model, or comparable computational method.",
      "No eligible infrared thermography, thermal imaging, thermal camera, or thermal-image-derived input.",
      "No eligible pain outcome, pain assessment, painful condition, analgesia/pain-control assessment, or pain-related clinical target.",
      "Animal, phantom, simulation-only, in vitro, or non-human-only study.",
      "Review, editorial, protocol, commentary, dataset note, or non-primary report.",
      "Conference abstract only or bibliographic abstract without a complete full paper.",
      "Protocol, trial registry, or registry-derived record without extractable results.",
      "No extractable model, validation, diagnostic, classification, segmentation, or performance information relevant to the review.",
      "Population or clinical setting is outside the review question.",
      "Outcome, target, or modeled construct is outside the review question.",
      "Same study/report superseded by another retained full text.",
      "Full text exists but cannot be assessed because language/translation is unavailable.",
      "Use only when no controlled reason fits; explain in decision_notes.",
      "Optional additional exclusion reason.",
      "Required explanation for unclear decisions, other, or any nuance important for audit.",
      "Required for human accountability.",
      "Required date of human eligibility decision."
    ),
    required_when = c(
      "always",
      "always",
      "always",
      rep("full_text_decision == exclude_full_text", length(full_text_exclusion_reason_values())),
      "optional",
      "required for unclear_needs_discussion, other, or nuanced decisions",
      "always",
      "always"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, FULL_TEXT_ELIGIBILITY_CODEBOOK_COLUMNS, drop = FALSE]
}

write_full_text_eligibility_xlsx <- function(template, unavailable, codebook, path) {
  require_openxlsx()
  ensure_parent_dir(path)

  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#D9EAF7",
    border = "Bottom",
    halign = "left",
    valign = "top"
  )
  body_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  editable_style <- openxlsx::createStyle(fgFill = "#FFF3CD", wrapText = TRUE, valign = "top")

  template <- xlsx_truncate_frame(template)
  unavailable <- xlsx_truncate_frame(unavailable)
  codebook <- xlsx_truncate_frame(codebook)

  openxlsx::addWorksheet(workbook, "eligibility", gridLines = TRUE)
  openxlsx::writeData(workbook, "eligibility", template, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::addStyle(workbook, "eligibility", header_style, rows = 1L, cols = seq_len(ncol(template)), gridExpand = TRUE)
  if (nrow(template) > 0L) {
    rows <- seq_len(nrow(template)) + 1L
    openxlsx::addStyle(workbook, "eligibility", body_style, rows = rows, cols = seq_len(ncol(template)), gridExpand = TRUE)
    editable_cols <- match(c("full_text_decision", "primary_exclusion_reason", "secondary_exclusion_reason", "decision_notes", "reviewer", "decision_date"), names(template))
    openxlsx::addStyle(workbook, "eligibility", editable_style, rows = rows, cols = editable_cols, gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::freezePane(workbook, "eligibility", firstActiveRow = 2L, firstActiveCol = 2L)
  openxlsx::addFilter(workbook, "eligibility", row = 1L, cols = seq_len(ncol(template)))
  eligibility_widths <- pmin(pmax(nchar(names(template), type = "chars") + 2L, 12L), 42L)
  openxlsx::setColWidths(workbook, "eligibility", cols = seq_len(ncol(template)), widths = eligibility_widths)

  openxlsx::addWorksheet(workbook, "unavailable_reference", gridLines = TRUE)
  openxlsx::writeData(workbook, "unavailable_reference", unavailable, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::addStyle(workbook, "unavailable_reference", header_style, rows = 1L, cols = seq_len(ncol(unavailable)), gridExpand = TRUE)
  if (nrow(unavailable) > 0L) {
    openxlsx::addStyle(workbook, "unavailable_reference", body_style, rows = seq_len(nrow(unavailable)) + 1L, cols = seq_len(ncol(unavailable)), gridExpand = TRUE)
  }
  openxlsx::freezePane(workbook, "unavailable_reference", firstActiveRow = 2L)
  openxlsx::addFilter(workbook, "unavailable_reference", row = 1L, cols = seq_len(ncol(unavailable)))
  unavailable_widths <- pmin(pmax(nchar(names(unavailable), type = "chars") + 2L, 12L), 42L)
  openxlsx::setColWidths(workbook, "unavailable_reference", cols = seq_len(ncol(unavailable)), widths = unavailable_widths)

  openxlsx::addWorksheet(workbook, "codebook", gridLines = TRUE)
  openxlsx::writeData(workbook, "codebook", codebook, startRow = 1L, startCol = 1L, colNames = TRUE)
  openxlsx::addStyle(workbook, "codebook", header_style, rows = 1L, cols = seq_len(ncol(codebook)), gridExpand = TRUE)
  if (nrow(codebook) > 0L) {
    openxlsx::addStyle(workbook, "codebook", body_style, rows = seq_len(nrow(codebook)) + 1L, cols = seq_len(ncol(codebook)), gridExpand = TRUE)
  }
  openxlsx::freezePane(workbook, "codebook", firstActiveRow = 2L)
  openxlsx::addFilter(workbook, "codebook", row = 1L, cols = seq_len(ncol(codebook)))
  openxlsx::setColWidths(workbook, "codebook", cols = seq_len(ncol(codebook)), widths = c(30, 42, 80, 34))

  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  invisible(path)
}

full_text_reviewer_specs <- function() {
  data.frame(
    reviewer_key = c("reviewer_1", "reviewer_2"),
    reviewer_id = c("reviewer_1", "reviewer_2"),
    reviewer_label = c("Revisor 1", "Revisor 2"),
    reviewer_role = c("Revisor 1", "Revisor 2"),
    workbook_name = c(
      "full_text_eligibility_reviewer_1.xlsx",
      "full_text_eligibility_reviewer_2.xlsx"
    ),
    csv_name = c(
      "full_text_eligibility_reviewer_1.csv",
      "full_text_eligibility_reviewer_2.csv"
    ),
    handoff_name = c(
      "HANDOFF_full_text_eligibility_reviewer_1.md",
      "HANDOFF_full_text_eligibility_reviewer_2.md"
    ),
    prompt_name = c(
      "PROMPT_full_text_eligibility_reviewer_1.md",
      "PROMPT_full_text_eligibility_reviewer_2.md"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

write_utf8_lines <- function(lines, path) {
  ensure_parent_dir(path)
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

build_reviewer_template <- function(template, reviewer_label) {
  out <- template
  out$reviewer <- reviewer_label
  out
}

format_allowed_values <- function(values) {
  paste0("- `", values, "`", collapse = "\n")
}

reviewer_handoff_lines <- function(spec, workbook_path, prompt_path, n_eligibility, n_unavailable) {
  c(
    paste0("# Handoff - Full-Text Eligibility - ", spec$reviewer_label),
    "",
    "## Objective",
    "",
    paste0(
      "Act as an independent full-text eligibility reviewer for the AI thermography pain systematic review. ",
      "This file is assigned to ", spec$reviewer_label, ". Do not consult or harmonize with the other reviewer during independent review."
    ),
    "",
    "## Scope",
    "",
    "Include a study at full-text eligibility only when the complete article supports all core scope elements:",
    "",
    "- Human participants or human clinical data.",
    "- Infrared thermography, thermal imaging, thermal camera, or thermal-image-derived input.",
    "- AI, machine learning, deep learning, computer vision, classifier, predictive model, segmentation, detection, or comparable computational analysis.",
    "- Pain, pain assessment, analgesia/pain control, or a painful clinical condition as a relevant clinical target.",
    "",
    "## Assigned Workbook",
    "",
    paste0("- Workbook: `", project_relative_path(workbook_path), "`"),
    paste0("- Prompt file: `", project_relative_path(prompt_path), "`"),
    paste0("- Records with retrieved PDFs for review: ", n_eligibility),
    paste0("- Records unavailable after documented retrieval: ", n_unavailable, " (reference only; do not adjudicate these in the reviewer workbook)."),
    "",
    "## What To Fill",
    "",
    "In the `eligibility` sheet, fill only the yellow decision fields for the PDF being reviewed:",
    "",
    "- `full_text_decision`",
    "- `primary_exclusion_reason`",
    "- `secondary_exclusion_reason`",
    "- `decision_notes`",
    "- `reviewer`",
    "- `decision_date`",
    "",
    "Do not edit bibliographic/provenance columns. Do not edit `unavailable_reference` or `codebook`.",
    "",
    "## Decision Values",
    "",
    "Allowed `full_text_decision` values:",
    "",
    format_allowed_values(full_text_decision_values()),
    "",
    "Allowed exclusion reason codes are listed in the `codebook` sheet and in the prompt file.",
    "",
    "## Important Boundary",
    "",
    "This is full-text eligibility screening, not final data extraction and not quality/risk-of-bias assessment. Extract only the minimum evidence needed to decide eligibility and record a concise rationale in `decision_notes`.",
    "",
    "## Next Pipeline Step",
    "",
    "After both independent reviewer workbooks are completed, the pipeline will validate allowed values and generate a full-text reconciliation/conflict file for human adjudication.",
    ""
  )
}

reviewer_prompt_lines <- function(spec) {
  c(
    paste0("# Prompt - ", spec$reviewer_label, " - Full-Text Eligibility"),
    "",
    "Use o prompt abaixo no ambiente independente correspondente antes de disponibilizar cada PDF.",
    "",
    "```text",
    paste0("Você atuará como ", spec$reviewer_label, " em uma revisão sistemática sobre inteligência artificial aplicada à termografia infravermelha/thermal imaging para avaliação, detecção, monitoramento ou análise de dor ou condições dolorosas em humanos."),
    "",
    "Tarefa: avaliar elegibilidade em texto completo, de forma independente, usando exclusivamente o PDF completo enviado e os metadados/record_id informados pelo usuário. Não use conhecimento externo, não pesquise na internet e não tente harmonizar sua decisão com outro revisor.",
    "",
    "Esta etapa NÃO é extração final de dados, NÃO é avaliação de qualidade metodológica e NÃO é análise de risco de viés. Extraia do artigo apenas as informações mínimas necessárias para decidir elegibilidade e justificar a decisão.",
    "",
    "Escopo de inclusão: inclua somente se o texto completo demonstrar todos os seguintes elementos:",
    "1. estudo com participantes humanos ou dados clínicos humanos;",
    "2. uso de termografia infravermelha, imagem térmica, câmera térmica ou variável derivada de imagem térmica;",
    "3. uso de IA, machine learning, deep learning, computer vision, classificador, modelo preditivo, segmentação, detecção ou análise computacional comparável;",
    "4. dor, avaliação de dor, analgesia/controle de dor ou condição clínica dolorosa como alvo clínico relevante.",
    "",
    "Valores permitidos para full_text_decision:",
    paste(full_text_decision_values(), collapse = "; "),
    "",
    "Use include_for_extraction quando TODOS os elementos do escopo estiverem claramente presentes no texto completo.",
    "Use exclude_full_text quando pelo menos um critério essencial estiver ausente ou quando o texto completo for inelegível.",
    "Use unclear_needs_discussion quando a elegibilidade não puder ser decidida com segurança a partir do PDF.",
    "",
    "Códigos permitidos para primary_exclusion_reason e secondary_exclusion_reason:",
    paste(full_text_exclusion_reason_values(), collapse = "; "),
    "",
    "Definições resumidas dos códigos de exclusão:",
    "- no_ai_model: sem IA/modelo computacional elegível.",
    "- no_thermal_input: sem termografia infravermelha/imagem térmica elegível.",
    "- no_pain_target: sem dor, condição dolorosa ou alvo clínico relacionado à dor.",
    "- not_human: animal, fantoma, simulação, in vitro ou sem dados humanos.",
    "- not_original: revisão, editorial, protocolo, comentário, nota de dataset ou relatório não primário.",
    "- abstract_only: resumo de congresso/bibliográfico sem artigo completo avaliável.",
    "- registry_no_results: registro/protocolo de ensaio sem resultados extraíveis.",
    "- no_extractable_model_data: sem informação extraível sobre modelo, validação, classificação, segmentação ou desempenho relevante.",
    "- wrong_population: população/contexto clínico fora da pergunta.",
    "- wrong_outcome: desfecho/alvo/modelo fora da pergunta.",
    "- duplicate: mesmo estudo substituído por outro texto completo retido.",
    "- language_unavailable: texto existe, mas não pode ser avaliado por barreira de idioma/tradução.",
    "- other: usar apenas quando nenhum código se aplicar; explicar em decision_notes.",
    "",
    "Para cada PDF enviado, responda somente com uma tabela de uma linha contendo exatamente estas colunas:",
    "record_id | full_text_decision | primary_exclusion_reason | secondary_exclusion_reason | decision_notes | reviewer | decision_date",
    "",
    paste0("Preencha reviewer exatamente como: ", spec$reviewer_label),
    "Preencha decision_date no formato YYYY-MM-DD.",
    "Se full_text_decision for include_for_extraction, deixe primary_exclusion_reason e secondary_exclusion_reason em branco.",
    "Se full_text_decision for exclude_full_text, preencha primary_exclusion_reason obrigatoriamente e secondary_exclusion_reason apenas se houver um segundo motivo real.",
    "Se full_text_decision for unclear_needs_discussion, explique objetivamente a incerteza em decision_notes.",
    "",
    "decision_notes deve ser curta, auditável e baseada no texto completo. Indique a evidência decisiva sem copiar trechos longos do artigo. Se possível, mencione seção/página de forma breve.",
    "",
    "Se o record_id não for informado e não puder ser inferido com segurança pelo nome do arquivo PDF, não decida: peça o record_id correto.",
    "Não invente dados ausentes. Não altere critérios. Em dúvida real, use unclear_needs_discussion.",
    "```",
    ""
  )
}

write_reviewer_package <- function(template, unavailable, codebook, spec, eligibility_dir) {
  package_dir <- ensure_dir(file.path(eligibility_dir, "reviewer_inputs", spec$reviewer_key))
  reviewer_template <- build_reviewer_template(template, spec$reviewer_label)

  workbook_path <- file.path(package_dir, spec$workbook_name)
  csv_path <- file.path(package_dir, spec$csv_name)
  handoff_path <- file.path(package_dir, spec$handoff_name)
  prompt_path <- file.path(package_dir, spec$prompt_name)

  csv_status <- "created"
  if (file.exists(csv_path)) {
    csv_status <- "preserved_existing"
  } else {
    stable_write_csv(reviewer_template, csv_path, FULL_TEXT_ELIGIBILITY_COLUMNS)
  }

  workbook_status <- "created"
  if (file.exists(workbook_path)) {
    workbook_status <- "preserved_existing"
  } else {
    write_full_text_eligibility_xlsx(reviewer_template, unavailable, codebook, workbook_path)
  }

  write_utf8_lines(
    reviewer_handoff_lines(
      spec = spec,
      workbook_path = workbook_path,
      prompt_path = prompt_path,
      n_eligibility = nrow(template),
      n_unavailable = nrow(unavailable)
    ),
    handoff_path
  )
  write_utf8_lines(reviewer_prompt_lines(spec), prompt_path)

  data.frame(
    reviewer_id = spec$reviewer_id,
    reviewer_label = spec$reviewer_label,
    reviewer_role = spec$reviewer_role,
    package_dir = project_relative_path(package_dir),
    workbook_path = project_relative_path(workbook_path),
    csv_path = project_relative_path(csv_path),
    handoff_path = project_relative_path(handoff_path),
    prompt_path = project_relative_path(prompt_path),
    workbook_status = workbook_status,
    csv_status = csv_status,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )[, FULL_TEXT_REVIEWER_PACKAGE_COLUMNS, drop = FALSE]
}

tracking_path <- file.path(CFG$pipeline_dir, "full_text", "retrieval_tracking", "full_text_retrieval_tracking.csv")
final_decisions_path <- file.path(CFG$pipeline_dir, "full_text", "title_abstract_final_decisions.csv")
eligibility_dir <- ensure_dir(file.path(CFG$pipeline_dir, "full_text", "eligibility"))

if (!file.exists(tracking_path)) {
  stop("Required CP11 tracking table is missing: ", project_relative_path(tracking_path), call. = FALSE)
}
if (!file.exists(final_decisions_path)) {
  stop("Required title/abstract final decision table is missing: ", project_relative_path(final_decisions_path), call. = FALSE)
}

tracking <- read_machine_csv(tracking_path)
validate_required_columns(tracking, RETRIEVAL_TRACKING_COLUMNS, tracking_path)
tracking <- as_character_frame(tracking[, RETRIEVAL_TRACKING_COLUMNS, drop = FALSE])

final_decisions <- read_machine_csv(final_decisions_path)
validate_required_columns(final_decisions, FINAL_DECISION_PROVENANCE_COLUMNS, final_decisions_path)
final_decisions <- as_character_frame(final_decisions[, FINAL_DECISION_PROVENANCE_COLUMNS, drop = FALSE])

tracking <- merge(tracking, final_decisions, by = "record_id", all.x = TRUE, sort = FALSE)
missing_provenance <- !nzchar(norm_empty_if_na(tracking$decision_rule))
if (any(missing_provenance)) {
  stop(
    "CP11B could not attach title/abstract decision provenance for record_id(s): ",
    paste(tracking$record_id[missing_provenance], collapse = ", "),
    call. = FALSE
  )
}

eligibility_template <- build_eligibility_template(tracking)
unavailable_reference <- build_unavailable_reference(tracking)
codebook <- build_codebook()

missing_pdf <- eligibility_template[!file.exists(file.path(CFG$pipeline_dir, "full_text", "pdfs", eligibility_template$pdf_filename)), "record_id"]
if (length(missing_pdf) > 0L) {
  stop(
    "CP11B found retrieved records without matching PDF files: ",
    paste(missing_pdf, collapse = ", "),
    call. = FALSE
  )
}

template_csv <- file.path(eligibility_dir, "full_text_eligibility_template.csv")
template_xlsx <- file.path(eligibility_dir, "full_text_eligibility_template.xlsx")
codebook_csv <- file.path(eligibility_dir, "full_text_eligibility_codebook.csv")
unavailable_csv <- file.path(eligibility_dir, "full_text_unavailable_reference.csv")
reviewer_manifest_csv <- file.path(eligibility_dir, "full_text_eligibility_reviewer_package_manifest.csv")

stable_write_csv(eligibility_template, template_csv, FULL_TEXT_ELIGIBILITY_COLUMNS)
stable_write_csv(codebook, codebook_csv, FULL_TEXT_ELIGIBILITY_CODEBOOK_COLUMNS)
stable_write_csv(unavailable_reference, unavailable_csv, FULL_TEXT_UNAVAILABLE_COLUMNS)
write_full_text_eligibility_xlsx(eligibility_template, unavailable_reference, codebook, template_xlsx)

reviewer_specs <- full_text_reviewer_specs()
reviewer_packages <- lapply(seq_len(nrow(reviewer_specs)), function(i) {
  write_reviewer_package(
    template = eligibility_template,
    unavailable = unavailable_reference,
    codebook = codebook,
    spec = reviewer_specs[i, , drop = FALSE],
    eligibility_dir = eligibility_dir
  )
})
reviewer_manifest <- do.call(rbind, reviewer_packages)
stable_write_csv(reviewer_manifest, reviewer_manifest_csv, FULL_TEXT_REVIEWER_PACKAGE_COLUMNS)

warnings <- character()
if (nrow(unavailable_reference) > 0L) {
  warnings <- c(
    warnings,
    paste0(nrow(unavailable_reference), " record(s) are unavailable for full-text eligibility review and should be counted as not retrieved in PRISMA.")
  )
}
preserved_workbooks <- reviewer_manifest$workbook_status == "preserved_existing"
if (any(preserved_workbooks)) {
  warnings <- c(
    warnings,
    paste0(
      sum(preserved_workbooks),
      " existing reviewer workbook(s) were preserved to avoid overwriting reviewer annotations."
    )
  )
}

pre_adjudicated_full_text_ids <- tracking$record_id[grepl("full_text", tracking$pre_adjudicated_decisions, fixed = TRUE)]
pre_adjudicated_full_text_ids <- unique(pre_adjudicated_full_text_ids[nzchar(pre_adjudicated_full_text_ids)])

review_file <- write_checkpoint(
  id = "CP11B",
  name = "prepare_full_text_eligibility",
  what_ran = paste(
    "Prepared the human full-text eligibility worksheet from CP11 retrieval",
    "tracking. Included only records with retrieved PDFs and provided a",
    "separate reference sheet for unavailable records. Attached reviewer and",
    "pre-adjudication provenance from title/abstract screening. Generated two",
    "independent full-text eligibility packages for Revisor 1 and Revisor 2.",
    "No eligibility decisions",
    "were pre-filled. Controlled decision values are documented in each",
    "workbook codebook and will be validated by the next checkpoint."
  ),
  numbers = c(
    retrieved_pdf_records_for_eligibility = nrow(eligibility_template),
    unavailable_reference_records = nrow(unavailable_reference),
    pre_adjudicated_full_text_records = length(pre_adjudicated_full_text_ids),
    pre_adjudicated_records_in_eligibility = sum(pre_adjudicated_full_text_ids %in% eligibility_template$record_id),
    pre_adjudicated_records_unavailable = sum(pre_adjudicated_full_text_ids %in% unavailable_reference$record_id),
    blank_decision_cells = sum(!nzchar(eligibility_template$full_text_decision)),
    reviewer_packages = nrow(reviewer_manifest),
    reviewer_workbooks_created = sum(reviewer_manifest$workbook_status == "created"),
    reviewer_workbooks_preserved = sum(reviewer_manifest$workbook_status == "preserved_existing"),
    reviewer_handoff_files = nrow(reviewer_manifest),
    reviewer_prompt_files = nrow(reviewer_manifest),
    codebook_rows = nrow(codebook)
  ),
  review_items = c(
    "Use the two reviewer-specific workbooks under reviewer_inputs; keep reviewer decisions independent.",
    "Provide each handoff and prompt only to its corresponding independent reviewer.",
    "For each PDF, complete only the yellow decision fields after reading the full text.",
    "Use the codebook sheet to fill include_for_extraction, exclude_full_text, or unclear_needs_discussion only.",
    "For exclude_full_text, provide at least one controlled exclusion reason.",
    "Do not enter extraction, appraisal, or readiness judgments in this worksheet.",
    "After both reviewer workbooks are completed, run the next validation/reconciliation checkpoint before data extraction."
  ),
  outputs = c(
    template_csv,
    template_xlsx,
    codebook_csv,
    unavailable_csv,
    reviewer_manifest_csv,
    file.path(CFG$project_root, reviewer_manifest$workbook_path),
    file.path(CFG$project_root, reviewer_manifest$csv_path),
    file.path(CFG$project_root, reviewer_manifest$handoff_path),
    file.path(CFG$project_root, reviewer_manifest$prompt_path),
    file.path(CFG$checkpoints_dir, "CP11B_prepare_full_text_eligibility.md")
  ),
  warnings = warnings,
  gate_question = "Approve CP11B only after the two independent full-text eligibility reviewer packages, handoffs, prompts, and codebooks are ready for human review."
)

cat("CP11B prepare full-text eligibility complete\n")
cat("Retrieved PDF records for eligibility:", nrow(eligibility_template), "\n")
cat("Unavailable reference records:", nrow(unavailable_reference), "\n")
cat("Reviewer packages:", nrow(reviewer_manifest), "\n")
cat("Eligibility workbook:", project_relative_path(template_xlsx), "\n")
cat("Review file:", project_relative_path(review_file), "\n")
