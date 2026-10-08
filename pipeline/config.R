# Define os caminhos, os limites de comparação e as opções de execução.
# A pasta de dados pode ficar fora do código e é escolhida por AITHERMO_DATA_ROOT.


arquivo_em_execucao <- function() {
  argumento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(argumento)) return(NA_character_)
  normalizePath(sub("^--file=", "", argumento[[1L]]), mustWork = FALSE)
}

localizar_raiz_codigo <- function() {
  candidatos <- c(
    getOption("aithermo.code_root", NA_character_),
    Sys.getenv("AITHERMO_CODE_ROOT", unset = NA_character_),
    getwd()
  )
  ativo <- arquivo_em_execucao()
  if (!is.na(ativo)) candidatos <- c(candidatos, dirname(dirname(ativo)), dirname(dirname(dirname(ativo))))
  candidatos <- unique(normalizePath(candidatos[!is.na(candidatos) & nzchar(candidatos)], mustWork = FALSE))
  for (candidato in candidatos) {
    atual <- candidato
    repeat {
      if (file.exists(file.path(atual, "renv.lock")) && dir.exists(file.path(atual, "pipeline"))) return(atual)
      anterior <- dirname(atual)
      if (identical(anterior, atual)) break
      atual <- anterior
    }
  }
  stop("Não foi possível localizar a raiz do código.", call. = FALSE)
}

localizar_raiz_dados <- function(raiz_codigo) {
  configurada <- getOption("aithermo.data_root", Sys.getenv("AITHERMO_DATA_ROOT", unset = ""))
  if (!nzchar(configurada)) configurada <- file.path(raiz_codigo, "dados")
  normalizePath(configurada, mustWork = FALSE)
}

configurar_ambiente_deterministico <- function() {
  Sys.setenv(TZ = "UTC")
  try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE)
  for (candidato in c("C.UTF-8", "pt_BR.UTF-8", "en_US.UTF-8", "UTF-8")) {
    resultado <- suppressWarnings(try(Sys.setlocale("LC_CTYPE", candidato), silent = TRUE))
    if (!inherits(resultado, "try-error")) break
  }
  set.seed(20260626L)
  list(
    lc_collate = Sys.getlocale("LC_COLLATE"),
    lc_ctype = Sys.getlocale("LC_CTYPE"),
    timezone = Sys.getenv("TZ", unset = "")
  )
}

LOCALE_INFO <- configurar_ambiente_deterministico()

CODE_ROOT <- localizar_raiz_codigo()
PROJECT_ROOT <- localizar_raiz_dados(CODE_ROOT)
PIPELINE_DIR <- file.path(PROJECT_ROOT, "pipeline")

CFG <- list(
  seed = 20260626L,
  locale = LOCALE_INFO,
  strict = tolower(Sys.getenv("AITHERMO_STRICT", unset = "true")) %in% c("1", "true", "yes"),
  code_root = CODE_ROOT,
  project_root = PROJECT_ROOT,
  pipeline_dir = PIPELINE_DIR,
  buscas_dir = file.path(PROJECT_ROOT, "BUSCAS"),
  inventory_csv = file.path(PROJECT_ROOT, "data_intermediate", "inventory", "buscas_complete_inventory.csv"),
  final_handoff = file.path(PROJECT_ROOT, "governanca", "REGISTRO_APROVACOES.md"),
  r_dir = file.path(CODE_ROOT, "pipeline", "R"),
  scripts_dir = file.path(CODE_ROOT, "pipeline", "etapas"),
  processed_dir = file.path(PIPELINE_DIR, "processed"),
  screening_dir = file.path(PIPELINE_DIR, "screening"),
  screening_reviewer_dir = file.path(PIPELINE_DIR, "screening", "reviewer_inputs"),
  outputs_dir = file.path(PIPELINE_DIR, "outputs"),
  snowballingo_dir = file.path(PIPELINE_DIR, "snowballingo"),
  logs_dir = file.path(PIPELINE_DIR, "logs"),
  checkpoints_dir = file.path(PIPELINE_DIR, "checkpoints"),
  tests_dir = file.path(CODE_ROOT, "testes"),
  fixtures_dir = file.path(CODE_ROOT, "testes", "fixtures"),
  parser_shortfall_tolerance = 0.05,
  fuzzy_high_threshold = 0.93,
  fuzzy_review_threshold = 0.88,
  preprint_publication_threshold = 0.90
)

PIPELINE_DIRS <- c(
  PIPELINE_DIR,
  CFG$processed_dir,
  CFG$screening_dir,
  CFG$screening_reviewer_dir,
  CFG$outputs_dir,
  CFG$snowballingo_dir,
  CFG$logs_dir,
  CFG$checkpoints_dir
)
