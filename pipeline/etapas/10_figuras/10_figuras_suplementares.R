# Gera as figuras suplementares a partir das camadas finais da revisão.
# As figuras complementam a descrição dos dados, sem acrescentar julgamentos.


inicio_etapa <- Sys.time()
argumento_arquivo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
source(file.path(dirname(normalizePath(argumento_arquivo[[1L]], mustWork = TRUE)), "00_contexto.R"))
source(file.path(CODE_ROOT, "pipeline", "etapas", "10_figuras", "07_estilo_editorial.R"))

pacotes <- c("readr", "dplyr", "tidyr", "ggplot2", "scales", "stringr", "forcats", "ragg")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1L), quietly = TRUE)]
if (length(faltantes)) stop("Pacotes ausentes: ", paste(faltantes, collapse = ", "), call. = FALSE)

entrada <- caminho("dados", "saida", "tabelas_figuras")
saida <- caminho("dados", "saida", "figuras_editoriais", "suplementares")
criar_pasta(saida)
ler <- function(nome) readr::read_csv(file.path(entrada, nome), show_col_types = FALSE, na = character())

paleta <- paleta_editorial()
tema <- tema_editorial_suplementar

exportar <- function(grafico, nome, largura, altura) {
  ggplot2::ggsave(file.path(saida, paste0(nome, ".pdf")), grafico,
                  width = largura, height = altura, units = "in",
                  device = grDevices::pdf, family = "Helvetica", bg = "white")
  ggplot2::ggsave(file.path(saida, paste0(nome, ".png")), grafico,
                  width = largura, height = altura, units = "in", dpi = 600,
                  device = ragg::agg_png, bg = "white")
  ggplot2::ggsave(file.path(saida, paste0(nome, ".tiff")), grafico,
                  width = largura, height = altura, units = "in", dpi = 600,
                  device = ragg::agg_tiff, compression = "lzw", bg = "white")
}

s1 <- ler("figura_s1_fontes.csv") |>
  dplyr::mutate(ocorrencias_importadas = as.numeric(ocorrencias_importadas),
                fonte = forcats::fct_reorder(fonte, ocorrencias_importadas))
g1 <- ggplot2::ggplot(s1, ggplot2::aes(ocorrencias_importadas, fonte)) +
  ggplot2::geom_col(width = .64, fill = paleta[["azul"]]) +
  ggplot2::geom_text(ggplot2::aes(label = scales::comma(ocorrencias_importadas)),
                     hjust = -.15, size = 3, colour = paleta[["tinta"]]) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .14))) +
  ggplot2::labs(x = "Imported record occurrences", y = NULL) + tema(9.3)
exportar(g1, "Figure_S1_search_sources", 7.4, 7.2)

s2 <- ler("figura_s2_sobreposicao.csv") |>
  dplyr::mutate(n_relatos = as.numeric(n_relatos),
                categoria = dplyr::recode(categoria_sobreposicao,
                  "Forte ou provável" = "Strong or probable",
                  "Possível" = "Possible",
                  "Sem sobreposição identificada" = "No overlap identified"),
                categoria = forcats::fct_reorder(categoria, n_relatos))
g2 <- ggplot2::ggplot(s2, ggplot2::aes(n_relatos, categoria)) +
  ggplot2::geom_col(width = .62, fill = paleta[["verde"]]) +
  ggplot2::geom_text(ggplot2::aes(label = n_relatos), hjust = -.35, fontface = "bold") +
  ggplot2::scale_x_continuous(limits = c(0, 28), breaks = seq(0, 25, 5), expand = c(0, 0)) +
  ggplot2::labs(x = "Reports (n = 33)", y = NULL) + tema()
exportar(g2, "Figure_S2_dataset_overlap", 7, 4.2)

mapa_metricas <- c("Acurácia" = "Accuracy", "Taxa de sucesso da classificação" = "Classification success rate",
                   "Taxa de reconhecimento" = "Recognition rate", "Sensibilidade" = "Sensitivity",
                   "Correlação de Spearman" = "Spearman correlation", "Acurácia balanceada" = "Balanced accuracy",
                   "Compatibilidade média com grupo de lombalgia" = "Mean compatibility with low-back-pain group",
                   "Condição predita" = "Predicted condition", "Especificidade" = "Specificity")
s3 <- ler("figura_s3_metricas.csv") |>
  dplyr::mutate(n_resultados_primarios = as.numeric(n_resultados_primarios),
                metrica = ifelse(familia_metrica %in% names(mapa_metricas), mapa_metricas[familia_metrica], familia_metrica),
                metrica = forcats::fct_reorder(metrica, n_resultados_primarios))
g3 <- ggplot2::ggplot(s3, ggplot2::aes(n_resultados_primarios, metrica)) +
  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n_resultados_primarios, yend = metrica),
                        colour = paleta[["claro"]], linewidth = 1.2) +
  ggplot2::geom_point(size = 3.5, colour = paleta[["azul"]]) +
  ggplot2::geom_text(ggplot2::aes(label = n_resultados_primarios), hjust = -.55, fontface = "bold") +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .09))) +
  ggplot2::labs(x = "Primary performance results", y = NULL) + tema(9.2)
exportar(g3, "Figure_S3_metric_families", 7.3, 5.8)

mapa_estagios <- c("Nível 0" = "Stage 0", "Nível 1" = "Stage 1", "Nível 2" = "Stage 2", "Níveis 3–4" = "Stages 3-4")
mapa_cenarios <- c(
  "Todas as unidades aplicáveis" = "All applicable units",
  "Dor clínica ou não experimental" = "Clinical or nonexperimental pain",
  "Exclui publicações em congresso" = "Excluding conference publications",
  "Exclui não revisados por pares" = "Excluding non-peer-reviewed reports",
  "Apenas dor experimental" = "Experimental pain only",
  "Validação externa, temporal ou multicêntrica" = "External, temporal, or multicenter validation",
  "Texto completo, revisado e não congresso" = "Full-text, peer-reviewed, nonconference reports",
  "Multimodal ou covariáveis não térmicas" = "Multimodal or nonthermal covariates",
  "Separação por participante demonstrada" = "Demonstrated participant-level separation",
  "Apenas térmico ou derivado térmico" = "Thermal or thermally derived input only",
  "Protocolo termográfico replicável" = "Replicable thermography protocol"
)
s4 <- ler("figura_s4_sensibilidade.csv") |>
  dplyr::mutate(n = as.numeric(n), estagio = mapa_estagios[estagio_clinico],
                cenario = stringr::str_wrap(mapa_cenarios[cenario], 34),
                proporcao = n / as.numeric(denominador_unidades))
g4 <- ggplot2::ggplot(s4, ggplot2::aes(proporcao, forcats::fct_rev(factor(cenario)), fill = estagio)) +
  ggplot2::geom_col(width = .66) +
  ggplot2::scale_fill_manual(values = c("Stage 0" = paleta[["claro"]], "Stage 1" = paleta[["ardosa"]],
                                         "Stage 2" = paleta[["verde"]], "Stages 3-4" = paleta[["ouro"]]), drop = FALSE) +
  ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::labs(x = "Within-scenario distribution", y = NULL) + tema(8.8)
exportar(g4, "Figure_S4_stage_sensitivity", 8, 7)

mapa_indicadores <- c("Código disponível" = "Code availability", "Dados disponíveis" = "Data availability",
                     "Modelo disponível" = "Model availability", "Equidade avaliada" = "Equity assessment")
mapa_respostas <- c("Não" = "No", "Sim" = "Yes", "Incerto" = "Unclear")
s5 <- ler("figura_s5_ciencia_aberta.csv") |>
  dplyr::mutate(n = as.numeric(n), denominador = as.numeric(denominador), proporcao = n / denominador,
                indicador_en = mapa_indicadores[indicador], resposta = mapa_respostas[categoria_resposta])
g5 <- ggplot2::ggplot(s5, ggplot2::aes(proporcao, forcats::fct_rev(indicador_en), fill = resposta)) +
  ggplot2::geom_col(width = .64) +
  ggplot2::geom_text(ggplot2::aes(label = ifelse(proporcao >= .08, n, "")),
                     position = ggplot2::position_stack(vjust = .5), fontface = "bold", size = 3) +
  ggplot2::scale_fill_manual(values = c("No" = paleta[["ardosa"]], "Unclear" = paleta[["ouro"]], "Yes" = paleta[["verde"]])) +
  ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::labs(x = "Proportion of assessed units", y = NULL) + tema()
exportar(g5, "Figure_S5_open_science_equity", 7, 4.5)

mapa_termografia <- c("Modelo da câmera" = "Camera model", "Faixa espectral" = "Spectral range",
                     "Resolução térmica" = "Spatial resolution", "NETD" = "NETD",
                     "Temperatura ambiente" = "Ambient temperature", "Aclimatação" = "Acclimatization",
                     "Emissividade" = "Emissivity", "Distância da câmera" = "Camera-to-participant distance",
                     "Definição da ROI" = "ROI definition", "Pré-processamento" = "Preprocessing",
                     "Dados radiométricos brutos" = "Raw radiometric data")
s6 <- ler("figura_s6_termografia.csv") |>
  dplyr::filter(categoria_resposta == "Relatado") |>
  dplyr::mutate(n = as.numeric(n), denominador = as.numeric(denominador), proporcao = n / denominador,
                item = mapa_termografia[variavel], item = forcats::fct_reorder(item, proporcao))
g6 <- ggplot2::ggplot(s6, ggplot2::aes(proporcao, item)) +
  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = proporcao, yend = item), colour = paleta[["claro"]], linewidth = 1.4) +
  ggplot2::geom_point(size = 3.8, colour = paleta[["verde"]]) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(n, "/", denominador)), hjust = -.3, fontface = "bold") +
  ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1.12), breaks = seq(0, 1, .25), expand = c(0, 0)) +
  ggplot2::labs(x = "Records with item reported (n = 50)", y = NULL) + tema(9.2)
exportar(g6, "Figure_S6_thermography_reporting", 7.3, 6.2)

mapa_componentes <- c("Capacidade" = "Capability", "Utilidade" = "Utility", "Adoção" = "Adoption")
mapa_subcomponentes <- c(
  "Objetivo" = "Purpose", "Fonte e integridade dos dados" = "Data source and integrity",
  "Métricas de desempenho" = "Performance metrics", "Validade interna" = "Internal validity",
  "Validade externa" = "External validity", "Transparência" = "Transparency",
  "Alinhamento com o domínio" = "Domain alignment", "Caso de uso" = "Use case",
  "Não maleficência" = "Nonmaleficence", "Privacidade" = "Privacy",
  "Segurança e qualidade" = "Safety and quality", "Uso em contexto de saúde" = "Use in a healthcare setting",
  "Integração técnica" = "Technical integration", "Número de serviços" = "Number of services",
  "Generalização e contextualização" = "Generalization and contextualization"
)
s7 <- ler("figura_5_tehai.csv") |>
  dplyr::mutate(n = as.numeric(n), denominador_subcomponente = as.numeric(denominador_subcomponente),
                proporcao = n / denominador_subcomponente, escore = paste("Score", escore),
                componente = mapa_componentes[componente_tehai],
                subcomponente = stringr::str_wrap(mapa_subcomponentes[subcomponente], 28))
g7 <- ggplot2::ggplot(s7, ggplot2::aes(proporcao, forcats::fct_rev(subcomponente), fill = escore)) +
  ggplot2::geom_col(width = .64) +
  ggplot2::facet_grid(componente ~ ., scales = "free_y", space = "free_y") +
  ggplot2::scale_fill_manual(values = c("Score 0" = paleta[["claro"]], "Score 1" = paleta[["ardosa"]],
                                         "Score 2" = paleta[["azul"]], "Score 3" = paleta[["verde"]]), drop = FALSE) +
  ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::labs(x = "Within-subcomponent distribution", y = NULL) + tema(8.7)
exportar(g7, "Figure_S7_tehai_subcomponents", 7.5, 9.2)

mapa_criterios <- c(
  "Padrões de referência heterogêneos" = "Heterogeneous reference standards",
  "Alvos ou definições de rótulo heterogêneos" = "Heterogeneous targets or label definitions",
  "Incerteza ou variância incompleta" = "Incomplete uncertainty or variance",
  "Denominador avaliado incompleto" = "Incomplete evaluated denominator",
  "Independência por participante não consistentemente demonstrada" = "Participant independence not consistently demonstrated"
)
s8 <- ler("figura_6_sintese.csv") |>
  dplyr::mutate(n_estudos = as.numeric(n_estudos), criterio = stringr::str_wrap(mapa_criterios[criterio_nao_atendido], 34))
g8 <- ggplot2::ggplot(s8, ggplot2::aes(grupo_candidato, criterio)) +
  ggplot2::geom_point(ggplot2::aes(size = n_estudos), shape = 21, fill = paleta[["ouro"]],
                      colour = paleta[["tinta"]], stroke = .35) +
  ggplot2::scale_size_area(max_size = 8, breaks = sort(unique(s8$n_estudos))) +
  ggplot2::labs(x = "Candidate synthesis group", y = NULL, size = "Studies") +
  tema(8.8) + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = paleta[["claro"]], linewidth = .3))
exportar(g8, "Figure_S8_synthesis_gate", 8, 6.5)

arquivos <- list.files(saida, full.names = TRUE)
registrar_execucao("10_figuras_suplementares", inicio_etapa, arquivos)
cat("Figuras suplementares concluídas em: ", saida, "\n", sep = "")
