# Gera as figuras principais a partir dos dados de exibição aprovados.
# Mantém os mesmos denominadores usados nas tabelas de síntese.




inicio_etapa <- Sys.time()
argumento_arquivo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(argumento_arquivo)) stop("Execute esta etapa com Rscript.", call. = FALSE)
source(file.path(dirname(normalizePath(argumento_arquivo[[1L]], mustWork = TRUE)), "00_contexto.R"))
source(file.path(CODE_ROOT, "pipeline", "etapas", "10_figuras", "07_estilo_editorial.R"))
root <- PASTA_RAIZ
input_dir <- caminho("dados", "saida", "tabelas_figuras")
out_dir <- caminho("dados", "saida", "figuras_editoriais")

pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "scales", "stringr", "forcats", "cowplot", "ragg", "digest")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Pacotes ausentes: ", paste(missing, collapse = ", "), call. = FALSE)

dirs <- file.path(out_dir, c("figuras", "dados_exibicao", "documentacao", "verificacao"))
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

read_source <- function(name) readr::read_csv(file.path(input_dir, name), show_col_types = FALSE, na = character())
f1 <- read_source("figura_2_construto_estagio.csv")
f2 <- read_source("figura_3_validade.csv")
f3 <- read_source("figura_4_metodologia.csv")
f4 <- read_source("figura_s6_termografia.csv")
f5 <- read_source("figura_5_tehai.csv")

stopifnot(sum(f1$n) == 50, sum(f2$n) == 38, unique(f4$denominador) == 50,
          sum(f5$n) == 435, identical(unique(f5$denominador_subcomponente), 29))

pal <- paleta_pain()
theme_pain <- tema_pain

construct_map <- c("Dor direta"="Direct pain", "Comportamento relacionado à dor"="Pain-related behavior",
                   "Condição dolorosa"="Painful condition", "Resposta terapêutica"="Treatment response", "Tarefa auxiliar"="Auxiliary task")
stage_map <- c("Não classificável"="Not classifiable", "Nível 0"="Stage 0", "Nível 1"="Stage 1", "Nível 2"="Stage 2", "Níveis 3–4"="Stages 3-4")
ref_map <- c("Independente"="Independent", "Incerto"="Unclear", "Não independente"="Not independent")
val_map <- c("Sem validação adequada"="No adequate validation", "Validação interna"="Internal validation",
             "Externa, temporal ou multicêntrica"="External, temporal, or multicenter validation")

x1 <- f1 |>
  dplyr::mutate(construct=construct_map[construto], stage=stage_map[estagio_clinico],
                construct=factor(construct, levels=rev(unname(construct_map))),
                stage=factor(stage, levels=unname(stage_map)))
p1 <- ggplot2::ggplot(x1, ggplot2::aes(stage, construct, fill=n)) +
  ggplot2::geom_tile(colour=pal[["white"]], linewidth=1.1) +
  ggplot2::geom_text(ggplot2::aes(label=n, colour=n >= 8), fontface="bold", size=4.1) +
  ggplot2::scale_fill_gradient(low=pal[["mist"]], high=pal[["slate_dark"]], limits=c(0,12), guide="none") +
  ggplot2::scale_colour_manual(values=c(`FALSE`=pal[["ink"]], `TRUE`=pal[["white"]]), guide="none") +
  ggplot2::scale_x_discrete(labels=c("Not classifiable"="Not\nclassifiable", "Stages 3-4"="Stages\n3-4")) +
  ggplot2::labs(x="Clinical evidence stage", y=NULL) +
  theme_pain(10.5) + ggplot2::theme(panel.grid=ggplot2::element_blank(), axis.text.y=ggplot2::element_text(size=10), axis.text.x=ggplot2::element_text(size=9.5), plot.margin=ggplot2::margin(12,12,10,12))

panel_data <- dplyr::bind_rows(
  f2 |> dplyr::count(construto, wt=n, name="n") |> dplyr::transmute(panel="A  Construct", category=construct_map[construto], n),
  f2 |> dplyr::count(independencia_padrao_referencia, wt=n, name="n") |> dplyr::transmute(panel="B  Reference\nstandard", category=ref_map[independencia_padrao_referencia], n),
  f2 |> dplyr::count(categoria_validacao, wt=n, name="n") |> dplyr::transmute(panel="C  Validation", category=val_map[categoria_validacao], n),
  f2 |> dplyr::count(estagio_clinico, wt=n, name="n") |> dplyr::transmute(panel="D  Clinical stage", category=stage_map[estagio_clinico], n)
) |>
  dplyr::group_by(panel) |> dplyr::arrange(n, .by_group=TRUE) |>
  dplyr::mutate(category_order=paste(panel, category, sep="___")) |> dplyr::ungroup()
panel_data$category_order <- factor(panel_data$category_order, levels=panel_data$category_order)
p2 <- ggplot2::ggplot(panel_data, ggplot2::aes(n, category_order)) +
  ggplot2::geom_segment(ggplot2::aes(x=0, xend=n, yend=category_order), linewidth=1.05, colour=pal[["pale"]], lineend="round") +
  ggplot2::geom_point(ggplot2::aes(size=n), shape=21, fill=pal[["slate_dark"]], colour=pal[["white"]], stroke=.45) +
  ggplot2::geom_text(data=dplyr::filter(panel_data,n>=7), ggplot2::aes(label=n), fontface="bold", colour=pal[["white"]], size=3.0) +
  ggplot2::geom_text(data=dplyr::filter(panel_data,n<7), ggplot2::aes(x=n+1.7,label=n), hjust=0, fontface="bold", colour=pal[["ink"]], size=3.25) +
  ggplot2::facet_wrap(~panel, ncol=2, scales="free_y") +
  ggplot2::scale_y_discrete(labels=function(z) sub("^.*___", "", z)) +
  ggplot2::scale_size_area(max_size=12.5, limits=c(0,30), guide="none") +
  ggplot2::scale_x_continuous(limits=c(0,38), breaks=c(0,10,20,30), expand=c(0,0)) +
  ggplot2::labs(x="Classifiable clinical model-task units (n = 38)", y=NULL) +
  theme_pain(10) + ggplot2::theme(panel.spacing=grid::unit(1.6,"lines"), axis.text.y=ggplot2::element_text(size=9), strip.text=ggplot2::element_text(hjust=0), panel.grid.major.x=ggplot2::element_line(colour=pal[["pale"]],linewidth=.3))

state_map <- c("Sim"="Documented/favorable", "Baixo"="Documented/favorable", "Parcial"="Partial/uncertain", "Incerto"="Partial/uncertain",
               "Não"="Absent/not reported", "Não relatado/não aplicável"="Absent/not reported", "Alto"="High risk/incomplete", "Incompleto"="High risk/incomplete")
indicator_map <- c("Risco de viés global"="Overall risk of bias", "Qualidade do relato"="Reporting quality",
                   "Relato termográfico global"="Overall thermography reporting", "Protocolo termográfico replicável"="Replicable thermography protocol",
                   "Separação em nível de participante"="Participant-level separation", "Intervalo de confiança"="Confidence interval",
                   "Denominador avaliado"="Evaluated denominator", "Código disponível"="Code availability", "Dados disponíveis"="Data availability",
                   "Modelo disponível"="Model availability", "Equidade avaliada"="Equity assessment")
x3 <- f3 |> dplyr::mutate(state=state_map[categoria_resposta], indicator=indicator_map[indicador])
x3$state[x3$indicador == "Relato termográfico global" & x3$categoria_resposta == "Incompleto"] <- "Partial/uncertain"
x3 <- x3 |> dplyr::group_by(indicator, denominador, ordem_exibicao, state) |> dplyr::summarise(n=sum(n), .groups="drop") |>
  dplyr::mutate(prop=n/denominador,
                panel=ifelse(indicator %in% c("Overall risk of bias","Participant-level separation","Confidence interval","Evaluated denominator","Replicable thermography protocol"),
                             "A  Validation and confidence", "B  Reporting, open science, and equity"),
                label=paste0(indicator, "  (n = ", denominador, ")"))
expected_sep <- x3 |> dplyr::filter(indicator == "Participant-level separation") |> dplyr::arrange(match(state,c("Absent/not reported","Partial/uncertain","Documented/favorable")))
stopifnot(sum(expected_sep$n)==54, setNames(expected_sep$n, expected_sep$state)[c("Absent/not reported","Partial/uncertain","Documented/favorable")] == c(19,13,22))
state_levels <- c("High risk/incomplete","Absent/not reported","Partial/uncertain","Documented/favorable")
x3$state <- factor(x3$state, levels=state_levels)
state_cols <- c("High risk/incomplete"=pal[["ink"]], "Absent/not reported"=pal[["slate"]], "Partial/uncertain"=pal[["gold"]], "Documented/favorable"=pal[["teal"]])
state_text <- c("High risk/incomplete"=pal[["white"]], "Absent/not reported"=pal[["white"]], "Partial/uncertain"=pal[["ink"]], "Documented/favorable"=pal[["white"]])
profile_panel <- function(data, panel_name, show_x=TRUE, show_legend=FALSE) {
  z <- data |> dplyr::filter(panel==panel_name) |> dplyr::arrange(ordem_exibicao)
  z$label <- factor(z$label, levels=rev(unique(z$label)))
  ggplot2::ggplot(z, ggplot2::aes(prop,label,fill=state)) +
    ggplot2::geom_col(width=.60) +
    ggplot2::geom_text(ggplot2::aes(label=ifelse(prop>=.065,n,""), colour=state), position=ggplot2::position_stack(vjust=.5), size=3.05, fontface="bold") +
    ggplot2::scale_fill_manual(values=state_cols, drop=FALSE) +
    ggplot2::scale_colour_manual(values=state_text, guide="none") +
    ggplot2::scale_x_continuous(labels=scales::percent, limits=c(0,1), expand=c(0,0)) +
    ggplot2::labs(title=panel_name, x=if(show_x) "Proportion of stated denominator" else NULL, y=NULL) +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2, byrow=TRUE)) +
    theme_pain(9.6) +
    ggplot2::theme(axis.text.y=ggplot2::element_text(size=8.7), panel.grid.major.y=ggplot2::element_blank(),
                   legend.position=if(show_legend) "bottom" else "none",
                   axis.text.x=if(show_x) ggplot2::element_text() else ggplot2::element_blank(),
                   axis.ticks.x=if(show_x) ggplot2::element_line(colour=pal[["ink"]],linewidth=.35) else ggplot2::element_blank(),
                   axis.line.x=if(show_x) ggplot2::element_line(colour=pal[["ink"]],linewidth=.35) else ggplot2::element_blank(),
                   plot.title=ggplot2::element_text(size=11.2, margin=ggplot2::margin(b=7)))
}
p3a <- profile_panel(x3,"A  Validation and confidence",show_x=FALSE,show_legend=FALSE)
p3b_leg <- profile_panel(x3,"B  Reporting, open science, and equity",show_x=TRUE,show_legend=TRUE)
leg3 <- cowplot::get_legend(p3b_leg)
p3b <- p3b_leg + ggplot2::theme(legend.position="none")
p3 <- cowplot::plot_grid(p3a,p3b,leg3,ncol=1,rel_heights=c(1,1.15,.22),align="v",axis="lr")

thermo_map <- c("Modelo da câmera"="Camera model", "Faixa espectral"="Spectral range", "Resolução térmica"="Spatial resolution", "NETD"="NETD",
                "Temperatura ambiente"="Ambient temperature", "Aclimatação"="Acclimatization", "Emissividade"="Emissivity", "Distância da câmera"="Camera-to-participant distance",
                "Definição da ROI"="ROI definition", "Pré-processamento"="Preprocessing", "Dados radiométricos brutos"="Raw radiometric data")
domain_map <- c("Modelo da câmera"="Device and specification", "Faixa espectral"="Device and specification", "Resolução térmica"="Device and specification", "NETD"="Device and specification",
                "Temperatura ambiente"="Environment", "Aclimatação"="Environment", "Emissividade"="Acquisition protocol", "Distância da câmera"="Acquisition protocol",
                "Definição da ROI"="ROI and preprocessing", "Pré-processamento"="ROI and preprocessing", "Dados radiométricos brutos"="Radiometric data")
x4 <- f4 |> dplyr::mutate(item=thermo_map[variavel], domain=domain_map[variavel], response=ifelse(categoria_resposta=="Relatado","Reported","Not reported"), prop=n/denominador)
item_order <- x4 |> dplyr::filter(response=="Reported") |> dplyr::arrange(domain, n) |> dplyr::pull(item)
x4$item <- factor(x4$item, levels=unique(item_order))
x4$domain <- factor(x4$domain, levels=c("Device and specification","Environment","Acquisition protocol","ROI and preprocessing","Radiometric data"))
x4$response <- factor(x4$response, levels=c("Not reported","Reported"))
p4_reported <- x4 |> dplyr::filter(response=="Reported") |> dplyr::mutate(value_label=paste0(n,"/",denominador,"  (",round(100*prop),"%)"))
p4 <- ggplot2::ggplot(p4_reported, ggplot2::aes(prop,item)) +
  ggplot2::geom_segment(ggplot2::aes(x=0,xend=prop,yend=item), colour=pal[["pale"]], linewidth=1.5, lineend="round") +
  ggplot2::geom_point(size=4.0, colour=pal[["teal"]]) +
  ggplot2::geom_text(ggplot2::aes(label=value_label), hjust=-.18, fontface="bold", size=3.05, colour=pal[["ink"]]) +
  ggplot2::facet_grid(domain~., scales="free_y", space="free_y", switch="y") +
  ggplot2::scale_x_continuous(labels=scales::percent, limits=c(0,1.23), breaks=c(0,.25,.5,.75,1), expand=c(0,0)) +
  ggplot2::labs(x="Records with item reported (n = 50)", y=NULL) +
  theme_pain(9.8) + ggplot2::theme(strip.placement="outside", strip.text.y.left=ggplot2::element_text(angle=0,hjust=0,size=8.7), axis.text.y=ggplot2::element_text(size=9))

comp_map <- c("Capacidade"="Capability", "Utilidade"="Utility", "Adoção"="Adoption")
x5 <- f5 |> dplyr::group_by(componente_tehai, escore) |> dplyr::summarise(n=sum(n), .groups="drop") |>
  tidyr::complete(componente_tehai=names(comp_map), escore=0:3, fill=list(n=0)) |>
  dplyr::group_by(componente_tehai) |> dplyr::mutate(denominator=sum(n), prop=n/denominator, component=comp_map[componente_tehai], component_label=paste0(component,"  (n = ",denominator,")"), score=paste("Score",escore)) |> dplyr::ungroup()
x5$component_label <- factor(x5$component_label, levels=rev(c("Capability  (n = 174)","Utility  (n = 145)","Adoption  (n = 116)")))
x5$score <- factor(x5$score, levels=paste("Score",0:3))
score_cols <- c("Score 0"=pal[["pale"]], "Score 1"=pal[["slate"]], "Score 2"=pal[["slate_dark"]], "Score 3"=pal[["teal"]])
p5 <- ggplot2::ggplot(x5, ggplot2::aes(prop,component_label,fill=score)) +
  ggplot2::geom_col(width=.64, position=ggplot2::position_stack(reverse=TRUE)) +
  ggplot2::geom_text(ggplot2::aes(label=ifelse(prop>=.055,n,""), colour=score), position=ggplot2::position_stack(vjust=.5,reverse=TRUE), fontface="bold", size=3.4) +
  ggplot2::scale_fill_manual(values=score_cols, drop=FALSE) +
  ggplot2::scale_colour_manual(values=c("Score 0"=pal[["ink"]], "Score 1"=pal[["white"]], "Score 2"=pal[["white"]], "Score 3"=pal[["white"]]), guide="none") +
  ggplot2::scale_x_continuous(labels=scales::percent, limits=c(0,1), expand=c(0,0)) +
  ggplot2::labs(x="Within-component distribution", y=NULL) +
  theme_pain(10.2) + ggplot2::theme(axis.text.y=ggplot2::element_text(size=10))

plots <- list(Figure_2=p1, Figure_3=p2, Figure_4=p3, Figure_5=p4, Figure_6=p5)
sizes <- list(Figure_2=c(7.2,4.2), Figure_3=c(7.4,6.55), Figure_4=c(7.5,8.15), Figure_5=c(7.4,7.05), Figure_6=c(7.2,3.35))

save_three <- function(plot, stem, wh) {
  ggplot2::ggsave(file.path(out_dir,"figuras",paste0(stem,"_final.pdf")), plot, width=wh[1], height=wh[2], units="in", device=grDevices::pdf, family="Helvetica", bg="white")
  ggplot2::ggsave(file.path(out_dir,"figuras",paste0(stem,"_final.png")), plot, width=wh[1], height=wh[2], units="in", dpi=600, device=ragg::agg_png, bg="white")
  ggplot2::ggsave(file.path(out_dir,"figuras",paste0(stem,"_final.tiff")), plot, width=wh[1], height=wh[2], units="in", dpi=600, device=ragg::agg_tiff, compression="lzw", bg="white")
}
invisible(Map(save_three, plots, names(plots), sizes))

source_names <- c("figura_2_construto_estagio.csv", "figura_3_validade.csv",
                  "figura_4_metodologia.csv", "figura_s6_termografia.csv",
                  "figura_5_tehai.csv")
file.copy(file.path(input_dir,source_names), file.path(out_dir,"dados_exibicao",source_names), overwrite=TRUE)
readr::write_csv(panel_data, file.path(out_dir,"dados_exibicao","Figure_3_display_data.csv"))
readr::write_csv(x3, file.path(out_dir,"dados_exibicao","Figure_4_display_data.csv"))
readr::write_csv(x4, file.path(out_dir,"dados_exibicao","Figure_5_display_data.csv"))
readr::write_csv(x5, file.path(out_dir,"dados_exibicao","Figure_6_display_data.csv"))

grDevices::pdf(file.path(out_dir,"verificacao","PROVA_FIGURAS_2_A_6.pdf"), width=8.27, height=11.69, family="Helvetica", onefile=TRUE)
for (nm in names(plots)) print(cowplot::ggdraw() + cowplot::draw_plot(plots[[nm]], x=.06, y=.14, width=.88, height=.72))
grDevices::dev.off()

construct_totals <- f1 |> dplyr::group_by(construto) |> dplyr::summarise(n=sum(n),.groups="drop")
construct_totals <- setNames(construct_totals$n,construct_totals$construto)
construct_order <- c("Dor direta","Comportamento relacionado à dor","Condição dolorosa","Resposta terapêutica","Tarefa auxiliar")
stage_totals <- f1 |> dplyr::group_by(estagio_clinico) |> dplyr::summarise(n=sum(n),.groups="drop")
stage_totals <- setNames(stage_totals$n,stage_totals$estagio_clinico)
stage_order <- c("Não classificável","Nível 0","Nível 1","Nível 2","Níveis 3–4")
layer_totals <- panel_data |> dplyr::group_by(panel) |> dplyr::summarise(n=sum(n),.groups="drop") |> dplyr::arrange(panel)
component_totals <- x5 |> dplyr::group_by(component) |> dplyr::summarise(n=sum(n),.groups="drop")
component_totals <- setNames(component_totals$n,component_totals$component)
primary_outcome_indicators <- c("Separação em nível de participante","Intervalo de confiança","Denominador avaliado")
primary_outcome_totals <- f3 |> dplyr::filter(indicador %in% primary_outcome_indicators) |>
  dplyr::group_by(indicador) |> dplyr::summarise(n=sum(n),.groups="drop")

checks <- data.frame(
  check=c(
    "model_task_units","construct_distribution","stage_distribution","clinical_units",
    "validity_chain_each_layer","participant_separation_total","participant_separation_values",
    "primary_outcome_indicators","thermography_applicable_units","thermography_records",
    "tehai_systems","tehai_component_denominators","tehai_assessments","stage_3_4_units"
  ),
  expected=c(
    "50","18/0/22/3/7","12/11/24/3/0","38","38/38/38/38","54","19/13/22",
    "54/54/54","47","50","29","174/145/116","435","0"
  ),
  observed=c(
    sum(f1$n),paste(construct_totals[construct_order],collapse="/"),paste(stage_totals[stage_order],collapse="/"),sum(f2$n),
    paste(layer_totals$n,collapse="/"),sum(expected_sep$n),
    paste(setNames(expected_sep$n,expected_sep$state)[c("Absent/not reported","Partial/uncertain","Documented/favorable")],collapse="/"),
    paste(sort(primary_outcome_totals$n),collapse="/"),
    unique(f3$denominador[f3$indicador=="Relato termográfico global"]),unique(f4$denominador),
    unique(f5$denominador_subcomponente),
    paste(component_totals[c("Capability","Utility","Adoption")],collapse="/"),sum(f5$n),
    sum(f1$n[f1$estagio_clinico=="Níveis 3–4"])
  ),
  stringsAsFactors=FALSE
)
checks$status <- ifelse(checks$expected==checks$observed,"PASS","FAIL")
readr::write_csv(checks,file.path(out_dir,"verificacao","NUMERIC_RECONCILIATION.csv"))
if(any(checks$status!="PASS")) stop("Scientific invariant failed; outputs are HOLD.",call.=FALSE)

legends <- c(
  "# Figure legends", "",
  "## Figure 1. Study selection", "Study-selection flow for the primary corpus. Late removal of 115 exact title duplicates is shown separately and was not retrospectively incorporated into the initial deduplication count. Thirty-three studies were included.", "",
  "## Figure 2. Pain-related construct and clinical evidence stage", "Heatmap of 50 model-task units cross-classified by operational pain-related construct and clinical evidence stage. Not classifiable was retained as distinct from Stage 0, and no unit reached Stages 3-4.", "",
  "## Figure 3. Interpretive chain of validity", "Parallel summaries of 38 classifiable clinical model-task units. Circle area and horizontal position both represent the absolute count. Panels summarize the same corpus independently and do not imply a causal or temporal pathway.", "",
  "## Figure 4. Methodological and reproducibility profile", "Separate profiles show validation and confidence, reporting, open science, and equity. Denominators are stated for every indicator, and no composite score was calculated.", "",
  "## Figure 5. Thermographic acquisition and signal documentation", "Documentation frequency across 50 thermographic acquisition records. Dot position shows the reported proportion; labels give absolute counts and percentages. NETD, noise-equivalent temperature difference; ROI, region of interest.", "",
  "## Figure 6. Complementary translational profile", "Unweighted TEHAI score distributions within Capability, Utility, and Adoption across 29 study-systems and 435 item-unit assessments. No weighting, global score, cutoff, or ranking was calculated."
)
writeLines(legends,file.path(out_dir,"documentacao","FIGURE_LEGENDS_PAIN_SUBMISSION.md"),useBytes=TRUE)

files <- list.files(out_dir, recursive=TRUE, full.names=TRUE)
manifest <- data.frame(path=sub(paste0("^",out_dir,"/"),"",files), bytes=file.info(files)$size,
                       sha256=vapply(files,digest::digest,character(1),file=TRUE,algo="sha256"))
readr::write_csv(manifest,file.path(out_dir,"verificacao","SHA256_MANIFEST.csv"))
writeLines(capture.output(sessionInfo()),file.path(out_dir,"verificacao","sessionInfo.txt"),useBytes=TRUE)
registrar_execucao("09_figuras_2_a_6", inicio_etapa, files)
cat("Figuras 2 a 6 concluídas em: ",out_dir,"\n",sep="")
