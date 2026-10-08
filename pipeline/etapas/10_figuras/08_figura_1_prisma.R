# Desenha o fluxograma PRISMA com as contagens fornecidas pelas etapas anteriores.
# As contagens não são recalculadas nem corrigidas pela apresentação gráfica.


inicio_etapa <- Sys.time()
argumento_arquivo <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (!length(argumento_arquivo)) stop("Execute esta etapa com Rscript.", call. = FALSE)
source(file.path(dirname(normalizePath(argumento_arquivo[[1L]], mustWork = TRUE)), "00_contexto.R"))
source(file.path(CODE_ROOT, "pipeline", "etapas", "10_figuras", "07_estilo_editorial.R"))

pacotes <- c("readr", "dplyr", "ggplot2", "ragg")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) stop("Pacotes ausentes: ", paste(faltantes, collapse = ", "), call. = FALSE)

entrada <- caminho("dados", "saida", "tabelas_figuras", "figura_1_prisma.csv")
saida <- caminho("dados", "saida", "figuras_editoriais", "figuras")
criar_pasta(saida)
dados <- readr::read_csv(entrada, show_col_types = FALSE, na = character())
exigir_colunas(dados, c("etapa", "n"), "Figura 1")

esperado <- c(3398, 1789, 1609, 1452, 115, 42, 5, 37, 4, 33)
if (!identical(as.numeric(dados$n), esperado)) stop("As contagens do fluxo de seleção divergiram.", call. = FALSE)

rotulos <- c(
  "Registros identificados" = "Records identified",
  "Duplicatas exatas removidas" = "Exact duplicates removed",
  "Registros triados" = "Records screened",
  "Excluídos por título/resumo" = "Records excluded after title and abstract screening",
  "Limpeza tardia de duplicatas exatas por título" = "Late removal of exact title duplicates",
  "Relatórios buscados" = "Reports sought for retrieval",
  "Relatórios não recuperados" = "Reports not retrieved",
  "Textos completos avaliados" = "Full-text reports assessed",
  "Textos completos excluídos" = "Full-text reports excluded",
  "Estudos incluídos" = "Studies included"
)

posicoes <- data.frame(
  etapa = names(rotulos),
  x = c(0, 1.75, 0, 1.75, 1.75, 0, 1.75, 0, 1.75, 0),
  y = c(10, 10, 8, 8, 7, 6, 6, 4, 4, 2),
  tipo = c("principal", "lateral", "principal", "lateral", "lateral",
           "principal", "lateral", "principal", "lateral", "incluido")
)
x <- dplyr::left_join(posicoes, dados, by = "etapa")
x$label <- paste0(unname(rotulos[x$etapa]), "\nn = ", format(x$n, big.mark = ",", scientific = FALSE))

paleta <- paleta_editorial()
setas_principais <- data.frame(x = 0, xend = 0, y = c(9.55, 7.55, 5.55, 3.55), yend = c(8.45, 6.45, 4.45, 2.45))
setas_laterais <- data.frame(x = .38, xend = 1.35, y = c(10, 8, 7, 6, 4), yend = c(10, 8, 7, 6, 4))

figura <- ggplot2::ggplot() +
  ggplot2::geom_segment(data = setas_principais, ggplot2::aes(x, y, xend = xend, yend = yend),
                        arrow = grid::arrow(length = grid::unit(.10, "inches")), linewidth = .42, colour = paleta[["cinza"]]) +
  ggplot2::geom_segment(data = setas_laterais, ggplot2::aes(x, y, xend = xend, yend = yend),
                        arrow = grid::arrow(length = grid::unit(.09, "inches")), linewidth = .38, colour = paleta[["cinza"]]) +
  ggplot2::geom_label(data = x, ggplot2::aes(x, y, label = label, fill = tipo),
                      colour = paleta[["tinta"]], size = 3, lineheight = .96, linewidth = .28,
                      label.padding = grid::unit(.24, "lines")) +
  ggplot2::scale_fill_manual(values = c(principal = paleta[["claro"]], lateral = paleta[["lateral"]], incluido = paleta[["incluido"]]), guide = "none") +
  ggplot2::coord_cartesian(xlim = c(-.72, 2.78), ylim = c(1.3, 10.7), clip = "off") +
  ggplot2::theme_void(base_family = "Helvetica", base_size = 10) +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = paleta[["branco"]], colour = NA),
                 plot.margin = ggplot2::margin(14, 18, 14, 14))

ggplot2::ggsave(file.path(saida, "Figure_1_final.pdf"), figura, width = 7.4, height = 7.2, units = "in", device = grDevices::pdf, family = "Helvetica", bg = "white")
ggplot2::ggsave(file.path(saida, "Figure_1_final.png"), figura, width = 7.4, height = 7.2, units = "in", dpi = 600, device = ragg::agg_png, bg = "white")
ggplot2::ggsave(file.path(saida, "Figure_1_final.tiff"), figura, width = 7.4, height = 7.2, units = "in", dpi = 600, device = ragg::agg_tiff, compression = "lzw", bg = "white")
registrar_execucao("08_figura_1_prisma", inicio_etapa, list.files(saida, "^Figure_1_", full.names = TRUE))
