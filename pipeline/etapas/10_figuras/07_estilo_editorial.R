# Define as cores, as fontes e o acabamento comum das figuras.
# A apresentação visual é separada dos dados e dos cálculos da revisão.


paleta_editorial <- function() {
  c(tinta = "#1F2933", azul = "#334E68", ardosa = "#6B87A1",
    cinza = "#627D98", verde = "#2F7774", ouro = "#C7A75B",
    claro = "#D9E2EC", nevoa = "#F5F7F9", lateral = "#F5F2EE",
    incluido = "#E7F0EF", branco = "#FFFFFF")
}

paleta_pain <- function() {
  p <- paleta_editorial()
  c(ink = p[["tinta"]], slate_dark = p[["azul"]], slate = p[["cinza"]],
    teal = p[["verde"]], pale = p[["claro"]], gold = p[["ouro"]],
    white = p[["branco"]], mist = p[["nevoa"]])
}

tema_pain <- function(base_size = 10.5) {
  p <- paleta_pain()
  ggplot2::theme_minimal(base_size = base_size, base_family = "Helvetica") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = p[["white"]], colour = NA),
      panel.background = ggplot2::element_rect(fill = p[["white"]], colour = NA),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = p[["pale"]], linewidth = .35),
      axis.line = ggplot2::element_line(colour = p[["ink"]], linewidth = .35),
      axis.ticks = ggplot2::element_line(colour = p[["ink"]], linewidth = .35),
      axis.text = ggplot2::element_text(colour = p[["ink"]]),
      axis.title = ggplot2::element_text(colour = p[["ink"]], face = "bold"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", colour = p[["ink"]], size = base_size + .5),
      legend.position = "bottom", legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = p[["ink"]], size = base_size - .5),
      plot.margin = ggplot2::margin(12, 15, 12, 12)
    )
}

tema_editorial_suplementar <- function(tamanho = 10) {
  p <- paleta_editorial()
  ggplot2::theme_minimal(base_family = "Helvetica", base_size = tamanho) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = p[["branco"]], colour = NA),
      panel.background = ggplot2::element_rect(fill = p[["branco"]], colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = p[["claro"]], linewidth = .35),
      axis.text = ggplot2::element_text(colour = p[["tinta"]]),
      axis.title = ggplot2::element_text(colour = p[["tinta"]], face = "bold"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", colour = p[["tinta"]]),
      legend.position = "bottom", legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(12, 16, 12, 12)
    )
}
