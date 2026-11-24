# grafico_barras_composicion(): barras horizontales con la composicion (en % del
# total, ponderada con fexp) de quienes trabajan en plataformas segun una variable
# categorica.
# orden: si la variable tiene un orden natural (edad, nivel educativo), vector con
# las categorias de arriba hacia abajo; si no, se ordena de mayor a menor.
grafico_barras_composicion <- function(datos, variable, titulo, orden = NULL) {
  d <- datos %>%
    filter(plataforma == "Plataforma", !is.na(.data[[variable]])) %>%
    count(categoria = as.character(.data[[variable]]), wt = fexp, name = "personas") %>%
    mutate(pct = 100 * personas / sum(personas))
  d$categoria <- if (is.null(orden)) reorder(d$categoria, d$pct) else factor(d$categoria, levels = rev(orden))
  d$etiqueta <- paste0(format(round(d$pct, 1), nsmall = 1, decimal.mark = ",", trim = TRUE), "%")
  ggplot(d, aes(x = categoria, y = pct)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = etiqueta), hjust = -0.2, size = 3.5) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(d$pct) * 1.15)) +
    labs(title = titulo, subtitle = "CABA, 2025", x = NULL, y = "% del total",
         caption = "Fuente: elaboración propia en base a EAH 2025, IDECBA") +
    tema_tp()
}
