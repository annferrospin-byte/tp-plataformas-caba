# grafico_densidad(): densidad de una variable numerica ponderada con fexp,
# comparando a quienes trabajan en plataformas con el resto de los ocupados.
# corte: si se indica, se excluyen los valores mayores o iguales al corte y se
# aclara en el subtitulo cuanto porcentaje de los casos queda afuera.
grafico_densidad <- function(datos, variable, xlab, titulo, corte = NULL,
                             subtitulo = "Población ocupada, CABA, 2025") {
  d <- datos %>%
    filter(!is.na(.data[[variable]])) %>%
    mutate(grupo = factor(if_else(plataforma == "Plataforma", "Plataformas", "Resto de ocupados"),
                          levels = c("Plataformas", "Resto de ocupados")))
  if (!is.null(corte)) {
    pct_fuera <- round(100 * mean(d[[variable]] >= corte), 1)
    d <- d %>% filter(.data[[variable]] < corte)
    subtitulo <- paste0(subtitulo, ".\nSe excluye el ", format(pct_fuera, decimal.mark = ","),
                        "% con valores de ", format(corte, big.mark = ".", decimal.mark = ","), " o más")
  }
  ggplot(d, aes(x = .data[[variable]], fill = grupo, weight = fexp)) +
    geom_density(alpha = 0.5) +
    scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = titulo, subtitle = subtitulo, x = xlab, y = "Densidad", fill = NULL,
         caption = "Fuente: elaboración propia en base a EAH 2025, IDECBA") +
    tema_tp()
}
