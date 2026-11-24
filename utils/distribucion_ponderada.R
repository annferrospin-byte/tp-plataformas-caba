# distribucion_ponderada(): distribucion (en % dentro de cada grupo) de una variable
# categorica, ponderada con fexp, para quienes trabajan en plataformas y el resto.
distribucion_ponderada <- function(datos, variable, etiqueta = variable) {
  datos %>%
    filter(!is.na(.data[[variable]])) %>%
    group_by(plataforma, categoria = as.character(.data[[variable]])) %>%
    summarise(personas = sum(fexp), casos = n(), .groups = "drop") %>%
    group_by(plataforma) %>%
    mutate(pct = round(100 * personas / sum(personas), 1)) %>%
    ungroup() %>%
    mutate(variable = etiqueta)
}
