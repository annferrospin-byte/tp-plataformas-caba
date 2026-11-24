# perfil_clusters(): perfil de cada cluster. Devuelve una lista con el perfil numerico
# y las composiciones por rama, nivel educativo y categoria ocupacional (en % dentro
# de cada cluster, sin ponderar). pct_pond es el peso del cluster en la poblacion.
perfil_clusters <- function(g) {
  g <- g %>% filter(!is.na(cluster))
  total <- sum(g$fexp)
  perfil <- g %>%
    group_by(cluster) %>%
    summarise(
      casos       = n(),
      pct_pond    = round(100 * sum(fexp) / total, 1),
      edad_media  = round(mean(edad), 1),
      horas_media = round(mean(horas_sem, na.rm = TRUE), 1),
      pct_varones = round(100 * mean(sexo_f == "Varón"), 1),
      con_ingreso = sum(!is.na(ing_hora_decl)),
      mediana_ing_hora = round(median(ing_hora_decl, na.rm = TRUE), 0),
      .groups = "drop"
    )
  comp <- function(v) {
    as.data.frame.matrix(round(prop.table(table(g$cluster, g[[v]]), margin = 1) * 100, 1)) %>%
      tibble::rownames_to_column("cluster")
  }
  list(perfil = perfil, rama = comp("rama_grp"), nivel = comp("nivel_grp"),
       categoria = comp("categori_f"))
}
