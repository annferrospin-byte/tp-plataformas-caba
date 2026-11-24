# concordancia_rama(): porcentaje de personas que coincide con la rama dominante de
# su cluster (en cada cluster se toma la rama mas frecuente, se suman esas personas
# y se divide por el total).
concordancia_rama <- function(cluster, rama) {
  round(100 * sum(apply(table(cluster, rama), 1, max)) / length(cluster))
}
