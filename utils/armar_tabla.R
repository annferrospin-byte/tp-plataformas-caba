# armar_tabla(): tabla de contingencia entre dos variables, con o sin pesos.
# Con pesos (w = "fexp") los pesos se reescalan (fexp / media de fexp): cada caso
# cuenta segun a cuantas personas representa, pero la tabla suma el tamano de la
# muestra y no la poblacion (con pesos crudos todo saldria significativo).
armar_tabla <- function(datos, x, y, w = NULL) {
  d  <- datos[!is.na(datos[[x]]) & !is.na(datos[[y]]), ]
  fx <- droplevels(as.factor(d[[x]]))
  fy <- droplevels(as.factor(d[[y]]))
  if (is.null(w)) {
    table(fx, fy)
  } else {
    peso_norm <- d[[w]] / mean(d[[w]])
    tabla <- tapply(peso_norm, list(fx, fy), sum)
    tabla[is.na(tabla)] <- 0
    tabla
  }
}
