# cuantil_pond(): cuantil ponderado (base R no trae uno).
# Devuelve el primer valor de x cuyo peso acumulado alcanza "prob".
cuantil_pond <- function(x, w, prob = 0.5) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]
  w <- w[ok]
  orden <- order(x)
  x <- x[orden]
  w <- w[orden]
  acumulado <- cumsum(w) / sum(w)
  x[which(acumulado >= prob)[1]]
}
