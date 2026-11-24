# benzecri(): inercia corregida de Benzecri para un MCA con Q variables activas.
# Solo cuentan los autovalores mayores que 1/Q. No comparar estos porcentajes con
# los de un PCA.
benzecri <- function(res, Q) {
  lambda <- res$eig[, 1]
  b <- ifelse(lambda > 1 / Q, ((Q / (Q - 1)) * (lambda - 1 / Q))^2, 0)
  rbind(inercia_corregida = round(100 * b / sum(b), 1),
        acumulada         = round(cumsum(100 * b / sum(b)), 1))
}
