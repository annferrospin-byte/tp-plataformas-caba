# tabla_coef(): coeficientes de un modelo lm con intervalo de confianza. Como la
# variable dependiente esta en logaritmos, efecto_pct es el cambio porcentual
# aproximado que implica cada coeficiente: 100 * (exp(coef) - 1).
tabla_coef <- function(modelo, etiqueta_modelo = "") {
  s  <- summary(modelo)$coefficients
  ic <- confint(modelo)
  data.frame(
    modelo         = etiqueta_modelo,
    termino        = rownames(s),
    coef           = s[, "Estimate"],
    error_est      = s[, "Std. Error"],
    p_valor        = s[, "Pr(>|t|)"],
    ic_inf         = ic[, 1],
    ic_sup         = ic[, 2],
    efecto_pct     = 100 * (exp(s[, "Estimate"]) - 1),
    efecto_pct_inf = 100 * (exp(ic[, 1]) - 1),
    efecto_pct_sup = 100 * (exp(ic[, 2]) - 1),
    row.names = NULL
  )
}
