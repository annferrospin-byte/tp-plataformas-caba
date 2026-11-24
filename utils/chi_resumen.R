# chi_resumen(): resume el test chi-cuadrado de independencia entre x e y.
# Devuelve chi2, grados de libertad, p-valor, V de Cramer y frecuencias esperadas.
# Si alguna frecuencia esperada es menor a 5 (version sin ponderar) calcula tambien
# un p-valor por simulacion (p_simulado).
chi_resumen <- function(datos, x, y = "plataforma", w = NULL) {
  tabla <- armar_tabla(datos, x, y, w)
  test  <- suppressWarnings(chisq.test(tabla))
  n     <- sum(tabla)
  k     <- min(dim(tabla)) - 1
  chi2  <- unname(test$statistic)

  p_sim <- NA_real_
  if (is.null(w) && min(test$expected) < 5) {
    set.seed(123)
    p_sim <- chisq.test(tabla, simulate.p.value = TRUE, B = 10000)$p.value
  }

  data.frame(
    variable         = x,
    ponderado        = !is.null(w),
    n                = round(n),
    categorias       = nrow(tabla),
    chi2             = round(chi2, 2),
    gl               = unname(test$parameter),
    p_valor          = test$p.value,
    v_cramer         = round(sqrt(chi2 / (n * k)), 3),
    min_esperada     = round(min(test$expected), 2),
    pct_celdas_bajas = round(100 * mean(test$expected < 5), 1),
    p_simulado       = p_sim
  )
}
