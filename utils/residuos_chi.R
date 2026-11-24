# residuos_chi(): residuos estandarizados del chi-cuadrado. Valores mayores a 2 o
# menores a -2 indican las celdas que mas se apartan de la independencia.
residuos_chi <- function(datos, x, y = "plataforma", w = NULL) {
  tabla <- armar_tabla(datos, x, y, w)
  round(suppressWarnings(chisq.test(tabla))$stdres, 2)
}
