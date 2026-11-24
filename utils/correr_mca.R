# correr_mca(): MCA ponderado con FactoMineR.
# activas: variables que construyen los ejes. sup: variables suplementarias
# (se ubican en el mapa pero no lo construyen). Se agrega "edad" como numerica
# suplementaria. Los pesos son la columna fexp de d.
correr_mca <- function(d, activas, sup) {
  b <- d %>%
    select(all_of(c(activas, sup, "edad"))) %>%
    mutate(across(all_of(c(activas, sup)), ~ factor(as.character(.x)))) %>%
    as.data.frame()
  FactoMineR::MCA(b,
                  quali.sup  = length(activas) + seq_along(sup),
                  quanti.sup = length(activas) + length(sup) + 1,
                  row.w = d$fexp, graph = FALSE)
}
