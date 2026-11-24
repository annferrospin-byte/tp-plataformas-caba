# ==============================================================================
# 06_chi2_estratificado.R
# Objetivo: separar el efecto del sexo del efecto de la rama. Sexo y rama estan muy
#           superpuestos (84% de varones en transporte), asi que se repite el test
#           de sexo contra trabajar en plataformas DENTRO de cada rama.
# Entradas: input/base_principal.rds
# Salidas:  output/tablas/chi_estratificado_sexo_por_rama.csv,
#           output/tablas/incidencia_rama_sexo.csv,
#           output/tablas/composicion_sexo_por_rama.csv
# ==============================================================================
library(tidyverse)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
base <- readRDS("input/base_principal.rds") %>% filter(!is.na(rama_grp))

# ---- 1. Composicion por sexo de cada rama (la superposicion) -----------------------
composicion <- base %>%
  count(rama_grp, sexo_f) %>%
  group_by(rama_grp) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# ---- 2. Incidencia por rama y sexo (ponderada) -------------------------------------
incidencia_rama_sexo <- base %>%
  group_by(rama_grp, sexo_f) %>%
  summarise(casos = n(),
            casos_plataforma = sum(plataforma == "Plataforma"),
            pct_plataforma = round(100 * weighted.mean(plataforma == "Plataforma", fexp), 1),
            .groups = "drop")

# ---- 3. Chi-cuadrado de sexo dentro de cada rama -----------------------------------
# En ramas con pocos casos de plataforma las frecuencias esperadas son bajas: se
# agrega el test exacto de Fisher.
ramas <- sort(unique(as.character(base$rama_grp)))

estratificado <- bind_rows(lapply(ramas, function(r) {
  d    <- base %>% filter(rama_grp == r)
  pond <- chi_resumen(d, "sexo_f", "plataforma", w = "fexp")
  sin  <- chi_resumen(d, "sexo_f", "plataforma")
  data.frame(
    rama = r,
    casos = nrow(d),
    casos_plataforma = sum(d$plataforma == "Plataforma"),
    mujeres_con_plataforma = sum(d$plataforma == "Plataforma" & d$sexo_f == "Mujer"),
    V_ponderado = pond$v_cramer, p_ponderado = signif(pond$p_valor, 3),
    V_sin_ponderar = sin$v_cramer, p_sin_ponderar = signif(sin$p_valor, 3),
    p_fisher = signif(fisher.test(table(d$sexo_f, d$plataforma))$p.value, 3),
    min_esperada = pond$min_esperada)
}))
print(as_tibble(estratificado), width = Inf)

write_csv(composicion, "output/tablas/composicion_sexo_por_rama.csv")
write_csv(incidencia_rama_sexo, "output/tablas/incidencia_rama_sexo.csv")
write_csv(estratificado, "output/tablas/chi_estratificado_sexo_por_rama.csv")
