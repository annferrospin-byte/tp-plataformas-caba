# ==============================================================================
# 08_regresion.R
# Objetivo: tercer metodo. Regresion lineal del logaritmo del ingreso horario para
#           ver si la brecha de quienes trabajan en plataformas persiste al
#           controlar por sexo, edad, nivel educativo, rama y categoria ocupacional
#           (es decir, si se explica por la composicion del grupo).
# Entradas: input/base_principal.rds
# Salidas:  output/tablas/regresion_*.csv,
#           output/graficos/regresion_efecto_plataforma.jpg, regresion_diagnostico.jpg
# ==============================================================================
library(tidyverse)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
base <- readRDS("input/base_principal.rds")

# ---- 1. Muestra y variables -------------------------------------------------------------
# Solo quienes declararon el ingreso (ing_hora_decl). Los factores se dejan sin orden
# (con un factor ordenado, lm usa contrastes polinomicos) y con una categoria de
# referencia explicita. No se incluyen las horas como control: el ingreso horario ya
# las usa en el denominador.
d <- base %>%
  filter(!is.na(ing_hora_decl), !is.na(rama_grp), !is.na(nivel_grp), !is.na(categori_f)) %>%
  mutate(
    ln_ing_hora = log(ing_hora_decl),
    plataforma  = relevel(factor(as.character(plataforma)), ref = "No plataforma"),
    sexo_f      = relevel(factor(as.character(sexo_f)), ref = "Varón"),
    nivel_f     = relevel(factor(as.character(nivel_grp), ordered = FALSE), ref = "Secundario completo"),
    rama_f      = relevel(factor(as.character(rama_grp)), ref = "Otras ramas"),
    cat_f       = relevel(factor(as.character(categori_f)), ref = "Asalariado"))
message("Casos en la regresion: ", nrow(d), " (plataformas: ", sum(d$plataforma == "Plataforma"), ")")

# ---- 2. Modelos ----------------------------------------------------------------------------
m1 <- lm(ln_ing_hora ~ plataforma, data = d)
m2 <- lm(ln_ing_hora ~ plataforma + sexo_f + edad + nivel_f, data = d)
m3 <- lm(ln_ing_hora ~ plataforma + sexo_f + edad + nivel_f + rama_f + cat_f, data = d)
m3_pond <- lm(ln_ing_hora ~ plataforma + sexo_f + edad + nivel_f + rama_f + cat_f, data = d, weights = fexp)

modelos <- list("1. Sin controles" = m1, "2. + sexo, edad y nivel educativo" = m2,
                "3. + rama y categoría ocupacional" = m3, "3 ponderado con fexp" = m3_pond)

coeficientes <- bind_rows(lapply(names(modelos), function(n) tabla_coef(modelos[[n]], n)))
coeficientes <- coeficientes %>% mutate(across(where(is.numeric), ~ signif(.x, 4)))

resumen_modelos <- bind_rows(lapply(names(modelos), function(n) {
  m <- modelos[[n]]
  tc <- tabla_coef(m, n) %>% filter(termino == "plataformaPlataforma")
  data.frame(modelo = n, casos = nobs(m), r2 = round(summary(m)$r.squared, 3),
             r2_ajustado = round(summary(m)$adj.r.squared, 3),
             efecto_plataforma_pct = round(tc$efecto_pct, 1),
             ic_inf_pct = round(tc$efecto_pct_inf, 1), ic_sup_pct = round(tc$efecto_pct_sup, 1),
             p_valor_plataforma = signif(tc$p_valor, 3))
}))
print(as_tibble(resumen_modelos), width = Inf)

# ---- 3. Graficos ----------------------------------------------------------------------------
# Etiquetas cortas para el eje: los nombres completos de los modelos no entran.
graf <- resumen_modelos %>%
  mutate(etiqueta = c("1. Sin controles",
                      "2. + sexo, edad\ny nivel educativo",
                      "3. + rama y categoría\nocupacional",
                      "3, ponderado\ncon fexp"),
         etiqueta = fct_rev(fct_inorder(etiqueta)))

g_efecto <- ggplot(graf, aes(x = efecto_plataforma_pct, y = etiqueta)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_pointrange(aes(xmin = ic_inf_pct, xmax = ic_sup_pct), color = "#1F5FA8",
                  linewidth = 1, size = 0.8) +
  geom_text(aes(label = paste0(round(efecto_plataforma_pct, 1), "%")),
            vjust = -1.3, color = "#1F5FA8", fontface = "bold", size = 4) +
  scale_x_continuous(limits = c(-42, 2), breaks = seq(-40, 0, 10),
                     labels = scales::label_number(suffix = "%")) +
  labs(title = str_wrap("La brecha de ingreso horario persiste al controlar por composición", 60),
       subtitle = str_wrap("Diferencia porcentual del ingreso horario de quienes trabajan en plataformas, con su intervalo de confianza del 95%", 75),
       x = "Diferencia respecto de quienes no trabajan en plataformas (%)", y = NULL,
       caption = str_wrap("Fuente: elaboración propia en base a EAH 2025, IDECBA. Regresión lineal del logaritmo del ingreso horario; solo ingresos declarados.", 85)) +
  tema_tp(13)

diag <- data.frame(ajustado = fitted(m3), residuo = resid(m3))
g_diag <- ggplot(diag, aes(x = ajustado, y = residuo)) +
  geom_point(alpha = 0.25, color = "#1F5FA8") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Diagnóstico del modelo 3: residuos contra valores ajustados",
       x = "Valor ajustado (log del ingreso horario)", y = "Residuo",
       caption = "Fuente: elaboración propia en base a la EAH 2025 (IDECBA).") +
  tema_tp(12)

# ---- 4. Guardado ----------------------------------------------------------------------------
write_csv(coeficientes, "output/tablas/regresion_coeficientes.csv")
write_csv(resumen_modelos, "output/tablas/regresion_resumen_modelos.csv")
ggsave("output/graficos/regresion_efecto_plataforma.jpg", g_efecto, width = 9, height = 5.5, units = "in", dpi = 300)
ggsave("output/graficos/regresion_diagnostico.jpg", g_diag, width = 20, height = 12, units = "cm", dpi = 300)
