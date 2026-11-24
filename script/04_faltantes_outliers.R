# ==============================================================================
# 04_faltantes_outliers.R
# Objetivo: diagnosticar los valores faltantes y los valores atipicos, y mostrar
#           como cambian los resultados segun la decision que se tome.
# Entradas: input/base_principal.rds
# Salidas:  output/tablas/faltantes_*.csv, imputacion_*.csv, sensibilidad_*.csv,
#           outliers_*.csv, percentiles_ponderados.csv,
#           output/graficos/outliers_boxplot.jpg
# ==============================================================================
library(tidyverse)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
base <- readRDS("input/base_principal.rds")

# ---- 1. Faltantes ---------------------------------------------------------------
# La EAH no deja celdas vacias: codifica (0 = no corresponde, 9 = Ns/Nc).
faltantes_grupo <- base %>%
  group_by(plataforma) %>%
  summarise(
    casos = n(),
    pct_nivel_nsnc       = round(100 * mean(is.na(nivel_grp)), 1),
    pct_rama_sin_clasif  = round(100 * mean(is.na(rama_grp)), 1),
    pct_categori_nsnc    = round(100 * mean(is.na(categori_f)), 1),
    pct_horas_no_corresp = round(100 * mean(is.na(horas_sem)), 1),
    pct_ingreso_imputado = round(100 * mean(codioph == 9), 1),
    pct_sin_ingresos     = round(100 * mean(codioph == 2), 1),
    .groups = "drop")

# Donde se concentra el ingreso imputado (sobre todos los ocupados del universo)
imputacion_categoria <- base %>%
  mutate(categoria = case_when(categori == 1 ~ "Patrón o empleador", categori == 2 ~ "Cuenta propia",
                               categori == 3 ~ "Asalariado", categori == 4 ~ "Trabajador familiar")) %>%
  group_by(categoria) %>%
  summarise(casos = n(), pct_imputado = round(100 * mean(codioph == 9), 1), .groups = "drop")

imputacion_comuna <- base %>%
  group_by(comuna) %>%
  summarise(casos = n(), pct_imputado = round(100 * mean(codioph == 9), 1), .groups = "drop")

# Decision: el ingreso se analiza solo para quienes declararon el monto
# (ing_hora_decl). Sensibilidad: que pasa si se suman los imputados.
sens_ingreso <- bind_rows(
  base %>% filter(!is.na(ing_hora_decl)) %>% mutate(criterio = "Solo declarantes", ing = ing_hora_decl),
  base %>% filter(!is.na(ing_hora_todos)) %>% mutate(criterio = "Declarantes + imputados", ing = ing_hora_todos)) %>%
  group_by(criterio, plataforma) %>%
  summarise(casos = n(),
            media   = round(weighted.mean(ing, fexp), 0),
            mediana = round(cuantil_pond(ing, fexp, 0.5), 0),
            .groups = "drop")

brecha_ingreso <- sens_ingreso %>%
  pivot_wider(names_from = plataforma, values_from = c(media, mediana, casos)) %>%
  mutate(brecha_media_pct   = round(100 * (1 - media_Plataforma / `media_No plataforma`), 1),
         brecha_mediana_pct = round(100 * (1 - mediana_Plataforma / `mediana_No plataforma`), 1))

# ---- 2. Outliers del ingreso horario ----------------------------------------------
# Criterio: rango intercuartilico (1,5 * IQR) sobre el logaritmo del ingreso horario.
ing <- base %>%
  filter(!is.na(ing_hora_decl)) %>%
  mutate(log_ing_hora = log(ing_hora_decl))

q1 <- unname(quantile(ing$log_ing_hora, 0.25))
q3 <- unname(quantile(ing$log_ing_hora, 0.75))
lim_inf <- q1 - 1.5 * IQR(ing$log_ing_hora)
lim_sup <- q3 + 1.5 * IQR(ing$log_ing_hora)

ing <- ing %>%
  mutate(tipo_outlier = case_when(log_ing_hora < lim_inf ~ "Bajo",
                                  log_ing_hora > lim_sup ~ "Alto",
                                  TRUE ~ "Normal"))

outliers_limites <- tibble(variable = "Ingreso horario ($ por hora)",
                           limite_inferior = round(exp(lim_inf)), limite_superior = round(exp(lim_sup)),
                           casos_analizados = nrow(ing))
outliers_conteo <- ing %>%
  count(plataforma, tipo_outlier) %>%
  group_by(plataforma) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# Horas semanales: mismo criterio, sobre la variable en niveles
horas <- base %>% filter(!is.na(horas_sem))
hq1 <- unname(quantile(horas$horas_sem, 0.25))
hq3 <- unname(quantile(horas$horas_sem, 0.75))
lim_h_inf <- hq1 - 1.5 * IQR(horas$horas_sem)
lim_h_sup <- hq3 + 1.5 * IQR(horas$horas_sem)

outliers_horas <- horas %>%
  mutate(atipica = horas_sem < lim_h_inf | horas_sem > lim_h_sup) %>%
  group_by(plataforma) %>%
  summarise(casos = n(), atipicas = sum(atipica), pct_atipicas = round(100 * mean(atipica), 1),
            maximo = max(horas_sem), limite_inferior = lim_h_inf, limite_superior = lim_h_sup,
            .groups = "drop")

# Los atipicos de ingreso: tienen horas dentro del rango habitual?
outliers_vs_horas <- ing %>%
  filter(tipo_outlier != "Normal") %>%
  mutate(horas_atipicas = horas_sem < lim_h_inf | horas_sem > lim_h_sup) %>%
  count(tipo_outlier, horas_atipicas)

# Decision: se mantienen los atipicos y se usan medianas. Sensibilidad:
p01 <- unname(quantile(ing$ing_hora_decl, 0.01))
p99 <- unname(quantile(ing$ing_hora_decl, 0.99))
ing <- ing %>% mutate(ing_hora_winsor = pmin(pmax(ing_hora_decl, p01), p99))

sens_outliers <- bind_rows(
  ing %>% mutate(version = "Con todos", ing_v = ing_hora_decl),
  ing %>% filter(tipo_outlier == "Normal") %>% mutate(version = "Sin outliers (IQR en logs)", ing_v = ing_hora_decl),
  ing %>% mutate(version = "Winsorizado p1-p99", ing_v = ing_hora_winsor)) %>%
  group_by(version, plataforma) %>%
  summarise(casos = n(),
            media   = round(weighted.mean(ing_v, fexp), 0),
            p25     = round(cuantil_pond(ing_v, fexp, 0.25), 0),
            mediana = round(cuantil_pond(ing_v, fexp, 0.50), 0),
            p75     = round(cuantil_pond(ing_v, fexp, 0.75), 0),
            .groups = "drop")

# Percentiles ponderados del ingreso horario
tabla_percentiles <- ing %>%
  group_by(plataforma) %>%
  summarise(casos = n(),
            p25_pond = round(cuantil_pond(ing_hora_decl, fexp, 0.25), 0),
            p50_pond = round(cuantil_pond(ing_hora_decl, fexp, 0.50), 0),
            p75_pond = round(cuantil_pond(ing_hora_decl, fexp, 0.75), 0),
            mensual_mediana_pond = round(cuantil_pond(ioph_2, fexp, 0.5), 0),
            .groups = "drop")

# ---- 3. Grafico del diagnostico de outliers ------------------------------------------
# Escala logaritmica: el ingreso horario tiene una cola larga hacia los valores altos y
# en escala lineal las cajas quedarian aplastadas contra el eje.
g_box <- ing %>%
  mutate(grupo = factor(if_else(plataforma == "Plataforma", "Plataformas", "Resto de ocupados"),
                        levels = c("Plataformas", "Resto de ocupados"))) %>%
  ggplot(aes(x = grupo, y = ing_hora_decl, fill = grupo)) +
  geom_boxplot(outlier.alpha = 0.35, outlier.size = 1.2, width = 0.55) +
  scale_y_log10(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_fill_manual(values = c("Plataformas" = "#1F5FA8", "Resto de ocupados" = "grey70"), guide = "none") +
  labs(title = "Los valores atípicos del ingreso horario son pocos en los dos grupos",
       subtitle = str_wrap("Ingreso horario de la ocupación principal, solo ingresos declarados. Población ocupada, CABA, 2025. Escala logarítmica: la distribución tiene una cola larga hacia los valores altos", 70),
       x = NULL, y = "Pesos por hora (escala logarítmica)",
       caption = str_wrap("Fuente: elaboración propia en base a EAH 2025, IDECBA. Los puntos son los valores atípicos según el criterio del rango intercuartílico sobre el logaritmo.", 80)) +
  tema_tp(11)

# ---- 4. Guardado ---------------------------------------------------------------------
tablas <- list(faltantes_por_grupo = faltantes_grupo, imputacion_por_categoria = imputacion_categoria,
               imputacion_por_comuna = imputacion_comuna, sensibilidad_ingreso = sens_ingreso,
               sensibilidad_ingreso_brechas = brecha_ingreso, outliers_limites = outliers_limites,
               outliers_conteo = outliers_conteo, outliers_horas = outliers_horas,
               outliers_vs_horas = outliers_vs_horas, sensibilidad_outliers = sens_outliers,
               percentiles_ponderados = tabla_percentiles)
for (nombre in names(tablas)) write_csv(tablas[[nombre]], file.path("output/tablas", paste0(nombre, ".csv")))
ggsave("output/graficos/outliers_boxplot.jpg", g_box, width = 8, height = 5.5, units = "in", dpi = 300)
