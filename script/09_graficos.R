# ==============================================================================
# 09_graficos.R
# Objetivo: los dos graficos principales: el comunicativo (incidencia por
#           rama y sexo) y el exploratorio (clusters con y sin la rama).
# Entradas: input/base_principal.rds, input/clusters_plataformas.rds,
#           output/tablas/clusters_sin_rama.csv
# Salidas:  output/graficos/grupo_27_comunicativo.jpg, grupo_27_exploratorio.jpg
# ==============================================================================
library(tidyverse)
library(scales)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Grafico comunicativo -----------------------------------------------------------------
base <- readRDS("input/base_principal.rds")

d2 <- base %>%
  filter(!is.na(rama_grp)) %>%
  group_by(rama_grp, sexo_f) %>%
  summarise(pct = 100 * weighted.mean(plataforma == "Plataforma", fexp), .groups = "drop") %>%
  mutate(
    rama_corta = case_when(
      rama_grp == "Transporte" ~ "Transporte",
      rama_grp == "Comercio, alojamiento y gastronomía" ~ "Comercio y gastronomía",
      rama_grp == "Servicios profesionales, financieros e info." ~ "Servicios profesionales",
      TRUE ~ "Otras ramas"),
    grupo = paste0(rama_corta, " · ", if_else(sexo_f == "Varón", "varones", "mujeres")),
    grupo = fct_reorder(grupo, pct),
    resalta = rama_grp == "Transporte" & sexo_f == "Varón",
    x_txt = pmax(pct, 3.3) + 0.5,
    etiqueta = paste0(format(round(pct, 1), decimal.mark = ","), "%"))

# El titulo pinta de azul "varones del transporte" si esta instalado ggtext;
# si no, se usa un titulo comun con "(en azul)".
usar_ggtext <- requireNamespace("ggtext", quietly = TRUE)
titulo_com <- if (usar_ggtext) {
  "El trabajo en plataformas llega al 26% entre los <span style='color:#1F5FA8'>varones del transporte</span><br>y no supera el 5% en ningún otro grupo"
} else {
  str_wrap("El trabajo en plataformas llega al 26% entre los varones del transporte (en azul) y no supera el 5% en ningún otro grupo", 66)
}
estilo_titulo <- if (usar_ggtext) ggtext::element_markdown(face = "bold", size = 15) else element_text(face = "bold", size = 15)

g_com <- ggplot(d2, aes(x = pct, y = grupo, fill = resalta)) +
  geom_vline(xintercept = 3.3, linetype = "dashed", color = "grey35") +
  geom_col(width = 0.72) +
  geom_text(aes(x = x_txt, label = etiqueta, color = resalta), hjust = 0, size = 4.2,
            fontface = "bold", show.legend = FALSE) +
  annotate("text", x = 3.6, y = 8.7, hjust = 0, vjust = 0, size = 3.6, color = "grey35",
           label = "Promedio general de los ocupados: 3,3%") +
  annotate("text", x = 0.8, y = 8, hjust = 0, size = 4, color = "white", fontface = "bold",
           label = "casi 8 veces el promedio general") +
  scale_fill_manual(values = c(`TRUE` = "#1F5FA8", `FALSE` = "grey75"), guide = "none") +
  scale_color_manual(values = c(`TRUE` = "#1F5FA8", `FALSE` = "grey35"), guide = "none") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 10),
                     labels = label_number(suffix = "%"), expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.1))) +
  labs(title = titulo_com,
       subtitle = str_wrap("Porcentaje de ocupados que consigue su trabajo a través de una app o plataforma, según rama de actividad y sexo. Ciudad de Buenos Aires, 2025", 100),
       x = "Ocupados que trabajan a través de plataformas (%)", y = "Rama de actividad y sexo",
       caption = str_wrap("Fuente: elaboración propia en base a la Encuesta Anual de Hogares 2025 (IDECBA). Ocupados sin servicio doméstico.", 135)) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = estilo_titulo, plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9, color = "grey40"),
        axis.text.y = element_text(size = 12), plot.margin = margin(5.5, 16, 5.5, 5.5))

ggsave("output/graficos/grupo_27_comunicativo.jpg", g_com, width = 24, height = 14, units = "cm", dpi = 300)

# ---- 2. Grafico exploratorio ---------------------------------------------------------------------
d_exp <- readRDS("input/clusters_plataformas.rds")
sr <- read_csv("output/tablas/clusters_sin_rama.csv", show_col_types = FALSE)

v1 <- paste0("Con la rama entre las variables\n(", sr$concordancia_pct[1], "% coincide con la rama dominante; por azar, ", sr$por_azar_pct[1], "%)")
v2 <- paste0("Sin la rama entre las variables\n(", sr$concordancia_pct[2], "% coincide con la rama dominante; por azar, ", sr$por_azar_pct[2], "%)")
d_exp <- d_exp %>% mutate(version = factor(if_else(panel == "con_rama", v1, v2), levels = c(v1, v2)))

g_exp <- ggplot(d_exp, aes(dim1, dim2, color = cluster, shape = rama)) +
  geom_jitter(width = 0.04, height = 0.04, size = 2.8, alpha = 0.8) +
  facet_wrap(~ version, scales = "free") +
  scale_x_continuous(labels = label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(decimal.mark = ",")) +
  scale_color_viridis_d(name = "Cluster (k-means, k = 3)", end = 0.9) +
  scale_shape_manual(values = c(16, 17, 15), name = "Rama de actividad",
                     labels = function(x) str_wrap(x, 26)) +
  guides(color = guide_legend(order = 1, nrow = 1, title.position = "left"),
         shape = guide_legend(order = 2, nrow = 1, title.position = "left")) +
  labs(title = str_wrap("¿Los grupos de trabajadores de plataformas siguen a la rama aunque no se use para armarlos?", 85),
       subtitle = str_wrap("Cada punto es una persona que trabaja en plataformas (184, levemente desplazadas para ver las superpuestas), ubicada según las dos primeras dimensiones del MCA de cada panel. Color: cluster (k-means, k = 3). Forma: rama de actividad.", 120),
       x = "Dimensión 1 del MCA", y = "Dimensión 2 del MCA",
       caption = str_wrap("Fuente: elaboración propia en base a la Encuesta Anual de Hogares 2025 (IDECBA). MCA ponderado; k-means sin ponderar. «Por azar»: promedio de 2.000 reasignaciones al azar de los clusters. Excluye 11 personas (9 de «Otras ramas» y 2 sin dato de horas). Cada panel tiene su propio MCA y sus propios clusters: ejes y colores no son comparables entre paneles.", 150)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.text = element_text(size = 9), legend.title = element_text(size = 10),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 8.5, color = "grey40"))

ggsave("output/graficos/grupo_27_exploratorio.jpg", g_exp, width = 25, height = 15, units = "cm", dpi = 300)
