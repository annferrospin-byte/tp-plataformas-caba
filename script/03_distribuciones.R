# ==============================================================================
# 03_distribuciones.R
# Objetivo: describir las distribuciones de las principales variables, comparando a
#           quienes trabajan en plataformas con el resto de los ocupados (todo
#           ponderado con el factor de expansion).
# Entradas: input/base_principal.rds
# Salidas:  output/tablas/distribuciones_numericas.csv,
#           output/tablas/distribuciones_categoricas.csv,
#           output/graficos/densidad_edad.jpg, densidad_horas.jpg, densidad_ingreso.jpg,
#           barras_rama.jpg, barras_sexo.jpg, barras_edad.jpg, barras_nivel.jpg,
#           barras_categoria.jpg
# ==============================================================================
library(tidyverse)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
base <- readRDS("input/base_principal.rds")

# ---- 1. Variables numericas: resumen ponderado -------------------------------
tabla_num <- base %>%
  select(plataforma, fexp, Edad = edad, `Horas semanales` = horas_sem,
         `Ingreso horario` = ing_hora_decl) %>%
  pivot_longer(c(Edad, `Horas semanales`, `Ingreso horario`),
               names_to = "variable", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  group_by(variable, plataforma) %>%
  summarise(casos   = n(),
            media   = weighted.mean(valor, fexp),
            p25     = cuantil_pond(valor, fexp, 0.25),
            mediana = cuantil_pond(valor, fexp, 0.50),
            p75     = cuantil_pond(valor, fexp, 0.75),
            minimo  = min(valor),
            maximo  = max(valor),
            .groups = "drop") %>%
  mutate(across(c(media, p25, mediana, p75, minimo, maximo), ~ round(.x, 1)))
print(as_tibble(tabla_num), width = Inf)

# ---- 2. Variables categoricas: distribucion ponderada --------------------------
tabla_cat <- bind_rows(
  distribucion_ponderada(base, "sexo_f", "Sexo"),
  distribucion_ponderada(base, "edad_grp", "Edad (tramos)"),
  distribucion_ponderada(base, "nivel_grp", "Nivel educativo"),
  distribucion_ponderada(base, "rama_grp", "Rama de actividad"),
  distribucion_ponderada(base, "categori_f", "Categoría ocupacional")
)

# ---- 3. Graficos ----------------------------------------------------------------
# Densidades: edad, horas e ingreso horario. En el ingreso se recortan los valores
# de $30.000 por hora o mas, solo por legibilidad del grafico.
g_edad <- grafico_densidad(base, "edad", "Edad", "Distribución etaria según inserción laboral")
g_horas <- grafico_densidad(base, "horas_sem", "Horas semanales trabajadas (todas las ocupaciones)",
                            "Horas trabajadas según inserción laboral")
g_ing <- grafico_densidad(base, "ing_hora_decl", "Pesos por hora",
                          "Ingreso horario según inserción laboral", corte = 30000)

# Barras: composicion de quienes trabajan en plataformas (como el grafico por rama).
# La rama se muestra con 6 categorias.
base_g <- base %>%
  mutate(rama_det = case_when(
    t37_coda_2 == 1 ~ "Industria",
    t37_coda_2 == 3 ~ "Comercio, alojamiento\ny gastronomía",
    t37_coda_2 == 4 ~ "Transporte, correo\ny mensajería",
    t37_coda_2 == 5 ~ "Servicios profesionales,\nfinancieros e info.",
    t37_coda_2 == 7 ~ "Enseñanza y salud",
    t37_coda_2 == 8 ~ "Artes y otros servicios",
    TRUE ~ NA_character_))

g_rama <- grafico_barras_composicion(base_g, "rama_det", "Trabajadores de plataformas por rama de actividad")
g_sexo <- grafico_barras_composicion(base_g, "sexo_f", "Trabajadores de plataformas por sexo")
g_edad_grp <- grafico_barras_composicion(base_g, "edad_grp", "Trabajadores de plataformas por tramo de edad",
                                         orden = c("Hasta 29", "30 a 44", "45 a 59", "60 o más"))
g_nivel <- grafico_barras_composicion(base_g, "nivel_grp", "Trabajadores de plataformas por nivel educativo",
                                      orden = c("Hasta primario completo", "Secundario incompleto", "Secundario completo",
                                                "Superior incompleto", "Superior completo"))
g_categoria <- grafico_barras_composicion(base_g, "categori_f", "Trabajadores de plataformas por categoría ocupacional")

# ---- 4. Guardado ---------------------------------------------------------------
write_csv(tabla_num, "output/tablas/distribuciones_numericas.csv")
write_csv(tabla_cat, "output/tablas/distribuciones_categoricas.csv")
graficos <- list(densidad_edad = g_edad, densidad_horas = g_horas, densidad_ingreso = g_ing,
                 barras_rama = g_rama, barras_sexo = g_sexo, barras_edad = g_edad_grp,
                 barras_nivel = g_nivel, barras_categoria = g_categoria)
for (nombre in names(graficos)) {
  ggsave(file.path("output/graficos", paste0(nombre, ".jpg")), graficos[[nombre]],
         width = 8, height = 5.5, units = "in", dpi = 300)
}
