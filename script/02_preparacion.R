# ==============================================================================
# 02_preparacion.R
# Objetivo: recodificar las variables y armar las dos bases de analisis: la de
#           ocupacion principal (universo de tu3) y la de ocupacion secundaria
#           (universo de tuso3). Tambien arma la tabla de variables utilizadas.
# Entradas: input/individual.rds, auxiliar/ (diccionario)
# Salidas:  input/base_principal.rds, input/base_secundaria.rds,
#           input/diccionario.rds, output/tablas/variables_utilizadas.csv
# ==============================================================================
library(tidyverse)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
individual <- readRDS("input/individual.rds")

# ---- 1. Base de la ocupacion principal ----------------------------------------
# tu3: 1 = trabaja a traves de una app o plataforma, 2 = no, 0 = no corresponde
base <- individual %>%
  filter(tu3 %in% c(1, 2)) %>%
  preparar() %>%
  mutate(plataforma = factor(if_else(tu3 == 1, "Plataforma", "No plataforma"),
                             levels = c("No plataforma", "Plataforma")))

# ---- 2. Base de la ocupacion secundaria ---------------------------------------
base_sec <- individual %>%
  filter(tuso3 %in% c(1, 2)) %>%
  preparar() %>%
  mutate(plataforma_sec = factor(if_else(tuso3 == 1, "Plataforma", "No plataforma"),
                                 levels = c("No plataforma", "Plataforma")))

stopifnot(nrow(base) == 6499, nrow(base_sec) == 749)
print(table(base$plataforma))
print(table(base_sec$plataforma_sec))

# ---- 3. Diccionario y tabla de variables utilizadas ---------------------------
dicc <- leer_diccionario()

vars_usadas <- c("id", "nhogar", "miembro", "comuna", "fexp", "edad", "sexo", "estado",
                 "categori", "sem_hs", "tu3", "tuso3", "t37_coda_2", "codioph", "ioph_2", "nivel")

tabla_variables <- dicc %>%
  mutate(variable = if_else(variable == "nivel6", "nivel", variable)) %>%   # el diccionario documenta "nivel6"
  filter(variable %in% vars_usadas) %>%
  distinct(variable, etiqueta, universo) %>%
  left_join(tibble(variable = vars_usadas,
                   valores_na = map_int(vars_usadas, ~ sum(is.na(individual[[.x]])))),
            by = "variable")

# ---- 4. Guardado ---------------------------------------------------------------
saveRDS(base, "input/base_principal.rds")
saveRDS(base_sec, "input/base_secundaria.rds")
saveRDS(dicc, "input/diccionario.rds")
write_csv(tabla_variables, "output/tablas/variables_utilizadas.csv")
