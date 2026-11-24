# ==============================================================================
# 05_chi2.R
# Objetivo: test chi-cuadrado de independencia entre trabajar en plataformas y
#           varias variables, para la ocupacion principal (tu3) y la secundaria
#           (tuso3). Version principal con pesos reescalados; version sin ponderar
#           como control. Tamano del efecto: V de Cramer.
# Entradas: input/base_principal.rds, input/base_secundaria.rds
# Salidas:  output/tablas/chi_principal_*.csv, chi_secundaria_*.csv,
#           chi_residuos_*.csv, incidencia_por_categoria.csv
# ==============================================================================
library(tidyverse)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
base     <- readRDS("input/base_principal.rds")
base_sec <- readRDS("input/base_secundaria.rds")

# p-valor < 0,05 sin ajustar por comparaciones multiples (se hicieron 13 tests).
lectura <- function(p) ifelse(p < 0.05, "Asociación", "Sin evidencia de asociación")

# ---- 1. Ocupacion principal ----------------------------------------------------
vars_chi <- c("sexo_f", "edad_grp", "nivel_grp", "rama_grp", "categori_f", "comuna_f", "ingreso_decl")

chi_sin  <- do.call(rbind, lapply(vars_chi, function(v) chi_resumen(base, v, "plataforma")))
chi_pond <- do.call(rbind, lapply(vars_chi, function(v) chi_resumen(base, v, "plataforma", w = "fexp")))
chi_sin$lectura  <- lectura(chi_sin$p_valor)
chi_pond$lectura <- lectura(chi_pond$p_valor)

chi_comparacion <- data.frame(
  variable = chi_pond$variable,
  V_sin_ponderar = chi_sin$v_cramer, V_ponderado = chi_pond$v_cramer,
  p_sin_ponderar = signif(chi_sin$p_valor, 3), p_ponderado = signif(chi_pond$p_valor, 3),
  lectura_sin_ponderar = chi_sin$lectura, lectura_ponderado = chi_pond$lectura)
print(as_tibble(chi_comparacion), width = Inf)

# ---- 2. Ocupacion secundaria -----------------------------------------------------
# rama y categoria describen la ocupacion PRINCIPAL; ingreso_decl se omite
# porque refiere al ingreso de la ocupacion principal.
vars_chi_sec <- c("sexo_f", "edad_grp", "nivel_grp", "rama_grp", "categori_f", "comuna_f")

chi_sec_sin  <- do.call(rbind, lapply(vars_chi_sec, function(v) chi_resumen(base_sec, v, "plataforma_sec")))
chi_sec_pond <- do.call(rbind, lapply(vars_chi_sec, function(v) chi_resumen(base_sec, v, "plataforma_sec", w = "fexp")))
chi_sec_sin$lectura  <- lectura(chi_sec_sin$p_valor)
chi_sec_pond$lectura <- lectura(chi_sec_pond$p_valor)

# ---- 3. Donde esta la asociacion ---------------------------------------------------
# Incidencia: % de cada categoria que trabaja en plataformas (ponderado)
incidencia <- bind_rows(lapply(c("rama_grp", "categori_f", "sexo_f"), function(v) {
  base %>%
    filter(!is.na(.data[[v]])) %>%
    group_by(variable = v, categoria = as.character(.data[[v]])) %>%
    summarise(casos = n(),
              pct_plataforma = round(100 * weighted.mean(plataforma == "Plataforma", fexp), 1),
              .groups = "drop")
}))

residuos_a_df <- function(m) as.data.frame.matrix(m) %>% tibble::rownames_to_column("categoria")
res_rama  <- residuos_a_df(residuos_chi(base, "rama_grp", "plataforma", w = "fexp"))
res_cat   <- residuos_a_df(residuos_chi(base, "categori_f", "plataforma", w = "fexp"))
res_sexo  <- residuos_a_df(residuos_chi(base, "sexo_f", "plataforma", w = "fexp"))
res_sec_rama  <- residuos_a_df(residuos_chi(base_sec, "rama_grp", "plataforma_sec", w = "fexp"))
res_sec_nivel <- residuos_a_df(residuos_chi(base_sec, "nivel_grp", "plataforma_sec", w = "fexp"))

# ---- 4. Guardado ---------------------------------------------------------------------
tablas <- list(chi_principal_ponderado = chi_pond, chi_principal_sin_ponderar = chi_sin,
               chi_principal_comparacion = chi_comparacion,
               chi_secundaria_ponderado = chi_sec_pond, chi_secundaria_sin_ponderar = chi_sec_sin,
               incidencia_por_categoria = incidencia,
               chi_residuos_rama = res_rama, chi_residuos_categoria = res_cat, chi_residuos_sexo = res_sexo,
               chi_residuos_sec_rama = res_sec_rama, chi_residuos_sec_nivel = res_sec_nivel)
for (nombre in names(tablas)) write_csv(tablas[[nombre]], file.path("output/tablas", paste0(nombre, ".csv")))
