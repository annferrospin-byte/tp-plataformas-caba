# ==============================================================================
# 01_carga_y_chequeo.R
# Objetivo: leer las bases originales de la EAH 2025 (raw/), verificar que estan
#           completas contra los totales de control del IDECBA (auxiliar/) y guardar
#           la base de personas en input/.
# Entradas: raw/eah2025_bu_ampliada_ind.txt, raw/eah2025_bu_ampliada_hog.txt,
#           auxiliar/eah2025_bu_ampliada_totales_de_control.xls
# Salidas:  input/individual.rds, output/tablas/chequeo_base.csv
# ==============================================================================
library(tidyverse)
library(readxl)
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Lectura de las bases originales --------------------------------------
ruta_ind <- list.files("raw", pattern = "_ind\\.txt$", full.names = TRUE)
ruta_hog <- list.files("raw", pattern = "_hog\\.txt$", full.names = TRUE)
stopifnot("Falta la base de personas en raw/" = length(ruta_ind) == 1,
          "Falta la base de hogares en raw/"  = length(ruta_hog) == 1)

individual <- read_delim(ruta_ind, delim = ";", locale = locale(encoding = "latin1"), show_col_types = FALSE)
hogar      <- read_delim(ruta_hog, delim = ";", locale = locale(encoding = "latin1"), show_col_types = FALSE)

# ---- 2. Chequeos de la base --------------------------------------------------
chequeo <- tibble(
  indicador = c("Personas encuestadas (filas de la base de personas)",
                "Variables de la base de personas",
                "Hogares encuestados (filas de la base de hogares)",
                "Población expandida: suma de fexp",
                "Personas con tu3 = 1 (trabajan a través de plataformas)",
                "Personas con tu3 = 2",
                "Personas con tu3 = 0 (no corresponde)"),
  valor_calculado = c(nrow(individual), ncol(individual), nrow(hogar),
                      round(sum(individual$fexp)), sum(individual$tu3 == 1),
                      sum(individual$tu3 == 2), sum(individual$tu3 == 0)),
  valor_esperado  = c(12457, 237, 5516, 3086714, 195, 6304, 5958)
)

# ---- 3. Chequeo contra los totales de control del IDECBA ---------------------
ruta_tc <- list.files("auxiliar", pattern = "totales_de_control", full.names = TRUE)
tc <- tryCatch({
  x <- suppressMessages(read_excel(ruta_tc[1], sheet = 1, col_names = FALSE))
  fila <- x[which(x[[1]] == "Total Ciudad")[1], ]
  as.numeric(unlist(fila[2:7]))   # viviendas, hogares, personas y los tres expandidos
}, error = function(e) NULL)

if (!is.null(tc) && length(tc) == 6 && !anyNA(tc)) {
  chequeo <- bind_rows(chequeo, tibble(
    indicador = c("Personas (totales de control del IDECBA)",
                  "Población expandida (totales de control del IDECBA)"),
    valor_calculado = c(nrow(individual), round(sum(individual$fexp))),
    valor_esperado  = tc[c(3, 6)]))
} else {
  message("No se pudieron leer los totales de control; se omite ese chequeo.")
}

chequeo <- chequeo %>% mutate(coincide = valor_calculado == valor_esperado)
print(as_tibble(chequeo), width = Inf)
if (!all(chequeo$coincide)) warning("Algun chequeo no coincide: revisar la carga de la base.")

# ---- 4. Guardado ---------------------------------------------------------------
write_csv(chequeo, "output/tablas/chequeo_base.csv")
saveRDS(individual, "input/individual.rds")
