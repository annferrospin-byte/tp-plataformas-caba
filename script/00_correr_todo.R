# ==============================================================================
# 00_correr_todo.R
# Objetivo: reproducir todo el analisis corriendo los scripts en orden.
# Requisito: abrir el proyecto (archivo .Rproj) para que el directorio de trabajo
#            sea la carpeta raiz, y tener los datos en raw/ y auxiliar/.
# ==============================================================================
scripts <- c("01_carga_y_chequeo.R", "02_preparacion.R", "03_distribuciones.R",
             "04_faltantes_outliers.R", "05_chi2.R", "06_chi2_estratificado.R",
             "07_mca_kmeans.R", "08_regresion.R", "09_graficos.R")
for (s in scripts) {
  message("\n==== ", s, " ====")
  source(file.path("script", s), encoding = "UTF-8")
}
message("\nListo. Resultados en output/tablas y output/graficos.")
