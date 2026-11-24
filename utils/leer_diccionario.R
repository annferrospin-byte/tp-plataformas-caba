# leer_diccionario(): lee el diseno de registros de la EAH (hoja de personas) y lo
# deja en formato largo: una fila por categoria de cada variable.
# Columnas: variable, etiqueta, valor, etiqueta_valor, universo.
leer_diccionario <- function(carpeta = "auxiliar") {
  ruta <- list.files(carpeta, pattern = "registros", full.names = TRUE)
  stopifnot("No se encontro el diseno de registros en auxiliar/" = length(ruta) == 1)
  suppressMessages(
    readxl::read_excel(ruta, sheet = "eah2025_bu_ampliada_ind") %>%
      slice(-(1:2)) %>%
      set_names(c("variable", "etiqueta", "valor", "etiqueta_valor", "universo")) %>%
      fill(variable, etiqueta, universo, .direction = "down")
  )
}
