# tema_tp(): tema comun de los graficos del trabajo: theme_minimal() con la leyenda abajo.
tema_tp <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(legend.position = "bottom")
}
