# preparar(): recodifica las variables de la EAH 2025 que usa el trabajo.
# Entrada: data frame de personas (base original). Salida: el mismo con variables nuevas.
# Los codigos de nivel, rama, sexo y categoria salen del diccionario de la EAH 2025.
preparar <- function(df) {
  df %>%
    mutate(
      sexo_f = factor(sexo, levels = c(1, 2), labels = c("Varón", "Mujer")),

      edad_grp = cut(edad,
                     breaks = c(-Inf, 29, 44, 59, Inf),
                     labels = c("Hasta 29", "30 a 44", "45 a 59", "60 o más")),

      # Nivel educativo. Ns/Nc (9) pasa a NA. Los codigos 0, 1 y 8 son muy pocos
      # casos y se agrupan con "hasta primario completo".
      nivel_grp = factor(
        case_when(
          nivel %in% c(0, 1, 2, 3, 8) ~ "Hasta primario completo",
          nivel == 4 ~ "Secundario incompleto",
          nivel == 5 ~ "Secundario completo",
          nivel == 6 ~ "Superior incompleto",
          nivel == 7 ~ "Superior completo",
          TRUE ~ NA_character_
        ),
        levels = c("Hasta primario completo", "Secundario incompleto",
                   "Secundario completo", "Superior incompleto",
                   "Superior completo"),
        ordered = TRUE
      ),

      # Rama de actividad en 4 grupos. "Sin especificar" (11) pasa a NA.
      rama_grp = factor(
        case_when(
          t37_coda_2 == 4 ~ "Transporte",
          t37_coda_2 == 3 ~ "Comercio, alojamiento y gastronomía",
          t37_coda_2 == 5 ~ "Servicios profesionales, financieros e info.",
          t37_coda_2 %in% c(1, 2, 6, 7, 8, 10) ~ "Otras ramas",
          TRUE ~ NA_character_
        )
      ),

      # Categoria ocupacional. 0 (no corresponde) y 9 (Ns/Nc) pasan a NA.
      # Entre quienes trabajan en plataformas hay 3 patrones y 4 trabajadores
      # familiares: se agrupan para que las frecuencias esperadas del chi-cuadrado
      # no queden por debajo de 1.
      categori_f = factor(
        case_when(
          categori == 3 ~ "Asalariado",
          categori == 2 ~ "Cuenta propia",
          categori %in% c(1, 4) ~ "Patrón o trabajador familiar",
          TRUE ~ NA_character_
        )
      ),

      comuna_f = factor(comuna),

      # Calidad de la declaracion del ingreso (solo para quienes tienen ingresos)
      ingreso_decl = factor(
        case_when(
          codioph == 1 ~ "Declaró el monto",
          codioph == 9 ~ "Ingreso imputado",
          TRUE ~ NA_character_
        )
      ),

      # sem_hs == 0 significa "no corresponde" (no trabajo la semana de referencia)
      horas_sem = if_else(sem_hs > 0, sem_hs, NA_real_),

      # Ingreso horario = ingreso mensual / (horas semanales * 4,33)
      ing_hora_decl = if_else(codioph == 1 & ioph_2 > 0 & sem_hs > 0,
                              ioph_2 / (sem_hs * 4.33), NA_real_),
      ing_hora_todos = if_else(codioph %in% c(1, 9) & ioph_2 > 0 & sem_hs > 0,
                               ioph_2 / (sem_hs * 4.33), NA_real_)
    )
}
