# ==============================================================================
# 07_mca_kmeans.R
# Objetivo: analisis de correspondencias multiples (MCA) y k-means para ver si hay
#           perfiles distintos entre quienes trabajan en plataformas, y repetir el
#           analisis sin la rama como variable activa (Clase 16, practicas 2 y 4).
# Entradas: input/base_principal.rds
# Salidas:  input/clusters_plataformas.rds, output/tablas/mca_*.csv, clusters_*.csv,
#           output/graficos/mca_mapa_categorias.jpg, silueta_por_k.jpg, clusters_mca.jpg
# ==============================================================================
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
for (f in list.files("utils", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (carpeta in c("input", "output/tablas", "output/graficos")) dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)

base  <- readRDS("input/base_principal.rds")
grupo <- base %>% filter(plataforma == "Plataforma") %>% mutate(id_fila = row_number())

# ---- 1. Variables categoricas para el MCA -----------------------------------------
grupo_mca <- grupo %>%
  mutate(horas_grp = cut(horas_sem, breaks = c(0, 35, 45, Inf),
                         labels = c("Hasta 35 hs", "36 a 45 hs", "Más de 45 hs"))) %>%
  select(id_fila, fexp, rama_grp, categori_f, horas_grp, nivel_grp, sexo_f, edad_grp, edad) %>%
  drop_na() %>%   # se pierden 2 personas sin dato de horas (no trabajaron la semana de referencia)
  mutate(
    categori_f = factor(if_else(categori_f == "Asalariado", "Asalariado", "No asalariado")),
    nivel_grp = factor(case_when(
      nivel_grp %in% c("Hasta primario completo", "Secundario incompleto") ~ "Hasta secundario incompleto",
      TRUE ~ as.character(nivel_grp))))
stopifnot(nrow(grupo_mca) == 193)

# ---- 2. Exclusion de "Otras ramas" ---------------------------------------------------
# "Otras ramas" tiene 9 personas: una categoria rara se aleja del origen y construye
# una dimension casi sola.
activas_a <- c("rama_grp", "categori_f", "horas_grp", "nivel_grp")
res_con_otras <- correr_mca(grupo_mca, activas_a, c("sexo_f", "edad_grp"))
contrib_con_otras <- as.data.frame(round(res_con_otras$var$contrib[, 1:3], 1)) %>%
  tibble::rownames_to_column("categoria")

grupo_mca2 <- grupo_mca %>% filter(rama_grp != "Otras ramas")
stopifnot(nrow(grupo_mca2) == 184)

# ---- 3. MCA final (rama activa) y MCA sin la rama -----------------------------------
Q_a <- 4
res_a <- correr_mca(grupo_mca2, activas_a, c("sexo_f", "edad_grp"))
res_b <- correr_mca(grupo_mca2, c("categori_f", "horas_grp", "nivel_grp"), c("sexo_f", "edad_grp", "rama_grp"))

inercia <- data.frame(dimension = rownames(res_a$eig),
                      autovalor = round(res_a$eig[, 1], 3),
                      pct_sin_corregir = round(res_a$eig[, 2], 1),
                      pct_corregido_benzecri = benzecri(res_a, Q_a)["inercia_corregida", ],
                      acumulado_corregido = benzecri(res_a, Q_a)["acumulada", ])
mca_coord <- as.data.frame(round(res_a$var$coord[, 1:3], 2)) %>% tibble::rownames_to_column("categoria")
mca_contrib <- as.data.frame(round(res_a$var$contrib[, 1:3], 1)) %>% tibble::rownames_to_column("categoria")
mca_coord_sup <- as.data.frame(round(res_a$quali.sup$coord[, 1:3], 2)) %>% tibble::rownames_to_column("categoria")

g_mca <- fviz_mca_var(res_a, choice = "var.cat", repel = TRUE, col.var = "black",
                      col.quali.sup = "gray50", labelsize = 4, title = "Mapa de categorías del MCA")

# ---- 4. Eleccion de k (sobre las dos primeras dimensiones) ---------------------------
coord_a <- res_a$ind$coord[, 1:2]

# Para cada k: suma de cuadrados dentro de los clusters (codo) y silueta media.
tabla_k <- bind_rows(lapply(2:8, function(k) {
  set.seed(2026)
  km_k <- kmeans(coord_a, centers = k, nstart = 25)
  data.frame(k = k,
             suma_cuadrados_dentro = round(km_k$tot.withinss, 1),
             silueta_media = round(mean(silhouette(km_k$cluster, dist(coord_a))[, "sil_width"]), 3))
}))

# Estadistico gap, con 100 simulaciones
set.seed(2026)
gap <- clusGap(coord_a, FUN = kmeans, nstart = 25, K.max = 8, B = 100, iter.max = 50)
tabla_k$gap <- round(gap$Tab[2:8, "gap"], 3)
tabla_k$gap_error_est <- round(gap$Tab[2:8, "SE.sim"], 3)
print(tabla_k)

# ---- 5. k-means con k = 3: numeracion de clusters por rama dominante --------------------
set.seed(2026)
km_a_raw <- kmeans(coord_a, centers = 3, nstart = 50)
rama <- factor(grupo_mca2$rama_grp)
orden <- c("Transporte", "Comercio, alojamiento y gastronomía", "Servicios profesionales, financieros e info.")
dom <- apply(table(km_a_raw$cluster, rama), 1, function(x) names(which.max(x)))
nuevo <- match(dom, orden)
stopifnot(!anyNA(nuevo), !anyDuplicated(nuevo))
cluster_a <- nuevo[km_a_raw$cluster]      # 1 = transporte, 2 = comercio, 3 = servicios
message("Tamanos de los clusters: ", paste(table(cluster_a), collapse = ", "))

sil_a <- silhouette(cluster_a, dist(coord_a))
silueta_cluster <- data.frame(cluster = 1:3, silueta_media = round(tapply(sil_a[, "sil_width"], sil_a[, "cluster"], mean), 2),
                              casos = as.vector(table(cluster_a)))

# ---- 6. Estabilidad: PAM y semillas ----------------------------------------------------------
set.seed(2026)
pm <- pam(coord_a, k = 3)
ari_pam <- round(mclust::adjustedRandIndex(cluster_a, pm$clustering), 3)
cambian_pam <- 184 - sum(apply(table(cluster_a, pm$clustering), 1, max))

ari_semillas <- sapply(1:20, function(s) {
  set.seed(s)
  mclust::adjustedRandIndex(cluster_a, kmeans(coord_a, centers = 3, nstart = 1)$cluster)
})
estabilidad <- data.frame(medida = c("Rand ajustado k-means vs PAM", "Personas que cambian de grupo (PAM)",
                                     "Rand ajustado, 20 semillas con un solo arranque: minimo",
                                     "... mediana", "... maximo"),
                          valor = c(ari_pam, cambian_pam, round(min(ari_semillas), 2),
                                    round(median(ari_semillas), 3), round(max(ari_semillas), 2)))

# ---- 7. Perfil de los clusters ---------------------------------------------------------------------
grupo_cl <- grupo %>% mutate(cluster = factor(cluster_a[match(id_fila, grupo_mca2$id_fila)]))
perfil <- perfil_clusters(grupo_cl)

# ---- 8. Sin la rama como variable activa ---------------------------------------------------------------
coord_b <- res_b$ind$coord[, 1:2]
set.seed(2026)
km_b <- kmeans(coord_b, centers = 3, nstart = 50)

set.seed(1)
azar_a <- round(mean(replicate(2000, concordancia_rama(sample(cluster_a), rama))))
azar_b <- round(mean(replicate(2000, concordancia_rama(sample(km_b$cluster), rama))))

sin_rama <- data.frame(
  panel = c("Con la rama entre las variables", "Sin la rama entre las variables"),
  concordancia_pct = c(concordancia_rama(cluster_a, rama), concordancia_rama(km_b$cluster, rama)),
  por_azar_pct = c(azar_a, azar_b),
  rand_ajustado_vs_rama = round(c(mclust::adjustedRandIndex(cluster_a, rama),
                                  mclust::adjustedRandIndex(km_b$cluster, rama)), 2),
  tamanos_clusters = c(paste(table(cluster_a), collapse = " / "), paste(table(km_b$cluster), collapse = " / ")))
print(as_tibble(sin_rama), width = Inf)

d_exp <- bind_rows(
  tibble(panel = "con_rama", dim1 = coord_a[, 1], dim2 = coord_a[, 2], cluster = factor(cluster_a), rama = rama),
  tibble(panel = "sin_rama", dim1 = coord_b[, 1], dim2 = coord_b[, 2], cluster = factor(km_b$cluster), rama = rama))

# ---- 9. Graficos ---------------------------------------------------------------------------------------------
g_k <- ggplot(tabla_k, aes(x = k, y = silueta_media)) +
  geom_line(color = "#1F5FA8", linewidth = 1) +
  geom_point(color = "#1F5FA8", size = 3) +
  geom_text(aes(label = format(silueta_media, nsmall = 3, decimal.mark = ",")), vjust = -1, size = 3.5) +
  scale_x_continuous(breaks = 2:8) +
  scale_y_continuous(limits = c(0.38, 0.48), labels = scales::label_number(decimal.mark = ",", accuracy = 0.01)) +
  labs(title = "Silueta media según la cantidad de clusters",
       subtitle = "k-means sobre las dos primeras dimensiones del MCA. El eje vertical no empieza en 0.",
       x = "Cantidad de clusters (k)", y = "Silueta media",
       caption = "Fuente: elaboración propia en base a EAH 2025, IDECBA") +
  tema_tp()

g_clusters <- ggplot(filter(d_exp, panel == "con_rama"), aes(x = dim1, y = dim2, color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_x_continuous(labels = scales::label_number(decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
  labs(title = "Trabajadores de plataformas según cluster",
       subtitle = "Plano de las dos primeras dimensiones del MCA",
       x = "Dimensión 1", y = "Dimensión 2", color = "Cluster",
       caption = "Fuente: elaboración propia en base a EAH 2025, IDECBA") +
  theme_minimal(base_size = 12)

# ---- 10. Guardado ----------------------------------------------------------------------------------------------
saveRDS(d_exp, "input/clusters_plataformas.rds")
tablas <- list(mca_contribuciones_con_otras_ramas = contrib_con_otras, mca_inercia = inercia,
               mca_coordenadas_categorias = mca_coord, mca_contribuciones = mca_contrib,
               mca_coordenadas_suplementarias = mca_coord_sup, clusters_eleccion_k = tabla_k,
               clusters_silueta_por_cluster = silueta_cluster, clusters_estabilidad = estabilidad,
               clusters_perfil = perfil$perfil, clusters_composicion_rama = perfil$rama,
               clusters_composicion_nivel = perfil$nivel, clusters_composicion_categoria = perfil$categoria,
               clusters_sin_rama = sin_rama)
for (nombre in names(tablas)) write_csv(tablas[[nombre]], file.path("output/tablas", paste0(nombre, ".csv")))
ggsave("output/graficos/mca_mapa_categorias.jpg", g_mca, width = 22, height = 15, units = "cm", dpi = 300)
ggsave("output/graficos/silueta_por_k.jpg", g_k, width = 8, height = 5, units = "in", dpi = 300)
ggsave("output/graficos/clusters_mca.jpg", g_clusters, width = 8, height = 5.5, units = "in", dpi = 300)
