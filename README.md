# Trabajo en plataformas en la Ciudad de Buenos Aires (EAH 2025)

Trabajo práctico final de **Ciencia de Datos para Economía y Negocios** (FCE-UBA, 2.º cuatrimestre 2026).

**Grupo 27:** Anna Martina Ferro y Ramiro Urbina.

## Objetivo e hipótesis

El trabajo a través de aplicaciones y plataformas suele presentarse como un fenómeno único, asociado al reparto y al transporte. La EAH 2025 permite ponerlo a prueba, porque pregunta de forma directa si el trabajo se obtiene por esa vía. El objetivo es caracterizar a quienes trabajan así en la Ciudad de Buenos Aires, ver qué variables se asocian con esa forma de trabajo y si dentro del grupo hay perfiles diferenciados.

**Hipótesis de trabajo.** En la primera entrega el trabajo en plataformas aparecía repartido entre ramas de actividad muy distintas, con perfiles de ingreso y de jornada también distintos. A partir de eso planteamos que su incidencia se asocia sobre todo con la rama de actividad, más que con las características sociodemográficas de las personas, y que dentro del grupo existen perfiles diferenciados.

Es una hipótesis exploratoria: se formuló mirando esta misma base y los métodos de este trabajo la evalúan sobre ella, así que los resultados describen asociaciones y no las confirman. El trabajo no tiene carácter causal.

## Base de datos y justificación

| | |
|---|---|
| **Base** | Encuesta Anual de Hogares (EAH) 2025, base usuarios ampliada |
| **Produce** | Instituto de Estadística y Censos de la Ciudad (IDECBA), Gobierno de la Ciudad de Buenos Aires |
| **Periodicidad** | Anual |
| **Período** | Año 2025 |
| **Unidad de observación** | Persona |
| **Tamaño** | 12.457 personas y 237 variables (base de personas); 5.516 hogares y 110 variables (base de hogares) |
| **Cobertura** | Hogares particulares de la Ciudad de Buenos Aires; población expandida: 3.086.714 personas |
| **Fuente** | https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2026/04/eah2025_bu_ampliada.zip |

### Por qué esta base

Elegimos la EAH 2025 por tres razones:

1. **Mide el fenómeno de forma directa.** La variable `tu3` pregunta si el trabajo se realiza u obtiene a través de una aplicación móvil o plataforma web, y `tuso3` hace lo mismo para la ocupación secundaria. No hace falta aproximarlo con la rama de actividad o la categoría ocupacional, como suele hacerse con otras encuestas.
2. **Permite caracterizar al grupo.** En la misma base están el ingreso, las horas trabajadas, la rama, la categoría ocupacional, el nivel educativo y la comuna, y el factor de expansión para llevar los resultados al total de la población.
3. **Es oficial y pública.** La produce el IDECBA con periodicidad anual y los microdatos se descargan libremente, así que el análisis es reproducible por cualquiera.

La contrapartida es el tamaño del grupo: 195 de las 12.457 personas encuestadas trabajan en plataformas en su ocupación principal. Alcanza para describir al conjunto, pero las estimaciones por subgrupo se apoyan en pocos casos y hay que leerlas con cuidado.

### Bases auxiliares

**No se usaron bases de datos auxiliares.** La carpeta `auxiliar/` contiene únicamente los archivos de apoyo que la propia EAH publica junto con la base: el diccionario de variables (diseño de registros), los totales de control que se usan para verificar la carga y los clasificadores de rama de actividad y ocupación. No hubo cruces con otras bases, por lo que no hubo pérdida de observaciones por ese motivo.

## Variables utilizadas

| Variable | Descripción | Tipo |
|---|---|---|
| `tu3` | ¿El trabajo se realiza u obtiene a través de una app o plataforma? (ocupación principal). Universo: ocupados, sin servicio doméstico | Categórica (1 sí, 2 no, 0 no corresponde) |
| `tuso3` | Lo mismo para la ocupación secundaria. Universo: ocupados con más de una ocupación | Categórica (1 sí, 2 no, 0 no corresponde) |
| `fexp` | Factor de expansión: a cuántas personas representa cada encuestado | Numérica |
| `sexo` | Sexo | Categórica (1 varón, 2 mujer) |
| `edad` | Edad en años cumplidos | Numérica |
| `nivel` | Máximo nivel educativo alcanzado | Categórica (7 categorías; 9 Ns/Nc) |
| `categori` | Categoría ocupacional | Categórica (patrón, cuenta propia, asalariado, trabajador familiar) |
| `t37_coda_2` | Rama de actividad del establecimiento (codificación agregada) | Categórica (11 categorías) |
| `comuna` | Comuna de residencia | Categórica (15 comunas) |
| `sem_hs` | Horas trabajadas en la semana de referencia, todas las ocupaciones | Numérica (0 = no corresponde) |
| `ioph_2` | Ingreso mensual de la ocupación principal, en pesos | Numérica |
| `codioph` | Calidad de la declaración del ingreso | Categórica (1 declaró, 2 sin ingresos, 9 imputado) |

La cantidad de valores faltantes de cada una está en `output/tablas/variables_utilizadas.csv`, que genera el script 02. La EAH no deja celdas vacías: la falta de dato está codificada (0 «no corresponde», 9 «Ns/Nc»), y los scripts la convierten en `NA` donde corresponde.

### Variables construidas

| Variable | Cómo se calcula |
|---|---|
| `ing_hora_decl` | `ioph_2 / (sem_hs * 4,33)`, solo para quienes declararon el monto (`codioph == 1`). Es el ingreso horario de la ocupación principal |
| `ing_hora_todos` | Igual, pero incluyendo los ingresos imputados (`codioph` 1 o 9). Se usa solo para el análisis de sensibilidad |
| `horas_sem` | `sem_hs`, con el 0 convertido a `NA` (no trabajó la semana de referencia) |
| `rama_grp` | Rama agrupada en 4 categorías: transporte; comercio, alojamiento y gastronomía; servicios profesionales, financieros e información; otras ramas |
| `categori_f` | Categoría ocupacional en 3 grupos: asalariado, cuenta propia, patrón o trabajador familiar |
| `nivel_grp` | Nivel educativo en 5 grupos, de «hasta primario completo» a «superior completo» |
| `edad_grp` | Tramos de edad: hasta 29, 30 a 44, 45 a 59, 60 o más |
| `horas_grp` | Tramos de jornada: hasta 35 horas, 36 a 45, más de 45 |

## Qué se hizo

1. **Carga y chequeo** de la base contra los totales de control del IDECBA (12.457 personas; suma de `fexp` = 3.086.714).
2. **Preparación** de las variables y armado de las bases de ocupación principal (universo de `tu3`: 6.499 personas, 195 en plataformas) y secundaria (universo de `tuso3`: 749 personas, 49 en plataformas).
3. **Distribuciones** de las principales variables, ponderadas con el factor de expansión.
4. **Valores faltantes y atípicos**: diagnóstico, criterio, decisión y análisis de sensibilidad.
5. **Test chi-cuadrado** de independencia (con pesos reescalados y sin ponderar) y tamaño del efecto (V de Cramér).
6. **Chi-cuadrado estratificado**: sexo contra plataformas dentro de cada rama.
7. **MCA y k-means** para buscar perfiles dentro del grupo, y repetición sin la rama como variable activa.
8. **Regresión lineal** del logaritmo del ingreso horario, para ver si la brecha persiste al controlar por composición.
9. **Gráficos finales** (comunicativo y exploratorio).

Todos los cálculos poblacionales usan el factor de expansión (`fexp`). Decisiones relevantes: el ingreso se analiza solo para quienes declararon el monto, los valores atípicos se mantienen, y «Otras ramas» (9 personas) se excluye del MCA.

## Estructura del repositorio

```
plataformas-caba-eah2025/
├── raw/          # Datos originales de la EAH 2025, sin modificar
├── auxiliar/     # Diccionario, totales de control y clasificadores del IDECBA
├── input/        # Datos procesados, listos para el análisis (.rds)
├── output/
│   ├── tablas/   # Tablas de resultados generadas por los scripts (.csv)
│   └── graficos/ # Visualizaciones generadas por los scripts (.jpg)
├── script/       # Un script por objetivo, numerados en orden de ejecución
├── utils/        # Funciones propias, un script por función
└── README.md
```

| Script | Objetivo |
|---|---|
| `00_correr_todo.R` | Corre todos los scripts en orden |
| `01_carga_y_chequeo.R` | Lee `raw/`, verifica la base y la guarda en `input/` |
| `02_preparacion.R` | Recodifica variables y arma las bases de análisis |
| `03_distribuciones.R` | Distribuciones de las principales variables |
| `04_faltantes_outliers.R` | Faltantes, atípicos, sensibilidad de las decisiones y boxplot del ingreso |
| `05_chi2.R` | Chi-cuadrado, ocupación principal y secundaria |
| `06_chi2_estratificado.R` | Chi-cuadrado de sexo dentro de cada rama |
| `07_mca_kmeans.R` | MCA, k-means, estabilidad de los clusters y repetición sin la rama |
| `08_regresion.R` | Regresión lineal del logaritmo del ingreso horario |
| `09_graficos.R` | Gráficos comunicativo y exploratorio |

Las funciones propias están en `utils/`: `cuantil_pond`, `preparar`, `leer_diccionario`, `armar_tabla`, `chi_resumen`, `residuos_chi`, `correr_mca`, `benzecri`, `concordancia_rama`, `perfil_clusters`, `tabla_coef`, `tema_tp`, `grafico_densidad`, `grafico_barras_composicion` y `distribucion_ponderada`.

## Cómo reproducir el análisis

1. Descargar el repositorio.
2. Descargar y descomprimir el zip de la EAH 2025 (link arriba). **Atención:** un archivo del zip tiene una «ñ» en el nombre y `unzip()` de R falla; en Windows usar `shell("tar -xf archivo.zip -C carpeta")`.
3. Copiar `eah2025_bu_ampliada_ind.txt` y `eah2025_bu_ampliada_hog.txt` a `raw/`, y el resto de los archivos del zip a `auxiliar/`.
4. Abrir `plataformas-caba-eah2025.Rproj` en RStudio (así el directorio de trabajo es la carpeta raíz).
5. Instalar los paquetes:

```r
install.packages(c("tidyverse", "readxl", "FactoMineR", "factoextra", "cluster", "mclust", "scales", "ggtext"))
```

(`ggtext` es opcional: solo se usa para colorear una parte del título del gráfico comunicativo.)

6. Correr `script/00_correr_todo.R`, o los scripts del 01 al 09 en orden.

Versión de R con la que se desarrolló: 4.6.1.

## Conclusiones principales

**La hipótesis queda apoyada en parte.**

**Se sostiene que la rama es lo que más distingue a este grupo.** De las siete variables que cruzamos con trabajar en plataformas, la rama de actividad es la única con un tamaño de efecto apreciable (V de Cramér = 0,28); le sigue de lejos la categoría ocupacional (0,13) y el resto queda por debajo de 0,09. La incidencia muestra lo mismo: el 26% de los varones del transporte trabaja en plataformas, contra menos del 5% en cualquier otro grupo por rama y sexo.

**Pero «la rama» no quiere decir que las personas no importen.** Al repetir el test dentro de cada rama, la brecha por sexo persiste en transporte y en servicios profesionales, y desaparece en comercio y gastronomía. Es decir que sexo y rama están superpuestos, pero el sexo no es solo un reflejo de la composición de cada rama.

**La parte de los perfiles diferenciados se sostiene con reservas.** Los tres grupos que encontramos se parecen mucho a las tres ramas: el 91% de las personas quedó en el grupo de su rama. Para ver si los grupos existían por sí mismos, repetimos todo el análisis sin usar la rama. Los grupos que aparecieron ya casi no se parecen a las ramas (49%, cuando repartir al azar daría 41%). Lo que llamamos perfiles son, en buena medida, las ramas de actividad vistas desde otro ángulo.

**Lo que sí aparece con claridad es la brecha de ingresos.** Quienes trabajan en plataformas ganan por hora alrededor de un 20% menos que el resto de los ocupados, aun comparando entre personas del mismo sexo, edad, nivel educativo, rama y categoría ocupacional. Sin esos controles la brecha es del 30%, así que la composición del grupo explica una parte, pero no toda.

## Limitaciones y próximos pasos

**De los datos.** Tenemos una sola edición de la encuesta, así que no podemos ver si el fenómeno crece o se achica (no verificamos desde cuándo la EAH hace esta pregunta). Son 195 personas, y al abrirlas por rama y sexo quedan grupos muy chicos: la cifra de las mujeres del transporte, por ejemplo, sale de 2 casos. La encuesta tampoco dice en qué plataforma trabaja cada persona. Y el ingreso por hora que calculamos divide el ingreso del trabajo principal por las horas de todos los trabajos, así que subestima el ingreso de quienes tienen más de uno.

**De los métodos.** Todo lo que mostramos son asociaciones: decir que dos cosas van juntas no es decir que una cause la otra. Los tests usan los factores de expansión reescalados y hay varias personas del mismo hogar en la muestra, así que los p-valores son aproximados. Hicimos trece tests y no los ajustamos por eso, de modo que los resultados cercanos a 0,05 pueden ser casualidad. Los grupos del clustering están poco separados entre sí y el método no usa los ponderadores, así que describen la muestra y no la población. La regresión, por último, estima una sola brecha para todas las ramas, y el test estratificado sugiere que la brecha no es igual en todas.

**Próximos pasos.** Las variables `tu16` y `tu17` permitirían mirar la dependencia encubierta de quienes trabajan formalmente por cuenta propia, que es la pregunta que quedó abierta desde la primera entrega. Una regresión con interacciones entre plataforma, sexo y rama permitiría ver si la brecha de ingresos también cambia según la rama. Y si la pregunta existe en ediciones anteriores de la EAH, se podría analizar la evolución del fenómeno.
