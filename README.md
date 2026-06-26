# Crecimiento con inclusión? El rol del desarrollo industrial en la desigualdad del ingreso (1970–2023)

## Integrantes

- Martín Flores (905761)
- Luca Derosa (911165)

## Objetivo

El trabajo analiza la relación entre el nivel de industrialización de un país y dos dimensiones de su desempeño económico: el crecimiento del PBI per cápita y el nivel de desigualdad del ingreso, medido a través del coeficiente de Gini. El análisis se realiza sobre un panel de 15 países pertenecientes a distintas regiones (América del Sur, América del Norte, Europa y Asia) para el período 1970–2023.

Las hipótesis que guían el trabajo son las siguientes:

- **Hipótesis principal:** los países con mayor nivel de industrialización, medido por el PBI industrial per cápita en 2023, presentan un mayor crecimiento acumulado del PBI per cápita total durante el período 1970–2023, en comparación con los países menos industrializados.
- **Hipótesis complementaria:** los países con mayor nivel de industrialización presentan menores niveles de desigualdad del ingreso que los países menos industrializados, independientemente de la región geográfica.

La clasificación de los países en los grupos "más industrializado" y "menos industrializado" se realiza según el nivel absoluto del PBI industrial per cápita en el año 2023, y no según su variación histórica. Este criterio fue adoptado luego de una corrección metodológica: clasificar por variación generaba un argumento parcialmente circular, dado que la industria es uno de los sectores con mayor efecto multiplicador sobre el PBI agregado, y producía resultados contraintuitivos en países con alto nivel industrial absoluto pero baja variación reciente.

## Datos

- **Fuente principal:** [World Development Indicators — Banco Mundial](https://databank.worldbank.org/source/world-development-indicators), consultada mediante el paquete de R `WDI`. Aporta las variables de PBI per cápita, coeficiente de Gini, población y exportaciones de servicios.
- **Fuente auxiliar:** [Argendata — Fundar](https://www.fundar.org.ar/argendata/), que aporta el índice de PBI industrial per cápita utilizado para clasificar a los países según su nivel de industrialización.
- **Período cubierto:** 1970–2023.
- **Unidad de análisis:** país y año. El panel final cuenta con 810 observaciones país-año y 11 variables, correspondientes a 15 países: Argentina, Brasil, México, Chile, Colombia, Estados Unidos, Alemania, Reino Unido, Francia, Turquía, China, Indonesia, Corea del Sur, España y Costa Rica.

## Análisis realizado

1. **Descarga** de los datos de WDI mediante API y de la base de Argendata, almacenados sin modificaciones en `raw/`.
2. **Limpieza y unión** de ambas fuentes: filtrado de los 15 países seleccionados en cada fuente por separado, renombrado de variables y unión por país y año, generando el panel consolidado en `input/`.
3. **Clasificación de países** según el nivel del PBI industrial per cápita en 2023, dividiendo la muestra en dos grupos a partir de la mediana. Se conserva además, a fines de trazabilidad, el criterio de clasificación de una instancia previa (por variación del índice 1970–2023), utilizado únicamente por el gráfico comunicacional según se detalla en la sección de limitaciones conocidas.
4. **Estadísticas descriptivas** generales, por grupo de industrialización y específicas del coeficiente de Gini sobre el panel completo.
5. **Diagnóstico de datos faltantes y outliers**: identificación de valores faltantes por variable y detección de valores atípicos mediante el criterio de rango intercuartílico. Se evalúa además el efecto de excluir a China sobre las estadísticas descriptivas y sobre la regresión que responde a la hipótesis complementaria, como evidencia de que la decisión de mantenerla en el análisis responde a un criterio conceptual y no a una conveniencia estadística.
6. **Análisis descriptivo de trayectorias** (Método 1): indexación del PBI per cápita a base 100 en 1970 y comparación gráfica entre grupos.
7. **Descomposición de tendencia y residuo** (Método 2): aplicada mediante suavizado LOESS a países seleccionados como estudio de caso ilustrativo.
8. **Inferencia estadística**: test de Welch para la comparación de medias de crecimiento entre grupos (hipótesis principal) y regresión lineal simple del coeficiente de Gini sobre el índice de industrialización, utilizando múltiples años de referencia por país (hipótesis complementaria).
9. **Visualizaciones**: un gráfico comunicacional de trayectorias indexadas y un gráfico exploratorio de la relación entre Gini e industrialización.

## Estructura del repositorio

Todo el contenido del trabajo se encuentra dentro de la carpeta `mi_trabajo/`, que constituye la raíz de ejecución del proyecto.

```
mi_trabajo/
├── raw/                                   # Datos crudos, tal como fueron descargados
│   ├── wdi_raw.csv                        # Descarga cruda desde la API de WDI
│   └── pib_indust_per_capita_comparado.csv # Base de Argendata (Fundar)
├── input/                                 # Datos procesados, listos para el análisis
│   ├── datos_panel.csv                    # Panel consolidado (WDI + Argendata)
│   ├── panel_indexado.csv                 # Panel con la columna de grupo de industrialización (criterio por nivel)
│   └── gini_temporal.csv                  # Gini por país en los años de referencia
├── auxiliar/
│   ├── auxiliar_grupos.csv                # Clasificación vigente de cada país, por nivel de industrialización
│   └── auxiliar_grupos_viejo.csv          # Clasificación de una instancia previa, por variación del índice (ver limitaciones conocidas)
├── output/
│   ├── tablas/                            # Tablas de resultados exportadas por 03_analisis.R
│   ├── graficos/                          # Gráfico comunicacional y gráfico exploratorio
│   └── Grupo_11_presentacion_final.pptx   # Presentación final del trabajo
├── script/
│   ├── 01_descarga.R                      # Descarga de datos desde WDI y lectura de Argendata
│   ├── 02_limpieza.R                      # Limpieza, renombrado y unión de las dos fuentes
│   ├── 03_analisis.R                      # Clasificación, estadísticas, diagnóstico e inferencia
│   └── 04_graficos.R                      # Construcción de las visualizaciones
└── README.md
```

## Reproducción

### Paquetes necesarios

```r
install.packages(c("tidyverse", "WDI", "ggtext", "ggrepel"))
```

### Paso previo

Abrir el proyecto desde `mi_trabajo/mi_trabajo.Rproj`, de modo que el directorio de trabajo de R coincida con la raíz desde la cual los scripts referencian sus rutas relativas (`raw/`, `input/`, `auxiliar/`, `output/`).

### Orden de ejecución

1. `script/01_descarga.R` — Descarga los datos desde la API de WDI y guarda la base cruda en `raw/wdi_raw.csv`. Requiere conexión a internet.
2. `script/02_limpieza.R` — Lee las bases de `raw/`, filtra los 15 países seleccionados, renombra variables y genera el panel consolidado en `input/datos_panel.csv`.
3. `script/03_analisis.R` — Lee `input/datos_panel.csv`, clasifica a los países según su nivel de industrialización, calcula estadísticas descriptivas, diagnostica datos faltantes y outliers, evalúa el efecto de excluir a China sobre las estadísticas descriptivas y sobre la regresión OLS, y ejecuta el test de Welch y la regresión OLS. Genera las tablas de `output/tablas/` (incluyendo `tabla_robustez_china.csv` y `tabla_robustez_china_ols.csv`), además de `input/panel_indexado.csv`, `input/gini_temporal.csv`, `auxiliar/auxiliar_grupos.csv` y `auxiliar/auxiliar_grupos_viejo.csv`.
4. `script/04_graficos.R` — Lee los archivos generados por el paso anterior y construye las dos visualizaciones del trabajo en `output/graficos/`.

Cada script guarda sus resultados en disco y el siguiente los lee desde archivo, por lo que pueden ejecutarse en sesiones de R independientes, siempre respetando el orden indicado la primera vez que se corre el pipeline completo.

## Limitaciones conocidas

El gráfico comunicacional (Método 1, `output/graficos/grafico_comunicacional.png`) clasifica a los países utilizando el criterio de una instancia previa del trabajo, por variación del índice de PBI industrial per cápita entre 1970 y 2023, en lugar del criterio vigente por nivel del índice en 2023. Esta decisión se mantiene por motivos de diseño visual y no fue actualizada al criterio corregido. Como consecuencia, este gráfico en particular puede mostrar un patrón distinto al que se desprende de la tabla de trayectorias y del resto del análisis, que sí utilizan el criterio vigente. Esta diferencia se documenta y se explica en detalle en la presentación final del trabajo.

## Conclusiones principales

Con el criterio de clasificación por nivel de industrialización en 2023, la evidencia no permite confirmar la hipótesis principal: el test de Welch no encuentra una diferencia estadísticamente significativa en el crecimiento del PBI per cápita entre el grupo más y el grupo menos industrializado (p = 0,301), y el análisis descriptivo de trayectorias muestra que el grupo menos industrializado finaliza el período con un nivel de PBI per cápita indexado incluso superior al del otro grupo.

Respecto de la hipótesis complementaria, la regresión entre el coeficiente de Gini y el índice de industrialización arroja un coeficiente con el signo esperado (mayor industrialización asociada a menor desigualdad), pero sin significancia estadística convencional bajo la especificación principal (p = 0,070, R² = 0,055). Al excluir a China de este análisis, la relación se fortalece de manera considerable (R² = 0,102, p = 0,016), lo que indica que este caso particular debilita, en lugar de sostener artificialmente, la evidencia a favor de la hipótesis complementaria. Pese a ello, se optó por mantener a China en el análisis principal, dado que su condición de valor atípico no responde a un error de medición sino al fenómeno de industrialización acelerada que el trabajo se propone estudiar.
