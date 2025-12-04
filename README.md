📘 Proyecto: Educación y Desarrollo Económico — Análisis Global y Caso Argentino

Autores: Tomás Dumont - Facundo Rojas

Materia: Ciencia de Datos – FCE UBA
Año: 2025

📌 Descripción general

Este proyecto analiza la relación entre inversión en educación y desarrollo económico a nivel global, utilizando datos del Banco Mundial (WDI).
El trabajo explora patrones internacionales, diferencias entre países desarrollados y emergentes, y analiza en detalle la trayectoria de Argentina comparada con estos grupos.

El repositorio contiene el flujo completo de trabajo reproducible: descarga de datos, limpieza, análisis exploratorio, visualizaciones, inferencia y presentación final.

🎯 Objetivo

Evaluar cómo la inversión educativa se relaciona con el nivel de desarrollo económico.
En particular, se busca:

Comparar gasto educativo (% del PBI) y PBI per cápita (PPP) entre países desarrollados y emergentes.

Ubicar la trayectoria argentina dentro de ese contexto global.

Estimar si existe evidencia estadística que apoye la hipótesis de una relación positiva entre educación y desarrollo.

🔍 Hipótesis

Los países que invierten más en educación tienden a presentar mayores niveles de PBI per cápita (PPP).
Argentina sigue parcialmente esta relación, aunque con menor consistencia que los países desarrollados.

📁 Estructura del proyecto

proyecto_educacion_y_desarrollo_economico/
│
├── data/
│   ├── raw/        # Datos originales descargados del Banco Mundial (WDI)
│   └── clean/      # Datos procesados y listos para análisis
│
├── scripts/        # Código ordenado y numerado para reproducir todo el proyecto
│   ├── 01_descarga_raw.R
│   ├── 02_limpieza_transformacion.R
│   ├── 03_eda_global.R
│   ├── 04_inferencia_modelos.R
│   └── 05_graficos_editorializados.R
│
├── output/
│   ├── tables/     # Tablas .csv generadas (resúmenes, outliers, hipótesis)
│   └── figures/    # Gráficos .png listos para usar en la presentación
│
├── presentacion/   # PowerPoint final del trabajo
│
└── README.md        # Este archivo

🧰 Fuentes de Datos y Variables

Los datos provienen del World Development Indicators del Banco Mundial.

Variables principales:

NY.GDP.PCAP.PP.CD — PBI per cápita (PPP, dólares constantes)

SE.XPD.TOTL.GD.ZS — Gasto educativo total (% del PBI)

SE.TER.ENRR — Tasa de matriculación terciaria

SE.ADT.LITR.ZS — Tasa de alfabetización adulta

Años analizados: 2000, 2010, 2019

📊 Gráficos principales

El análisis incluye visualizaciones editoriales:

1. Gasto educativo promedio por grupo (desarrollados vs emergentes)

Archivo: output/figures/gasto_educacion_promedio.png

2. PBI per cápita promedio por grupo

Archivo: output/figures/pbi_promedio_grupos.png

3. Argentina: PBI vs inversión educativa (Índice 2000 = 100)

Archivo: output/figures/argentina_indexado.png

🧪 Metodología y análisis

El proyecto incluye:

✔ Limpieza de datos

Manejo de valores faltantes

Identificación y tratamiento de outliers (IQR)

Estandarización de variables

✔ Análisis exploratorio

Histogramas, boxplots, distribuciones

Diferencias entre grupos de países

Verificación de supuestos básicos

✔ Inferencia

Regresión lineal del PBI sobre la inversión educativa

Test t entre grupos

ANOVA para evaluar diferencias globales

Interpretación detallada de coeficientes

🧾 Resultados principales

Existe una relación positiva entre gasto educativo y desarrollo económico.

Los países desarrollados invierten de forma más estable y sostenida, con mejores resultados en PBI per cápita.

Argentina muestra niveles relativamente altos de inversión educativa, pero con inestabilidad macroeconómica, lo que atenúa los efectos sobre el PBI per cápita.

La evidencia respalda parcialmente la hipótesis, pero revela que la calidad institucional, la estabilidad y la eficiencia del gasto son determinantes adicionales.

🖥 Reproducibilidad

Para reproducir el proyecto completo:

# 1. Instalar librerías necesarias
install.packages(c("WDI", "tidyverse", "ggplot2", "dplyr", "broom"))

# 2. Ejecutar los scripts en orden
source("scripts/01_descarga_raw.R")

source("scripts/02_limpieza_transformacion.R")

source("scripts/03_eda_global.R")

source("scripts/04_inferencia_modelos.R")

source("scripts/05_graficos_editorializados.R")

