# 03_eda_global.R
# Análisis exploratorio + gráficos editorializados + Argentina destacada

library(dplyr)
library(ggplot2)
library(tidyr)

# ---------------------------------------------------------------------
# 1. Cargar datos limpios
# ---------------------------------------------------------------------

df <- read.csv("data/clean/wb_global_clean.csv")

# ---------------------------------------------------------------------
# 2. Dimensiones, estructura y primeras filas
# ---------------------------------------------------------------------

cat("Dimensiones (filas, columnas):\n")
print(dim(df))

cat("\nPrimeras filas:\n")
print(head(df))

cat("\nEstructura del dataset:\n")
str(df)

cat("\nEstadísticas descriptivas:\n")
print(summary(df))

# ---------------------------------------------------------------------
# 3. Missing por variable
# ---------------------------------------------------------------------

missing_table <- sapply(df, function(x) sum(is.na(x)))
cat("\nMissing por variable:\n")
print(missing_table)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(as.data.frame(missing_table),
          "output/tables/missing_por_variable.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# 4. Frecuencia por año
# ---------------------------------------------------------------------

freq_year <- table(df$year)
cat("\nFrecuencia por año:\n")
print(freq_year)

write.csv(as.data.frame(freq_year),
          "output/tables/frecuencia_por_año.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------
# 5. Crear variable de grupo (desarrollados vs emergentes)
# ---------------------------------------------------------------------

df <- df %>%
  mutate(
    grupo_desarrollo = ifelse(
      income == "High income",
      "Países de altos ingresos",
      "Países emergentes y en desarrollo"
    )
  )

# ---------------------------------------------------------------------
# 6. Resumen por grupo y año (promedios globales)
# ---------------------------------------------------------------------

df_resumen <- df %>%
  group_by(grupo_desarrollo, year) %>%
  summarise(
    mean_gdp_pc_ppp   = mean(gdp_pc_ppp,   na.rm = TRUE),
    mean_edu_exp_gdp  = mean(edu_exp_gdp,  na.rm = TRUE),
    mean_tertiary_enr = mean(tertiary_enr, na.rm = TRUE),
    mean_literacy     = mean(literacy,     na.rm = TRUE),
    .groups = "drop"
  )

write.csv(df_resumen,
          "output/tables/resumen_por_grupo_y_anio.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------
# 7. Extraer datos de Argentina
# ---------------------------------------------------------------------

arg <- df %>%
  filter(country == "Argentina") %>%
  select(country, year, gdp_pc_ppp, edu_exp_gdp, tertiary_enr, literacy)

# ---------------------------------------------------------------------
# 8. Carpeta de figuras
# ---------------------------------------------------------------------

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# =====================================================================
#   GRÁFICO 1 — Gasto educativo: promedios por grupo (líneas)
# =====================================================================

graf_01 <- ggplot(
  df_resumen,
  aes(x = factor(year), y = mean_edu_exp_gdp,
      group = grupo_desarrollo, color = grupo_desarrollo)
) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c(
    "Países de altos ingresos"          = "#1f78b4",
    "Países emergentes y en desarrollo" = "#33a02c"
  )) +
  ggtitle(
    "Gasto público en educación según nivel de desarrollo",
    subtitle = "Promedio del % del PBI destinado a educación.\nComparación entre países de altos ingresos y emergentes (2000, 2010, 2019)."
  ) +
  xlab("Año") +
  ylab("Promedio de % del PBI destinado a educación") +
  labs(
    color = "Grupo de países",
    caption = "Fuente: World Bank, World Development Indicators (WDI)."
  ) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave("output/figures/grafico_01_edu_promedio_lineas.png",
       graf_01, width = 9, height = 6)

# =====================================================================
#   GRÁFICO 2 — PBI per cápita: promedios por grupo (líneas)
# =====================================================================

graf_02 <- ggplot(
  df_resumen,
  aes(x = factor(year), y = mean_gdp_pc_ppp,
      group = grupo_desarrollo, color = grupo_desarrollo)
) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c(
    "Países de altos ingresos"          = "#e41a1c",
    "Países emergentes y en desarrollo" = "#377eb8"
  )) +
  ggtitle(
    "Nivel de desarrollo económico según grupo de países",
    subtitle = "PBI per cápita (PPP, USD). Promedios por grupo en 2000, 2010 y 2019."
  ) +
  xlab("Año") +
  ylab("PBI per cápita promedio (PPP, USD)") +
  labs(
    color = "Grupo de países",
    caption = "Fuente: World Bank, World Development Indicators (WDI)."
  ) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )

ggsave("output/figures/grafico_02_gdp_promedio_lineas.png",
       graf_02, width = 9, height = 6)


# =====================================================================
#   Argentina: PBI vs inversión educativa
#   (Índice 2000 = 100)
# =====================================================================

arg <- df %>%
  filter(country == "Argentina") %>%
  select(country, year, gdp_pc_ppp, edu_exp_gdp, tertiary_enr, literacy)
# 1) Ordenar Argentina por año
arg_ord <- arg %>%
  arrange(year)

# 2) Crear índices 2000 = 100 para PBI pc y % PBI en educación
base_gdp  <- arg_ord$gdp_pc_ppp[arg_ord$year == 2000]
base_edu  <- arg_ord$edu_exp_gdp[arg_ord$year == 2000]

arg_idx <- arg_ord %>%
  mutate(
    gdp_index  = (gdp_pc_ppp  / base_gdp) * 100,
    edu_index  = (edu_exp_gdp / base_edu) * 100
  )

# 3) Pasar a formato “largo” para graficar dos líneas
arg_idx_long <- arg_idx %>%
  select(year, gdp_index, edu_index) %>%
  tidyr::pivot_longer(
    cols = c(gdp_index, edu_index),
    names_to = "serie",
    values_to = "valor"
  ) %>%
  mutate(
    serie = dplyr::recode(
      serie,
      "gdp_index" = "PBI per cápita (PPP)",
      "edu_index" = "Gasto público en educación (% del PBI)"
    )
  )
graf_arg_indice <- ggplot(
  arg_idx_long,
  aes(x = factor(year), y = valor, group = serie, color = serie)
) +
  geom_line(size = 1.3) +
  geom_point(size = 3.5) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c(
    "PBI per cápita (PPP)"                     = "#1f78b4",
    "Gasto público en educación (% del PBI)"   = "#e31a1c"
  )) +
  ggtitle(
    "Argentina: Evolución del PBI per cápita y la inversión educativa",
    subtitle = "Ambas series INDEXADAS (año 2000 = 100). Permite comparar la variación relativa entre ingresos y gasto educativo."
  ) +
  xlab("Año (series indexadas a 2000 = 100)") +
  ylab("Índice (2000 = 100)") +
  labs(
    color   = "",
    caption = "NOTA: Los valores se expresan como índices usando 2000 = 100 para ambas series.\nFuente: World Bank, World Development Indicators (WDI). Elaboración propia."
  ) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    plot.caption  = element_text(size = 9),
    legend.position = "bottom"
  )
ggsave(
  "output/figures/grafico_03_argentina_indice_pbi_educacion.png",
  graf_arg_indice,
  width = 9, height = 6
)

# =====================================================================
#   GRAFICO 4 — Barras comparativas con escalas separadas + LABELS
# =====================================================================

df_barras <- data.frame(
  grupo = c("Altos ingresos", "Emergentes", "Argentina"),
  edu = c(
    mean(df_resumen$mean_edu_exp_gdp[df_resumen$grupo_desarrollo=="Países de altos ingresos"]),
    mean(df_resumen$mean_edu_exp_gdp[df_resumen$grupo_desarrollo=="Países emergentes y en desarrollo"]),
    mean(arg$edu_exp_gdp, na.rm = TRUE)
  ),
  gdp = c(
    mean(df_resumen$mean_gdp_pc_ppp[df_resumen$grupo_desarrollo=="Países de altos ingresos"]),
    mean(df_resumen$mean_gdp_pc_ppp[df_resumen$grupo_desarrollo=="Países emergentes y en desarrollo"]),
    mean(arg$gdp_pc_ppp, na.rm = TRUE)
  )
)

# Pasar a largo
df_barras_long <- tidyr::pivot_longer(
  df_barras,
  cols = c(edu, gdp),
  names_to = "variable",
  values_to = "valor"
) %>%
  mutate(
    variable = dplyr::recode(variable,
                             "edu" = "Inversión educativa (% del PBI)",
                             "gdp" = "PBI per cápita (PPP, USD)"
    )
  )

colores <- c(
  "Altos ingresos" = "#1f78b4",
  "Emergentes"     = "#33a02c",
  "Argentina"      = "black"
)

graf_04_barras <- ggplot(df_barras_long, aes(x = grupo, y = valor, fill = grupo)) +
  
  geom_bar(stat = "identity", width = 0.65) +
  
  # === ETIQUETAS NUMÉRICAS ===
  geom_text(
    aes(label = round(valor, 1)),
    vjust = -0.5,
    size = 4
  ) +
  
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = colores) +
  theme_minimal(base_size = 13) +
  ggtitle(
    "Comparación internacional: inversión educativa y PBI per cápita",
    subtitle = "Escalas separadas y valores numéricos para facilitar la lectura"
  ) +
  xlab("") +
  ylab("") +
  labs(
    fill = "Grupo",
    caption = "Fuente: World Bank (WDI). Cálculos propios."
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

ggsave(
  "output/figures/grafico_04_barras_comparacion_escalas.png",
  graf_04_barras,
  width = 10, height = 6
)