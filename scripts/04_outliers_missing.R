# 04_outliers_missing.R
# Análisis de datos faltantes y outliers

library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------
# 1. Cargar datos limpios
# ---------------------------------------------------------------------

df <- read.csv("data/clean/wb_global_clean.csv")

# Variables numéricas de interés
vars_num <- c("gdp_pc_ppp", "edu_exp_gdp", "tertiary_enr", "literacy")

# ---------------------------------------------------------------------
# 2. RESUMEN DE MISSING POR VARIABLE
# ---------------------------------------------------------------------

n_total <- nrow(df)

missing_resumen <- data.frame(
  variable   = names(df),
  n_missing  = sapply(df, function(x) sum(is.na(x))),
  prop_missing = round(sapply(df, function(x) mean(is.na(x))) * 100, 2)
)

print(missing_resumen)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(
  missing_resumen,
  "output/tables/missing_resumen_completo.csv",
  row.names = FALSE
)

# También un enfoque más focalizado solo en las variables del modelo
missing_modelo <- missing_resumen %>%
  filter(variable %in% vars_num)

write.csv(
  missing_modelo,
  "output/tables/missing_variables_modelo.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 3. MISSING POR AÑO Y GRUPO DE DESARROLLO (para comentario cualitativo)
# ---------------------------------------------------------------------

df <- df %>%
  mutate(
    grupo_desarrollo = ifelse(
      income == "High income",
      "Países de altos ingresos",
      "Países emergentes y en desarrollo"
    )
  )

missing_por_anio_grupo <- df %>%
  group_by(grupo_desarrollo, year) %>%
  summarise(
    n_obs             = n(),
    miss_gdp_pc_ppp   = sum(is.na(gdp_pc_ppp)),
    miss_edu_exp_gdp  = sum(is.na(edu_exp_gdp)),
    miss_tertiary_enr = sum(is.na(tertiary_enr)),
    miss_literacy     = sum(is.na(literacy)),
    .groups = "drop"
  )

print(missing_por_anio_grupo)

write.csv(
  missing_por_anio_grupo,
  "output/tables/missing_por_anio_y_grupo.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 4. DETECCIÓN DE OUTLIERS (regla IQR por año y variable)
# ---------------------------------------------------------------------

# Pasamos a formato largo para calcular IQR por año y variable
df_long <- df %>%
  select(country, year, all_of(vars_num)) %>%
  pivot_longer(
    cols = all_of(vars_num),
    names_to = "variable",
    values_to = "valor"
  )

# Función para calcular límites IQR
calc_bounds <- function(x) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  c(q1 = q1, q3 = q3, iqr = iqr, lower = lower, upper = upper)
}

# Calcular bounds por variable y año
bounds <- df_long %>%
  group_by(variable, year) %>%
  summarise(
    q1    = quantile(valor, 0.25, na.rm = TRUE),
    q3    = quantile(valor, 0.75, na.rm = TRUE),
    iqr   = q3 - q1,
    lower = q1 - 1.5 * iqr,
    upper = q3 + 1.5 * iqr,
    .groups = "drop"
  )

# Unir bounds y marcar outliers
df_long_out <- df_long %>%
  left_join(bounds, by = c("variable", "year")) %>%
  mutate(
    es_outlier = ifelse(
      !is.na(valor) & (valor < lower | valor > upper),
      TRUE, FALSE
    )
  )

outliers_detectados <- df_long_out %>%
  filter(es_outlier) %>%
  arrange(variable, year, desc(valor))

print(outliers_detectados)

write.csv(
  outliers_detectados,
  "output/tables/outliers_detectados_iqr.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 5. OPCIONAL: CREAR UNA VERSIÓN CON MARCA DE OUTLIERS (SIN ELIMINARLOS)
# ---------------------------------------------------------------------

# Volver a formato ancho, manteniendo una bandera de outlier por variable
df_out_flag <- df_long_out %>%
  select(country, year, variable, valor, es_outlier) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  ) %>%
  # Nota: al volver a wide para valores, perdemos la bandera por variable
  # Si se quiere mantener bandera por variable, habría que crear columnas
  # específicas (ej: gdp_pc_ppp_outlier = ...). Para el TP, alcanza con
  # la tabla de outliers_detectados_iqr.csv
  distinct()

# Por ahora NO sobrescribimos el clean original, solo dejamos la info de outliers
# El usuario tomará decisiones en el informe (eliminar, winsorizar, etc.)

