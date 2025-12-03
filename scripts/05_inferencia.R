
# Regresión múltiple, test t y ANOVA

library(dplyr)
library(broom)

# ---------------------------------------------------------------------
# Cargar datos limpios
# ---------------------------------------------------------------------

df <- read.csv("data/clean/wb_global_clean.csv")

# Variables del modelo
vars_modelo <- c("gdp_pc_ppp", "edu_exp_gdp", "tertiary_enr", "literacy")

# Mantener solo filas con datos completos para el modelo
df_model <- df %>%
  select(country, year, income, all_of(vars_modelo)) %>%
  na.omit()

# Crear variable de grupo simplificada
df_model <- df_model %>%
  mutate(
    grupo_desarrollo = ifelse(
      income == "High income",
      "Altos ingresos",
      "Emergentes"
    )
  )

# ---------------------------------------------------------------------
# 2. REGRESIÓN MÚLTIPLE
#    gdp_pc_ppp ~ edu_exp_gdp + tertiary_enr + literacy
# ---------------------------------------------------------------------

modelo <- lm(
  gdp_pc_ppp ~ edu_exp_gdp + tertiary_enr + literacy,
  data = df_model
)

summary(modelo)
modelo_tidy <- broom::tidy(modelo)
modelo_glance <- broom::glance(modelo)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

write.csv(modelo_tidy,
          "output/tables/regresion_coeficientes.csv",
          row.names = FALSE)

write.csv(modelo_glance,
          "output/tables/regresion_resumen.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------
# 3. TEST T — Comparación de medias
#    gdp_pc_ppp entre Altos ingresos vs Emergentes
# ---------------------------------------------------------------------

t_test_res <- t.test(
  gdp_pc_ppp ~ grupo_desarrollo,
  data = df_model
)

# Convertir a tabla
t_test_table <- broom::tidy(t_test_res)

write.csv(
  t_test_table,
  "output/tables/test_t_gdp_altosingresos_vs_emergentes.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 4. ANOVA — ¿Varía la inversión educativa según el nivel de desarrollo?
# ---------------------------------------------------------------------

anova_model <- aov(
  edu_exp_gdp ~ grupo_desarrollo,
  data = df_model
)

anova_resumen <- broom::tidy(anova_model)

write.csv(
  anova_resumen,
  "output/tables/anova_inversion_educativa_por_grupo.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
#.   REGRESIÓN SOLO PARA ARGENTINA
#    Relación entre inversión educativa y PBI 2000-2019
# ---------------------------------------------------------------------

arg <- df_model %>% filter(country == "Argentina")

modelo_arg <- lm(
  gdp_pc_ppp ~ edu_exp_gdp + tertiary_enr + literacy,
  data = arg
)

modelo_arg_tidy <- broom::tidy(modelo_arg)

write.csv(
  modelo_arg_tidy,
  "output/tables/regresion_argentina_coeficientes.csv",
  row.names = FALSE
)
-