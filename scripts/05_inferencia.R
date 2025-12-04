# 05_inferencia.R
# Regresión múltiple, tests t, ANOVA y robustez a outliers

library(dplyr)
library(broom)
library(tidyr)


# ---------------------------------------------------------------------
# 1. Cargar datos LIMPIOS para el modelo
#    (salidos del script 04_outliers_missing.R)
# ---------------------------------------------------------------------

df <- read.csv("data/processed/panel_clean.csv")

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

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------
# 2. REGRESIÓN MÚLTIPLE (MODELO BASE)
#    gdp_pc_ppp ~ edu_exp_gdp + tertiary_enr + literacy
# ---------------------------------------------------------------------

modelo_base <- lm(
  gdp_pc_ppp ~ edu_exp_gdp + tertiary_enr + literacy,
  data = df_model
)

modelo_tidy  <- broom::tidy(modelo_base)
modelo_glance <- broom::glance(modelo_base)

write.csv(modelo_tidy,
          "output/tables/regresion_coeficientes.csv",
          row.names = FALSE)

write.csv(modelo_glance,
          "output/tables/regresion_resumen.csv",
          row.names = FALSE)

# ---------------------------------------------------------------------
# 3. ROBUSTEZ A OUTLIERS: MODELO WINSORIZADO 1–99%
# ---------------------------------------------------------------------

winsorizar <- function(x, probs = c(0.01, 0.99)) {
  qs <- quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}

df_model_w <- df_model %>%
  mutate(
    gdp_pc_ppp_w   = winsorizar(gdp_pc_ppp),
    edu_exp_gdp_w  = winsorizar(edu_exp_gdp),
    tertiary_enr_w = winsorizar(tertiary_enr)
  )

modelo_winsor <- lm(
  gdp_pc_ppp_w ~ edu_exp_gdp_w + tertiary_enr_w + literacy,
  data = df_model_w
)

modelo_winsor_tidy <- broom::tidy(modelo_winsor)
modelo_winsor_glance <- broom::glance(modelo_winsor)

# Guardar coeficientes del modelo winsorizado
write.csv(
  modelo_winsor_tidy,
  "output/tables/regresion_coeficientes_winsor.csv",
  row.names = FALSE
)

# Tabla de comparación base vs winsor
comparacion_coef <- modelo_tidy %>%
  select(term,
         estimate_base  = estimate,
         std.error_base = std.error,
         p.value_base   = p.value) %>%
  left_join(
    modelo_winsor_tidy %>%
      select(term,
             estimate_winsor  = estimate,
             std.error_winsor = std.error,
             p.value_winsor   = p.value),
    by = "term"
  )

write.csv(
  comparacion_coef,
  "output/tables/regresion_coeficientes_comparacion_base_vs_winsor.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 4. TEST T — Comparación de medias de gdp_pc_ppp:
#    Altos ingresos vs Emergentes (COMPLEMENTARIO)
# ---------------------------------------------------------------------

t_test_res <- t.test(
  gdp_pc_ppp ~ grupo_desarrollo,
  data = df_model
)

t_test_table <- broom::tidy(t_test_res)

write.csv(
  t_test_table,
  "output/tables/test_t_gdp_altosingresos_vs_emergentes.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 5. TEST T ALINEADO CON LA HIPÓTESIS:
#    Q4 vs Q1 de gasto educativo (edu_exp_gdp) sobre gdp_pc_ppp
#    (usamos año 2019 como corte)
# ---------------------------------------------------------------------

df_2019 <- df_model %>%
  filter(year == 2019) %>%
  drop_na(edu_exp_gdp, gdp_pc_ppp)

df_2019 <- df_2019 %>%
  mutate(cuartil_edu = ntile(edu_exp_gdp, 4))

q1 <- df_2019 %>% filter(cuartil_edu == 1)
q4 <- df_2019 %>% filter(cuartil_edu == 4)

t_test_q4_q1 <- t.test(q4$gdp_pc_ppp, q1$gdp_pc_ppp, var.equal = FALSE)

t_test_q4_q1_tabla <- broom::tidy(t_test_q4_q1) %>%
  mutate(
    n_q4        = nrow(q4),
    n_q1        = nrow(q1),
    mean_q4     = mean(q4$gdp_pc_ppp, na.rm = TRUE),
    mean_q1     = mean(q1$gdp_pc_ppp, na.rm = TRUE),
    diff_means  = mean_q4 - mean_q1
  )

write.csv(
  t_test_q4_q1_tabla,
  "output/tables/test_t_gdp_q4_vs_q1_edu_2019.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 6. ANOVA — gdp_pc_ppp según terciles de matrícula terciaria
#    (también en 2019, alineado con la idea de educación ↔ desarrollo)
# ---------------------------------------------------------------------

df_2019_terciles <- df_2019 %>%
  drop_na(tertiary_enr) %>%
  mutate(tercile_tertiary = ntile(tertiary_enr, 3))

anova_tertiary <- aov(
  gdp_pc_ppp ~ factor(tercile_tertiary),
  data = df_2019_terciles
)

anova_tertiary_tabla <- broom::tidy(anova_tertiary)

write.csv(
  anova_tertiary_tabla,
  "output/tables/anova_gdp_por_terciles_tertiary_2019.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 7. ANOVA ORIGINAL — ¿Varía la inversión educativa según nivel de desarrollo?
#    (lo dejamos como complemento)
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
# 8. REGRESIÓN SOLO PARA ARGENTINA
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
