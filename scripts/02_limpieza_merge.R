# 02_limpieza_merge.R
library(dplyr)

# 1) Cargar datos crudos desde el archivo guardado
df <- read.csv("data/raw/wb_global_raw.csv")

# 2) Filtrar solo países (excluir agregados tipo "World", "High income", etc.)
df <- df %>%
  filter(region != "Aggregates")

# 3) Seleccionar y renombrar variables de interés
df_clean <- df %>%
  select(
    country,
    iso2c,
    year,
    gdp_pc_ppp   = NY.GDP.PCAP.PP.CD,   # GDP per capita PPP
    edu_exp_gdp  = SE.XPD.TOTL.GD.ZS,   # Gasto educación % PIB
    tertiary_enr = SE.TER.ENRR,         # Matrícula terciaria
    literacy     = SE.ADT.LITR.ZS,      # Alfabetización
    region,
    income                               # <<< NUEVO: grupo de ingreso del WB
  )

# 4) Guardar datasets limpios
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write.csv(df_clean, "data/clean/wb_global_clean.csv", row.names = FALSE)
write.csv(df_clean, "data/processed/wb_global_panel_3years.csv", row.names = FALSE)





