library(WDI)

indicators <- c(
  "NY.GDP.PCAP.PP.CD",  # GDP per capita PPP
  "SE.XPD.TOTL.GD.ZS",  # Education spending % GDP
  "SE.TER.ENRR",        # Tertiary enrollment
  "SE.ADT.LITR.ZS"      # Literacy rate
)

years <- c(2000, 2010, 2019)

wb_raw <- WDI(
  country = "all",
  indicator = indicators,
  start = min(years),
  end   = max(years),
  extra = TRUE,
  cache = NULL
)
#Guardar los datos
write.csv(wb_raw, "data/raw/wb_global_raw.csv", row.names = FALSE)
