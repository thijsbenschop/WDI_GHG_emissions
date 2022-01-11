# Analyze CO2 emissions series from different sources
# November/December 2021

# Load packages
library(data.table)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)
library(plotly)

rm(list = ls())

# Set working directory
#setwd("C:/Users/wb460271/OneDrive - WBG/Documents/WDI-GHG")
setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")

####### Read and reshape data ####### 
## PIK, downloaded from https://zenodo.org/record/5494497#.YZJPvmC0uUk # v.2.3.1 (1750-2019)
# Gütschow, J.; Günther, A.; Pflüger, M. (2021): The PRIMAP-hist national historical emissions time series v2.3.1 (1850-2019). zenodo. doi:`10.5281/zenodo.5494497`.
PIK <- as.data.table(read.csv("./Data_private/PIK/Guetschow-et-al-2021-PRIMAP-hist_v2.3.1_20-Sep_2021.csv"))
#PIK <- as.data.table(read.csv("./Data_private/PIK/PRIMAP-hist_v1.1_06-Mar-2017.csv"))

dim(PIK)
colnames(PIK)

# Drop columns only keep years >1960, WDI starts in 1960
PIK[, paste0("X", 1750:1959) := NULL]

# Compare HISTCR/HISTTP 
#HISTCR: In this scenario country-reported data (CRF, BUR, UNFCCC) is prioritized over third-party data (CDIAC, FAO, Andrew, EDGAR, BP).
#HISTTP: In this scenario third-party data (CDIAC, FAO, Andrew, EDGAR, BP) is prioritized over country-reported data (CRF, BUR, UNFCCC)) 
table(PIK[, "scenario..PRIMAP.hist."])
setnames(PIK, "scenario..PRIMAP.hist.", "scenario")

#PIK <- PIK[scenario == "HISTCR"]
#PIK[, "scenario" := NULL]

table(PIK[, "source"])
table(PIK[, "entity"])
table(PIK[, "area..ISO3."])
table(PIK[, "unit"]) # check units -> convert to CO2e?
table(PIK[, "entity"])
table(PIK[, "category..IPCC2006_PRIMAP."])

# Check which WDI countries available
# Load country WDI list (217 countries)
WDI_countries <- as.data.table(read.csv("Data/wdi_country_list.csv"))
setnames(WDI_countries, c("long_name", "ISO3"))

PIK_countries <- unique(PIK$area..ISO3.)
length(PIK_countries) # 215 countries
table(WDI_countries$ISO3 %in% PIK_countries) # 198 WDI countries available in PIK
WDI_countries[!(ISO3 %in% PIK_countries)] # list of missing countries
table(PIK_countries %in% WDI_countries$ISO3)
PIK_countries[!(PIK_countries %in% WDI_countries$ISO3)] # list of missing countries

# Drop countries/regions not in WDI
PIK <- PIK[area..ISO3. %in% WDI_countries$ISO3]

# Rename variables
colnames(PIK)
setnames(PIK, c("area..ISO3.", "category..IPCC2006_PRIMAP."),
         c("ISO3", "category"))

# Convert data in long format
PIK_long <- melt(PIK, id.vars = c("source", "ISO3", "entity", "unit", "category", "scenario"),
                 variable.name = "year", value.factor = FALSE, variable.factor = FALSE)
PIK_long[, year := as.numeric(substring(year, 2, 5))]
dim(PIK_long)
colnames(PIK_long)

# Compare HISTCR/HISTTP 
#HISTCR: In this scenario country-reported data (CRF, BUR, UNFCCC) is prioritized over third-party data (CDIAC, FAO, Andrew, EDGAR, BP).
#HISTTP: In this scenario third-party data (CDIAC, FAO, Andrew, EDGAR, BP) is prioritized over country-reported data (CRF, BUR, UNFCCC)) 
PIK_scenario <- dcast(PIK_long, 
                      formula = ... ~ scenario) 
PIK_scenario[, dif_scenario := 100 * (HISTCR - HISTTP)]
PIK_scenario[, reldif_scenario := 100 * dif_scenario / HISTTP]

table(PIK_scenario$HISTCR == 0, PIK_scenario$HISTTP == 0)

table(PIK_scenario$dif_scenario == 0)
table(PIK_scenario$dif_scenario < 0.01)

table(abs(PIK_scenario$dif_scenario) < 1)

PIK_scenario %>% filter(entity == "CO2") %>% summary()

# Plot series for all country
# HISTCR
PIK_long %>% 
  filter(entity == "CO2" & category == "M.0.EL") %>%
  filter(scenario == "HISTCR") %>%
  ggplot(aes(x = year, y = value, group = c(ISO3))) +
  geom_line()

# HISTTP
PIK_long %>% 
  filter(entity == "CO2" & category == "M.0.EL") %>%
  filter(scenario == "HISTTP") %>%
  ggplot(aes(x = year, y = value, group = c(ISO3))) +
  geom_line()

## CAIT data for comparison
CAIT <- as.data.table(read_xlsx("Data_private/CAIT/ghg-emissions/CW_CAIT_GHG_Emissions.xlsx"))
CAIT

CAIT_long <- melt(CAIT, id.vars = c("Country", "Source", "Sector", "Gas"))
setnames(CAIT_long, "value", "CO2_emissions_ktCO2")
setnames(CAIT_long, "variable", "Year")
CAIT_long[, Source := NULL]

CAIT_wide <- dcast(data = CAIT_long %>% filter(Sector == "Total excluding LUCF" & Gas == "CO2"), #  & Gas == "All GHG"
                   formula = Year ~ Country,
                   value.var = "CO2_emissions_ktCO2")
class(CAIT_wide$Year)
CAIT_wide[, Year := as.numeric(as.character((Year)))]


# Plot series for each country
cur_ISO3 <- "RUS"
cur_PIK_long <- PIK_long %>% filter(ISO3 == cur_ISO3, category == "M.0.EL")
cur_CO2a <- cur_PIK_long %>% filter(entity == "CO2", scenario == "HISTCR") %>% select(value)
cur_CO2b <- cur_PIK_long %>% filter(entity == "CO2", scenario == "HISTTP")  %>% select(value)
cur_CO2_CAIT <- c(rep(NA, 30), t(1000 * CAIT %>% filter(Sector == "Total excluding LUCF", Gas == "CO2", Country == cur_ISO3) %>%
                                   select(as.character(1990:2018))), NA)
cur_CO2 <- cur_PIK_long 

  
table(cur_PIK_long$entity)
table(cur_PIK_long$category)

cur_data <- data.frame(year = cur_PIK_long %>% filter(entity == "CO2", scenario == "HISTCR") %>% select(year), 
                       cur_CO2a = cur_CO2a, cur_CO2a = cur_CO2a, cur_CO2_CAIT=cur_CO2_CAIT)

fig <- plot_ly(cur_data, x = ~year, y = ~value, name = 'trace 0', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~value.1, name = 'trace 1', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~cur_CO2_CAIT, name = 'trace 2', mode = 'markers')

fig

PIK_long %>% 
  filter(ISO3 == cur_ISO3 & category == "M.0.EL") %>%
  filter(scenario == "HISTCR") %>%
  ggplot(aes(x = year, y = value, group = entity, color = entity)) +
  geom_line()+
  geom_point() + 
  labs(cur_ISO3)

       
"category..IPCC2006_PRIMAP."       
setnames(PIK, "country", "Nation")
PIK
PIK_long <- melt(PIK, id.vars = c("Nation", "scenario", "category", "entity", "unit"))
PIK_long
PIK_long[, Year := as.numeric(substring(variable, 2, 5))]
setnames(PIK_long, "value", "CO2_emissions_ktCO2")
PIK_long[, scenario := NULL]
PIK_long[, unit := NULL]

PIK_world_wide <- dcast(data = PIK_long %>% filter(Nation == "EARTH" & entity == "CO2"),
                        formula = Year ~ category ,
                        value.var = "CO2_emissions_ktCO2")

PIK_world_wide[, WORLD_PIK := (CAT0 - CAT5) / 1000]

PIK_world_wide[, WORLD_PIK_1 := (CAT0 - CAT5) / 1000]
PIK_world_wide[, WORLD_PIK_2 := (CAT0 - CAT5 - CAT6 - CAT7) / 1000]

# CATM0EL for all countries
PIK_country_wide <- dcast(data = PIK_long %>% filter(category == "CATM0EL" & entity == "CO2"),
                          formula = Year ~ Nation ,
                          value.var = "CO2_emissions_ktCO2")

PIK_wide_USA <- dcast(data = PIK_long %>% filter(Nation == "USA" & entity == "CO2"),
                      formula = Year ~ category ,
                      value.var = "CO2_emissions_ktCO2")

PIK_wide_USA[, USA_PIK := CATM0EL / 1000]

PIK_wide_CHN <- dcast(data = PIK_long %>% filter(Nation == "CHN" & entity == "CO2"),
                      formula = Year ~ category ,
                      value.var = "CO2_emissions_ktCO2")

PIK_wide_CHN[, CHN_PIK := (CAT0 - CAT5) / 1000]

PIK_wide_RUS <- dcast(data = PIK_long %>% filter(Nation == "RUS" & entity == "CO2"),
                      formula = Year ~ category ,
                      value.var = "CO2_emissions_ktCO2")

PIK_wide_RUS[, RUS_PIK := (CAT0 - CAT5) / 1000]




#### Further explore PIK data (country coverage, etc.) ####
colnames(PIK_long)
class(PIK_long)

table(PIK_long)

# Create one data.table for each GHG 
PIK_CO2 <- 
  
  
  
  