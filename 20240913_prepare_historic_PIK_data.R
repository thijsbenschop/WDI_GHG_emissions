# Title: 20240913_prepare_historic_PIK_data.R
# Description: Script to prepare historic emissions data from PIK for 80 years IBRD 
# Date: 9/13/2024
# Author: Thijs Benschop

rm(list=ls())

#### Load libraries and data ####
library(dplyr)
library(data.table)
library(ggplot2)

setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")

rm(list = ls())

# Data downloaded from https://zenodo.org/record/7727475
# Version 2.4.2 of PRIMAP-HIST dataset
list.files("./data_private/PIK")
#data_PIK <- as.data.table(read.csv("./data_private/PIK/Guetschow-et-al-2023-PRIMAP-hist_v2.4.1_final_16-Feb-2023.csv"))
data_PIK <- as.data.table(read.csv("./data_private/PIK/Guetschow-et-al-2023a-PRIMAP-hist_v2.4.2_final_09-Mar-2023.csv"))

dim(data_PIK)
colnames(data_PIK)

# Drop columns only keep years >1940
data_PIK[, paste0("X", 1750:1939) := NULL]

# Rename columns
colnames(data_PIK)
setnames(data_PIK, "scenario..PRIMAP.hist.", "scenario")
setnames(data_PIK, "category..IPCC2006_PRIMAP.", "category")
setnames(data_PIK, "area..ISO3.", "ISO3")
colnames(data_PIK)

# Compare HISTCR/HISTTP -> use HISTTP
#HISTCR: In this scenario country-reported data (CRF, BUR, UNFCCC) is prioritized over third-party data (CDIAC, FAO, Andrew, EDGAR, BP).
#HISTTP: In this scenario third-party data (CDIAC, FAO, Andrew, EDGAR, BP) is prioritized over country-reported data (CRF, BUR, UNFCCC)) 
table(data_PIK[, "scenario"])

# use third-party data
data_PIK <- data_PIK[scenario == "HISTCR"]
data_PIK[, "scenario" := NULL]

table(data_PIK[, "source"])
table(data_PIK[, "entity"])
table(data_PIK[, "ISO3"])
table(data_PIK[, "unit"]) # check units -> convert to CO2e?
table(data_PIK[, "entity"])
table(data_PIK[, "category"])

# Check which WDI countries available
# Load country WDI list (217 countries)
WDI_countries <- as.data.table(read.csv("Data/wdi_country_list.csv"))
setnames(WDI_countries, c("long_name", "ISO3"))

PIK_countries <- unique(data_PIK$ISO3)
length(PIK_countries) # 215 countries
table(WDI_countries$ISO3 %in% PIK_countries) # 198 WDI countries available in PIK
WDI_countries[!(ISO3 %in% PIK_countries)] # list of missing countries (not in PIK data)
# are these countries/territories included in other countries?
table(PIK_countries %in% WDI_countries$ISO3)
PIK_countries[!(PIK_countries %in% WDI_countries$ISO3)] # list of missing countries (not in WDI list)
# AIA - Anguilla, ANT - , ATA - Antartica, COK - Cook Islands, NIU - Niue, 
# SHN - Saint Helena, TKL - Tokelau, TWN - Taiwan, VAT - Holy See
# others are aggregates

# Drop countries/regions not in WDI
data_PIK <- data_PIK[ISO3 %in% WDI_countries$ISO3]

# Convert data in long format
PIK_long <- melt(data_PIK, id.vars = c("source", "ISO3", "entity", "unit", "category"), #, "scenario"),
                 variable.name = "year", value.factor = FALSE, variable.factor = FALSE)
PIK_long[, year := as.numeric(substring(year, 2, 5))]
dim(PIK_long)
colnames(PIK_long)
table(PIK_long$unit)

PIK_long[, value := value / 1000] # convert to Mt from gigagram (gigagram = 10^9 g = 10^6 kg = 10^3 t = kt)

# Add "Time", "Country", "SCALE" columns as in DCS template
PIK_long[, Time := paste0("YR", year)]
PIK_long[, Country := ISO3]
PIK_long[, SCALE := 0]

# Prepare data for each indicator
table(PIK_long$entity)
table(PIK_long$category)

# Prepare datafile 
PIK_long

data_for_export <- PIK_long %>% 
  filter(ISO3 %in% WDI_countries$ISO3) %>% # only WDI countries
  filter(category == "M.0.EL") %>% # only total over all categories/sectors
  filter(entity == "KYOTOGHG (AR4GWP100)") %>% # use AR4 all Kyoto gases
  left_join(WDI_countries, 
            by = c("ISO3" = "ISO3")) %>%
  dplyr::mutate(Country = ISO3,
                `Country name` = long_name,
                Series = "EN.GHG.TOTL.KT.CE.AR4",
                `Series name` = "Total greenhouse gas emissions (MtCo2e)",
                Time = year,
                Data = value,
                OBS_metadata = "") %>%
  select(Country,	`Country name`,	Series,	`Series name`,	Time,	Data,	OBS_metadata) 

write.xlsx(data_for_export, "historic_data_PIK.xlsx")  

# Data structure
#Country	Country name	Series	Series name	Time	Data	OBS_metadata



# # 1) EN.GHG.TOTL.KT.CE
# EN.GHG.TOTL.KT.CE <- PIK_long[entity == "KYOTOGHG (AR4GWP100)" & category == "M.0.EL", 
#                               .(Time, Country, SCALE, value)]
# 
# PIK_long[, Series := "EN.GHG.TOTL.KT.CE"]
# 
# # 1) EN.GHG.TOTL.KT.CE
# EN.GHG.TOTL.KT.CE <- PIK_long[entity == "KYOTOGHG (AR4GWP100)" & category == "M.0.EL", 
#                               .(Time, Country, SCALE, value)]
# 
# PIK_long[, Series := "EN.GHG.TOTL.KT.CE"]
# 
# # 1) EN.GHG.TOTL.KT.CE
# EN.GHG.TOTL.KT.CE <- PIK_long[entity == "KYOTOGHG (AR4GWP100)" & category == "M.0.EL", 
#                               .(Time, Country, SCALE, value)]
# 
# PIK_long[, Series := "EN.GHG.TOTL.KT.CE"]






