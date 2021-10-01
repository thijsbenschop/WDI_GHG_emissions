# Analyze CO2 emissions series from different sources
# September 2021

# Load packages
library(data.table)
library(ggplot2)
library(dplyr)
library(readxl)

rm(list = ls())

# Set working directory
#setwd("C:/Users/wb460271/OneDrive - WBG/Documents/WDI-GHG")
setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")

####### Read and reshape data ####### 
## CDIAC by country - use total CO2
CDIAC <- as.data.table(read.csv("Data/CDIAC/nation_1751_2014_edited.csv"))
class(CDIAC)
colnames(CDIAC)

# Reshape wide
CDIAC_wide <- dcast(data = CDIAC, 
                    formula = Year ~ Nation, 
                    value.var = "CO2_emissions_ktCO2")
CDIAC_wide

## CDIAC global - use total CO2
CDIAC_world <- as.data.table(read.csv("Data/CDIAC/global_1751_2014_edited.csv"))

CDIAC_world_wide <- dcast(data = CDIAC_world, 
                          formula = Year ~ Nation, 
                          value.var = "CO2_emissions_ktCO2")
CDIAC_world_wide

# add sum of all countries
CDIAC_world_wide[, WORLD_sum := rowSums(CDIAC_wide, na.rm = T) / 1000]

# add difference between sum and WORLD estimate -> difference due to double counting of some countries, e.g. Japan
CDIAC_world_wide[, diff_abs := WORLD_sum - WORLD]
CDIAC_world_wide[, diff_rel := 100 * (diff_abs / WORLD)]

## CAIT data
CAIT <- as.data.table(read_xlsx("Data/CAIT/ghg-emissions/CW_CAIT_GHG_Emissions.xlsx"))
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

## WDI
WDI <- as.data.table(read.csv("Data/WDI/API_EN.ATM.CO2E.KT_DS2_en_csv_v2_2917960_edited.csv"))
colnames(WDI)
setnames(WDI, colnames(WDI)[1], "Nation")
WDI_world <- WDI[Nation == "World"]
WDI_world_long <- melt(WDI_world, id.vars = "Nation")
WDI_world_long[, Year := as.numeric(substring(variable, 2, 5))]
setnames(WDI_world_long, "value", "CO2_emissions_ktCO2")
class(WDI_world_long$Year)

WDI_long <- melt(WDI, id.vars = "Nation")
WDI_long[, Year := as.numeric(substring(variable, 2, 5))]
setnames(WDI_long, "value", "CO2_emissions_ktCO2")

WDI_world_wide <- dcast(data = WDI_world_long,
                        formula = Year ~ Nation,
                        value.var = "CO2_emissions_ktCO2")
WDI_world_wide

WDI_wide <- dcast(data = WDI_long,
                        formula = Year ~ Nation,
                        value.var = "CO2_emissions_ktCO2")
WDI_wide

## PIK
PIK <- as.data.table(read.csv("./Data/PIK/PRIMAP-hist_v1.1_06-Mar-2017.csv"))
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

## IEA

#### Combine data from different sources####
# World
combined_sources <- merge(CDIAC_world_wide, WDI_world_wide, by = "Year", all = TRUE)
setnames(combined_sources, "World", "WORLD_WDI")
setnames(combined_sources, "WORLD", "WORLD_CDIAC")
combined_sources <- merge(combined_sources, PIK_world_wide, by = "Year", all = TRUE)
combined_sources <- merge(combined_sources, CAIT_wide[, .(Year, WORLD)], by = "Year", all = TRUE)
setnames(combined_sources, "WORLD", "WORLD_CAIT")
combined_sources[, WORLD_WDI := WORLD_WDI / 1000]
#combined_sources[, World := NULL]

# USA
combined_sources_USA <- merge(CDIAC_wide[, .(Year, `UNITED STATES OF AMERICA`)], WDI_wide[, .(Year, `United States`)], by = "Year", all = TRUE)
setnames(combined_sources_USA, "UNITED STATES OF AMERICA", "CDIAC_USA")
setnames(combined_sources_USA, "United States", "WDI_USA")
combined_sources_USA <- merge(combined_sources_USA, PIK_wide_USA, by = "Year", all = TRUE)
combined_sources_USA[, WDI_USA := WDI_USA / 1000]
combined_sources_USA[, CDIAC_USA := CDIAC_USA / 1000]
combined_sources_USA <- merge(combined_sources_USA, CAIT_wide[, .(Year, USA)], by = "Year", all = TRUE)
setnames(combined_sources_USA, "USA", "CIAT_USA")
combined_sources_USA %>% filter(Year > 1960)

# China
combined_sources_CHN <- merge(CDIAC_wide[, .(Year, `CHINA (MAINLAND)`)], WDI_wide[, .(Year, China)], by = "Year", all = TRUE)
setnames(combined_sources_CHN, "CHINA (MAINLAND)", "CDIAC_CHN")
setnames(combined_sources_CHN, "China", "WDI_CHN")
combined_sources_CHN <- merge(combined_sources_CHN, PIK_wide_CHN, by = "Year", all = TRUE)
#combined_sources_CHN <- merge(combined_sources_CHN, PIK_wide_CHN[, .(Year, CHN_PIK)], by = "Year", all = TRUE)
combined_sources_CHN[, WDI_CHN := WDI_CHN / 1000]
combined_sources_CHN[, CDIAC_CHN := CDIAC_CHN / 1000]
combined_sources_CHN %>% filter(Year > 1960)

#### Plot series ####
## World
# Plot series WORLD and sum of all countries
ggplot(data=CDIAC_world_wide, aes(Year)) +
  geom_line(aes(y = WORLD, colour = "WORLD"))+
  geom_line(aes(y = WORLD_sum, colour = "Sum of all countries"))

# Plot differences (relative and absolute -> difference expected to be negative, as sum can be smaller than total)
ggplot(data=CDIAC_world_wide, aes(x = Year, y = diff_abs)) +
  geom_line()
ggplot(data=CDIAC_world_wide, aes(x = Year, y = diff_rel)) +
  geom_line()

# Plot series WORLD (CDIAC) and WDI from 1990 and PIK
ggplot(data=combined_sources %>% filter(Year >= 1960), aes(Year)) +
  ggtitle("World") +
  geom_line(aes(y = WORLD_CAIT, colour = "CAIT"), size = 1)+
  geom_line(aes(y = WORLD_CDIAC, colour = "CDIAC"), size = 1)+
  geom_line(aes(y = WORLD_WDI, colour = "WDI"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK CAT0"), size = 1) +
  geom_line(aes(y = WORLD_PIK_1, colour = "PIK CAT0 - CAT5"), size = 1) +
  geom_line(aes(y = WORLD_PIK_2, colour = "PIK CAT0 - CAT5 - CAT6 - CAT7"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK CAT1 + CAT2"), size = 1)

## USA
# Plot series WORLD (CDIAC) and WDI from 1990 and PIK
ggplot(data=combined_sources_USA %>% filter(Year >= 1960), aes(Year)) +
  ggtitle("USA") +
  geom_line(aes(y = CDIAC_USA , colour = "CDIAC - USA"), size = 1)+
  geom_line(aes(y = WDI_USA , colour = "WDI - USA"), size = 1) +
  geom_line(aes(y = CIAT_USA, colour = "CIAT"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1) +
  geom_line(aes(y = USA_PIK, colour = "PIK - CATM.0.EL"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1)


## China
# Plot series WORLD (CDIAC) and WDI from 1990 and PIK
ggplot(data=combined_sources_CHN %>% filter(Year >= 1960), aes(Year)) +
  ggtitle("China") +
  geom_line(aes(y = CDIAC_CHN , colour = "CDIAC - CHN"), size = 1)+
  geom_line(aes(y = WDI_CHN , colour = "WDI - CHN"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1) +
  geom_line(aes(y = CHN_PIK, colour = "PIK - CAT0 - CAT5"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1)

## Russia

#### Check country coverage per year in WDI CO2 series ####
sum(colSums(is.na(WDI_wide[Year == 1989])))
sum(colSums(is.na(WDI_wide[Year == 1990])))
