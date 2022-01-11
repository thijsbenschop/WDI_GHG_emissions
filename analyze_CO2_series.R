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
CDIAC <- as.data.table(read.csv("Data_private/CDIAC/nation_1751_2014_edited.csv"))
class(CDIAC)
colnames(CDIAC)

# Reshape wide
CDIAC_wide <- dcast(data = CDIAC, 
                    formula = Year ~ Nation, 
                    value.var = "CO2_emissions_ktCO2")
CDIAC_wide

# https://niwa.co.nz/atmosphere/faq/how-can-carbon-emissions-be-weighed
# multiply weight by 3 2/3 (C -> CO2)
CDIAC_wide <- CDIAC_wide %>% mutate(across(!Year , ~.x *(11/3)))

## CDIAC global - use total CO2
CDIAC_world <- as.data.table(read.csv("Data_private/CDIAC/global_1751_2014_edited.csv"))

CDIAC_world_wide <- dcast(data = CDIAC_world, 
                          formula = Year ~ Nation, 
                          value.var = "CO2_emissions_ktCO2")
CDIAC_world_wide
# multiply weight by 3 2/3 (C -> CO2)
CDIAC_world_wide <- CDIAC_world_wide %>% mutate(across(!Year , ~.x *(11/3)))

# add sum of all countries
CDIAC_world_wide[, WORLD_sum := rowSums(CDIAC_wide, na.rm = T) / 1000]

# add difference between sum and WORLD estimate -> difference due to double counting of some countries, e.g. Japan
CDIAC_world_wide[, diff_abs := WORLD_sum - WORLD]
CDIAC_world_wide[, diff_rel := 100 * (diff_abs / WORLD)]

## CAIT data
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

## WDI
WDI <- as.data.table(read.csv("Data_private/WDI/API_EN.ATM.CO2E.KT_DS2_en_csv_v2_2917960_edited.csv"))
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
PIK <- as.data.table(read.csv("./Data_private/PIK/PRIMAP-hist_v1.1_06-Mar-2017.csv"))
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

## IEA
IEA <- as.data.table(read_xls("Data_private/IEA/CO2Highlights2020.xls",
                              sheet = "CO2 FC",
                              range = "A24:AX193"))
IEA_long <- melt(IEA, id.vars = "Region/Country/Economy")
IEA_long
IEA_long[, value_numeric := as.numeric(value)]
IEA_long[, value := NULL]

setnames(IEA_long, "value_numeric", "CO2_emissions_ktCO2")
setnames(IEA_long, "variable", "Year")
setnames(IEA_long, "Region/Country/Economy", "Country")

IEA_wide <- dcast(data = IEA_long %>% filter(Country != "Africa"),
                  formula = Year ~ Country,
                  value.var = "CO2_emissions_ktCO2")
IEA_wide
IEA_wide[, Year := as.numeric(as.character((Year)))]

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
saveRDS(combined_sources, "Data/data_for_plots/combined_sources.rds")

# USA
combined_sources_USA <- merge(CDIAC_wide[, .(Year, `UNITED STATES OF AMERICA`)], WDI_wide[, .(Year, `United States`)], by = "Year", all = TRUE)
setnames(combined_sources_USA, "UNITED STATES OF AMERICA", "CDIAC_USA")
setnames(combined_sources_USA, "United States", "WDI_USA")
combined_sources_USA <- merge(combined_sources_USA, PIK_wide_USA, by = "Year", all = TRUE)
combined_sources_USA[, WDI_USA := WDI_USA / 1000]
combined_sources_USA[, CDIAC_USA := CDIAC_USA / 1000]
combined_sources_USA <- merge(combined_sources_USA, CAIT_wide[, .(Year, USA)], by = "Year", all = TRUE)
setnames(combined_sources_USA, "USA", "CAIT_USA")
combined_sources_USA <- merge(combined_sources_USA, IEA_wide[,.(Year, `United States`)], by = "Year", all = TRUE)
setnames(combined_sources_USA, "United States", "IEA_USA")

combined_sources_USA %>% filter(Year > 1960)
saveRDS(combined_sources_USA, "Data/data_for_plots/combined_sources_USA.rds")

# China
combined_sources_CHN <- merge(CDIAC_wide[, .(Year, `CHINA (MAINLAND)`)], WDI_wide[, .(Year, China)], by = "Year", all = TRUE)
setnames(combined_sources_CHN, "CHINA (MAINLAND)", "CDIAC_CHN")
setnames(combined_sources_CHN, "China", "WDI_CHN")
combined_sources_CHN <- merge(combined_sources_CHN, PIK_wide_CHN, by = "Year", all = TRUE)
#combined_sources_CHN <- merge(combined_sources_CHN, PIK_wide_CHN[, .(Year, CHN_PIK)], by = "Year", all = TRUE)
combined_sources_CHN[, WDI_CHN := WDI_CHN / 1000]
combined_sources_CHN[, CDIAC_CHN := CDIAC_CHN / 1000]
combined_sources_CHN <- merge(combined_sources_CHN, CAIT_wide[, .(Year, CHN)], by = "Year", all = TRUE)
setnames(combined_sources_CHN, "CHN", "CAIT_CHN")
combined_sources_CHN <- merge(combined_sources_CHN, IEA_wide[,.(Year, `People's Rep. of China`)], by = "Year", all = TRUE)
# or `China (incl. Hong Kong, China)`?
setnames(combined_sources_CHN, "People's Rep. of China", "IEA_CHN")

combined_sources_CHN %>% filter(Year > 1960)
saveRDS(combined_sources_CHN, "Data/data_for_plots/combined_sources_CHN.rds")

# Russia
combined_sources_RUS <- merge(CDIAC_wide[, .(Year, `RUSSIAN FEDERATION`)], WDI_wide[, .(Year, `Russian Federation`)], by = "Year", all = TRUE)
setnames(combined_sources_RUS, "RUSSIAN FEDERATION", "CDIAC_RUS")
combined_sources_RUS <- merge(combined_sources_RUS, CDIAC_wide[, .(Year, USSR)], by = "Year", all = TRUE)
setnames(combined_sources_RUS, "USSR", "CDIAC_USSR")
setnames(combined_sources_RUS, "Russian Federation", "WDI_RUS")
combined_sources_RUS <- merge(combined_sources_RUS, PIK_wide_RUS, by = "Year", all = TRUE)
#combined_sources_RUS <- merge(combined_sources_RUS, PIK_wide_CHN[, .(Year, CHN_PIK)], by = "Year", all = TRUE)
combined_sources_RUS[, WDI_RUS := WDI_RUS / 1000]
combined_sources_RUS[, CDIAC_RUS := CDIAC_RUS / 1000]
combined_sources_RUS <- merge(combined_sources_RUS, CAIT_wide[, .(Year, RUS)], by = "Year", all = TRUE)
setnames(combined_sources_RUS, "RUS", "CAIT_RUS")
combined_sources_RUS <- merge(combined_sources_RUS, IEA_wide[,.(Year, `Russian Federation`)], by = "Year", all = TRUE)
setnames(combined_sources_RUS, "Russian Federation", "IEA_RUS")

combined_sources_RUS %>% filter(Year > 1960)
saveRDS(combined_sources_RUS, "Data/data_for_plots/combined_sources_RUS.rds")

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
  geom_line(aes(y = WORLD_CAIT, colour = "CAIT"), size = 1)+
  geom_line(aes(y = WORLD_CDIAC, colour = "CDIAC"), size = 1)+
  geom_line(aes(y = WORLD_WDI, colour = "WDI"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK CAT0"), size = 1) +
  geom_line(aes(y = WORLD_PIK_1, colour = "PIK CAT0 - CAT5"), size = 1) +
  geom_line(aes(y = WORLD_PIK_2, colour = "PIK CAT0 - CAT5 - CAT6 - CAT7"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK CAT1 + CAT2"), size = 1) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "World", subtitle = "1960-latest datapoint")

ggplot(data=combined_sources %>% filter(Year >= 1980 & Year <= 2000), aes(Year)) +
  geom_line(aes(y = WORLD_CAIT, colour = "CAIT"), size = 1)+
  geom_line(aes(y = WORLD_CDIAC, colour = "CDIAC"), size = 1)+
  geom_line(aes(y = WORLD_WDI, colour = "WDI"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK CAT0"), size = 1) +
  geom_line(aes(y = WORLD_PIK_1, colour = "PIK CAT0 - CAT5"), size = 1) +
  geom_line(aes(y = WORLD_PIK_2, colour = "PIK CAT0 - CAT5 - CAT6 - CAT7"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK CAT1 + CAT2"), size = 1) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "World", subtitle = "1980-2000")

## USA
# Plot series WORLD (CDIAC) and WDI from 1990 and PIK
ggplot(data=combined_sources_USA %>% filter(Year >= 1960), aes(Year)) +
  geom_line(aes(y = CDIAC_USA , colour = "CDIAC - USA"), size = 1)+
  geom_line(aes(y = WDI_USA , colour = "WDI - USA"), size = 1) +
  geom_line(aes(y = CAIT_USA, colour = "CAIT"), size = 1) +
  geom_line(aes(y = IEA_USA, colour = "IEA"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1) +
  geom_line(aes(y = USA_PIK, colour = "PIK - CATM.0.EL"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "USA", subtitle = "1960-latest datapoint") #+

ggplot(data=combined_sources_USA %>% filter(Year >= 1980 & Year <= 2000), aes(Year)) +
  geom_line(aes(y = CDIAC_USA , colour = "CDIAC - USA"), size = 1)+
  geom_line(aes(y = WDI_USA , colour = "WDI - USA"), size = 1) +
  geom_line(aes(y = CAIT_USA, colour = "CAIT"), size = 1) +
  geom_line(aes(y = IEA_USA, colour = "IEA"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1) +
  geom_line(aes(y = USA_PIK, colour = "PIK - CATM.0.EL"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "USA", subtitle = "1980-2000") #+

## China
# Plot series WORLD (CDIAC) and WDI from 1990 and PIK
ggplot(data=combined_sources_CHN %>% filter(Year >= 1960), aes(Year)) +
  geom_line(aes(y = CDIAC_CHN, colour = "CDIAC - CHN"), size = 1)+
  geom_line(aes(y = WDI_CHN, colour = "WDI - CHN"), size = 1) +
  geom_line(aes(y = CAIT_CHN, colour = "CAIT"), size = 1) +
  geom_line(aes(y = IEA_CHN, colour = "IEA"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1) +
  geom_line(aes(y = CHN_PIK, colour = "PIK - CATM.0.EL"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "China", subtitle = "1960-latest datapoint")

# Plot series WORLD (CDIAC) and WDI from 1990 and PIK
ggplot(data=combined_sources_CHN %>% filter(Year >= 1980 & Year <= 2000), aes(Year)) +
  ggtitle("China 1980-2000") +
  geom_line(aes(y = CDIAC_CHN , colour = "CDIAC - CHN"), size = 1.2)+
  geom_line(aes(y = WDI_CHN , colour = "WDI - CHN"), size = 1.2) +
  geom_line(aes(y = CAIT_CHN, colour = "CAIT"), size = 1.2) +
  geom_line(aes(y = IEA_CHN, colour = "IEA"), size = 1.2) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1.2) +
  geom_line(aes(y = CHN_PIK, colour = "PIK - CATM.0.EL"), size = 1.2) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1.2) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "China", subtitle = "1980-2000")

## Russia
ggplot(data=combined_sources_RUS %>% filter(Year >= 1960), aes(Year)) +
  ggtitle("Russian Federation 1960-2020") +
  geom_line(aes(y = CDIAC_RUS , colour = "CDIAC - RUS"), size = 1)+
  geom_line(aes(y = WDI_RUS , colour = "WDI - RUS"), size = 1) +
  geom_line(aes(y = CAIT_RUS, colour = "CAIT"), size = 1) +
  geom_line(aes(y = IEA_RUS, colour = "IEA"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1) +
  geom_line(aes(y = RUS_PIK, colour = "PIK - CAT0 - CAT5"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "Russian Federation", subtitle = "1960-latest datapoint")

ggplot(data=combined_sources_RUS %>% filter(Year >= 1980 & Year <= 2000), aes(Year)) +
  ggtitle("Russian Federation 1960-2020") +
  geom_line(aes(y = CDIAC_RUS , colour = "CDIAC - RUS"), size = 1) +
  geom_line(aes(y = CDIAC_USSR/1000, colour = "CDIAC - USSR"), size = 1)+
  geom_line(aes(y = WDI_RUS , colour = "WDI - RUS"), size = 1) +
  geom_line(aes(y = CAIT_RUS, colour = "CAIT"), size = 1) +
  geom_line(aes(y = IEA_RUS, colour = "IEA"), size = 1) +
  geom_line(aes(y = CAT0/1000, colour = "PIK - CAT0"), size = 1) +
  geom_line(aes(y = RUS_PIK, colour = "PIK - CAT0 - CAT5"), size = 1) +
  geom_line(aes(y = (CAT1 + CAT2)/1000, colour = "PIK - CAT1 + CAT2"), size = 1) +
  labs(x = "Year", y= "CO2 emissions (ktCO2)", title = "Russian Federation", subtitle = "1980-2000")

ggplot(data=combined_sources_RUS %>% filter(Year >= 1960), aes(Year)) +
  geom_line(aes(y = CDIAC_RUS), colour = "red", linetype = 2, size = 1) +

#### Check country coverage per year in WDI CO2 series ####
sum(colSums(is.na(WDI_wide[Year == 1989])))
sum(colSums(is.na(WDI_wide[Year == 1990])))



#### Country coverage ####
CDIAC_coverage <- cbind(CDIAC_wide$Year, rowSums(is.na(CDIAC_wide[, -1])), rowSums(!is.na(CDIAC_wide[, -1])))
CDIAC_coverage
dim(CDIAC_wide)
dim(CDIAC_wide[, -1])

#### Further explore PIK data (country coverage, etc.) ####
colnames(PIK_long)
class(PIK_long)

table(PIK_long)

# Create one data.table for each GHG 
PIK_CO2 <- 



