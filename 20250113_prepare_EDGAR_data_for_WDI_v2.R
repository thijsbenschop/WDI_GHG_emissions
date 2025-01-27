# Title: 20241121_prepare_EDGAR_data_for_WDI.r
# Description: Script to prepare emissions data from PIK for ingestion in WDI
# Date: 5/20/2023
# Updated: 9/18/2024 -> to match csc data from climate group
# Updated: 10/15/2024 -> create 1990 dummies for change + replace World aggregates
# + add totals including LULUCF
# Updated: 11/21/2024 - use EDGAR 2024 data https://edgar.jrc.ec.europa.eu/dataset_ghg2024)
# New data structure and historic values changed
# Updated: 01/13/2025 - finalize after conversation with Jichong (new categories, changes to GWP)
# Author: Thijs Benschop

rm(list=ls())

#### Load libraries and data ####
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)
library(openxlsx)
library(tidyr)
library(wbstats)
setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")
rm(list = ls())

options(scipen=999)

#### Load EDGAR data ####
# Version EDGAR_2024_GHG // Published November 2024
# Data downloaded from https://edgar.jrc.ec.europa.eu/dataset_ghg2024
# One dataset per GHG + total

folders <- list.dirs("./Data_private/EDGAR/2024/") # list unzipped folders
folders <- folders[-1] # remove root folder
file_names <- basename(folders) 
#file_names[which(file_names == "EDGAR_AR5_GHG_1970_2022b")] <- "EDGAR_AR5_GHG_1970_2022" # remove suffix "b" from filename

# Load all files in memory and reshape files
for(i in 1:length(file_names)){
  print(i)
  cur_path_filename <- paste0(folders[i], "/", file_names[i], ".xlsx")
  cur_data <- as.data.table(readxl::read_xlsx(cur_path_filename,
                                              sheet = "IPCC 2006",
                                              skip = 9))
  
  
  # Convert data in long format
  cur_data_long <- melt(cur_data, id.vars = c("IPCC_annex", 
                                              "C_group_IM24_sh", 
                                              "Country_code_A3",
                                              "Name",
                                              "ipcc_code_2006_for_standard_report",
                                              "ipcc_code_2006_for_standard_report_name",
                                              "Substance",
                                              "fossil_bio"),
                        variable.name = "year",
                        value.factor = FALSE, variable.factor = FALSE)
  
  # Convert year to numeric (from Y_1970 to 1970)
  cur_data_long[, year := as.numeric(substring(year, 3, 6))]
  
  # Drop some columns
  cur_data_long[, c("IPCC_annex", "C_group_IM24_sh", "Name", "ipcc_code_2006_for_standard_report_name") := NULL]  
  
  dim(cur_data_long)
  colnames(cur_data_long)

  assign(file_names[i], cur_data_long)
  rm(cur_data_long, cur_data)
}

# Append all datasets
file_names
EDGAR_long <- rbind(get(file_names[1]), # total GHG AR5
                    get(file_names[3]), # CH4
                    get(file_names[4]), # CO2 bio
                    #get(file_names[5]), # F-gases, don't use gas-specific, use AR5
                    get(file_names[2]), # F gases AR5
                    get(file_names[6]), # N2O
                    get(file_names[7])) # CO2 fossil from IEA

dim(EDGAR_long)
colnames(EDGAR_long)
head(EDGAR_long)
#View(EDGAR_AR5_GHG_1970_2022)

# Rename columns
colnames(EDGAR_long)
setnames(EDGAR_long, "ipcc_code_2006_for_standard_report", "category")
setnames(EDGAR_long, "Country_code_A3", "ISO3")
colnames(EDGAR_long)

#### Load Grassi LULUCF data ####
# Data unchanged
GRASSI <- as.data.table(read_xlsx("./Data_private/Grassi/National inventories LULUCF data 2000-2020 (Dec 2022).xlsx",
          sheet = "Table 5",
          skip = 3)) #,
#          n_max = 195))
colnames(GRASSI)

table(GRASSI$`gap-filling`, useNA = "ifany")
GRASSI <- GRASSI %>% filter(`gap-filling` == "gap-filled") # remove headings and white lines

table(GRASSI$`LAND CATEGORY`, useNA = "ifany")
#table(GRASSI$`UNFCCC country`, useNA = "ifany")

GRASSI[, "Av.. 2000-2020" := NULL] # drop 2000-2020
GRASSI[, c("gap-filling", "UNFCCC country", "Unit") := NULL] # keep 2000-2020

# Convert data in long format
GRASSI_long <- melt(GRASSI, id.vars = c("country code", "LAND CATEGORY"),
                      variable.name = "year",
                      value.factor = FALSE, 
                    variable.factor = FALSE)
# GRASSI_long[, c("ISO3",   "year",   "Substance",    "category", "value", "fossil_bio") :=
#               .(`country code`, year, "LULUCF", 0, value, "bio")]
# GRASSI_long[, `country code` := NULL]
# setcolorder(GRASSI_long, c("ISO3", "category", "Substance", "fossil_bio", "year", "value"))

GRASSI_long <- GRASSI_long %>% mutate(Series = case_when(
  `LAND CATEGORY` == "DEFORESTATION" ~ "EN.GHG.CO2.LU.DF.MT.CE.AR5", # Carbon dioxide (CO2) net fluxes from LULUCF - Deforestation (Mt CO2e)    
  `LAND CATEGORY` == "FOREST LAND" ~ "EN.GHG.CO2.LU.FL.MT.CE.AR5", # Carbon dioxide (CO2) net fluxes from LULUCF - Forest Land (Mt CO2e)     
  `LAND CATEGORY` == "LULUCF net" ~ "EN.GHG.CO2.LU.MT.CE.AR5",	   # Carbon dioxide (CO2) net fluxes from LULUCF - Total excluding fires (Mt CO2e)  
  `LAND CATEGORY` == "ORGANIC SOILS" ~ "EN.GHG.CO2.LU.OS.MT.CE.AR5", # Carbon dioxide (CO2) net fluxes from LULUCF - Organic Soil (Mt CO2e)
  `LAND CATEGORY` == "OTHER LAND USES" ~ "EN.GHG.CO2.LU.OL.MT.CE.AR5" # Carbon dioxide (CO2) net fluxes from LULUCF - Other Land (Mt CO2e)
))

GRASSI_long[, c("ISO3",   "year",   "Substance",    "category", "value", "fossil_bio", "Series") :=
               .(`country code`, year, "CO2", `LAND CATEGORY`, value, "bio", Series)]
GRASSI_long

################################################################################
#### Select WDI countrues only ####
# Check which WDI countries available
# # Load country WDI list (217 countries)
WDI_countries <- as.data.table(read.csv("Data/wdi_country_list.csv"))
setnames(WDI_countries, c("long_name", "ISO3"))

# # Get all WDI countries from API
# WDI_countries <- wbstats::wb_countries()
# WDI_countries <- WDI_countries %>% 
#   filter(!(region == "Aggregates")) %>%  # drop regions
#   select(iso3c, country) # keep iso3 code and name
# dim(WDI_countries)
# setnames(WDI_countries, c("long_name", "ISO3"))

EDGAR_countries <- unique(EDGAR_long$ISO3)
length(EDGAR_countries) # 223 countries
table(WDI_countries$ISO3 %in% EDGAR_countries) # 203 WDI countries available in EDGAR
WDI_countries[!(WDI_countries$ISO3 %in% EDGAR_countries)] # list of missing countries (not in EDGAR data)
# are these countries/territories included in other countries? -> need to combine countries?
table(EDGAR_countries %in% WDI_countries$ISO3)
EDGAR_countries[!(EDGAR_countries %in% WDI_countries$ISO3)] # list of missing countries (not in WDI list)

# AIA: Anguilla
# AIR: Aruba
# ANT: Netherlands Antilles (Note: This code was deprecated in 2010 and replaced with separate codes for Bonaire, Sint Eustatius, and Saba)
# COK: Cook Islands
# ESH: Western Sahara
# FLK: Falkland Islands (Malvinas)
# GLP: Guadeloupe
# GUF: French Guiana
# MSR: Montserrat
# MTQ: Martinique
# MYT: Mayotte
# NIU: Niue
# PCN: Pitcairn Islands (new in 2024)
# REU: Réunion
# SCG: Serbia and Montenegro (no longer in use; now Serbia and Montenegro are separate countries with codes SRB and MNE respectively)
# SEA: ?
# SHN: Saint Helena, Ascension and Tristan da Cunha
# SPM: Saint Pierre and Miquelon
# TKL: Tokelau
# TWN: Taiwan, Province of China
# WLF: Wallis and Futuna

GRASSI_countries <- unique(GRASSI_long$ISO3)
length(GRASSI_countries) # 197 countries
table(WDI_countries$ISO3 %in% GRASSI_countries) # 193 WDI countries available in GRASSI
WDI_countries[!(ISO3 %in% GRASSI_countries)] # list of missing countries (not in GRASSI data)
# are these countries/territories included in other countries? -> need to combine countries?
table(GRASSI_countries %in% WDI_countries$ISO3)
GRASSI_countries[!(GRASSI_countries %in% WDI_countries$ISO3)] # list of missing countries (not in WDI list)

# Drop countries/regions not in WDI
GRASSI_long <- GRASSI_long[ISO3 %in% WDI_countries$ISO3]

# Drop countries/regions not in WDI
EDGAR_long <- EDGAR_long[ISO3 %in% WDI_countries$ISO3]

#PIK_long[, value := value / 1000] # convert to Mt from gigagram (gigagram = 10^9 g = 10^6 kg = 10^3 t = kt)

#### Prepare GRASSI data ####
#GRASSI_long %>% 

# Compare with CSC data from Jichong
#### Compare CSC data from Jichong with our results ####
# See dashboard with CSC data: https://public.tableau.com/app/profile/david.groves/viz/WBGCSC-HistoricalGHGEmissions-v2d/GHGEmissions
# read spreadsheet
CSC_data_LULUCF <- as.data.frame(read_xlsx("CSC-GHG_emissions-LATEST.xlsx", 
                                    sheet = "C total subs & gas (V8+Grassi)"))
dim(CSC_data_LULUCF)
table(CSC_data_LULUCF$`CSC Sector`)
CSC_data_LULUCF <- 
  CSC_data_LULUCF %>% 
  filter(`CSC Sector` == "Land Use, Land Use Change, and Forestry") %>%
  select(c(Code, `CSC Sector`, `CSC Subsector`, Gas, as.character(2000:2022))) %>% # select columns
  tidyr::pivot_longer(cols = as.character(2000:2022), # to long format
                      names_to = "year") %>% 
  mutate(Series = case_when(
    `CSC Subsector` == "LULUCF - Deforestation" ~ "EN.GHG.CO2.LU.DF.MT.CE.AR5",   
    `CSC Subsector` == "LULUCF - Organic Soil " ~ "EN.GHG.CO2.LU.OS.MT.CE.AR5",    
    `CSC Subsector` == "LULUCF - Forest Land" ~ "EN.GHG.CO2.LU.FL.MT.CE.AR5",   
    `CSC Subsector` == "LULUCF - Other Land" ~ "EN.GHG.CO2.LU.OL.MT.CE.AR5")) %>%
  select(c(Code, Gas, year, value, Series))

CSC_data_LULUCF <- rbind(CSC_data_LULUCF,
                         CSC_data_LULUCF %>% 
  group_by(Code, Gas, year) %>%
  summarise(value = sum(value)) %>%
  # add all rows with same code - gas - year - sector_WDI combination
  mutate(year = as.integer(year)) %>%
  mutate(Series = "EN.GHG.CO2.LU.MT.CE.AR5",
         value = round(value, 4)))

dim(CSC_data_LULUCF)
compare_CSC_GRASSI <- GRASSI_long %>% left_join(CSC_data_LULUCF,
                                                by = c('ISO3'='Code', 
                                                       'year'='year',
                                                       'Substance' = 'Gas',
                                                       'Series' = 'Series'))
dim(compare_CSC_GRASSI)
compare_CSC_GRASSI <- compare_CSC_GRASSI %>% mutate(x_div_y = value.x / round(value.y, 4))
summary(compare_CSC_GRASSI$x_div_y)
table(compare_CSC_GRASSI$x_div_y, compare_CSC_GRASSI$Series, useNA= "always")

#View(compare_CSC_GRASSI %>% filter(is.na(x_div_y)))
#View(compare_CSC_GRASSI %>% filter(is.nan(x_div_y)))

test_G <- compare_CSC_GRASSI %>% filter((x_div_y > 1.001 | x_div_y < 0.999) & !is.na(x_div_y))
table(test_G$Series) # no records

#View(test_G %>% filter(Series == "EN.GHG.CH4.FE.MT.CE.AR5"))

#### Prepare EDGAR data ####
# Add "Time", "Country", "SCALE" columns as in DCS template
EDGAR_long[, Time := paste0("YR", year)]
EDGAR_long[, Country := ISO3]
EDGAR_long[, SCALE := 0]

# Prepare data for each indicator
table(EDGAR_long$Substance)
table(EDGAR_long$category)

#### Combine gases ####
head(EDGAR_long)
table(EDGAR_long$Substance)

# Create var for GHG
EDGAR_long[, GHG := Substance]
EDGAR_long[Substance %in% c("GWP_100_AR5_HCFC", 
                            "GWP_100_AR5_HFC", 
                            "GWP_100_AR5_NF3",
                            "GWP_100_AR5_PFC",
                            "GWP_100_AR5_SF6"), GHG := "FGAS"]
EDGAR_long[Substance %in% c("CO2", 
                            "CO2bio"), GHG := "CO2"]
table(EDGAR_long$Substance, EDGAR_long$GHG, useNA = "always")

# Drop CO2bio (already included in CO2)
EDGAR_long <- EDGAR_long[-which(Substance == "CO2bio"), ]

# Test: drop CH4/bio

#View(EDGAR_long %>% filter(year == 2022 & ISO3 == "NLD"))
#EDGAR_long <- EDGAR_long[-which(Substance == "N2O" & fossil_bio == "bio"), ]

table(EDGAR_long$GHG, useNA = "ifany")

# Group gases, e.g. sum all F-gases
EDGAR_long <- EDGAR_long %>% 
  #select(!c(Substance, fossil_bio)) %>%
  dplyr::group_by(ISO3, category, year, GHG) %>%
  dplyr::summarise(value = sum(value, na.rm = T)) %>% 
  as.data.table()
head(EDGAR_long)

#### Convert to CO2eq ####
# use GWP values AR5 from https://ghgprotocol.org/sites/default/files/Global-Warming-Potential-Values%20(Feb%2016%202016)_1.pdf
# N2O - 265 (was 298)
# CH4 - 28 (was 25)

#EDGAR_long[GHG == "CH4", value := value * 28]
#EDGAR_long[GHG == "N2O", value := value * 265]

EDGAR_long <- EDGAR_long %>% mutate(GWP_factor  = case_when(
  (GHG == "CO2" | GHG == "FGAS" | GHG == "GWP_100_AR5_GHG") ~ 1, # CO2 and F-gases and total (already converted)
  (GHG == "N2O") ~ 265,
  (GHG == "CH4" & category %in% c("2.B", "2.C", "4.A", "4.B", "4.C", "4.D",
                                  "1.A.1.bc", "1.B.1", "1.B.2", "5.B")) ~ 30, # fugitive CH4 emissions
  (GHG == "CH4" & !(category %in% c("2.B", "2.C", "4.A", "4.B", "4.C", "4.D",
                                    "1.A.1.bc", "1.B.1", "1.B.2", "5.B"))) ~ 28)) # CH4

table(EDGAR_long$GHG, EDGAR_long$GWP_factor, useNA = "ifany")

EDGAR_long <- EDGAR_long %>% mutate(value := value * GWP_factor)

# See email Jichong 9/16/2024 Re: Questions on CSC GHG emissions data
# See email EDGAR Marilena Muntean 11/14/2024 - fugutive CH4 emissions get GWP 30
# •	IN - Industrial Processes CH4 (2B and 2C) used a GWP AR5-100 factor of 30 instead of 28 and you only need aggregate CH4 "fossil"
# •	Waste - Solid Waste CH4 (4A, 4B) used a GWP AR5-100 factor of 30 instead of 28 and you only need aggregate CH4 "fossil"
# •	Waste - Solid Waste CH4 (4C) used a GWP AR5-100 factor of 30 instead of 28 and you need aggregate CH4 "fossil" and CH4 "bio"
# •	Waste - Wastewater Treatment (4D) used a GWP AR5-100 factor of 30 instead of 28 and you only need aggregate CH4 "fossil"

#### Convert to Mt from kt and round ####
EDGAR_long <- EDGAR_long %>% mutate(value = round(value / 1000, 8))

#View(EDGAR_long %>% filter(ISO3 == "NLD" & year == "2020"))

#### Combine categories/sectors ####
table(EDGAR_long$category)

# Create var for categories -> use only first digit
# See page 6 of https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/0_Overview/V0_1_Overview.pdf
# EDGAR_long[, sector := substring(category, 1, 1)]
# See for mapping file compare_CSC_EDGAR_NLD_2020.xlsx

EDGAR_long <- EDGAR_long %>% mutate(sector_WDI = case_when(
  (GHG == "CH4" & category %in% c("3.A.1",	"3.A.2",	"3.C.1", "3.C.7")) ~ "AG",
  (GHG == "CH4" & category %in% c("3.A.1",	"3.A.2",	"3.C.1")) ~ "AG",
  (GHG == "CH4" & category %in% c("1.A.4", "1.A.5")) ~ "BU",
  (GHG == "CH4" & category %in% c("1.A.1.a")) ~ "PI",
  (GHG == "CH4" & category %in% c("1.A.1.bc", "1.B.1", "1.B.2", "5.B")) ~ "FE",
  (GHG == "CH4" & category %in% c("1.A.2")) ~ "IC",
  (GHG == "CH4" & category %in% c("1.A.3.a", "1.A.3.b_noRES", "1.A.3.c", "1.A.3.d", "1.A.3.e")) ~ "TR",
  (GHG == "CH4" & category %in% c("2.B", "2.C")) ~ "IP",
  (GHG == "CH4" & category %in% c("4.A", "4.B", "4.C", "4.D")) ~ "WA",
  (GHG == "CO2" & category %in% c("3.C.2", "3.C.3")) ~ "AG",
  (GHG == "CO2" & category %in% c("1.A.4", "1.A.5")) ~ "BU",
  (GHG == "CO2" & category %in% c("1.A.1.a")) ~ "PI",
  (GHG == "CO2" & category %in% c("1.A.1.bc", "1.B.1", "1.B.2", "5.B")) ~ "FE",
  (GHG == "CO2" & category %in% c("1.A.2")) ~ "IC",
  (GHG == "CO2" & category %in% c("1.A.3.a", "1.A.3.b_noRES", "1.A.3.c", "1.A.3.d", "1.A.3.e")) ~ "TR",
  (GHG == "CO2" & category %in% c("2.A.1", "2.A.2", "2.A.3", "2.A.4", "2.B", "2.C", "2.D")) ~ "IP",
  (GHG == "CO2" & category %in% c("4.C", "3.A.2")) ~ "WA",
  (GHG == "N2O" & category %in% c("3.A.2", "3.C.1", "3.C.4", "3.C.5", "3.C.6")) ~ "AG",
  (GHG == "N2O" & category %in% c("1.A.4", "1.A.5")) ~ "BU",
  (GHG == "N2O" & category %in% c("1.A.1.a")) ~ "PI",
  (GHG == "N2O" & category %in% c("1.A.1.bc", "1.B.1", "1.B.2", "5.B")) ~ "FE",
  (GHG == "N2O" & category %in% c("1.A.2")) ~ "IC",
  (GHG == "N2O" & category %in% c("1.A.3.a", "1.A.3.b_noRES", "1.A.3.c", "1.A.3.d", "1.A.3.e")) ~ "TR",
  (GHG == "N2O" & category %in% c("2.B", "2.G", "5.A")) ~ "IP",
  (GHG == "N2O" & category %in% c("4.B", "4.C", "4.D")) ~ "WA", ## check 
  (GHG == "FGAS" & category %in% c("2.B", "2.C", "2.E", "2.F", "2.G")) ~ "IP",
  GHG == "GWP_100_AR5_GHG" ~ "ALL")) # all GHG together

table(EDGAR_long$sector_WDI, useNA = "always") # any non-classified lines?

unclassified <- EDGAR_long %>% filter(is.na(sector_WDI)) # check unclassified lines
table(unclassified$category, unclassified$GHG) # view GHG/category pairs that aren't classified
#View(unclassified)
# added "1.A.3.e" (off-road, all), "2.A.2" (CO2), "2.D" (CO2), "3.A.2" (N2O), "3.C.7" (CH4), "5.B" (all)
EDGAR_long <- EDGAR_long %>% filter(!is.na(sector_WDI)) # need to classify there!!!!

# Group by WDI sector
EDGAR_long <- EDGAR_long %>% 
  dplyr::group_by(ISO3, year, GHG, sector_WDI) %>% # fossil_bio
  dplyr::summarise(value = sum(value, na.rm = T)) %>% 
  as.data.table()
head(EDGAR_long)

table(EDGAR_long$GHG)
table(EDGAR_long$sector)

# Add sum of all sectors
EDGAR_long_tot <- EDGAR_long[, sum(value, na.rm = T),
           by = list(ISO3, year, GHG)]
EDGAR_long_tot[, sector_WDI := "ALL"] # 0 = all
EDGAR_long_tot[ , value := V1]
EDGAR_long_tot[ , V1 := NULL]
# drop ALL - for FGAS
EDGAR_long_tot <- EDGAR_long_tot %>% filter(!(GHG %in% c("FGAS", "GWP_100_AR5_GHG"))) # total over all GHG already in database

head(EDGAR_long)
head(EDGAR_long_tot)

table(EDGAR_long$sector_WDI, EDGAR_long$GHG)
table(EDGAR_long_tot$sector_WDI, EDGAR_long_tot$GHG)

EDGAR_combined <- rbind(EDGAR_long, EDGAR_long_tot)

##### Add indicator names/codes for EDGAR data #####
## Prepare files for upload in DCS
# Need columns: Time	Country	Series	SCALE	Data
# E.g. YR1960	AFG	SP.POP.TOTL	0	8996967

# Check if all lines labelled (gas and sector)
table(EDGAR_combined$GHG, EDGAR_combined$sector_WDI, useNA = "always") # 29 combinations

# Read mapping gas/sector -> indicator_code WDI
mapping_to_indicator_code <- as.data.frame(read_xlsx("compare_CSC_EDGAR_NLD_2020.xlsx", 
                                                     sheet = "mapping_to_indicator_code"))

EDGAR_combined <- EDGAR_combined %>% left_join(mapping_to_indicator_code, 
                           by = c('GHG'='Specific_subject', 
                                  'sector_WDI'='Ext_1'))

EDGAR_combined <- EDGAR_combined %>%
  mutate(value = round(value, 4))

table(is.na(EDGAR_combined$Series_code_new))
table(EDGAR_combined$Series_code_new)
dim(EDGAR_combined)
EDGAR_combined
#View(EDGAR_combined %>% filter(ISO3 == "NLD" & year == "2022"))

#### Add data for 1990 comparison for years 1991 - 2030 ####
EDGAR_1990 <- EDGAR_combined %>% filter(year == 1990) %>%
  filter(Series_code_new %in% c("EN.GHG.ALL.MT.CE.AR5", # Total greenhouse gas emissions excluding LULUCF (Mt CO2e)
                                "EN.GHG.CO2.MT.CE.AR5", #	Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)
                                "EN.GHG.CH4.MT.CE.AR5",	# Methane (CH4) emissions (total) excluding LULUCF (Mt CO2e)
                                "EN.GHG.N2O.MT.CE.AR5")) %>% 	# Nitrous oxide (N2O) emissions (total) excluding LULUCF (Mt CO2e)
  mutate(Series_code_new = gsub(".MT.CE.", ".1990.", Series_code_new)) 

EDGAR_1990_values <- EDGAR_1990 %>% mutate(year = 1991)

for(i in 1992:2030){
  print(i)
  EDGAR_1990_values <- rbind(EDGAR_1990_values, 
                      EDGAR_1990 %>% mutate(year = i))
}

dim(EDGAR_1990_values)

EDGAR_combined <- rbind(EDGAR_combined,
                        EDGAR_1990_values)

#### Add World data from report ####
# Spreadsheet downloaded from https://edgar.jrc.ec.europa.eu/report_2024
GHG_totals_by_country <- as.data.table(read_xlsx("./Data_private/EDGAR/2024/EDGAR_2024_GHG_booklet_2024.xlsx", 
                                                 sheet = "GHG_totals_by_country"))
GHG_totals_by_country_prep <- melt(GHG_totals_by_country, 
                                   id.vars = c("EDGAR Country Code", 
                                               "Country"),
                                   variable.name = "year",
                                   value.factor = FALSE, variable.factor = FALSE) %>% 
  filter(`Country` == "GLOBAL TOTAL") %>%
  mutate(Time = paste0("YR", year),
         Country = "WLD",
         Series = "EN.GHG.ALL.MT.CE.AR5", # Total greenhouse gas emissions excluding LULUCF (Mt CO2e)
         SCALE = 0,
         Data = value) %>% 
  select(Time, Country, Series, SCALE, Data)
GHG_totals_by_country_prep
table(GHG_totals_by_country_prep$Series)

GHG_by_sector_and_country <- as.data.table(read_xlsx("./Data_private/EDGAR/2024/EDGAR_2024_GHG_booklet_2024.xlsx", 
                                                 sheet = "GHG_by_sector_and_country"))
GHG_by_sector_and_country_prep <- melt(GHG_by_sector_and_country, 
                                   id.vars = c("Substance",
                                               "Sector",
                                               "EDGAR Country Code", 
                                               "Country"),
                                   variable.name = "year",
                                   value.factor = FALSE, variable.factor = FALSE) %>% 
  filter(`Country` == "GLOBAL TOTAL") %>%
  mutate(Time = paste0("YR", year),
         Country = "WLD",
         Series = dplyr::if_else(Substance == 'CO2' & Sector == 'Agriculture', "EN.GHG.CO2.AG.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Agriculture (Mt CO2e)
                                 dplyr::if_else(Substance == 'CO2' & Sector == 'Buildings', "EN.GHG.CO2.BU.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Building (Energy) (Mt CO2e)
                                                dplyr::if_else(Substance == 'CO2' & Sector == 'Fuel Exploitation', "EN.GHG.CO2.FE.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Fugitive Emissions (Energy) (Mt CO2e)
                                                               dplyr::if_else(Substance == 'CO2' & Sector == 'Industrial Combustion', "EN.GHG.CO2.IC.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Industrial Combustion (Energy) (Mt CO2e)
                                                                              dplyr::if_else(Substance == 'CO2' & Sector == 'Power Industry', "EN.GHG.CO2.PI.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Power Industry (Energy) (Mt CO2e)
                                                                                             dplyr::if_else(Substance == 'CO2' & Sector == 'Processes', "EN.GHG.CO2.IP.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Industrial Processes (Mt CO2e)
                                                                                                            dplyr::if_else(Substance == 'CO2' & Sector == 'Transport', "EN.GHG.CO2.TR.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Transport (Energy) (Mt CO2e)
                                                                                                                           dplyr::if_else(Substance == 'CO2' & Sector == 'Waste', "EN.GHG.CO2.WA.MT.CE.AR5", #Carbon dioxide (CO2) emissions from Waste (Mt CO2e)
                                                                                                                                          dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Agriculture', "EN.GHG.CH4.AG.MT.CE.AR5", #Methane (CH4) emissions from Agriculture (Mt CO2e)
                                                                                                                                                         dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Buildings', "EN.GHG.CH4.BU.MT.CE.AR5", #Methane (CH4) emissions from Building (Energy) (Mt CO2e)
                                                                                                                                                                        dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Fuel Exploitation', "EN.GHG.CH4.FE.MT.CE.AR5", #Methane (CH4) emissions from Fugitive Emissions (Energy) (Mt CO2e)
                                                                                                                                                                                       dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Industrial Combustion', "EN.GHG.CH4.IC.MT.CE.AR5", #Methane (CH4) emissions from Industrial Combustion (Energy) (Mt CO2e)
                                                                                                                                                                                                      dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Power Industry', "EN.GHG.CH4.PI.MT.CE.AR5", #Methane (CH4) emissions from Power Industry (Energy) (Mt CO2e)
                                                                                                                                                                                                                     dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Processes', "EN.GHG.CH4.IP.MT.CE.AR5", #Methane (CH4) emissions from Industrial Processes (Mt CO2e)
                                                                                                                                                                                                                                    dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Transport', "EN.GHG.CH4.TR.MT.CE.AR5", #Methane (CH4) emissions from Transport (Energy) (Mt CO2e)
                                                                                                                                                                                                                                                   dplyr::if_else(Substance == 'GWP_100_AR5_CH4' & Sector == 'Waste', "EN.GHG.CH4.WA.MT.CE.AR5", #Methane (CH4) emissions from Waste (Mt CO2e)
                                                                                                                                                                                                                                                                  dplyr::if_else(Substance == 'GWP_100_AR5_F-gases' & Sector == 'Processes', "EN.GHG.FGAS.IP.MT.CE.AR5", #F-gases emissions from Industrial Processes (Mt CO2e)
                                                                                                                                                                                                                                                                                 dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Agriculture', "EN.GHG.N2O.AG.MT.CE.AR5", #Nitrous oxide (N2O) emissions from Agriculture (Mt CO2e)
                                                                                                                                                                                                                                                                                                dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Buildings', "EN.GHG.N2O.BU.MT.CE.AR5", #Nitrous oxide (N2O) emissions from Building (Energy) (Mt CO2e)
                                                                                                                                                                                                                                                                                                               dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Fuel Exploitation', "EN.GHG.N2O.FE.MT.CE.AR5", #Nitrous oxide (N2O) emissions from Fugitive Emissions (Energy) (Mt CO2e)
                                                                                                                                                                                                                                                                                                                              dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Industrial Combustion', "EN.GHG.N2O.IC.MT.CE.AR5", #Nitrous oxide (N2O) emissions from Industrial Combustion (Energy) (Mt CO2e)
                                                                                                                                                                                                                                                                                                                                             dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Power Industry', "EN.GHG.N2O.PI.MT.CE.AR5", #Nitrous oxide (N2O) emissions from Power Industry (Energy) (Mt CO2e)
                                                                                                                                                                                                                                                                                                                                                            dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Processes', "EN.GHG.N2O.IP.MT.CE.AR5", #Nitrous oxide (N2O) emissions from Industrial Processes (Mt CO2e)
                                                                                                                                                                                                                                                                                                                                                                           dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Transport', "EN.GHG.N2O.TR.MT.CE.AR5", #Nitrous oxide (N2O) emissions from Transport (Energy) (Mt CO2e)
                                                                                                                                                                                                                                                                                                                                                                                          dplyr::if_else(Substance == 'GWP_100_AR5_N2O' & Sector == 'Waste', "EN.GHG.N2O.WA.MT.CE.AR5", "Not found") #Nitrous oxide (N2O) emissions from Waste (Mt CO2e)
                                                                                                                                                                                                                                                                                                                                                                                          )
                                                                                                                                                                                                                                                                                                                                                                           )
                                                                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                                                             )
                                                                                                                                                                                                                                                                                                                              )
                                                                                                                                                                                                                                                                                                               )
                                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                                 )
                                                                                                                                                                                                                                                                  )
                                                                                                                                                                                                                                                   )
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                     )
                                                                                                                                                                                                      )
                                                                                                                                                                                       )
                                                                                                                                                                        )
                                                                                                                                                         )
                                                                                                                                          )
                                                                                                                           )
                                                                                                            )
                                                                                             )
                                                                              )
                                                               )
                                                )
                                 ),
         SCALE = 0,
         Data = value) %>% 
  select(Time, Country, Series, SCALE, Data)
GHG_by_sector_and_country_prep 
table(GHG_by_sector_and_country_prep$Series)

# Create totals by GHG
GHG_by_sector_and_country_totals_prep <- melt(GHG_by_sector_and_country, 
                                              id.vars = c("Substance",
                                                          "Sector",
                                                          "EDGAR Country Code", 
                                                          "Country"),
                                              variable.name = "year",
                                              value.factor = FALSE, variable.factor = FALSE) %>% 
  filter(`Country` == "GLOBAL TOTAL") %>%
  filter(Substance != "GWP_100_AR5_F-gases") %>% 
  group_by(Substance, `EDGAR Country Code`, Country, year) %>% 
  summarise(value = sum(value)) %>%
  mutate(Time = paste0("YR", year),
         Country = "WLD",
         Series = dplyr::if_else(Substance == 'CO2', 
                                 "EN.GHG.CO2.MT.CE.AR5", # Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)
                                 dplyr::if_else(Substance == 'GWP_100_AR5_CH4', 
                                                "EN.GHG.CH4.MT.CE.AR5", # Methane (CH4) emissions (total) excluding LULUCF (Mt CO2e)
                                                dplyr::if_else(Substance == 'GWP_100_AR5_N2O', 
                                                               "EN.GHG.N2O.MT.CE.AR5", # Nitrous oxide (N2O) emissions (total) excluding LULUCF (Mt CO2e)
                                                               "Not found")
                                 )
         ),
         SCALE = 0,
         Data = value) %>% ungroup() %>%
  dplyr::select(Time, Country, Series, SCALE, Data)
GHG_by_sector_and_country_totals_prep
table(GHG_by_sector_and_country_totals_prep$Series)

# Add 1990 levels
GHG_1990_levels_WLD <- rbind(GHG_totals_by_country_prep, GHG_by_sector_and_country_totals_prep) %>%
  filter(Time == "YR1990") %>%
  mutate(Series = gsub(".MT.CE.", ".1990.", Series)) 
GHG_1990_levels_WLD_prep <- GHG_1990_levels_WLD %>% mutate(Time = "YR1991")

for(i in 1992:2030){
  print(i)
  GHG_1990_levels_WLD_prep <- rbind(GHG_1990_levels_WLD_prep, 
                                    GHG_1990_levels_WLD 
                                    %>% mutate(Time = paste0("YR", i)))
}
GHG_1990_levels_WLD_prep
table(GHG_1990_levels_WLD_prep$Series)

# EN.GHG.CO2.1990.AR5, # Carbon dioxide (CO2) emissions (total) excluding LULUCF (1990 level for computation of change)	
# EN.GHG.CH4.1990.AR5, # Methane (CH4) emissions (total) excluding LULUCF (1990 level for computation of change)	
# EN.GHG.N2O.1990.AR5, # Nitrous oxide (N2O) emissions (total) excluding LULUCF (1990 level for computation of change)	
# EN.GHG.ALL.1990.AR5, # Total greenhouse gas emissions excluding LULUCF (1990 level for computation of change)	

# LULUCF
LULUCF_macroregions <- as.data.table(read_xlsx("./Data_private/EDGAR/2024/EDGAR_2024_GHG_booklet_2024.xlsx", 
                                                 sheet = "LULUCF_macroregions"))
LULUCF_macroregions_prep <- melt(LULUCF_macroregions, 
                                   id.vars = c("Substance", 
                                               "Macro-region",
                                               "Sector"),
                                   variable.name = "year",
                                   value.factor = FALSE, variable.factor = FALSE) %>% 
  filter(`Macro-region` == "GLOBAL TOTAL") %>%
  filter(Substance %in% c("CO2", "GWP_100_AR5_GHG")) %>% 
  filter(Sector != "Fires") %>%
  mutate(Time = paste0("YR", year),
         Country = "WLD",
         Series = dplyr::if_else(Substance == 'CO2' & Sector == 'Deforestation', "EN.GHG.CO2.LU.DF.MT.CE.AR5", # Carbon dioxide (CO2) net fluxes from LULUCF - Deforestation (Mt CO2e)
                                 if_else(Substance == 'CO2' & Sector == 'Other Land', "EN.GHG.CO2.LU.OL.MT.CE.AR5", # Carbon dioxide (CO2) net fluxes from LULUCF - Other Land (Mt CO2e)
                                         if_else(Substance == 'CO2' & Sector == 'Forest Land', "EN.GHG.CO2.LU.FL.MT.CE.AR5", # Carbon dioxide (CO2) net fluxes from LULUCF - Forest Land (Mt CO2e)
                                                 if_else(Substance == 'CO2' & Sector == 'Organic Soil', "EN.GHG.CO2.LU.OS.MT.CE.AR5", # Carbon dioxide (CO2) net fluxes from LULUCF - Organic Soil (Mt CO2e)
                                                         if_else(Substance == 'GWP_100_AR5_GHG ', "Net LULUCF tot add to total",
                                                                 "Not found")
                                                         )
                                                 )
                                         )
                                 ),
         SCALE = 0,
         Data = value) %>%
   select(Time, Country, Series, SCALE, Data)
LULUCF_macroregions_prep

# Create total including LULUCF
GHG_totals_including_LULUCF_prep <- GHG_totals_by_country_prep %>%
  left_join(LULUCF_macroregions_prep %>% filter(Series == "Not found"),
            by = c("Time", "Country", "SCALE")) %>%
  filter(!is.na(Data.y) & !is.na(Data.x)) %>%
  mutate(Data = Data.x + Data.y,
         Series = "EN.GHG.ALL.LU.MT.CE.AR5") %>% # Total greenhouse gas emissions including LULUCF (Mt CO2e))
  select(Time, Country, Series, SCALE, Data)
GHG_totals_including_LULUCF_prep
table(GHG_totals_including_LULUCF_prep$Series)

LULUCF_macroregions_prep <- LULUCF_macroregions_prep %>%
  filter(Series != "Not found") # remove net category from LULUCF file after using the calculate total incl. LULUCF

# Merge all indicators for World
World_combined_prep <- rbind(GHG_totals_by_country_prep,
                             GHG_by_sector_and_country_prep,
                             GHG_by_sector_and_country_totals_prep,
                             GHG_1990_levels_WLD_prep,
                             LULUCF_macroregions_prep,
                             GHG_totals_including_LULUCF_prep)
table(World_combined_prep$Series) # 38 indicators for World

# + EN.GHG.CO2.1990.AR5	Carbon dioxide (CO2) emissions (total) excluding LULUCF (1990 level for computation of change)
# + EN.GHG.CO2.MT.CE.AR5	Carbon dioxide (CO2) emissions (total) excluding LULUCF (Mt CO2e)
# + EN.GHG.CO2.AG.MT.CE.AR5	Carbon dioxide (CO2) emissions from Agriculture (Mt CO2e)
# + EN.GHG.CO2.BU.MT.CE.AR5	Carbon dioxide (CO2) emissions from Building (Energy) (Mt CO2e)
# + EN.GHG.CO2.FE.MT.CE.AR5	Carbon dioxide (CO2) emissions from Fugitive Emissions (Energy) (Mt CO2e)
# + EN.GHG.CO2.IC.MT.CE.AR5	Carbon dioxide (CO2) emissions from Industrial Combustion (Energy) (Mt CO2e)
# + EN.GHG.CO2.IP.MT.CE.AR5	Carbon dioxide (CO2) emissions from Industrial Processes (Mt CO2e)
# + EN.GHG.CO2.PI.MT.CE.AR5	Carbon dioxide (CO2) emissions from Power Industry (Energy) (Mt CO2e)
# + EN.GHG.CO2.TR.MT.CE.AR5	Carbon dioxide (CO2) emissions from Transport (Energy) (Mt CO2e)
# + EN.GHG.CO2.WA.MT.CE.AR5	Carbon dioxide (CO2) emissions from Waste (Mt CO2e)
# + EN.GHG.CO2.LU.DF.MT.CE.AR5	Carbon dioxide (CO2) net fluxes from LULUCF - Deforestation (Mt CO2e)
# + EN.GHG.CO2.LU.FL.MT.CE.AR5	Carbon dioxide (CO2) net fluxes from LULUCF - Forest Land (Mt CO2e)
# + EN.GHG.CO2.LU.OS.MT.CE.AR5	Carbon dioxide (CO2) net fluxes from LULUCF - Organic Soil (Mt CO2e)
# + EN.GHG.CO2.LU.OL.MT.CE.AR5	Carbon dioxide (CO2) net fluxes from LULUCF - Other Land (Mt CO2e)
# + EN.GHG.FGAS.IP.MT.CE.AR5	F-gases emissions from Industrial Processes (Mt CO2e)
# + EN.GHG.CH4.1990.AR5	Methane (CH4) emissions (total) excluding LULUCF (1990 level for computation of change)
# + EN.GHG.CH4.MT.CE.AR5	Methane (CH4) emissions (total) excluding LULUCF (Mt CO2e)
# + EN.GHG.CH4.AG.MT.CE.AR5	Methane (CH4) emissions from Agriculture (Mt CO2e)
# + EN.GHG.CH4.BU.MT.CE.AR5	Methane (CH4) emissions from Building (Energy) (Mt CO2e)
# + EN.GHG.CH4.FE.MT.CE.AR5	Methane (CH4) emissions from Fugitive Emissions (Energy) (Mt CO2e)
# + EN.GHG.CH4.IC.MT.CE.AR5	Methane (CH4) emissions from Industrial Combustion (Energy) (Mt CO2e)
# + EN.GHG.CH4.IP.MT.CE.AR5	Methane (CH4) emissions from Industrial Processes (Mt CO2e)
# + EN.GHG.CH4.PI.MT.CE.AR5	Methane (CH4) emissions from Power Industry (Energy) (Mt CO2e)
# + EN.GHG.CH4.TR.MT.CE.AR5	Methane (CH4) emissions from Transport (Energy) (Mt CO2e)
# + EN.GHG.CH4.WA.MT.CE.AR5	Methane (CH4) emissions from Waste (Mt CO2e)
# + EN.GHG.N2O.1990.AR5	Nitrous oxide (N2O) emissions (total) excluding LULUCF (1990 level for computation of change)
# + EN.GHG.N2O.MT.CE.AR5	Nitrous oxide (N2O) emissions (total) excluding LULUCF (Mt CO2e)
# + EN.GHG.N2O.AG.MT.CE.AR5	Nitrous oxide (N2O) emissions from Agriculture (Mt CO2e)
# + EN.GHG.N2O.BU.MT.CE.AR5	Nitrous oxide (N2O) emissions from Building (Energy) (Mt CO2e)
# + EN.GHG.N2O.FE.MT.CE.AR5	Nitrous oxide (N2O) emissions from Fugitive Emissions (Energy) (Mt CO2e)
# + EN.GHG.N2O.IC.MT.CE.AR5	Nitrous oxide (N2O) emissions from Industrial Combustion (Energy) (Mt CO2e)
# + EN.GHG.N2O.IP.MT.CE.AR5	Nitrous oxide (N2O) emissions from Industrial Processes (Mt CO2e)
# + EN.GHG.N2O.PI.MT.CE.AR5	Nitrous oxide (N2O) emissions from Power Industry (Energy) (Mt CO2e)
# + EN.GHG.N2O.TR.MT.CE.AR5	Nitrous oxide (N2O) emissions from Transport (Energy) (Mt CO2e)
# + EN.GHG.N2O.WA.MT.CE.AR5	Nitrous oxide (N2O) emissions from Waste (Mt CO2e)
# + EN.GHG.ALL.1990.AR5	Total greenhouse gas emissions excluding LULUCF (1990 level for computation of change)
# + EN.GHG.ALL.MT.CE.AR5	Total greenhouse gas emissions excluding LULUCF (Mt CO2e)
# + EN.GHG.ALL.LU.MT.CE.AR5	Total greenhouse gas emissions including LULUCF (Mt CO2e)

#### Export EDGAR data ####
EDGAR_for_export <- EDGAR_combined %>% 
  mutate(SCALE = 0) %>% # SCALE variable needed in DCS, all 0
  select(year, ISO3, Series_code_new, SCALE, value) %>%
  dplyr::rename(Time = year,
                Country = ISO3,
                Series = Series_code_new,
                Data = value) %>%
  mutate(Time = paste0("YR", Time))

EDGAR_for_export <- rbind(EDGAR_for_export,
                          World_combined_prep %>% filter(Series %in% EDGAR_for_export$Series),
                          World_combined_prep %>% filter(Series == "EN.GHG.ALL.LU.MT.CE.AR5"))



#View(EDGAR_for_export %>% filter(Country == "NLD" & Time == "YR2022"))

# Write complete file as csv
getwd()
write.csv(EDGAR_for_export, paste0("./Data/", format(Sys.Date(), "%Y%m%d"), "GHG_data_for_DCS_combined.csv"))

# Write XSLX file with one sheet per indicator
list_of_indicators <- unique(EDGAR_for_export$Series)
list_of_indicators

wb2 <- createWorkbook()
for(i in 1:length(list_of_indicators)){
  addWorksheet(wb2, 
               list_of_indicators[i])
  writeData(wb2, 
            list_of_indicators[i], 
            EDGAR_for_export %>% 
              filter(Series == list_of_indicators[i]))
}

saveWorkbook(wb2, file = paste0("./Data/", 
                               format(Sys.Date(), "%Y%m%d"), 
                               "GHG_data_for_DCS_by_sheet.xlsx"), 
             overwrite = FALSE)

##### Export GRASSI data #####
## Prepare files for upload in DCS
# Need columns: Time	Country	Series	SCALE	Data
# E.g. YR1960	AFG	SP.POP.TOTL	0	8996967

# Check if all lines labelled (gas and sector)
table(GRASSI_long$`LAND CATEGORY`, useNA = "always") # 5 indicators

GRASSI_long <- GRASSI_long %>%
  mutate(value = round(value, 4))

table(is.na(GRASSI_long$Series))
dim(GRASSI_long)

GRASSI_for_export <- GRASSI_long %>% 
  mutate(SCALE = 0) %>% # SCALE variable needed in DCS, all 0
  select(year, ISO3, Series, SCALE, value) %>%
  dplyr::rename(Time = year,
                Country = ISO3,
                Series = Series,
                Data = value) %>%
  mutate(Time = paste0("YR", Time)) 
head(GRASSI_for_export)

World_combined_prep %>% filter(Series %in% GRASSI_for_export$Series)

GRASSI_for_export <- rbind(GRASSI_for_export,
                          World_combined_prep %>% filter(Series %in% GRASSI_for_export$Series))


#View(EDGAR_for_export %>% filter(Country == "NLD" & Time == "YR2022"))

# Write complete file as csv
getwd()
write.csv(GRASSI_for_export, paste0("./Data/", format(Sys.Date(), "%Y%m%d"), "GHG_LULUCF_data_for_DCS_combined.csv"))

# Write XSLX file with one sheet per indicator
list_of_indicators_LULUCF <- unique(GRASSI_for_export$Series)
list_of_indicators_LULUCF

wb1 <- createWorkbook()
for(i in 1:length(list_of_indicators_LULUCF)){
  addWorksheet(wb1, 
               list_of_indicators_LULUCF[i])
  writeData(wb1, 
            list_of_indicators_LULUCF[i], 
            GRASSI_for_export %>% 
              filter(Series == list_of_indicators_LULUCF[i]))
}

saveWorkbook(wb1, file = paste0("./Data/", 
                                format(Sys.Date(), "%Y%m%d"), 
                                "GHG_LULUCF_data_for_DCS_by_sheet.xlsx"), overwrite = FALSE)

#### Compare CSC data from Jichong with our results ####
# See dashboard with CSC data: https://public.tableau.com/app/profile/david.groves/viz/WBGCSC-HistoricalGHGEmissions-v2d/GHGEmissions
# read spreadsheet
CSC_data <- as.data.frame(read_xlsx("CSC-GHG_emissions-LATEST.xlsx", 
                                    sheet = "C total subs & gas (V8+Grassi)"))
dim(CSC_data)
CSC_data <- 
  CSC_data %>% select(c(Code, `CSC Sector`, `CSC Subsector`, Gas, as.character(2000:2022))) %>% # select columns
  tidyr::pivot_longer(cols = as.character(2000:2022), # to long format
               names_to = "year") %>% 
    mutate(sector_WDI = case_when(
      `CSC Sector` == "Agriculture" ~ "AG",  
      `CSC Subsector` == "EN - Building" ~ "BU",   
      `CSC Subsector` == "EN - Fugitive Emissions" ~ "FE",    
      `CSC Subsector` == "EN - Industrial Combustion" ~ "IC",   
      `CSC Sector` == "Industrial Processes" ~ "IP",    
      `CSC Subsector` == "EN - Power Industry" ~ "PI",   
      `CSC Subsector` == "EN - Transport" ~ "TR",    
      `CSC Sector` == "Waste" ~ "WA",
      `CSC Sector` == "Land Use, Land Use Change, and Forestry" ~ "LU")) %>%
    select(c(Code, Gas, year, value, sector_WDI)) %>% 
  group_by(Code, Gas, year, sector_WDI) %>%
  summarise(value = sum(value)) %>%
  # add all rows with same code - gas - year - sector_WDI combination
  mutate(year = as.integer(year)) %>%
  mutate(value = round(value, 4))
dim(CSC_data)
compare_CSC_WDI <- EDGAR_combined %>% left_join(CSC_data,
                                                by = c('ISO3'='Code', 
                                                       'year'='year',
                                                       'GHG' = 'Gas',
                                                       'sector_WDI' = 'sector_WDI'))
dim(compare_CSC_WDI)
compare_CSC_WDI <- compare_CSC_WDI %>% filter(year >= 2000) %>%
  filter(sector_WDI != "ALL")
compare_CSC_WDI <- compare_CSC_WDI %>% mutate(x_div_y = value.x / round(value.y, 4))
summary(compare_CSC_WDI$x_div_y)
test <- compare_CSC_WDI %>% filter((x_div_y > 1.001 | x_div_y < 0.999) & !is.na(x_div_y))
table(test$Series_code_new)

#View(test %>% filter(Series_code_new == "EN.GHG.CH4.FE.MT.CE.AR5"))
test %>% filter(Series_code_new == "EN.GHG.CH4.FE.MT.CE.AR5" & ISO3 == "NLD")

tail(CSC_data %>% filter(sector_WDI == "FE" & Code == "NLD" & Gas == "CH4"))

#write.csv(EDGAR_long, "./Data_private/EDGAR/EDGAR_long_prep.csv")

# View(tidyr::spread(EDGAR_long, key = GHG, value = value) %>%
#        arrange(ISO3) %>%
#        filter(ISO3 == "NLD"))

##### Recreate data structure as created by climate group / Jichong #####


##### Comparison with previous year data (2024 vs. 2023) ####
EDGAR_for_export_previous_year <- read.csv("./Data/20241016GHG_data_for_DCS_combined.csv")
head(EDGAR_for_export_previous_year)
EDGAR_for_export_previous_year <- EDGAR_for_export_previous_year %>% 
  select(-X) %>%
  rename(Data_2023 = Data)

EDGAR_for_export_previous_year <- EDGAR_for_export_previous_year[, .(Data_2023 = Data)]

EDGAR_for_export_combined <- as.data.frame(EDGAR_for_export) %>%
  rename(Data_2024 = Data)

head(EDGAR_for_export_combined)
head(EDGAR_for_export_previous_year)

dim(EDGAR_for_export_combined)
dim(EDGAR_for_export_previous_year)

EDGAR_for_export_combined <- EDGAR_for_export_combined %>% left_join(EDGAR_for_export_previous_year)

EDGAR_for_export_combined <- EDGAR_for_export_combined %>% 
  mutate(dif_24_23 = Data_2024 - Data_2023) %>% 
  mutate(reldif_24_23 = round(100 * dif_24_23 / Data_2023, 3))

table(EDGAR_for_export_combined$Series,
      EDGAR_for_export_combined$dif_24_23 == 0) # about half the values are identical

# change in total
hist(EDGAR_for_export_combined[EDGAR_for_export_combined$Series == "EN.GHG.ALL.MT.CE.AR5", "reldif_24_23"],
     bin = 100)

ggplot(EDGAR_for_export_combined %>% 
         filter(Series == "EN.GHG.ALL.MT.CE.AR5" & Time == "YR2022" & Country == "NLD")
       %>% select(reldif_24_23), aes(x=reldif_24_23)) + 
  geom_histogram(binwidth = 1)

##### Grassi LULUCF data
# Downloaded from https://zenodo.org/records/7650360


