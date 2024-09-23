# Title: 20230905_prepare_EDGAR_data_for_WDI.r
# Description: Script to prepare emissions data from PIK for ingestion in WDI
# Date: 5/20/2023
# Updated: 9/5/2024
# Author: Thijs Benschop

rm(list=ls())

#### Load libraries and data ####
library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)
library(readxl)
library(openxlsx)
setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")
rm(list = ls())

# Version 8.0 of EDGAR data // Published November 2023
# Data downloaded from https://edgar.jrc.ec.europa.eu/dataset_ghg80
# One dataset per GHG + total

folders <- list.dirs("./Data_private/EDGAR/") # list unzipped folders
folders <- folders[-1] # remove root folder
file_names <- basename(folders) 
file_names[which(file_names == "EDGAR_AR5_GHG_1970_2022b")] <- "EDGAR_AR5_GHG_1970_2022" # remove suffix "b" from filename

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
EDGAR_long <- rbind(#get(file_names[1]), # total GHG AR4, don't use, use AR5
                    get(file_names[2]), # total GHG AR5
                    get(file_names[3]), # CH4
                    get(file_names[4]), # CO2 bio
                    #get(file_names[5]), # F-gases, don't use gas-specific, use AR5
                    get(file_names[6]), # F gases AR5
                    get(file_names[7]), # N2O
                    get(file_names[8])) # CO2 fossil 

dim(EDGAR_long)
colnames(EDGAR_long)
#View(EDGAR_AR5_GHG_1970_2022)

# Rename columns
colnames(EDGAR_long)
setnames(EDGAR_long, "ipcc_code_2006_for_standard_report", "category")
setnames(EDGAR_long, "Country_code_A3", "ISO3")
colnames(EDGAR_long)

#### Load Grassi LULUCF data
GRASSI <- as.data.table(read_xlsx("./Data_private/Grassi/National inventories LULUCF data 2000-2020 (Dec 2022).xlsx",
          sheet = "Table 5",
          skip = 3,
          n_max = 195))
colnames(GRASSI)
GRASSI[, "Av.. 2000-2020" := NULL] # drop 2000-2020
GRASSI[, c("LAND CATEGORY", "gap-filling", "UNFCCC country", "Unit") := NULL] # drop 2000-2020

# Convert data in long format
GRASSI_long <- melt(GRASSI, id.vars = c("country code"),
                      variable.name = "year",
                      value.factor = FALSE, 
                    variable.factor = FALSE)
GRASSI_long[, c("ISO3",   "year",   "Substance",    "category", "value", "fossil_bio") :=
              .(`country code`, year, "LULUCF", 0, value, "bio")]
GRASSI_long[, `country code` := NULL]
setcolorder(GRASSI_long, c("ISO3", "category", "Substance", "fossil_bio", "year", "value"))

#EDGAR_long <- rbind(EDGAR_long, GRASSI_long)

################################################################################
# Check which WDI countries available
# Load country WDI list (217 countries)
WDI_countries <- as.data.table(read.csv("Data/wdi_country_list.csv"))
setnames(WDI_countries, c("long_name", "ISO3"))

EDGAR_countries <- unique(EDGAR_long$ISO3)
length(EDGAR_countries) # 223 countries
table(WDI_countries$ISO3 %in% EDGAR_countries) # 203 WDI countries available in EDGAR
WDI_countries[!(ISO3 %in% EDGAR_countries)] # list of missing countries (not in EDGAR data)
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
# REU: Réunion
# SCG: Serbia and Montenegro (no longer in use; now Serbia and Montenegro are separate countries with codes SRB and MNE respectively)
# SEA: ?
# SHN: Saint Helena, Ascension and Tristan da Cunha
# SPM: Saint Pierre and Miquelon
# TKL: Tokelau
# TWN: Taiwan, Province of China
# WLF: Wallis and Futuna

GRASSI_countries <- unique(GRASSI_long$ISO3)
length(GRASSI_countries) # 223 countries
table(WDI_countries$ISO3 %in% GRASSI_countries) # 203 WDI countries available in EDGAR
WDI_countries[!(ISO3 %in% GRASSI_countries)] # list of missing countries (not in EDGAR data)
# are these countries/territories included in other countries? -> need to combine countries?
table(GRASSI_countries %in% WDI_countries$ISO3)
GRASSI_countries[!(GRASSI_countries %in% WDI_countries$ISO3)] # list of missing countries (not in WDI list)

# Drop countries/regions not in WDI
EDGAR_long <- EDGAR_long[ISO3 %in% WDI_countries$ISO3]

#PIK_long[, value := value / 1000] # convert to Mt from gigagram (gigagram = 10^9 g = 10^6 kg = 10^3 t = kt)

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

# Create var for gas
EDGAR_long[, GHG := Substance]
EDGAR_long[Substance %in% c("GWP_100_AR5_HCFC", 
                            "GWP_100_AR5_HFC", 
                            "GWP_100_AR5_NF3",
                            "GWP_100_AR5_PFC",
                            "GWP_100_AR5_SF6"), GHG := "FGAS"]
EDGAR_long[Substance %in% c("CO2", 
                            "CO2bio"), GHG := "CO2"]

# Drop CO2bio (already included in CO2)
EDGAR_long <- EDGAR_long[-which(Substance == "CO2bio"), ]

# Group gases
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

EDGAR_long[GHG == "CH4", value := value * 28]
EDGAR_long[GHG == "N2O", value := value * 265]

#### Convert to Mt from kt and round ####
EDGAR_long <- EDGAR_long %>% mutate(value = round(value / 1000, 4))

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
  (GHG == "CH4" & category %in% c("1.A.1.bc", "1.B.1", "1.B.2")) ~ "FE",
  (GHG == "CH4" & category %in% c("1.A.2")) ~ "IC",
  (GHG == "CH4" & category %in% c("1.A.3.a", "1.A.3.b_noRES", "1.A.3.c", "1.A.3.d", "1.A.3.e")) ~ "TR",
  (GHG == "CH4" & category %in% c("2.B", "2.C")) ~ "IP",
  (GHG == "CH4" & category %in% c("4.A", "4.B", "4.C", "4.D")) ~ "WA",
  (GHG == "CO2" & category %in% c("3.C.2", "3.C.3")) ~ "AG",
  (GHG == "CO2" & category %in% c("1.A.4", "1.A.5")) ~ "BU",
  (GHG == "CO2" & category %in% c("1.A.1.a")) ~ "PI",
  (GHG == "CO2" & category %in% c("1.A.1.bc", "1.B.1", "1.B.2")) ~ "FE",
  (GHG == "CO2" & category %in% c("1.A.2")) ~ "IC",
  (GHG == "CO2" & category %in% c("1.A.3.a", "1.A.3.b_noRES", "1.A.3.c", "1.A.3.d", "1.A.3.e")) ~ "TR",
  (GHG == "CO2" & category %in% c("2.A.1", "2.A.2", "2.A.3", "2.A.4", "2.B", "2.C", "2.D")) ~ "IP",
  (GHG == "CO2" & category %in% c("4.C", "3.A.2")) ~ "WA",
  (GHG == "N2O" & category %in% c("3.A.2", "3.C.1", "3.C.4", "3.C.5", "3.C.6")) ~ "AG",
  (GHG == "N2O" & category %in% c("1.A.4", "1.A.5")) ~ "BU",
  (GHG == "N2O" & category %in% c("1.A.1.a")) ~ "PI",
  (GHG == "N2O" & category %in% c("1.A.1.bc", "1.B.1", "1.B.2")) ~ "FE",
  (GHG == "N2O" & category %in% c("1.A.2")) ~ "IC",
  (GHG == "N2O" & category %in% c("1.A.3.a", "1.A.3.b_noRES", "1.A.3.c", "1.A.3.d", "1.A.3.e")) ~ "TR",
  (GHG == "N2O" & category %in% c("2.B", "2.G", "5.A")) ~ "IP",
  (GHG == "N2O" & category %in% c("4.B", "4.C", "4.D")) ~ "WA",
  (GHG == "FGAS" & category %in% c("2.B", "2.C", "2.E", "2.F", "2.G")) ~ "IP",
  GHG == "GWP_100_AR5_GHG" ~ "ALL")) # all GHG together

table(EDGAR_long$sector_WDI, useNA = "always") # any non-classified lines?

unclassified <- EDGAR_long %>% filter(is.na(sector_WDI)) # check unclassified lines
table(unclassified$category, unclassified$GHG) # view GHG/category pairs that aren't classified
#View(unclassified)
# added "1.A.3.e" (off-road, all), "2.A.2" (CO2), "2.D" (CO2), "3.A.2" (N2O), "3.C.7" (CH4), "5.B" (all)
EDGAR_long <- EDGAR_long %>% filter(!is.na(sector_WDI)) # need to classify there!!!!

# Group by sector
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
EDGAR_long_tot <- EDGAR_long_tot %>% filter(!(GHG %in% c("FGAS", "GWP_100_AR5_GHG"))) # total over all GHG already in databse

head(EDGAR_long)
head(EDGAR_long_tot)

table(EDGAR_long$sector_WDI, EDGAR_long$GHG)
table(EDGAR_long_tot$sector_WDI, EDGAR_long_tot$GHG)

EDGAR_combined <- rbind(EDGAR_long, EDGAR_long_tot)

##### Export data #####
## Prepare files for upload in DCS
# Need columns: Time	Country	Series	SCALE	Data
# E.g. YR1960	AFG	SP.POP.TOTL	0	8996967

# Check if all lines labelled (gas and sector)
table(EDGAR_combined$GHG, EDGAR_combined$sector_WDI, useNA = "always") # 29 combinations

# Read mapping gas/sector -> indicator_code WDI
mapping_to_indicator_code <- as.data.frame(read_xlsx("compare_CSC_EDGAR_NLD_2020.xlsx", sheet = "mapping_to_indicator_code"))

EDGAR_combined <- EDGAR_combined %>% left_join(mapping_to_indicator_code, 
                           by = c('GHG'='Specific_subject', 
                                  'sector_WDI'='Ext_1'))

count(is.na(EDGAR_combined$Series_code_new))
table(EDGAR_combined$Series_code_new)
dim(EDGAR_combined)
EDGAR_combined

EDGAR_for_export <- EDGAR_combined %>% 
  mutate(SCALE = 0) %>% # SCALE variable needed in DCS, all 0
  select(year, ISO3, Series_code_new, SCALE, value) %>%
  dplyr::rename(Time = year,
                Country = ISO3,
                Series = Series_code_new,
                Data = value) %>%
  mutate(Time = paste0("YR", Time))

#View(EDGAR_for_export %>% filter(Country == "NLD" & Time == "YR2022"))

# Write complete file as csv
getwd()
write.csv(EDGAR_for_export, paste0("./Data/", format(Sys.Date(), "%Y%m%d"), "GHG_data_for_DCS_combined.csv"))

# Write XSLX file with one sheet per indicator
list_of_indicators <- unique(EDGAR_for_export$Series)
list_of_indicators

wb <- createWorkbook()
for(i in 1:length(list_of_indicators)){
  addWorksheet(wb, 
               list_of_indicators[i])
  writeData(wb, 
            list_of_indicators[i], 
            EDGAR_for_export %>% 
              filter(Series == list_of_indicators[i]))
}

saveWorkbook(wb, file = paste0("./Data/", 
                               format(Sys.Date(), "%Y%m%d"), 
                               "GHG_data_for_DCS_by_sheet.xlsx"), overwrite = FALSE)


#write.csv(EDGAR_long, "./Data_private/EDGAR/EDGAR_long_prep.csv")

# View(tidyr::spread(EDGAR_long, key = GHG, value = value) %>%
#        arrange(ISO3) %>%
#        filter(ISO3 == "NLD"))

##### Grassi LULUCF data
# Downloaded from https://zenodo.org/records/7650360


################################################################################
table(EDGAR_long$GHG)
# sectors:
# 0 - all
# 1 - 
# 2 - 
# 3 - 
# 4 - 
# 5 - 

# Calculate proposed indicators

# divide by total EN.GHG.TOTL.KT.CE

# Save file for each new indicator
list_indicators <- ls()
list_indicators <- list_indicators[which(substring(list_indicators,1,3) == "EN.")]

for(cur_name in list_indicators){
  print(cur_name)
  cur_data <- get(cur_name)
  write.csv(cur_data, paste0("./New_data_for_WDI_from_EDGAR/", cur_name, ".csv"))
}

# Create one file for all indicators

# 
# #### old indicator selection/calculation #####
# # 1	EN.GHG.TOTL.KT.CE	Greenhouse gas emissions: All Kyoto Gases (Total excluding LULUCF)	KYOTOGHG (AR5GWP100)	National Total excluding LULUCF	absolute emissions
# EN.GHG.TOTL.KT.CE <- EDGAR_long[sector == 0 & GHG == "GWP_100_AR5_GHG", 
#                                 .(ISO3, year, value)]
# 
# # 2	EN.GHG.CO2E.KT	Greenhouse gas emissions: Carbon Dioxide (CO2) (Total excluding LULUCF)	CO2	National Total excluding LULUCF	absolute emissions
# EN.GHG.CO2E.KT <- EDGAR_long[sector == 0 & GHG == "CO2", 
#                              .(ISO3, year, value)]
# 
# # 3	EN.GHG.METH.KT.CE	Greenhouse gas emissions: Methane (CH4) (Total excluding LULUCF)	CH4	National Total excluding LULUCF	absolute emissions
# EN.GHG.METH.KT.CE <- EDGAR_long[sector == 0 & GHG == "CH4", 
#                                 .(ISO3, year, value)]
# 
# # 4	EN.GHG.NOXE.KT.CE	Greenhouse gas emissions: Nitrous Oxide (N2O) (Total excluding LULUCF)	N2O	National Total excluding LULUCF	absolute emissions
# EN.GHG.NOXE.KT.CE <- EDGAR_long[sector == 0 & GHG == "N2O", 
#                                 .(ISO3, year, value)]
# 
# # 5	EN.GHG.FGAS.KT.CE	Greenhouse gas emissions: Fluorinated Gases (Total excluding LULUCF)	FGASES (AR5GWP100)	National Total excluding LULUCF	absolute emissions
# EN.GHG.FGAS.KT.CE <- EDGAR_long[sector == 0 & GHG == "FGAS ", 
#                                 .(ISO3, year, value)]
# 
# # 6	EN.GHG.TOTL.AG.KT.CE	Greenhouse gas emissions: All Kyoto Gases (Sector = Agriculture)	KYOTOGHG (AR5GWP100)	Agriculture	absolute emissions
# EN.GHG.TOTL.AG.KT.CE <- EDGAR_long[sector == 3 & GHG == "GWP_100_AR5_GHG", 
#                                    .(ISO3, year, value)]
# 
# # 7	EN.GHG.TOTL.EG.KT.CE	Greenhouse gas emissions: All Kyoto Gases (Sector = Energy)	KYOTOGHG (AR5GWP100)	Energy	absolute emissions
# EN.GHG.TOTL.EG.KT.CE <- EDGAR_long[sector == 1 & GHG == "GWP_100_AR5_GHG", 
#                                    .(ISO3, year, value)]
# 
# # 8	EN.GHG.TOTL.IN.KT.CE	Greenhouse gas emissions: All Kyoto Gases (Sector = Industrial Processes and Product Use)	KYOTOGHG (AR5GWP100)	Industrial Processes and Product Use	absolute emissions
# EN.GHG.TOTL.IN.KT.CE <- EDGAR_long[sector == 2 & GHG == "GWP_100_AR5_GHG", 
#                                    .(ISO3, year, value)]
# 
# # 9	EN.GHG.TOTL.OT.KT.CE	Greenhouse gas emissions: All Kyoto Gases (Sector = Other)	KYOTOGHG (AR5GWP100)	Other	absolute emissions
# EN.GHG.TOTL.OT.KT.CE <- EDGAR_long[sector == 5 & GHG == "GWP_100_AR5_GHG", 
#                                    .(ISO3, year, value)]
# 
# # 10	EN.GHG.TOTL.WA.KT.CE	Greenhouse gas emissions: All Kyoto Gases (Sector = Waste)	KYOTOGHG (AR5GWP100)	Waste	absolute emissions
# EN.GHG.TOTL.WA.KT.CE <- EDGAR_long[sector == 4 & GHG == "GWP_100_AR5_GHG", 
#                                    .(ISO3, year, value)]
# 
# # 11	EN.GHG.CO2E.AG.KT	Greenhouse gas emissions: Carbon Dioxide (CO2) (Sector = Agriculture)	CO2	Agriculture	absolute emissions
# EN.GHG.CO2E.AG.KT <- EDGAR_long[sector == 3 & GHG == "CO2", 
#                                 .(ISO3, year, value)]
# 
# # 12	EN.GHG.CO2E.EG.KT	Greenhouse gas emissions: Carbon Dioxide (CO2) (Sector = Energy)	CO2	Energy	absolute emissions
# EN.GHG.CO2E.EG.KT <- EDGAR_long[sector == 1 & GHG == "CO2", 
#                                 .(ISO3, year, value)]
# 
# # 13	EN.GHG.CO2E.IN.KT	Greenhouse gas emissions: Carbon Dioxide (CO2) (Sector = Industrial Processes and Product Use)	CO2	Industrial Processes and Product Use	absolute emissions
# EN.GHG.CO2E.IN.KT <- EDGAR_long[sector == 2 & GHG == "CO2", 
#                                 .(ISO3, year, value)]
# 
# # 14	EN.GHG.CO2E.OT.KT	Greenhouse gas emissions: Carbon Dioxide (CO2) (Sector = Other)	CO2	Other	absolute emissions
# EN.GHG.CO2E.OT.KT <- EDGAR_long[sector == 5 & GHG == "CO2", 
#                                 .(ISO3, year, value)]
# 
# # 15	EN.GHG.CO2E.WA.KT	Greenhouse gas emissions: Carbon Dioxide (CO2) (Sector = Waste)	CO2	Waste	absolute emissions
# EN.GHG.CO2E.WA.KT <- EDGAR_long[sector == 4 & GHG == "CO2", 
#                                 .(ISO3, year, value)]
# 
# # 16	EN.GHG.METH.AG.KT.CE	Greenhouse gas emissions: Methane (CH4) (Sector = Agriculture)	CH4	Agriculture	absolute emissions
# EN.GHG.METH.AG.KT.CE <- EDGAR_long[sector == 3 & GHG == "CH4", 
#                                    .(ISO3, year, value)]
# 
# # 17	EN.GHG.METH.EG.KT.CE	Greenhouse gas emissions: Methane (CH4) (Sector = Energy)	CH4	Energy	absolute emissions
# EN.GHG.METH.EG.KT.CE <- EDGAR_long[sector == 1 & GHG == "CH4", 
#                                    .(ISO3, year, value)]
# 
# # 18	EN.GHG.METH.IN.KT.CE	Greenhouse gas emissions: Methane (CH4) (Sector = Industrial Processes and Product Use)	CH4	Industrial Processes and Product Use	absolute emissions
# EN.GHG.METH.IN.KT.CE <- EDGAR_long[sector == 2 & GHG == "CH4", 
#                                    .(ISO3, year, value)]
# 
# # 19	EN.GHG.METH.OT.KT.CE	Greenhouse gas emissions: Methane (CH4) (Sector = Other)	CH4	Other	absolute emissions
# EN.GHG.METH.OT.KT.CE <- EDGAR_long[sector == 5 & GHG == "CH4", 
#                                    .(ISO3, year, value)]
# 
# # 20	EN.GHG.METH.WA.KT.CE	Greenhouse gas emissions: Methane (CH4) (Sector = Waste)	CH4	Waste	absolute emissions
# EN.GHG.METH.WA.KT.CE <- EDGAR_long[sector == 4 & GHG == "CH4", 
#                                    .(ISO3, year, value)]
# 
# # 21	EN.GHG.NOXE.AG.KT.CE	Greenhouse gas emissions: Nitrous Oxide (N2O) (Sector = Agriculture)	N2O	Agriculture	absolute emissions
# EN.GHG.NOXE.AG.KT.CE <- EDGAR_long[sector == 3 & GHG == "N2O", 
#                                    .(ISO3, year, value)]
# 
# # 22	EN.GHG.NOXE.EG.KT.CE	Greenhouse gas emissions: Nitrous Oxide (N2O) (Sector = Energy)	N2O	Energy	absolute emissions
# EN.GHG.NOXE.EG.KT.CE <- EDGAR_long[sector == 1 & GHG == "N2O", 
#                                    .(ISO3, year, value)]
# 
# # 23	EN.GHG.NOXE.IN.KT.CE	Greenhouse gas emissions: Nitrous Oxide (N2O) (Sector = Industrial Processes and Product Use)	N2O	Industrial Processes and Product Use	absolute emissions
# EN.GHG.NOXE.IN.KT.CE <- EDGAR_long[sector == 2 & GHG == "N2O", 
#                                    .(ISO3, year, value)]
# 
# # 24	EN.GHG.NOXE.OT.KT.CE	Greenhouse gas emissions: Nitrous Oxide (N2O) (Sector = Other)	N2O	Other	absolute emissions
# EN.GHG.NOXE.OT.KT.CE <- EDGAR_long[sector == 5 & GHG == "N2O", 
#                                    .(ISO3, year, value)]
# 
# # 25	EN.GHG.NOXE.WA.KT.CE	Greenhouse gas emissions: Nitrous Oxide (N2O) (Sector = Waste)	N2O	Waste	absolute emissions
# EN.GHG.NOXE.WA.KT.CE <- EDGAR_long[sector == 4 & GHG == "N2O", 
#                                    .(ISO3, year, value)]
# 
# # 26	EN.GHG.FGAS.IN.KT.CE	Greenhouse gas emissions: Fluorinated Gases (Sector = Industrial Processes and Product Use)	FGASES (AR5GWP100)	Industrial Processes and Product Use	absolute emissions
# EN.GHG.FGAS.IN.KT.CE <- EDGAR_long[sector == 2 & GHG == "FGAS ", 
#                                    .(ISO3, year, value)]
# 
# # 27	EN.GHG.CO2E.LU.KT.CE	Greenhouse gas emissions: Carbon Dioxide (CO2) (Sector = LULUCF)	CO2	Land Use, Land Use Change, and Forestry	absolute emissions
# EN.GHG.CO2E.LU.KT.CE <- GRASSI_long[, .(ISO3, year, value)]
# 
# # 28	EN.GHG.METH.LU.KT.CE	Greenhouse gas emissions: Methane (CH4) (Sector = LULUCF)	CH4	Land Use, Land Use Change, and Forestry	absolute emissions
# # 29	EN.GHG.NOXE.LU.KT.CE	Greenhouse gas emissions: Nitrous Oxide (N2O) (Sector = LULUCF)	N2O	Land Use, Land Use Change, and Forestry	absolute emissions
# 
# # 30	EN.GHG.TOTL.PC	Greenhouse gas emissions: All Kyoto Gases (Total excluding LULUCF) per capita	KYOTOGHG (AR5GWP100)	National Total excluding LULUCF	per capita
# EN.GHG.TOTL.PC <- EDGAR_long[sector == 0 & GHG == "GWP_100_AR5_GHG", 
#                              .(ISO3, year, value)] 
# # add population data in DCS?
# 
# # 31	EN.GHG.CO2E.PC	Greenhouse gas emissions: Carbon Dioxide (CO2) (Total excluding LULUCF) per capita	CO2	National Total excluding LULUCF	per capita
# EN.GHG.CO2E.PC <- EDGAR_long[sector == 0 & GHG == "CO2", 
#                              .(ISO3, year, value)]
# # add population data in DCS?
# 
# # 32	EN.GHG.TOTL.PP.GD	Greenhouse gas emissions: All Kyoto Gases (Total excluding LULUCF) per 2017 PPP $ of GDP	KYOTOGHG (AR5GWP100)	National Total excluding LULUCF	per GDP
# EN.GHG.TOTL.PP.GD <- EDGAR_long[sector == 0 & GHG == "GWP_100_AR5_GHG", 
#                                 .(ISO3, year, value)]
# # add GDP data in DCS?
# 
# # 33	EN.GHG.CO2E.PP.GD	Greenhouse gas emissions: Carbon Dioxide (CO2) (Total excluding LULUCF) per 2017 PPP $ of GDP	CO2	National Total excluding LULUCF	per GDP
# EN.GHG.CO2E.PP.GD <- EDGAR_long[sector == 0 & GHG == "CO2", 
#                                 .(ISO3, year, value)]
# # add GDP data in DCS?
# 
# # 36	EN.GHG.TOTL.ZG	Greenhouse gas emissions: All Kyoto Gases (Total excluding LULUCF) % change from 1990	KYOTOGHG (AR5GWP100)	National Total excluding LULUCF	% change
# EN.GHG.TOTL.KT.CE_1990 <- EN.GHG.TOTL.KT.CE %>% filter(year == 1990)
# EN.GHG.TOTL.ZG <- merge(EN.GHG.TOTL.KT.CE, EN.GHG.TOTL.KT.CE_1990, 
#                         by = "ISO3",
#                         all.x = T)
# EN.GHG.TOTL.ZG <- EN.GHG.TOTL.ZG[, value := round(100 * (value.x - value.y)/value.y, digits = 2)] %>%
#   select(ISO3, year.x, value) %>%
#   filter(year.x > 1990) %>%
#   rename(year = year.x)
# rm(EN.GHG.TOTL.KT.CE_1990)
# 
# # 37	EN.GHG.CO2E.ZG	Greenhouse gas emissions: Carbon Dioxide (CO2) (Total excluding LULUCF) % change from 1990	CO2	National Total excluding LULUCF	% change
# EN.GHG.CO2E.KT.CE_1990 <- EN.GHG.CO2E.KT %>% filter(year == 1990)
# EN.GHG.CO2E.ZG <- merge(EN.GHG.TOTL.KT.CE, EN.GHG.CO2E.KT.CE_1990, 
#                         by = "ISO3",
#                         all.x = T)
# EN.GHG.CO2E.ZG <- EN.GHG.CO2E.ZG[, value := round(100 * (value.x - value.y)/value.y, digits = 2)] %>%
#   select(ISO3, year.x, value) %>%
#   filter(year.x > 1990) %>%
#   rename(year = year.x)
# rm(EN.GHG.CO2E.KT.CE_1990)
# 
# # 38	EN.GHG.METH.ZG	Greenhouse gas emissions: Methane (CH4) (Total excluding LULUCF) % change from 1990	CH4	National Total excluding LULUCF	% change
# EN.GHG.METH.KT.CE_1990 <- EN.GHG.METH.KT.CE %>% filter(year == 1990)
# EN.GHG.METH.ZG <- merge(EN.GHG.TOTL.KT.CE, EN.GHG.METH.KT.CE_1990, 
#                         by = "ISO3",
#                         all.x = T)
# EN.GHG.METH.ZG <- EN.GHG.METH.ZG[, value := round(100 * (value.x - value.y)/value.y, digits = 2)] %>%
#   select(ISO3, year.x, value) %>%
#   filter(year.x > 1990) %>%
#   rename(year = year.x)
# rm(EN.GHG.METH.KT.CE_1990)
# 
# # 39	EN.GHG.N2OX.ZG	Greenhouse gas emissions: Nitrous Oxide (N2O) (Total excluding LULUCF) % change from 1990	N2O	National Total excluding LULUCF	% change
# EN.GHG.NOXE.KT.CE_1990 <- EN.GHG.NOXE.KT.CE %>% filter(year == 1990)
# EN.GHG.N2OX.ZG <- merge(EN.GHG.TOTL.KT.CE, EN.GHG.NOXE.KT.CE_1990, 
#                         by = "ISO3",
#                         all.x = T)
# EN.GHG.N2OX.ZG <- EN.GHG.N2OX.ZG[, value := round(100 * (value.x - value.y)/value.y, digits = 2)] %>%
#   select(ISO3, year.x, value) %>%
#   filter(year.x > 1990) %>%
#   rename(year = year.x)
# rm(EN.GHG.NOXE.KT.CE_1990)
# 
# # 40	EN.GHG.CO2E.ZS	Greenhouse gas emissions: Carbon Dioxide (CO2) (Total excluding LULUCF) share of total GHG emissions	CO2	National Total excluding LULUCF	share of total
# EN.GHG.CO2E.KT <- EDGAR_long[sector == 0 & GHG == "CO2", 
#                              .(ISO3, year, value)]
# # divide by total EN.GHG.TOTL.KT.CE
# 
# # 41	EN.GHG.METH.ZS	Greenhouse gas emissions: Methane (CH4) (Total excluding LULUCF) share of total GHG emissions	CH4	National Total excluding LULUCF	share of total
# EN.GHG.METH.KT.CE <- EDGAR_long[sector == 0 & GHG == "CH4", 
#                                 .(ISO3, year, value)]
# # divide by total EN.GHG.TOTL.KT.CE
# 
# # 42	EN.GHG.N2OX.ZS	Greenhouse gas emissions: Nitrous Oxide (N2O) (Total excluding LULUCF) share of total GHG emissions	N2O	National Total excluding LULUCF	share of total
# EN.GHG.NOXE.KT.CE <- EDGAR_long[sector == 0 & GHG == "N2O", 
#                                 .(ISO3, year, value)]
# # divide by total EN.GHG.TOTL.KT.CE
# 
# # 43	EN.GHG.FGAS.ZS	Greenhouse gas emissions: Fluorinated Gases (Total excluding LULUCF) share of total GHG emissions	FGASES (AR5GWP100)	National Total excluding LULUCF	share of total
# EN.GHG.FGAS.KT.CE <- EDGAR_long[sector == 0 & GHG == "FGAS ", 
#                                 .(ISO3, year, value)]
# 
