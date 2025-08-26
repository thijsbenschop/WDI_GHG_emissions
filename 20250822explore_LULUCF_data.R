## Explore/Analyze LULUCF data from European Forest Observatory
# Data received from EFO on 2024-08-22
# See email from David Groves
# Three versions of the data

setwd("C:/Users/WB460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")

library(readxl)
library(dplyr)
library(data.table)

# Read data from Excel spreadsheet
LULUCF_data <- as.data.table(read_xlsx("Data_private/EFO_LULUCF/20250822timeseries_NGHGI.xlsx"))
head(LULUCF_data)

# Create three columns with data for each version of the data according to the column version
# Which versions available
table(LULUCF_data$Version)

LULUCF_data_v1 <- LULUCF_data %>%
  filter(Version == "V1.0 (NGHGI 2023)") %>%
  select(-Version) %>%
  rename(value_v1 = CFluxes_yr)
head(LULUCF_data_v1)

LULUCF_data_v2 <- LULUCF_data %>%
  filter(Version == "V2.0 (NGHGI 2024)") %>%
  select(-Version) %>%
  rename(value_v2 = CFluxes_yr)
head(LULUCF_data_v2)

LULUCF_data_v3 <- LULUCF_data %>%
  filter(Version == "V3.0 (NGHGI 2025)") %>%
  select(-Version) %>%
  rename(value_v3 = CFluxes_yr)
head(LULUCF_data_v3)

# Which years available in each version
table(LULUCF_data_v1$Year) # 2000-2022
table(LULUCF_data_v2$Year) # 2000-2023
table(LULUCF_data_v3$Year) # 2000-2023

# Which gases available
table(LULUCF_data_v1$Gas) # CO2 only
table(LULUCF_data_v2$Gas) # CO2 only
table(LULUCF_data_v3$Gas) # CO2 only

# Which categories available
table(LULUCF_data_v1$Category) # Various LULUCF categories
table(LULUCF_data_v2$Category) # Various LULUCF categories
table(LULUCF_data_v3$Category) # Various LULUCF categories  

# Merge the three versions into one data table
LULUCF_data_merged <- merge(LULUCF_data_v1, 
                            LULUCF_data_v2, 
                            by = c("ISO3", "Year", "Gas", "Category"), all = TRUE)
LULUCF_data_merged <- merge(LULUCF_data_merged, 
                            LULUCF_data_v3, 
                            by = c("ISO3", "Year", "Gas", "Category"), all = TRUE)
head(LULUCF_data_merged)

# Check differences between versions
LULUCF_data_merged <- LULUCF_data_merged %>%
  mutate(diff_v2_v1 = value_v2 - value_v1,
         diff_v3_v2 = value_v3 - value_v2,
         diff_v3_v1 = value_v3 - value_v1)
head(LULUCF_data_merged)

summary(LULUCF_data_merged$diff_v2_v1)
summary(LULUCF_data_merged$diff_v3_v2)
summary(LULUCF_data_merged$diff_v3_v1)

# Number of values equal to 0
table(LULUCF_data_merged$diff_v2_v1 == 0, useNA = "ifany")
table(LULUCF_data_merged$diff_v3_v2 == 0, useNA = "ifany")
table(LULUCF_data_merged$diff_v3_v1 == 0, useNA = "ifany")

#### Compare V2 and V3 ####
# Focus on V2 and V3 since they have more recent data
# Table diff_v3_v2 == 0 by Year and Category
table(LULUCF_data_merged$Year[LULUCF_data_merged$diff_v3_v2 == 0], 
      LULUCF_data_merged$Category[LULUCF_data_merged$diff_v3_v2 == 0], useNA = "ifany")

# Add relative difference for v3_v2
LULUCF_data_merged <- LULUCF_data_merged %>%
  mutate(rel_diff_v3_v2 = ifelse(is.na(value_v2) | value_v2 == 0, NA, diff_v3_v2 / abs(value_v2))) %>%
  mutate(rel_diff_v3_v2_perc = round(rel_diff_v3_v2 * 100, 2))

# Table diff_v3_v2 == 0 by Country and Year
table(LULUCF_data_merged$ISO3[LULUCF_data_merged$diff_v3_v2 == 0], 
      LULUCF_data_merged$Year[LULUCF_data_merged$diff_v3_v2 == 0], useNA = "ifany")
# Countries with many 0 differences
table(LULUCF_data_merged$ISO3[LULUCF_data_merged$diff_v3_v2 == 0], useNA = "ifany")

# View data
View(LULUCF_data_merged %>% select(ISO3, Year, Category, value_v1, value_v2, value_v3, diff_v3_v2, rel_diff_v3_v2_perc) %>%
       filter(abs(diff_v3_v2) >  0.001) %>%
       arrange(ISO3, Year, Category))

LULUCF_data_merged %>% select(ISO3, Year, Category, value_v2, value_v3, diff_v3_v2, rel_diff_v3_v2_perc) %>%
  filter(abs(diff_v3_v2) >  0.00001) %>%
  arrange(ISO3, Year, Category) %>% select(ISO3) %>% table()

# What is the average absolute relative difference between v2 and v3 for values with a difference larger than 0.001
summary(abs(LULUCF_data_merged$rel_diff_v3_v2_perc[!is.na(LULUCF_data_merged$rel_diff_v3_v2_perc) & abs(LULUCF_data_merged$diff_v3_v2) > 0.001]))

# Show all values with an absolute relative difference larger than 100%
View(LULUCF_data_merged %>% select(ISO3, Year, Category, value_v2, value_v3, diff_v3_v2, rel_diff_v3_v2_perc) %>%
  filter(abs(rel_diff_v3_v2_perc) >  100) %>%
  arrange(ISO3, Year, Category))

#### Compare V1 and V2 ####
# Add relative difference for v2_v1
LULUCF_data_merged <- LULUCF_data_merged %>%
  mutate(rel_diff_v2_v1 = ifelse(is.na(value_v1) | value_v1 == 0, NA, diff_v2_v1 / abs(value_v1))) %>%
  mutate(rel_diff_v2_v1_perc = round(rel_diff_v2_v1 * 100, 2))
# Table diff_v2_v1 == 0 by Year and Category
table(LULUCF_data_merged$Year[LULUCF_data_merged$diff_v2_v1 == 0], 
      LULUCF_data_merged$Category[LULUCF_data_merged$diff_v2_v1 == 0], useNA = "ifany")
# Countries with many 0 differences
table(LULUCF_data_merged$ISO3[LULUCF_data_merged$diff_v2_v1 == 0], useNA = "ifany")

# View data
View(LULUCF_data_merged %>% select(ISO3, Year, Category, value_v1, value_v2, value_v3, diff_v2_v1, rel_diff_v2_v1_perc) %>%
       filter(abs(diff_v2_v1) >  0.001) %>%
       arrange(ISO3, Year, Category))

# What is the average absolute relative difference between v1 and v2 for values with a difference larger than 0.001
summary(abs(LULUCF_data_merged$rel_diff_v2_v1_perc[!is.na(LULUCF_data_merged$rel_diff_v2_v1_perc) & abs(LULUCF_data_merged$diff_v2_v1) > 0.001]))

# Which countries have no absolute difference between v1 and v2 > 0.001
table(LULUCF_data_merged$ISO3[!LULUCF_data_merged$ISO3 %in% 
                               LULUCF_data_merged$ISO3[!is.na(LULUCF_data_merged$diff_v2_v1) & 
                                                         abs(LULUCF_data_merged$diff_v2_v1) > 0.001]])

#### Compare V1 and V3 ####
# Add relative difference for v3_v1
LULUCF_data_merged <- LULUCF_data_merged %>%
  mutate(rel_diff_v3_v1 = ifelse(is.na(value_v1) | value_v1 == 0, NA, diff_v3_v1 / abs(value_v1))) %>%
  mutate(rel_diff_v3_v1_perc = round(rel_diff_v3_v1 * 100, 2))

# Which countries have no absolute difference between v1 and v3 > 0.001
table(LULUCF_data_merged$ISO3[!LULUCF_data_merged$ISO3 %in% 
                               LULUCF_data_merged$ISO3[!is.na(LULUCF_data_merged$diff_v3_v1) & 
                                                         abs(LULUCF_data_merged$diff_v3_v1) > 0.001]])

# Compare values between 2023 and 2022 in LULUCF_data_v2
LULUCF_data_v2_2022 <- LULUCF_data_v2 %>%
  filter(Year == 2022) %>%
  select(-Year) %>%
  rename(value_v2_2022 = value_v2)
head(LULUCF_data_v2_2022)
LULUCF_data_v2_2023 <- LULUCF_data_v2 %>%
  filter(Year == 2023) %>%
  select(-Year) %>%
  rename(value_v2_2023 = value_v2)
head(LULUCF_data_v2_2023)
LULUCF_data_v2_2022_2023 <- merge(LULUCF_data_v2_2022, 
                                 LULUCF_data_v2_2023, 
                                 by = c("ISO3", "Gas", "Category"), all = TRUE)
head(LULUCF_data_v2_2022_2023)
LULUCF_data_v2_2022_2023 <- LULUCF_data_v2_2022_2023 %>%
  mutate(diff_v2_2023_2022 = value_v2_2023 - value_v2_2022,
         rel_diff_v2_2023_2022 = ifelse(is.na(value_v2_2022) | value_v2_2022 == 0, NA, diff_v2_2023_2022 / abs(value_v2_2022)),
         rel_diff_v2_2023_2022_perc = round(rel_diff_v2_2023_2022 * 100, 2))
head(LULUCF_data_v2_2022_2023)
summary(LULUCF_data_v2_2022_2023$diff_v2_2023_2022)
summary(LULUCF_data_v2_2022_2023$rel_diff_v2_2023_2022_perc)
# View data
View(LULUCF_data_v2_2022_2023 %>% select(ISO3, Category, value_v2_2022, value_v2_2023, diff_v2_2023_2022, rel_diff_v2_2023_2022_perc) %>%
       filter(abs(diff_v2_2023_2022) >  0.001) %>%
       arrange(ISO3, Category))
View(LULUCF_data_v2_2022_2023)

# Compare values between 2023 and 2022 in LULUCF_data_v3
LULUCF_data_v3_2022 <- LULUCF_data_v3 %>%
  filter(Year == 2022) %>%
  select(-Year) %>%
  rename(value_v3_2022 = value_v3)
head(LULUCF_data_v3_2022)
LULUCF_data_v3_2023 <- LULUCF_data_v3 %>%
  filter(Year == 2023) %>%
  select(-Year) %>%
  rename(value_v3_2023 = value_v3)
head(LULUCF_data_v3_2023)
LULUCF_data_v3_2022_2023 <- merge(LULUCF_data_v3_2022, 
                                  LULUCF_data_v3_2023, 
                                  by = c("ISO3", "Gas", "Category"), all = TRUE)
head(LULUCF_data_v3_2022_2023)

LULUCF_data_v3_2022_2023 <- LULUCF_data_v3_2022_2023 %>%
  mutate(diff_v3_2023_2022 = value_v3_2023 - value_v3_2022,
         rel_diff_v3_2023_2022 = ifelse(is.na(value_v3_2022) | value_v3_2022 == 0, NA, diff_v3_2023_2022 / abs(value_v3_2022)),
         rel_diff_v3_2023_2022_perc = round(rel_diff_v3_2023_2022 * 100, 2))
head(LULUCF_data_v3_2022_2023)
summary(LULUCF_data_v3_2022_2023$diff_v3_2023_2022)

#Select rows where absolute difference is larger than 0.001
summary(LULUCF_data_v3_2022_2023$rel_diff_v3_2023_2022_perc)
# View data
View(LULUCF_data_v3_2022_2023 %>% select(ISO3, Category, value_v3_2022, value_v3_2023, diff_v3_2023_2022, rel_diff_v3_2023_2022_perc) %>%
       filter(abs(diff_v3_2023_2022) >  0.001) %>%
       arrange(ISO3, Category))
LULUCF_data_v3_2022_2023 %>% select(ISO3, Category, value_v3_2022, value_v3_2023, diff_v3_2023_2022, rel_diff_v3_2023_2022_perc) %>%
  filter(abs(diff_v3_2023_2022) >  0.001) %>%
  arrange(ISO3, Category) %>% select(ISO3) %>% table()
