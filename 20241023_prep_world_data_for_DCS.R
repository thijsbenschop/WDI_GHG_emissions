#Read Excel file

library(dplyr)
library(data.table)
library(ggplot2)
library(readxl)
library(openxlsx)
library(tidyr)

setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")
world_data <- as.data.table(read_xlsx("World_values_Emissions_EDGAR.xlsx", sheet = "upload"))

export_data <- world_data %>% 
  pivot_longer(!c(Code, Name), 
               names_to = "year", 
               values_to = "value") %>%
  dplyr::rename(Time = year,
                Series = Code,
                Data = value) %>%
  mutate(Country = "WLD") %>%
  mutate(SCALE = 0) %>% # SCALE variable needed in DCS, all 0
  mutate(Time = paste0("YR", Time)) %>%
  select(Time, Country, Series, SCALE, Data)

export_data
# Time	Country	Series	SCALE	Data

write.xlsx(export_data, "./Data/20241023_GHG_world_values.xlsx")


