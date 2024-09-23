################################################################################
## Calculate scores for new GHG emissions indicators
## June 7, 2024
## Thijs Benschop
################################################################################

setwd("C:/Users/WB460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")
# 
# #libraries
# library(tidyverse)
# library(wbstats)
# library(flextable)
# library(here)
# library(DT)
# library(ggbeeswarm)
# library(plotly)
# library(readxl)
# 
# #current year
# cur_year = 2024
# 
# #download the data using wbstats
# #get country metadata
# country_metadata <- wb_countries()
# 
# # Make LMIC country list
# lmics <- unique(country_metadata[which(
#   country_metadata$income_level == 'Low income' |
#     country_metadata$income_level == 'Lower middle income' |
#     country_metadata$income_level == 'Upper middle income'),]$iso3c)
# 
# # Make IBRD country list
# IBRD <- unique(country_metadata[which(
#   country_metadata$lending_type=="IBRD"),]$iso3c)
# 
# #make IDA country list
# IDA <- unique(country_metadata[which(
#   country_metadata$lending_type=="IDA"),]$iso3c)
# 
# #Make IBRD + IDA
# IBRD_IDA <- unique(c(IBRD, IDA))
# 
# # Make a FCS country list
# FCS <- read_excel(here("01_raw_data","CLASS_FY24.xlsx"),
#                   sheet="Groups") %>%
#   filter(GroupCode=="FCS")
# 
# FCS <- unique(FCS$CountryCode)
# 
# ---
#   title: "WDI New Indicator Tool"
# format: 
#   html:
#   embed-resources: true
# execute:
#   echo: false
# warning: false
# ---
 
library(tidyverse)
library(wbstats)
library(flextable)
library(here)
library(DT)
library(ggbeeswarm)
library(plotly)
library(readxl)
library(data.table)

#path
dir <- getwd() #here()

#parameters
end_date=2023

scen <- read.csv("scenarios.csv") # different scenarios
colnames(scen)[1] <- "scenario"

####Load raw indicator data
# Load all datafiles
new_indicators <- list.files("./New_data_for_WDI_from_EDGAR/",
                             pattern = "EN.")

new_indicators_1 <- new_indicators[1]
indic_data <- as.data.table(read.csv(paste0("./New_data_for_WDI_from_EDGAR/", new_indicators_1)))
indic_data[, X:= NULL]
indic_data[, Indicator.Code := substring(new_indicators_1, 1, nchar(new_indicators_1) - 4)]
indic_data

indic_data <- indic_data[-which(is.na(value)),]

for(i in 2:length(new_indicators)){
  print(new_indicators[i])
  cur_indic_data <- as.data.table(read.csv(paste0("./New_data_for_WDI_from_EDGAR/", new_indicators[i])))
  cur_indic_data[, X:= NULL]
  cur_indic_data[, Indicator.Code := substring(new_indicators[i], 1, nchar(new_indicators[i]) - 4)]
  cur_indic_data
  indic_data <- rbind(indic_data, cur_indic_data)
  rm(cur_indic_data)
}

dim(indic_data)
table(indic_data$Indicator.Code)

# Rename variables
indic_data <- rename(indic_data, Country.Code=ISO3)
indic_data <- rename(indic_data, Year=year)

head(indic_data)

indic_data <- indic_data[-which(is.na(value)),]

#### calculate statistics from raw data
edgarc <- indic_data %>%
  group_by(Indicator.Code, Country.Code) %>%
  mutate(percountry_obs = n(),
         percountry_maxyear = max(Year),
         percountry_minyear = min(Year),
         percountry_meanyear = mean(Year)) %>%
  ungroup() %>%
  group_by(Indicator.Code) %>% 
  summarise(total_obs = n(),
            yearmean = mean(Year),
            yearmedian = median(Year),
            n_country  = n_distinct(Country.Code),          # Number of countries covered
            n_years    = n_distinct(Year),                  # Number of years covered
            countryobs_avg = mean(percountry_obs),          # Average number of obs per country
            countryobs_max = max(percountry_obs),           # Max number of obs per country
            yearlatest_mean = mean(percountry_maxyear),     # Mean latest year per country
            yearlatest_median = median(percountry_maxyear), # Median latest year per country
            yearlatest = max(percountry_maxyear),           # Latest year
            yearfirst_mean = mean(percountry_minyear),      # Mean first year per country
            yearfirst_median = median(percountry_minyear),  # Median first year per country
            yearfirst = min(percountry_minyear),            # First year
            yearmean_mean = mean(percountry_meanyear),
            yearmean_median = median(percountry_meanyear),
            n_lmic     = sum(unique(Country.Code) %in% lmics)) %>%
  mutate(span_years = yearlatest - yearfirst + 1,
         cov_years  = round(100 * ifelse(span_years == 0, 0, 
                                         (n_years - 1) / span_years), 2)
  ) %>%
  mutate(nonmiss = round(100 * total_obs/(n_country*span_years), 2),
         nonmiss_tot = round(100 * total_obs/(max(n_country)*max(span_years)), 2),
  ) #%>%
  #merge(wdic2000, by = "Indicator.Code")


countrymeta <- wbstats::wb_countries()
# Make LMIC country list
lmics <- unique(countrymeta[which(
  countrymeta$income_level == 'Low income' |
    countrymeta$income_level == 'Lower middle income' |
    countrymeta$income_level == 'Upper middle income'),]$iso3c)

# Find what percentage of n_countries are lmic
tmp <- indic_data %>% 
  select(Indicator.Code, Country.Code) %>%
  distinct() %>%
  group_by(Indicator.Code) %>%
  summarise(p_lmic = round(100 * (
    sum(ifelse(Country.Code %in% lmics, 1, 0)) / length(lmics)), 2))

# Merge represtativeness with main
edgarc <- merge(wdic, tmp, by = 'Indicator.Code')


score_fun  <- function(dataset) {
  
  get(dataset) %>%
  #edgarc %>%
    mutate(ncountry_score = n_country/217*100,
           plmic_score = p_lmic,
           yearlatest_score = (yearlatest - 2008)/(end_date-2008)*100,
           yearlatestmedian_score = (yearlatest_median - 2001)/(end_date-2001)*100,
           spanyears_score = span_years/(end_date-1960)*100,
           nonmiss_score = nonmiss,
           frontier_score = (ncountry_score + plmic_score + yearlatest_score + yearlatestmedian_score + spanyears_score + nonmiss_score)/6
    ) %>%
    mutate(
      country_score=case_when(
        n_country < scen$n_country[1] ~ 1,
        between(n_country,scen$n_country[1],scen$n_country[2])  ~ 2,
        between(n_country,scen$n_country[2],scen$n_country[3]) ~ 3,
        n_country >= scen$n_country[3] ~ 4
      )   ,
      p_lmic_score=case_when(
        p_lmic < scen$p_lmic[1] ~ 1,
        between(p_lmic,scen$p_lmic[1],scen$p_lmic[2])  ~ 2,
        between(p_lmic,scen$p_lmic[2],scen$p_lmic[3]) ~ 3,
        p_lmic >= scen$p_lmic[3] ~ 4
      )   ,
      yearlatest_median_score=case_when(
        yearlatest_median < scen$yearlatest_median[1] ~ 1,
        between(yearlatest_median,scen$yearlatest_median[1],scen$yearlatest_median[2])  ~ 2,
        between(yearlatest_median,scen$yearlatest_median[2],scen$yearlatest_median[3]) ~ 3,
        yearlatest_median >= scen$yearlatest_median[3] ~ 4
      )   ,
      yearlatest_score=case_when(
        yearlatest < scen$yearlatest[1] ~ 1,
        between(yearlatest,scen$yearlatest[1],scen$yearlatest[2])  ~ 2,
        between(yearlatest,scen$yearlatest[2],scen$yearlatest[3]) ~ 3,
        yearlatest >= scen$yearlatest[3] ~ 4
      )   ,
      span_years_score=case_when(
        span_years < scen$span_years[1] ~ 1,
        between(span_years,scen$span_years[1],scen$span_years[2])  ~ 2,
        between(span_years,scen$span_years[2],scen$span_years[3]) ~ 3,
        span_years >= scen$span_years[3] ~ 4
      )  ,
      nonmiss_score=case_when(
        nonmiss < scen$nonmiss[1] ~ 1,
        between(nonmiss,scen$nonmiss[1],scen$nonmiss[2])  ~ 2,
        between(nonmiss,scen$nonmiss[2],scen$nonmiss[3]) ~ 3,
        nonmiss >= scen$nonmiss[3] ~ 4
      )
    ) %>%
    mutate( #do a Distance to Threshold approach, where the frontiers are set by the thresholds
      country_score_hybrid=case_when(
        n_country < scen$n_country[4] ~ 0,
        between(n_country,scen$n_country[4],scen$n_country[1])  ~ (n_country-scen$n_country[4])/(scen$n_country[1]-scen$n_country[4]),
        between(n_country,scen$n_country[1],scen$n_country[2])  ~ 1+(n_country-scen$n_country[1])/(scen$n_country[2]-scen$n_country[1]),
        between(n_country,scen$n_country[2],scen$n_country[3]) ~ 2+(n_country-scen$n_country[2])/(scen$n_country[3]-scen$n_country[2]),
        between(n_country,scen$n_country[3],scen$n_country[5]) ~ 3 + (n_country-scen$n_country[3])/(scen$n_country[5]-scen$n_country[3]),
        n_country > scen$n_country[5] ~ 4,
      )   ,
      p_lmic_score_hybrid=case_when(
        p_lmic < scen$p_lmic[4] ~ 0,
        between(p_lmic,scen$p_lmic[4],scen$p_lmic[1])  ~ (p_lmic-scen$p_lmic[4])/(scen$p_lmic[1]-scen$p_lmic[4]),
        between(p_lmic,scen$p_lmic[1],scen$p_lmic[2])  ~ 1+(p_lmic-scen$p_lmic[1])/(scen$p_lmic[2]-scen$p_lmic[1]),
        between(p_lmic,scen$p_lmic[2],scen$p_lmic[3]) ~ 2+(p_lmic-scen$p_lmic[2])/(scen$p_lmic[3]-scen$p_lmic[2]),
        between(p_lmic,scen$p_lmic[3],scen$p_lmic[5]) ~ 3 + (p_lmic-scen$p_lmic[3])/(scen$p_lmic[5]-scen$p_lmic[3]),
        p_lmic > scen$p_lmic[5] ~ 4,
      )   ,
      yearlatest_median_score_hybrid=case_when(
        yearlatest_median < scen$yearlatest_median[4] ~ 0,
        between(yearlatest_median,scen$yearlatest_median[4],scen$yearlatest_median[1])  ~ (yearlatest_median-scen$yearlatest_median[4])/(scen$yearlatest_median[1]-scen$yearlatest_median[4]),
        between(yearlatest_median,scen$yearlatest_median[1],scen$yearlatest_median[2])  ~ 1+(yearlatest_median-scen$yearlatest_median[1])/(scen$yearlatest_median[2]-scen$yearlatest_median[1]),
        between(yearlatest_median,scen$yearlatest_median[2],scen$yearlatest_median[3]) ~ 2+(yearlatest_median-scen$yearlatest_median[2])/(scen$yearlatest_median[3]-scen$yearlatest_median[2]),
        between(yearlatest_median,scen$yearlatest_median[3],scen$yearlatest_median[5]) ~ 3 + (yearlatest_median-scen$yearlatest_median[3])/(scen$yearlatest_median[5]-scen$yearlatest_median[3]),
        yearlatest_median > scen$yearlatest_median[5] ~ 4,
      )   ,
      yearlatest_score_hybrid=case_when(
        yearlatest < scen$yearlatest[4] ~ 0,
        between(yearlatest,scen$yearlatest[4],scen$yearlatest[1])  ~ (yearlatest-scen$yearlatest[4])/(scen$yearlatest[1]-scen$yearlatest[4]),
        between(yearlatest,scen$yearlatest[1],scen$yearlatest[2])  ~ 1+(yearlatest-scen$yearlatest[1])/(scen$yearlatest[2]-scen$yearlatest[1]),
        between(yearlatest,scen$yearlatest[2],scen$yearlatest[3]) ~ 2+(yearlatest-scen$yearlatest[2])/(scen$yearlatest[3]-scen$yearlatest[2]),
        between(yearlatest,scen$yearlatest[3],scen$yearlatest[5]) ~ 3 + (yearlatest-scen$yearlatest[3])/(scen$yearlatest[5]-scen$yearlatest[3]),
        yearlatest > scen$yearlatest[5] ~ 4,
      )   ,
      span_years_score_hybrid=case_when(
        span_years < scen$span_years[4] ~ 0,
        between(span_years,scen$span_years[4],scen$span_years[1])  ~ (span_years-scen$span_years[4])/(scen$span_years[1]-scen$span_years[4]),
        between(span_years,scen$span_years[1],scen$span_years[2])  ~ 1+(span_years-scen$span_years[1])/(scen$span_years[2]-scen$span_years[1]),
        between(span_years,scen$span_years[2],scen$span_years[3]) ~ 2+(span_years-scen$span_years[2])/(scen$span_years[3]-scen$span_years[2]),
        between(span_years,scen$span_years[3],scen$span_years[5]) ~ 3 + (span_years-scen$span_years[3])/(scen$span_years[5]-scen$span_years[3]),
        span_years > scen$span_years[5] ~ 4,
      )    ,
      nonmiss_score_hybrid=case_when(
        nonmiss < scen$nonmiss[4] ~ 0,
        between(nonmiss,scen$nonmiss[4],scen$nonmiss[1])  ~ (nonmiss-scen$nonmiss[4])/(scen$nonmiss[1]-scen$nonmiss[4]),
        between(nonmiss,scen$nonmiss[1],scen$nonmiss[2])  ~ 1+(nonmiss-scen$nonmiss[1])/(scen$nonmiss[2]-scen$nonmiss[1]),
        between(nonmiss,scen$nonmiss[2],scen$nonmiss[3]) ~ 2+(nonmiss-scen$nonmiss[2])/(scen$nonmiss[3]-scen$nonmiss[2]),
        between(nonmiss,scen$nonmiss[3],scen$nonmiss[5]) ~ 3 + (nonmiss-scen$nonmiss[3])/(scen$nonmiss[5]-scen$nonmiss[3]),
        nonmiss > scen$nonmiss[5] ~ 4,
      )
    ) %>%    
    mutate(loose=if_else( #calculate whether indicator would fall in loose, median, or stringent as well
      (n_country < scen$n_country[1] |
         p_lmic     < scen$p_lmic[1] |
         yearlatest_median  < scen$yearlatest_median[1] |
         yearlatest  < scen$yearlatest[1] |
         nonmiss  < scen$nonmiss[1] |
         (yearlatest < 2015 & span_years < scen$span_years[1]))
      ,"Yes", "No"),
      median=if_else(
        (n_country < scen$n_country[2] |
           p_lmic     < scen$p_lmic[2] |
           yearlatest_median  < scen$yearlatest_median[2] |
           yearlatest  < scen$yearlatest[2] |
           nonmiss  < scen$nonmiss[2] |
           (yearlatest < 2015 & span_years < scen$span_years[2]))
        ,"Yes", "No"),
      stringent=if_else(
        (n_country < scen$n_country[3] |
           p_lmic     < scen$p_lmic[3] |
           yearlatest_median  < scen$yearlatest_median[3] |
           yearlatest  < scen$yearlatest[3] |
           nonmiss  < scen$nonmiss[3] |
           (yearlatest < 2015 & span_years < scen$span_years[3]))
        ,"Yes", "No")
    ) %>%      
    
    mutate(
      threshold_score=(country_score+p_lmic_score+yearlatest_median_score+yearlatest_score+ span_years_score+ nonmiss_score)/6,
      
      hybrid_score=(country_score_hybrid+p_lmic_score_hybrid+yearlatest_median_score_hybrid+yearlatest_score_hybrid+ span_years_score_hybrid + nonmiss_score_hybrid )/6,
    ) %>%
    mutate(
      geographic_score= (country_score+p_lmic_score)/2,
      temporal_score=(yearlatest_median_score+yearlatest_score+ span_years_score)/3,
      completeness_score=nonmiss_score,
      threshold_score_wgtd=(geographic_score+temporal_score+completeness_score)/3
    ) %>%   
    mutate(
      geographic_score_hybrid= (country_score_hybrid+p_lmic_score_hybrid)/2,
      temporal_score_hybrid=(yearlatest_median_score_hybrid+yearlatest_score_hybrid+ span_years_score_hybrid)/3,
      completeness_score_hybrid=nonmiss_score_hybrid,
      hybrid_score_wgtd=(geographic_score_hybrid+temporal_score_hybrid+completeness_score_hybrid)/3,
      hybrid_score_wgtd2=(geographic_score_hybrid+temporal_score_hybrid)/2
    ) 
}

edgarc_score_table <- score_fun('edgarc')


edgarc_score_table_for_report <- edgarc_score_table %>% select(Indicator.Code, hybrid_score_wgtd2, 
                                                               n_country, p_lmic, yearlatest, span_years, 
                                                               nonmiss_tot)

getwd()
proposed_indicators <- read.csv("proposed_indicators_EDGAR.csv")

edgarc_score_table_for_report <- merge(proposed_indicators, edgarc_score_table_for_report,
      by.x = "code", by.y = "Indicator.Code")

edgarc_score_table_for_report <-
  edgarc_score_table_for_report[ order(match(edgarc_score_table_for_report$code, proposed_indicators$code)), ]


# Export score_table current WDI GHG indicators
write.csv(edgarc_score_table_for_report, "EDGAR_score_table.csv")
