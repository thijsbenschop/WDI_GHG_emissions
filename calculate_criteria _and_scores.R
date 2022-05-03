### Calculate criteria and scoring for existing indicators

library(flexdashboard)
library(tidyverse)
library(reshape2)
library(plotly)
library(data.table)
library(cowplot)
library(knitr)
library(flexdashboard)
library(shiny)
library(DT)
library(miniUI)
library(plotly)
library(readxl)

rm(list = ls())
setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions")

#### GET DATA ####
## Current WDI indicators
wdic <- read.csv("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/wdi-criteria/wdic.csv")
scen <- read.csv("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/wdi-criteria/scenarios.csv")
colnames(scen)[1] <- "scenario"

# Select set of indicators
cur_ghg_indicators <- read_xlsx("current_WDI_indicators.xlsx")

wdic <- wdic[wdic$Indicator.Code %in% cur_ghg_indicators$`Series code`, ]

### Scoring System
score_table <- wdic %>%
  mutate(ncountry_score = n_country/217*100,
         plmic_score = p_lmic,
         yearlatest_score = (yearlatest - 2008)/(2020-2008)*100,
         yearlatestmedian_score = (yearlatest_median - 2001)/(2020-2001)*100,
         spanyears_score = span_years/(2020-1960)*100,
         nonmiss_score = nonmiss,
         uniquevisitors_score = ntile(uniquevisitors, 100),
         frontier_score = (ncountry_score + plmic_score + yearlatest_score + yearlatestmedian_score + spanyears_score + nonmiss_score + uniquevisitors_score)/7
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
    )   ,
    uniquevisitors_score=case_when(
      uniquevisitors < scen$uniquevisitors[1] ~ 1,
      between(uniquevisitors,scen$uniquevisitors[1],scen$uniquevisitors[2])  ~ 2,
      between(uniquevisitors,scen$uniquevisitors[2],scen$uniquevisitors[3]) ~ 3,
      uniquevisitors >= scen$uniquevisitors[3] ~ 4
    )   ,
    nonmiss_score=case_when(
      nonmiss < scen$nonmiss[1] ~ 1,
      between(nonmiss,scen$nonmiss[1],scen$nonmiss[2])  ~ 2,
      between(nonmiss,scen$nonmiss[2],scen$nonmiss[3]) ~ 3,
      nonmiss >= scen$nonmiss[3] ~ 4
    )          
  ) %>%
  mutate( #do a hybrid approach, where the frontiers are set by the thresholds
    country_score_hybrid=case_when(
      n_country < scen$n_country[1] ~ n_country/scen$n_country[1],
      between(n_country,scen$n_country[1],scen$n_country[2])  ~ 1+(n_country-scen$n_country[1])/(scen$n_country[2]-scen$n_country[1]),
      between(n_country,scen$n_country[2],scen$n_country[3]) ~ 2+(n_country-scen$n_country[2])/(scen$n_country[3]-scen$n_country[2]),
      n_country >= scen$n_country[3] ~ 3 + (n_country-scen$n_country[3])/(217-scen$n_country[3])
    )   ,
    p_lmic_score_hybrid=case_when(
      p_lmic < scen$p_lmic[1] ~ p_lmic/p_lmic[1],
      between(p_lmic,scen$p_lmic[1],scen$p_lmic[2])  ~ 1+(p_lmic-scen$p_lmic[1])/(scen$p_lmic[2]-scen$p_lmic[1]),
      between(p_lmic,scen$p_lmic[2],scen$p_lmic[3]) ~ 2+(p_lmic-scen$p_lmic[2])/(scen$p_lmic[3]-scen$p_lmic[2]),
      p_lmic >= scen$p_lmic[3] ~ 3 + (p_lmic-scen$p_lmic[3])/(100-scen$p_lmic[3])
    )   ,
    yearlatest_median_score_hybrid=case_when(
      yearlatest_median < scen$yearlatest_median[1] ~ yearlatest_median/scen$yearlatest_median[1],
      between(yearlatest_median,scen$yearlatest_median[1],scen$yearlatest_median[2])  ~ 1+(yearlatest_median-scen$yearlatest_median[1])/(scen$yearlatest_median[2]-scen$yearlatest_median[1]),
      between(yearlatest_median,scen$yearlatest_median[2],scen$yearlatest_median[3]) ~ 2+(yearlatest_median-scen$yearlatest_median[2])/(scen$yearlatest_median[3]-scen$yearlatest_median[2]),
      yearlatest_median >= scen$yearlatest_median[3] ~ 3 + (yearlatest_median-scen$yearlatest_median[3])/(2020-scen$yearlatest_median[3])
    )   ,
    yearlatest_score_hybrid=case_when(
      yearlatest < scen$yearlatest[1] ~ yearlatest/scen$yearlatest[1],
      between(yearlatest,scen$yearlatest[1],scen$yearlatest[2])  ~ 1+(yearlatest-scen$yearlatest[1])/(scen$yearlatest[2]-scen$yearlatest[1]),
      between(yearlatest,scen$yearlatest[2],scen$yearlatest[3]) ~ 2+(yearlatest-scen$yearlatest[2])/(scen$yearlatest[3]-scen$yearlatest[2]),
      yearlatest >= scen$yearlatest[3] ~ 3 + (yearlatest-scen$yearlatest[3])/(2020-scen$yearlatest[3])
    )   ,
    span_years_score_hybrid=case_when(
      span_years < scen$span_years[1] ~ span_years/scen$span_years[1],
      between(span_years,scen$span_years[1],scen$span_years[2])  ~ 1+(span_years-scen$span_years[1])/(scen$span_years[2]-scen$span_years[1]),
      between(span_years,scen$span_years[2],scen$span_years[3]) ~ 2+(span_years-scen$span_years[2])/(scen$span_years[3]-scen$span_years[2]),
      span_years >= scen$span_years[3] ~ 3 + (span_years-scen$span_years[3])/((2020-1960)-scen$span_years[3])
    )   ,
    uniquevisitors_score_hybrid=case_when(
      uniquevisitors < scen$uniquevisitors[1] ~ uniquevisitors/scen$uniquevisitors[1],
      between(uniquevisitors,scen$uniquevisitors[1],scen$uniquevisitors[2])  ~ 1+(uniquevisitors-scen$uniquevisitors[1])/(scen$uniquevisitors[2]-scen$uniquevisitors[1]),
      between(uniquevisitors,scen$uniquevisitors[2],scen$uniquevisitors[3]) ~ 2+(uniquevisitors-scen$uniquevisitors[2])/(scen$uniquevisitors[3]-scen$uniquevisitors[2]),
      uniquevisitors >= scen$uniquevisitors[3] ~ 3 + (uniquevisitors-scen$uniquevisitors[3])/(max(uniquevisitors, na.rm=TRUE)-scen$uniquevisitors[3])
    )   ,
    nonmiss_score_hybrid=case_when(
      nonmiss < scen$nonmiss[1] ~ nonmiss/scen$nonmiss[1],
      between(nonmiss,scen$nonmiss[1],scen$nonmiss[2])  ~ 1+(nonmiss-scen$nonmiss[1])/(scen$nonmiss[2]-scen$nonmiss[1]),
      between(nonmiss,scen$nonmiss[2],scen$nonmiss[3]) ~ 2+(nonmiss-scen$nonmiss[2])/(scen$nonmiss[3]-scen$nonmiss[2]),
      nonmiss >= scen$nonmiss[3] ~ 3 + (nonmiss-scen$nonmiss[3])/(100-scen$nonmiss[3])
    )          
  ) %>%    
  mutate(loose=if_else( #calculate whether indicator would fall in loose, median, or stringent as well
    (n_country < scen$n_country[1] |
       p_lmic     < scen$p_lmic[1] |
       yearlatest_median  < scen$yearlatest_median[1] |
       yearlatest  < scen$yearlatest[1] |
       nonmiss  < scen$nonmiss[1] |
       (yearlatest < 2015 & span_years < scen$span_years[1]) |
       uniquevisitors     < scen$uniquevisitors[1]), "Yes", "No"),
    
    median=if_else(
      (n_country < scen$n_country[2] |
         p_lmic     < scen$p_lmic[2] |
         yearlatest_median  < scen$yearlatest_median[2] |
         yearlatest  < scen$yearlatest[2] |
         nonmiss  < scen$nonmiss[2] |
         (yearlatest < 2015 & span_years < scen$span_years[2]) |
         uniquevisitors     < scen$uniquevisitors[2]), "Yes", "No"),
    
    stringent=if_else(
      (n_country < scen$n_country[3] |
         p_lmic     < scen$p_lmic[3] |
         yearlatest_median  < scen$yearlatest_median[3] |
         yearlatest  < scen$yearlatest[3] |
         nonmiss  < scen$nonmiss[3] |
         (yearlatest < 2015 & span_years < scen$span_years[3]) |
         uniquevisitors     < scen$uniquevisitors[3]), "Yes", "No")
  ) %>%      
  
  mutate(
    threshold_score=(country_score+p_lmic_score+yearlatest_median_score+yearlatest_score+ span_years_score+ uniquevisitors_score+ nonmiss_score )/7,
    
    hybrid_score=(country_score_hybrid+p_lmic_score_hybrid+yearlatest_median_score_hybrid+yearlatest_score_hybrid+ span_years_score_hybrid+ uniquevisitors_score_hybrid+ nonmiss_score_hybrid )/7,
  ) %>%
  mutate(
    geographic_score= (country_score+p_lmic_score)/2,
    temporal_score=(yearlatest_median_score+yearlatest_score+ span_years_score)/3,
    completeness_score=nonmiss_score,
    usage_score=uniquevisitors_score,
    threshold_score_wgtd=(geographic_score+temporal_score+completeness_score+usage_score)/4
  ) %>%   
  mutate(
    geographic_score_hybrid= (country_score_hybrid+p_lmic_score_hybrid)/2,
    temporal_score_hybrid=(yearlatest_median_score_hybrid+yearlatest_score_hybrid+ span_years_score_hybrid)/3,
    completeness_score_hybrid=nonmiss_score_hybrid,
    usage_score_hybrid=uniquevisitors_score_hybrid,
    hybrid_score_wgtd=(geographic_score_hybrid+temporal_score_hybrid+completeness_score_hybrid+usage_score_hybrid)/4
  ) 

# Export score_table current WDI GHG indicators
write.csv(score_table, "current_WDI_indicators_scores.csv")
