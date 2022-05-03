# Calculate criteria for new set of indicators

library(tidyverse)
library(reshape2)
library(RSiteCatalyst)
library(dplyr)
library(tidyr)
library(data.table)
library(tidyselect)

# Comments
# which indicators not in WDI? how to compare - use indicator.code?
# Start with list of all 333 CCDR indicators, drop 32 with matching codein CCDR and WDI databases, drop 3 with proprietary license
# set of indicators with only future values -> to be excluded? 98 indicators
# span years, add 1, otherwise 0 if only 1 year
# p_lmic -> if larger number of countries, automatically goes down, look also at absolute number of lmic?
# information on primary/derived?

rm(list = ls())

setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions/Data")

# rewrite get_data.R script from https://github.com/DivyanshiWadhwa/wdi-criteria to analyze CCDR indicators

#### GET DATA ####

# INSTRUCTIONS TO RUN FIRST TIME:
# Specify URL where file is stored and destination to download
# Uncomment url and download file when you need to download first time or redownload
# and update WDI file again.
# Change working directory to where you want to download WDI.

url <- "http://databank.worldbank.org/data/download/CCDR_csv.zip"
setwd("C:/Users/wb460271/OneDrive - WBG/Documents/WDI - links and docs/CCDR_criteria")
destfile <- "ccdr.zip"
#download.file(url, destfile)

#### CLEAN DATA ####
data        <- as.data.table(read.table(unz(destfile, "CCDRData.csv"),header=T, quote="\"", sep=","))
topics      <- as.data.table(read.table(unz(destfile, "CCDRSeries.csv"),header=T, quote="\"", sep=","))
countrymeta <- as.data.table(read.table(unz(destfile, "CCDRCountry.csv"),header=T, quote="\"", sep=","))

seriesmeta <- read.csv("ccdr-series-meta.csv") # go be updated for CCDR, also add info on whether included or not in WDI

# List from CCDR_data_clean_v2(1).xlsx
# not_in_WDI <- c("AG.LND.FRST.ZS", "CC.ACLD.PRTS.NO", "CC.ADPO.MIEX.AA", "CC.AG.NTR.TOHA", "CC.ASP.COVG.QT",
#                 "CC.ASP.COVG.ZS", "CC.ASP.TRAF.PC", "CC.AVPB.PTPI.AR", "CC.AVPB.TPOP.AG", "CC.CHIC.BTFP.AG",
#                 "CC.CHIC.CFPI.AG", "CC.CO2.EMSE.EL", "CC.COAL.EMIS.CH", "CC.COAL.EMPR.CH", "CC.CUF.STUN",
#                 "CC.EATM.PINV.CE", "CC.EG.CAP.GAEX", "CC.EG.CAP.GAIM", "CC.EG.COAL.MW", "CC.EG.COAL.PROD",
#                 "CC.EG.CONS.COAL.PC", "CC.EG.CONS.GAS.PC", "CC.EG.CONS.OIL.PC", "CC.EG.EMIS.MAN", 
#                 "CC.EG.FIMP.BD", "CC.EG.FIMP.ZS", "CC.EG.INTS.KW", "CC.EG.SOLR.KW", "CC.EG.STL.PROD",
#                 "CC.EG.SUBF.PC", "CC.EG.SUBS.PC", "CC.EG.SUBS.RATE", "CC.EG.SUBS.ZS", "CC.EG.WIND.PC",
#                 "CC.EG.WIND.TOTL", "CC.EN.ATM.COAL.C", "CC.EN.CRI.INDX", "CC.ENR.NRG", "CC.ENTX.ENE.ZS",
#                 "CC.ENTX.ENV.ZS", "CC.ENV.GDS.CADV", "CC.ENV.TRAD.EX", "CC.ENV.TRAD.IM", "CC.ESG.AGMA", 
#                 "CC.FDX.FTN.ZS", "CC.FDX.FTY.ZS", "CC.FDX.PMT.ZS", "CC.FDX.SAV.ZS", "CC.FGHG.SYS.LU",
#                 "CC.FLD.BELW.ZS", "CC.FLD.CITY.ZS", "CC.FLD.TOTL.ZS", "CC.FOOD.ANIM.ZS", "CC.FOOD.VEGT.ZS", 
#                 "CC.FRVOL.MOD.IW", "CC.FSU.PECA", "CC.GAS.PPBC.PR", "CC.GHG.EMSE.EL", "CC.GHG.FSYS.CH",
#                 "CC.GHG.GRPE", "CC.GHG.MEMG.PO", "CC.GHG.PECA", "CC.GHG.SDEG.OF", "CC.GNBD.ISS.BD", 
#                 "CC.INCP.KRGC", "CC.ISG.NAMA", "CC.KBA.MRN.ZS", "CC.KBA.TERR.ZS", "CC.MT.GE.EST", 
#                 "CC.MT.PS.EST", "CC.MT.RL.EST", "CC.MT.RQ.EST", "CC.MT.VA.EST", "CC.NAPD.EMDA.EQ",
#                 "CC.NCO.GHG.EL", "CC.NDPD.EMDA.EQ", "CC.NEFO.IMCR.BA", "CC.NEPD.EMDA.EQ", 
#                 "CC.NIPD.EMDA.EQ", "CC.OIL.PSBC.PR", "CC.OPCO.AG.CA1", "CC.OPCO.PLTY.SU", 
#                 "CC.PRI.REG.NO", "CC.PRI.SIGN.NO", "CC.PRNX.CAT1.ZS", "CC.PRNX.CAT2.ZS", 
#                 "CC.PRNX.CAT3.ZS", "CC.PRNX.CAT4.ZS", "CC.PRNX.CAT5.ZS", "CC.PRO.ANI", 
#                 "CC.PSVOL.MOD.RO", "CC.PUN.TOTL", "CC.RIC.PECA", "CC.RISK.AST.ZS",
#                 "CC.RISK.WELL.ZS", "CC.SE.CAT1.ZS", "CC.SE.CAT2.ZS", "CC.SE.CAT3.ZS",
#                 "CC.SE.CAT4.ZS", "CC.SE.COMP.ZS", "CC.SE.NYRS.AVG", "CC.SH.AIRP.AIR",
#                 "CC.SH.AIRP.AMB", "CC.SLDD.TAX.AF", "CC.SP.COV.ZS", "CC.SP.EXP.ZS", 
#                 "CC.TCFD.COMP.FI", "CC.THHZ.RANK.RF", "CC.TNET.CYC.ZS", "CC.TNET.EAR.ZS", 
#                 "CC.TNET.FLD.ZS", "CC.TNET.INV.ZS", "CC.TNET.NAT.ZS", "CC.TNET.REP.FAC", 
#                 "CC.TOT.GHG.GR", "CC.TOTL.COCA.OP", "EG.ELC.CARB.KT", "EG.TEG.RNEW.ZS", "EG.TEG.VAR.ZS")

seriesmeta_wdi <- read.csv("wdi-series-meta.csv") # go be updated for CCDR, also add info on whether included or not in WDI

#seriesmeta$in_WDI <- seriesmeta$Indicator.Code %in% seriesmeta_wdi$Indicator.Code
#table(seriesmeta$Indicator.Code %in% seriesmeta_wdi$Indicator.Code)
#table(seriesmeta$Indicator.Name %in% seriesmeta_wdi$Indicator.Name)
#table(seriesmeta$Indicator.Code %in% not_in_WDI)

# Check which indicators are already in WDI, those we drop as not consider for inclusion
table(seriesmeta$Indicator.Code %in% seriesmeta_wdi$Indicator.Code) # 32 to be excluded, based on code
table(seriesmeta$Indicator.Name %in% seriesmeta_wdi$Indicator.Name) # 28 to be excluded, based on name

in_WDI <- c("account.t.d", # see email Hiroko November 29, 2021 with Excel spreadsheet
            "account.t.d.7",
            "AG.LND.AGRI.ZS",
            "AG.LND.ARBL.ZS",
            "AG.LND.FRST.ZS",
            "EG.ELC.ACCS.ZS",
            "EN.POP.SLUM.UR.ZS",
            "ER.H2O.INTR.PC",
            "GC.DOD.TOTL.GD.ZS",
            "GC.TAX.TOTL.GD.ZS",
            "IC.ELC.OUTG",
            "IC.ELC.OUTG.ZS",
            "IC.FRM.FEMO.ZS",
            "IC.FRM.OUTG.ZS",
            "NY.GDP.COAL.RT.ZS",
            "NY.GDP.MINR.RT.ZS",
            "NY.GDP.MKTP.CD",
            "NY.GDP.MKTP.KD.ZG",
            "NY.GDP.NGAS.RT.ZS",
            "NY.GDP.PCAP.CD",
            "NY.GDP.PETR.RT.ZS",
            "NY.GDP.TOTL.RT.ZS",
            "SE.ADT.LITR.ZS",
            "SG.GEN.PARL.ZS",
            "SH.H2O.BASW.ZS",
            "SH.STA.BASS.ZS",
            "SH.STA.HYGN.ZS",
            "SI.POV.GINI",
            "SI.POV.UMIC",
            "SL.TLF.TOTL.FE.ZS",
            "SL.UEM.TOTL.ZS",
            "SP.POP.TOTL",
            "SP.URB.TOTL.IN.ZS")

already_in_WDI <- seriesmeta$Indicator.Code[seriesmeta$Indicator.Code %in% seriesmeta_wdi$Indicator.Code]
#data <- data[!(Indicator.Code %in% already_in_WDI)] # drop 32 indicators
length(unique(data$Indicator.Code))
data <- data[!(Indicator.Code %in% in_WDI)] # drop 33 indicators
dim(data)
length(unique(data$Indicator.Code))

length(already_in_WDI)
length(in_WDI)
already_in_WDI %in% in_WDI
in_WDI %in% already_in_WDI

# Check which indicators don't have an open license (don't consider proprietary data)
table(seriesmeta$license)
not_open_license <- seriesmeta$Indicator.Code[seriesmeta$license != "open"]
data <- data[!(Indicator.Code %in% not_open_license)] # drop 3 indicators

# Reshape to long, clean up indicator names, drop all missing "values"
datal <- data.table::melt(data, id.vars = c("ï..Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code")) 
datal <- datal %>%
  rename("Country.Name" = "ï..Country.Name") %>%
  rename("Year" = "variable") %>% 
  drop_na("value")

# Convert year variable as numeric
datal[, Year := str_replace(datal$Year, "X", "")]
datal[, Year := as.numeric(as.character(datal$Year))]

# Merge relevant metadata
countrymeta <- countrymeta %>%
  rename("Country.Code" = "ï..Country.Code") %>%
  select("Country.Code", "Region", "Income.Group", "Lending.category")

# Rename column ï..Series.Code to Indicator.Code
topics <- topics %>% 
  rename("Indicator.Code" = "ï..Series.Code")
topics[, Indicator.Name := NULL] # conflict with Indicator.Name in datal

# Merge countrymeta and topics in long data
datal <- datal %>%
  merge(countrymeta, by = "Country.Code", all.x = T) %>%
  merge(topics, by = "Indicator.Code", all.x = T) %>%
  filter(Region != "") # dropping COK and NIU 
datal[, X := NULL] # drop empty variable X


datal2 <- datal

setwd("C:/Users/wb460271/OneDrive - WBG/Documents/GitHub/WDI_GHG_emissions/Data")

dataw <- as.data.table(read.csv("wide_version.csv"))
datal <- melt(dataw, id.vars =  c("scenario", "country",  "time"))

datal <- datal[scenario == "HISTTP"]

datal <- datal %>% rename(Country.Code = country) %>% 
  rename(Year = time) %>%
  rename(Indicator.Code = variable) 

#### CREATE CRITERIA INDICATORS ####
# Total years spanned, if future predictions, take most recent value up until last year
cur_year  <- as.numeric(format(Sys.Date(), "%Y")) # current year using system time
totalspan <- min(max(datal$Year), cur_year - 1) - min(datal$Year) + 1 # 60 (1961-2020), 141 with future predictions until 2100

# Total countries covered
totalcountries <- 217 #length(unique(datal$Country.Code)) # 217 countries

# Make LMIC country list
lmics <- unique(countrymeta[which(
  countrymeta$Income.Group == 'Low income' |
    countrymeta$Income.Group == 'Lower middle income' |
    countrymeta$Income.Group == 'Upper middle income'),]$Country.Code)

# Average and maximum number of observations per country and in total
# Question: Should the median and means be weighted by number of observations?

# Total number of observations
# wdic <- datal %>%
#   group_by(Indicator.Code) %>%
#   summarise(total_obs = n(),
#             yearmean = mean(Year),
#             yearmedian = median(Year))

# Non-weighted averages
# countryobs <- datal %>%
#   group_by(Indicator.Code, Country.Code) %>%
#   summarise(percountry_obs = n(),
#             percountry_maxyear = max(Year),
#             percountry_minyear = min(Year),
#             percountry_meanyear = mean(Year)) %>%
#   ungroup() %>%
#   group_by(Indicator.Code) %>% 
#   summarise(n_country  = n_distinct(Country.Code),          # Number of countries covered
# #            n_years    = n_distinct(Year),                  # Number of years covered
#             countryobs_avg = mean(percountry_obs),          # Average number of obs per country
#             countryobs_max = max(percountry_obs),           # Max number of obs per country
#             yearlatest_mean = mean(percountry_maxyear),     # Mean latest year per country
#             yearlatest_median = median(percountry_maxyear), # Median latest year per country
#             yearlatest = max(percountry_maxyear),           # Latest year
#             yearfirst_mean = mean(percountry_minyear),      # Mean first year per country
#             yearfirst_median = median(percountry_minyear),  # Median first year per country
#             yearfirst = min(percountry_minyear),            # First year
#             yearmean_mean = mean(percountry_meanyear),
#             yearmean_median = median(percountry_meanyear),
#             n_lmic     = sum(unique(Country.Code) %in% lmics))

#wdic <- merge(wdic, countryobs, by = "Indicator.Code", all = T)

# Weighted averages
ccdrc <- datal %>%
  #filter(Year < cur_year) %>%                    # filter out future predictions, losing 98 indicators with min year 2021 and above
  group_by(Indicator.Code, Country.Code) %>%
  mutate(percountry_obs = n(),                   # number of obs per country and indicator pair
         percountry_maxyear = max(Year),         # most recent year for country and indicator pair (top at current year - 1)
         percountry_minyear = min(Year),         # earliest year for country and indicator pair
         percountry_meanyear = mean(Year)) %>%   # mean of year for country and indicator pair (exclude obs after current year - 1)
  ungroup() %>%
  group_by(Indicator.Code) %>% 
  summarise(total_obs         = n(),                         # number of obs. per indicator
            yearmean          = mean(Year),                  # mean year per indicator
            yearmedian        = median(Year),                # median year per indicator
            n_country         = n_distinct(Country.Code),    # Number of countries covered
            n_years           = n_distinct(Year),            # Number of years covered
            countryobs_avg    = mean(percountry_obs),        # Average number of obs per country
            countryobs_max    = max(percountry_obs),         # Max number of obs per country
            yearlatest_mean   = mean(percountry_maxyear),    # Mean latest year per country
            yearlatest_median = median(percountry_maxyear),  # Median latest year per country
            yearlatest        = max(percountry_maxyear),     # Latest year
            yearfirst_mean    = mean(percountry_minyear),    # Mean first year per country
            yearfirst_median  = median(percountry_minyear),  # Median first year per country
            yearfirst         = min(percountry_minyear),     # First year
            yearmean_mean     = mean(percountry_meanyear),   # Mean year over all countries and obs.
            yearmean_median   = median(percountry_meanyear), # Median year over all countries and obs.
            n_lmic = sum(unique(Country.Code) %in% lmics)) %>% # Number of lmics countries covered
  mutate(only_pred = yearfirst >= cur_year, # only future values
         span_years = yearlatest - yearfirst + 1,            # Number of years spanned by indicator + 1
         cov_years  = round(100 * ifelse(span_years == 0, 0, # 
                                         (n_years - 1) / span_years), 2),
         nonmiss =  100 * total_obs / (span_years * n_country) # number of observations/span of years*number of countries
         # nonmiss_total = 100 *total_obs / (217 * 60) # number of observations/span of WDI *number of countries in WDI
  )

# Check which indicators only have predicted values
table(ccdrc$yearfirst)
table(ccdrc$only_pred)# 98 indicators

# To be added from Adobe Analytics if possible
ccdrc$uniquevisitors <- 100 # to be added
ccdrc$pageviews <- 100 # to be added
ccdrc$visitors <- 100 # to be added

dim(ccdrc)

# Get number of countries per year and indicator
ccdry <- datal %>%
  group_by(Indicator.Code, Year) %>%
  summarise(percountry_obs = n())

# Get number of indicators per country
ccdri <- datal %>%
  group_by(Country.Code) %>%
  summarise(percountry_indicators = n_distinct(Indicator.Code))

# Extract data topic from series metadata
seriesmeta2 <- seriesmeta %>%
  select(Indicator.Code, datatopic)

# Get number of indicators by country and data topic (no variation in data topic)
ccdrit <- datal %>%
  merge(seriesmeta2, by = "Indicator.Code", all.x = T) %>%
  group_by(Country.Code, datatopic) %>%
  summarise(percountry_indicators = n_distinct(Indicator.Code))

# Get number of indicators by data topic
ccdrt <- datal %>%
  merge(seriesmeta2, by = "Indicator.Code", all.x = T) %>%
  group_by(datatopic) %>%
  summarise(pertheme_indicators = n_distinct(Indicator.Code))

# Get number of indicators by year and data topic 
ccdryc <- datal %>%
  merge(seriesmeta2, by = "Indicator.Code", all.x = T) %>%
  group_by(Year, datatopic) %>%
  summarise(peryear_indicators = n_distinct(Indicator.Code))

# Extract data topic and type from series metadata
seriesmeta3 <- seriesmeta %>%
  select(Indicator.Code, datatopic, type)

# Get number of indicators by type and data topic 
ccdrtt <- datal %>%
  merge(seriesmeta3, by = "Indicator.Code", all.x = T) %>%
  group_by(datatopic, type) %>%
  summarise(pertheme_indicators = n_distinct(Indicator.Code))

## Note: Vinny did the span years differently. Sanity check with Brian later. He takes median value instead of absolute...

# Find what percentage of n_countries are lmic
tmp <- datal %>% 
  select(Indicator.Code, Country.Code) %>%
  distinct() %>%
  group_by(Indicator.Code) %>%
  summarise(p_lmic = round(100 * (
    sum(ifelse(Country.Code %in% lmics, 1, 0)) / length(unique(Country.Code))), 2))

# Merge representativeness with main
ccdrc <- merge(ccdrc, tmp, by = 'Indicator.Code')

#### Plotting #####
setwd("C:/Users/wb460271/OneDrive - WBG/Documents/WDI - links and docs/CCDR_criteria")

scen <- read.csv("scenarios.csv") # different scenarios
colnames(scen)[1] <- "scenario"

wdic <- ccdrc # read.csv("wdic.csv")
wdic$database <- "wdi"
ccdrc$database <- "ccdr"
#combinedc <- rbind(ccdrc[, (names(ccdrc) != "only_pred")], wdic[, colnames(ccdrc[, (names(ccdrc) != "only_pred")])])
combinedc <- wdic
#colnames(ccdrc) %in% colnames(wdic)

# Density plot fill colors by groups
# Density of years in WDI and ccdr
options(repr.P.width=4,repr.P.height=1)
combinedc %>% filter(yearfirst < 2021) %>% ggplot(aes(x=yearfirst , fill=database)) +
  geom_rect(aes(xmin = 1960, xmax = 1980, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = 1980, xmax = 2000, ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = 2000, xmax = 2021, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  geom_rect(aes(xmin = 2000, xmax = 2021, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  geom_density(alpha=0.4, color = NA) +
  geom_vline(aes(xintercept=2000),
             linetype="dashed") 

# Criteria 1 n_country
crit1 <- ccdrc %>% filter(yearfirst < 2021) %>% ggplot(aes(x=n_country)) +
  geom_rect(aes(xmin = 0, xmax = scen[scen$scenario == "Loose", "n_country"], ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Loose", "n_country"],
                xmax = scen[scen$scenario == "Median", "n_country"], ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Median", "n_country"],
                xmax = scen[scen$scenario == "Stringent", "n_country"], ymin = -Inf, ymax = Inf),
            fill = "yellow", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Stringent", "n_country"], 
                xmax = 220, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  #  geom_density(alpha=0.4, color = NA) +
  geom_bar(alpha=1, color = NA) + 
  labs(title = "Number of countries", x = "Number of countries", y = "Number of indicators in CCDR") +
  geom_label(x = scen[scen$scenario == "Median", "n_country"], y = 20, 
             label = paste0(
               "Mean: ", ccdrc %>% filter(yearfirst < 2021) %>% select(n_country) %>% summarise(mean = mean(n_country)),
               "\nNumber in red: ", ccdrc %>% filter(yearfirst < 2021) %>% select(n_country) %>% summarise(num_red = sum(n_country < scen[scen$scenario == "Loose", "n_country"])),
               "\n Number in orange: ", ccdrc %>% filter(yearfirst < 2021) %>% select(n_country) %>% summarise(num_orange = sum(n_country >= scen[scen$scenario == "Loose", "n_country"] & n_country < scen[scen$scenario == "Median", "n_country"])),
               "\n Number in yellow: ", ccdrc %>% filter(yearfirst < 2021) %>% select(n_country) %>% summarise(num_orange = sum(n_country >= scen[scen$scenario == "Median", "n_country"] & n_country < scen[scen$scenario == "Stringent", "n_country"])))
  )
png("ccdr_criteria1.png")
crit1
dev.off()

# Criteria 2 p_lmic 
crit2 <- ccdrc %>% filter(yearfirst < 2021) %>% 
  mutate(p_lmic = round(p_lmic)) %>% ggplot(aes(x=p_lmic)) +
  geom_rect(aes(xmin = 0, xmax = scen[scen$scenario == "Loose", "n_country"], ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Loose", "p_lmic"], 
                xmax = scen[scen$scenario == "Median", "p_lmic"], ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Median", "p_lmic"], 
                xmax = scen[scen$scenario == "Stringent", "p_lmic"], ymin = -Inf, ymax = Inf),
            fill = "yellow", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Stringent", "p_lmic"], 
                xmax = 100, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  geom_bar(alpha=1, color = NA) + 
  labs(title = "Share of lmic countries covered (%)", x = "Share of lmic covered(%)", y = "Number of indicators in CCDR") +
  geom_label(x = scen[scen$scenario == "Median", "p_lmic"] - 22, y = 25, 
             label = paste0(
               "Mean: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(p_lmic) %>% summarise(mean = mean(p_lmic)),
               "\nNumber in red: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(p_lmic) %>% summarise(num_red = sum(p_lmic < scen[scen$scenario == "Loose", "p_lmic"])),
               "\n Number in orange: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(p_lmic) %>% summarise(num_orange = sum(p_lmic >= scen[scen$scenario == "Loose", "p_lmic"] & p_lmic < scen[scen$scenario == "Median", "p_lmic"])),
               "\n Number in yellow: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(p_lmic) %>% summarise(num_orange = sum(p_lmic >= scen[scen$scenario == "Median", "p_lmic"] & p_lmic < scen[scen$scenario == "Stringent", "p_lmic"]))))

png("ccdr_criteria2.png")
crit2
dev.off()

# Criteria 3 yearlatest_median   
crit3 <- ccdrc %>% filter(yearfirst < 2021) %>% 
  mutate(p_lmic = round(yearlatest_median )) %>% ggplot(aes(x=yearlatest_median )) +
  geom_rect(aes(xmin = 2000, xmax = scen[scen$scenario == "Loose", "yearlatest_median"], ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Loose", "yearlatest_median"], 
                xmax = scen[scen$scenario == "Median", "yearlatest_median"], ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Median", "yearlatest_median"], 
                xmax = scen[scen$scenario == "Stringent", "yearlatest_median"], ymin = -Inf, ymax = Inf),
            fill = "yellow", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Stringent", "yearlatest_median"], 
                xmax = 2021, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  #  geom_density(alpha=0.4, color = NA) +
  geom_bar(alpha=1, color = NA) + 
  labs(title = "Median of latest year (rounded)", x = "Median of latest year", y = "Number of indicators in CCDR") +
  geom_label(x = scen[scen$scenario == "Median", "yearlatest_median"] - 8, y = 55, 
             label = paste0(
               "Mean: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest_median) %>% summarise(mean = mean(yearlatest_median)),
               "\nNumber in red: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest_median) %>% summarise(num_red = sum(yearlatest_median < scen[scen$scenario == "Loose", "yearlatest_median"])),
               "\n Number in orange: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest_median) %>% summarise(num_orange = sum(yearlatest_median >= scen[scen$scenario == "Loose", "yearlatest_median"] & yearlatest_median < scen[scen$scenario == "Median", "yearlatest_median"])),
               "\n Number in yellow: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest_median) %>% summarise(num_orange = sum(yearlatest_median >= scen[scen$scenario == "Median", "yearlatest_median"] & yearlatest_median < scen[scen$scenario == "Stringent", "yearlatest_median"]))))

png("ccdr_criteria3.png")
crit3
dev.off()

# Criteria 4 yearlatest    
crit4 <- ccdrc %>% filter(yearfirst < 2021) %>% 
  ggplot(aes(x=yearlatest )) +
  geom_rect(aes(xmin = 2000, xmax = scen[scen$scenario == "Loose", "yearlatest"], ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Loose", "yearlatest"], 
                xmax = scen[scen$scenario == "Median", "yearlatest"], ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Median", "yearlatest"], 
                xmax = scen[scen$scenario == "Stringent", "yearlatest"], ymin = -Inf, ymax = Inf),
            fill = "yellow", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Stringent", "yearlatest"], 
                xmax = 2021, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  #  geom_density(alpha=0.4, color = NA) +
  geom_bar(alpha=1, color = NA)  + 
  labs(title = "Year latest", x = "Year latest", y = "Number of indicators in CCDR") +
  geom_label(x = scen[scen$scenario == "Median", "yearlatest"] - 10, y = 75, 
             label = paste0(
               "Mean: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest) %>% summarise(mean = mean(yearlatest)),
               "\nNumber in red: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest) %>% summarise(num_red = sum(yearlatest < scen[scen$scenario == "Loose", "yearlatest"])),
               "\n Number in orange: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest) %>% summarise(num_orange = sum(yearlatest >= scen[scen$scenario == "Loose", "yearlatest"] & yearlatest < scen[scen$scenario == "Median", "yearlatest"])),
               "\n Number in yellow: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(yearlatest) %>% summarise(num_orange = sum(yearlatest >= scen[scen$scenario == "Median", "yearlatest"] & yearlatest < scen[scen$scenario == "Stringent", "yearlatest"]))))

png("ccdr_criteria4.png")
crit4
dev.off()

# Criteria 5 span_years    
crit5 <- ccdrc %>% filter(yearfirst < 2021) %>% 
  ggplot(aes(x=span_years )) +
  geom_rect(aes(xmin = 0, xmax = scen[scen$scenario == "Loose", "span_years"], ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Loose", "span_years"], 
                xmax = scen[scen$scenario == "Median", "span_years"], ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Median", "span_years"], 
                xmax = scen[scen$scenario == "Stringent", "span_years"], ymin = -Inf, ymax = Inf),
            fill = "yellow", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Stringent", "span_years"], 
                xmax = 61, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  #  geom_density(alpha=0.4, color = NA) +
  geom_bar(alpha=1, color = NA)  + 
  labs(title = "Span of years", x = "Span of years", y = "Number of indicators in CCDR") +
  geom_label(x = scen[scen$scenario == "Median", "span_years"] + 30, y = 50, 
             label = paste0(
               "Mean: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(span_years) %>% summarise(mean = mean(span_years)),
               "\nNumber in red: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(span_years) %>% summarise(num_red = sum(span_years < scen[scen$scenario == "Loose", "span_years"])),
               "\n Number in orange: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(span_years) %>% summarise(num_orange = sum(span_years > scen[scen$scenario == "Loose", "span_years"] & span_years < scen[scen$scenario == "Median", "span_years"])),
               "\n Number in yellow: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(span_years) %>% summarise(num_orange = sum(span_years > scen[scen$scenario == "Median", "span_years"] & span_years < scen[scen$scenario == "Stringent", "span_years"]))))

png("ccdr_criteria5.png")
crit5
dev.off()

# Criteria 6 nonmiss    
crit6_dens <- ccdrc %>% filter(yearfirst < 2021) %>% 
  ggplot(aes(x=nonmiss)) +
  geom_rect(aes(xmin = 0, xmax = scen[scen$scenario == "Loose", "nonmiss"], ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Loose", "nonmiss"], 
                xmax = scen[scen$scenario == "Median", "nonmiss"], ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Median", "nonmiss"], 
                xmax = scen[scen$scenario == "Stringent", "nonmiss"], ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Stringent", "nonmiss"], 
                xmax = 100, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  geom_density(alpha=1, color = "black")  + 
  labs(title = "Share of lmic countries covered (%)", x = "Share of lmic covered(%)", y = "Number of indicators in CCDR") +
  geom_label(x = scen[scen$scenario == "Median", "nonmiss"] - 22, y = 25, 
             label = paste0(
               "Mean: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(mean = mean(nonmiss)),
               "\nNumber in red: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(num_red = sum(nonmiss < scen[scen$scenario == "Loose", "nonmiss"])),
               "\n Number in orange: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(num_orange = sum(nonmiss > scen[scen$scenario == "Loose", "nonmiss"] & nonmiss < scen[scen$scenario == "Median", "nonmiss"])),
               "\n Number in yellow: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(num_orange = sum(nonmiss > scen[scen$scenario == "Median", "nonmiss"] & nonmiss < scen[scen$scenario == "Stringent", "nonmiss"]))))

crit6 <- ccdrc %>% filter(yearfirst < 2021) %>% 
  mutate(nonmiss = round(nonmiss)) %>% ggplot(aes(x=nonmiss)) +
  geom_rect(aes(xmin = 0, xmax = scen[scen$scenario == "Loose", "nonmiss"], ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Loose", "nonmiss"], 
                xmax = scen[scen$scenario == "Median", "nonmiss"], ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Median", "nonmiss"], 
                xmax = scen[scen$scenario == "Stringent", "nonmiss"], ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  geom_rect(aes(xmin = scen[scen$scenario == "Stringent", "nonmiss"], 
                xmax = 100, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.03) +
  geom_bar(alpha=1, color = NA) +
  labs(title = "Share of lmic countries covered (%)", x = "Share of lmic covered(%)", y = "Number of indicators in CCDR") +
  geom_label(x = 50, y = 75, 
             label = paste0(
               "Mean: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(mean = mean(nonmiss)),
               "\nNumber in red: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(num_red = sum(nonmiss < scen[scen$scenario == "Loose", "nonmiss"])),
               "\n Number in orange: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(num_orange = sum(nonmiss > scen[scen$scenario == "Loose", "nonmiss"] & nonmiss < scen[scen$scenario == "Median", "nonmiss"])),
               "\n Number in yellow: ", 
               ccdrc %>% filter(yearfirst < 2021) %>% select(nonmiss) %>% summarise(num_orange = sum(nonmiss > scen[scen$scenario == "Median", "nonmiss"] & nonmiss < scen[scen$scenario == "Stringent", "nonmiss"]))))

png("ccdr_criteria6.png")
crit6
dev.off()

# Number of countries
ggplot(combinedc, aes(x=n_country , fill=database)) +
  geom_rect(aes(xmin = 0, xmax = 35, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = 35, xmax = 50, ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = 50, xmax = 65, ymin = -Inf, ymax = Inf),
            fill = "greenyellow", alpha = 0.03) +
  geom_rect(aes(xmin = 65, xmax = 220, ymin = -Inf, ymax = Inf),
            fill = "chartreuse4", alpha = 0.03) +
  geom_density(alpha=0.4, color = NA) +
  geom_vline(aes(xintercept=43),
             linetype="dashed", lwd = 1) +
  geom_text(aes(x=43,y=0.005,label="43 countries"), vjust=0)

#### Scoring ####
#ccdrc
summary(ccdrc$n_country)
summary(ccdrc$p_lmic)
summary(ccdrc$yearlatest)
summary(ccdrc$yearlatest_median)
summary(ccdrc$span_years)
summary(ccdrc$nonmiss)
summary(ccdrc$uniquevisitors)

score_table <- ccdrc  %>% filter(yearfirst < 2021) %>%
  # Divyanshi's proposal
  mutate(ncountry_score_1 = n_country/217*100, # share of max. number of countries
         plmic_score_1 = p_lmic, # share of lmic countries
         yearlatest_score_1 = (yearlatest - 2008)/(2020-2008)*100, 
         yearlatest_median_score_1 = (yearlatest_median - 2001)/(2020-2001)*100,
         span_years_score_1 = span_years/(2020-1960)*100,
         nonmiss_score_1 = nonmiss,
         uniquevisitors_score_1 = ntile(uniquevisitors, 100),
         totalscore_1 = (ncountry_score_1 + plmic_score_1 + yearlatest_score_1 + yearlatest_median_score_1 + 
                           span_years_score_1 + nonmiss_score_1 + uniquevisitors_score_1)/7
  ) %>%
  # Brian's proposal
  mutate(
    ncountry_score_2=case_when(
      n_country < scen$n_country[1] ~ 1,
      between(n_country,scen$n_country[1],scen$n_country[2]) ~ 2,
      between(n_country,scen$n_country[2],scen$n_country[3]) ~ 3,
      n_country >= scen$n_country[3] ~ 4
    )   ,
    plmic_score_2=case_when(
      p_lmic < scen$p_lmic[1] ~ 1,
      between(p_lmic,scen$p_lmic[1],scen$p_lmic[2])  ~ 2,
      between(p_lmic,scen$p_lmic[2],scen$p_lmic[3]) ~ 3,
      p_lmic >= scen$p_lmic[3] ~ 4
    )   ,
    yearlatest_median_score_2=case_when(
      yearlatest_median < scen$yearlatest_median[1] ~ 1,
      between(yearlatest_median,scen$yearlatest_median[1],scen$yearlatest_median[2])  ~ 2,
      between(yearlatest_median,scen$yearlatest_median[2],scen$yearlatest_median[3]) ~ 3,
      yearlatest_median >= scen$yearlatest_median[3] ~ 4
    )   ,
    yearlatest_score_2=case_when(
      yearlatest < scen$yearlatest[1] ~ 1,
      between(yearlatest,scen$yearlatest[1],scen$yearlatest[2])  ~ 2,
      between(yearlatest,scen$yearlatest[2],scen$yearlatest[3]) ~ 3,
      yearlatest >= scen$yearlatest[3] ~ 4
    )   ,
    span_years_score_2=case_when(
      span_years < scen$span_years[1] ~ 1,
      between(span_years,scen$span_years[1],scen$span_years[2])  ~ 2,
      between(span_years,scen$span_years[2],scen$span_years[3]) ~ 3,
      span_years >= scen$span_years[3] ~ 4
    )   ,
    uniquevisitors_score_2=case_when(
      uniquevisitors < scen$uniquevisitors[1] ~ 1,
      between(uniquevisitors,scen$uniquevisitors[1],scen$uniquevisitors[2])  ~ 2,
      between(uniquevisitors,scen$uniquevisitors[2],scen$uniquevisitors[3]) ~ 3,
      uniquevisitors >= scen$uniquevisitors[3] ~ 4
    )   ,
    nonmiss_score_2=case_when(
      nonmiss < scen$nonmiss[1] ~ 1,
      between(nonmiss,scen$nonmiss[1],scen$nonmiss[2])  ~ 2,
      between(nonmiss,scen$nonmiss[2],scen$nonmiss[3]) ~ 3,
      nonmiss >= scen$nonmiss[3] ~ 4
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
  # number of flags loose, median, stringent
  mutate(
    numflags_loose     = (ncountry_score_2 == 1) + (plmic_score_2 == 1) + (yearlatest_score_2 == 1) +
      (yearlatest_median_score_2 == 1) + (span_years_score_2 == 1) +
      (nonmiss_score_2 == 1) + (uniquevisitors_score_2 == 1),
    numflags_median    = (ncountry_score_2 <= 2) + (plmic_score_2 <= 2) + (yearlatest_score_2 <= 2) +
      (yearlatest_median_score_2 <= 2) + (span_years_score_2 <= 2) +
      (nonmiss_score_2 <= 2) + (uniquevisitors_score_2 <= 2),
    numflags_stringent = (ncountry_score_2 <= 3) + (plmic_score_2 <= 3) + (yearlatest_score_2 <= 3) +
      (yearlatest_median_score_2 <= 3) + (span_years_score_2 <= 3) +
      (nonmiss_score_2 <= 3) + (uniquevisitors_score_2 <= 3)
  ) %>%
  mutate(
    totalscore_2 = (ncountry_score_2 + plmic_score_2 + yearlatest_score_2 + yearlatest_median_score_2 + 
                      span_years_score_2 + nonmiss_score_2 + uniquevisitors_score_2)/7,
  ) %>%
  select(Indicator.Code, n_country, p_lmic, yearlatest_median, yearlatest, nonmiss,
         span_years, uniquevisitors, loose, median, stringent, totalscore_1, totalscore_2,
         numflags_loose, numflags_median, numflags_stringent) %>%
  arrange(totalscore_2)

score_table$numflags_loose # number of flags not passing loose criteria
score_table$numflags_stringent # number of flags not passing stringent criteria
score_table$numflags_median # number of flags not passing median criteria

ggplot(score_table, aes(x=totalscore_2, y = totalscore_1)) +
  geom_point(size=2) +
  labs(title = "Scatterplot between different scoring", 
       x = "Threshold scoring", 
       y = "Distance to frontier scoring")

#### Plotting scoring ####
# Number of flags for loose, median, stringent
score_table$numflags_loose # number of flags not passing loose criteria
score_table$numflags_stringent # number of flags not passing stringent criteria
score_table$numflags_median # number of flags not passing median criteria

  geom_density(alpha = 0.4, color = "black") +
  labs(title = "Distance to frontier scoring", 
       x = "Distance to frontier score")

ggplot(score_table, aes(x=totalscore_2)) +
  geom_density(alpha = 0.4, color = "black") +
  labs(title = "Threshold scoring", 
       x = "Threshold score")

ggplot(score_table, aes(x=numflags_loose)) +
  geom_bar(alpha = 0.4, color = "black") +
  labs(title = "Loose thresholds", 
       x = "Number of flags 'on'",
       y = "Number of indicators")

ggplot(score_table %>% mutate(numflags_median = numflags_median - 1), aes(x=numflags_median)) +
  geom_bar(alpha = 0.4, color = "black") +
  labs(title = "Median thresholds", 
       x = "Number of flags 'on'",
       y = "Number of indicators")

ggplot(score_table %>% mutate(numflags_stringent = numflags_stringent - 1), aes(x=numflags_stringent)) +
  geom_bar(alpha = 0.4, color = "black") +
  labs(title = "Stringent thresholds", 
       x = "Number of flags 'on'",
       y = "Number of indicators")

table(score_table$numflags_loose)
barplot(score_table$numflags_loose)


ggplot(combinedc, aes(x=n_country , fill=database)) +
  geom_rect(aes(xmin = 0, xmax = 35, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  geom_rect(aes(xmin = 35, xmax = 50, ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.03) +
  geom_rect(aes(xmin = 50, xmax = 65, ymin = -Inf, ymax = Inf),
            fill = "greenyellow", alpha = 0.03) +
  geom_rect(aes(xmin = 65, xmax = 220, ymin = -Inf, ymax = Inf),
            fill = "chartreuse4", alpha = 0.03) +
  geom_density(alpha=0.4, color = NA) +
  geom_vline(aes(xintercept=43),
             linetype="dashed", lwd = 1) +
  geom_text(aes(x=43,y=0.005,label="43 countries"), vjust=0)

write.csv(ccdrc, "ccdrc.csv")
write.csv(ccdry, "ccdry.csv")
write.csv(ccdri, "ccdri.csv")
write.csv(ccdrit, "ccdrit.csv")
write.csv(ccdrt, "ccdrt.csv")
write.csv(ccdryc, "ccdryc.csv")
write.csv(ccdrtt, "ccdrtt.csv")
write.csv(score_table, "ccdr_score_table.csv")
write.csv(combinedc, "combinedc.csv")

# Combine results and export as xlsx
colnames(ccdrc)
dim(ccdrc)
colnames(seriesmeta)
dim(seriesmeta)
colnames(topics)
dim(topics)
colnames(score_table)
dim(score_table)

export_file <- seriesmeta %>% 
  merge(topics, by = "Indicator.Code", all.x = T) %>%
  merge(ccdrc, by = "Indicator.Code", all.x = T) %>%
  merge(score_table[, c("Indicator.Code", "loose", "median", "stringent", "totalscore_1",      
                        "totalscore_2", "numflags_loose", "numflags_median", "numflags_stringent")],
        by = "Indicator.Code", all.x = T)
dim(export_file)
colnames(export_file)
#writexl::write_xlsx(export_file, "CCDR_criteria_overview_xx.xlsx")

mutate(ncountry_score = n_country/217*100, # share of max. number of countries
       plmic_score = p_lmic, # share of lmic countries
       yearlatest_score = (yearlatest - 2008)/(2020-2008)*100, 
       yearlatestmedian_score = (yearlatest_median - 2001)/(2020-2001)*100,
       spanyears_score = span_years/(2020-1960)*100,
       nonmiss_score = nonmiss,
       uniquevisitors_score = ntile(uniquevisitors, 100),
       totalscore = (ncountry_score + plmic_score + yearlatest_score + yearlatestmedian_score + spanyears_score + 
                       nonmiss_score + uniquevisitors_score) / 7)
       
       
       
       
       DT::datatable(score_table,
                     rownames=FALSE,
                     colnames = c("Indicator Code", "Indicator", "No. Countries", 'LMIC Coverage (%)', 'Last Year (Median)', 
                                  'Last Year (Max)', 'Share of non-missing data', 'No. Years Spanned', 
                                  'Unique visitors', 'Topic', "Fails Loose Criteria?", "Fails Median Criteria?", "Fails Stringent Criteria?", "Overall Score", "Divyanshi Score"),
                     class='cell-border stripe',
                     escape = FALSE,
                     extensions = c ('Buttons', 'FixedHeader'), 
                     options=list(
                       dom = 'Bfrtip',
                       paging = FALSE,
                       buttons = c('copy', 'csv', 'excel'),
                       scroller = T,
                       scrollX = T,
                       scrollY = T
                     )
       ) %>%
         formatStyle(columns=c('loose', 'median', 'stringent'), 
                     backgroundColor = styleEqual(
                       levels=c("Yes", "No"),
                       values=c("#ae2012", "#005f73")
                     ),
                     color="white"
         ) %>%
         formatRound(columns=c('n_country', 'p_lmic', 'nonmiss', 'overall_score', 'totalscore'), digits=2) 
       
       #### Collect all daa at indicator level ####
       
       
       
       #############################################//Part II. Adobe Analytics//#########################################################
       # References: 
       # https://www.rdocumentation.org/packages/WDI/versions/2.6.0/source
       # https://randyzwitch.com/rsitecatalyst/
       
       # Adobe analytics API login info
       SCAuth("ADOBE_SUPPORT_USER:World Bank Group", "99dc916a8f4a73fef13cbc3041299aa0")
       
       # Get indicator list
       dat <- ccdrc %>%
         select("Indicator.Code")
       
       # get the Adobe analytics data for 1599 indicators
       pageName = paste("en:wb:datamain:/indicator/", dat$Indicator.Code, sep="")
       pageviews_visits = QueueTrended("wbgglobalprod", "2020-1-01", "2020-12-31", 
                                       date.granularity = "year",
                                       c("uniquevisitors", "pageviews", "visits"), 
                                       "page", selected = pageName)
       
       pageviews_visits = QueueTrended("wbgglobalprod", "2020-1-01", "2020-12-31", 
                                       date.granularity = "year",
                                       c("uniquevisitors", "pageviews", "visits"), 
                                       "page", selected = "AG.CON.FERT.ZS")
       
       # data frame with 0 columns and 0 rows
       
       # WDI_Adobe <- data.frame(
       #   Indicator.Code = dat$Indicator.Code,
       #   uniquevisitors = pageviews_visits$uniquevisitors,
       #   pageviews = pageviews_visits$pageviews,
       #   visitors = pageviews_visits$visits
       # )
       
       WDI_Adobe <- data.frame(
         Indicator.Code = dat$Indicator.Code,
         uniquevisitors = rep(100, length(dat$Indicator.Code)),
         pageviews = rep(100, length(dat$Indicator.Code)),
         visitors = rep(100, length(dat$Indicator.Code)))
       
       ccdrc <- merge(ccdrc, WDI_Adobe, by = "Indicator.Code")
       
       ccdrc <- merge(ccdrc, seriesmeta, by = "Indicator.Code", all.x = T)
       ccdry <- merge(ccdry, seriesmeta, by = "Indicator.Code", all.x = T)
       
       write.csv(ccdrc, "ccdrc.csv")
       write.csv(ccdry, "ccdry.csv")
       write.csv(ccdri, "ccdri.csv")
       write.csv(ccdrit, "ccdrit.csv")
       write.csv(ccdrt, "ccdrt.csv")
       write.csv(ccdryc, "ccdryc.csv")
       write.csv(ccdrtt, "ccdrtt.csv")
       
       ## Code to fill in some missing data! No need to run.
       ## Only to keep track on what I had done. Mostly to update indicators for which we didn't have classifications or certain metadata
       # 
       # wdicnull <- wdic %>%
       #   filter(is.na(datatopic) | is.na(type)) %>%
       #   select("Indicator.Code", "Indicator.Name", "datatopic", "type", "primary")
       # 
       # write.csv(wdicnull, "wdicnull.csv")
       # 
       # wdi1 <- read_excel("C:/Users/wb482273/Downloads/WDI_topics_db ds wdi.xlsx")
       # wdi2 <- read_excel("C:/Users/wb482273/Downloads/WDI_metrics_coverage_fusion_rev.xlsx")
       # wdinull <- read_excel("C:/Users/wb482273/Downloads/wdicnull.xlsx")
       # 
       # wdi2$indicatorcode <- toupper(wdi2$indicatorcode)
       # wdi1 <- wdi1 %>%
       #   rename("indicatorcode" = "debt s")
       # 
       # wdi3 <- merge(wdi2, wdi1, by = "indicatorcode", all.x = T)
       # 
       # wdi3 <- wdi3 %>%
       #   select(indicatorcode, type, primary, indicatorname.x, `Topic in Datasite`) %>%
       #   rename("Indicator.Code" = "indicatorcode",
       #          "Indicator.Name" = "indicatorname.x",
       #          "datatopic" = "Topic in Datasite") %>%
       #   filter(!duplicated(Indicator.Code))
       # 
       # wdi4 <- wdi3 %>%
       #   merge(wdinull, by = "Indicator.Code", all = T) %>%
       #   mutate(type.x = ifelse(is.na(type.x), type.y, type.x),
       #          primary.x = ifelse(is.na(primary.x), primary.y, primary.x),
       #          Indicator.Name.x = ifelse(is.na(Indicator.Name.x), Indicator.Name.y, Indicator.Name.x),
       #          datatopic.x = ifelse(is.na(datatopic.x), datatopic.y, datatopic.x),
       #          datatopic.x = ifelse(datatopic.x == "NA", NA, datatopic.x)) %>%
       #   rename("type" = "type.x",
       #          "Indicator.Name" = "Indicator.Name.x",
       #          "primary" = "primary.x",
       #          "datatopic" = "datatopic.x") %>%
       #   select(Indicator.Code, Indicator.Name, type, primary, datatopic)
       # 
       # write.csv(wdi4, "wdi-series-meta.csv")
       # 
       # 
       