# ==============================================================================
# Create project in metadata editor for new EDGAR/Grassi emissions indicators
# October 18, 2024
# Adding following indicators: all EN.GHG.* indicators
# 
# ==============================================================================
library(rlist)
library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(metadataeditr)

# Link to API documentation of metadata editor
# https://metadataeditorqa.worldbank.org/api-documentation/editor/

rm(list=ls())

# --- NADA API credentials and catalog URL -------------------------------------
my_keys <- read.csv("C:/Users/wb460271/OneDrive - WBG/Documents/TB_NADA_API_keys.csv", 
                    header=F, stringsAsFactors=F)
set_api_key(my_keys[9,1])  # for metadataeditorqa
set_api_url("https://metadataeditorqa.worldbank.org/index.php/api/")
set_api_verbose(TRUE)
# ------------------------------------------------------------------------------

#### read in metadata for DCS from spreadsheet ####
read.xlsx()


#### Get WDI metadata ##########################################################
# Read all WDI data (csv from Sep 27, 2023)
WDI_data <- read.csv("C:/Users/wb460271/OneDrive - WBG/Documents/WDI - links and docs/WDI_metadata_schema/WDI_CSV_20230927/WDIData.csv")
dim(WDI_data)

# API call for metadata, example indicator SP.POP.TOTL
# https://api.worldbank.org/v2/sources/2/series/SP.POP.TOTL/metadata?format=JSON
api_meta_prefix <- "https://api.worldbank.org/v2/sources/2/series/"
api_meta_suffix <- "/metadata?format=JSON"

# Get list of all WDI indicators
all_indicators <- jsonlite::fromJSON("https://api.worldbank.org/v2/sources/2/indicator?per_page=1500&format=json")
all_indicators_id <- all_indicators[[2]]$id

# Create first dataframe
indic_code <- all_indicators_id[1]

url_meta_JSON <- paste0(api_meta_prefix, indic_code, api_meta_suffix)

cur_metadata <- jsonlite::fromJSON(url_meta_JSON)

# Get data for current indicator - subset data for current indicator
cur_data <- WDI_data %>% filter(Indicator.Code == indic_code)

# Extract first year with data and last year with data for each indicator
cur_data <- remove_empty(cur_data, which = "cols")

# All years with at least one non-missing value
cur_years <- as.numeric(substring(colnames(cur_data)[which(startsWith(colnames(cur_data), "X"))], 2, 5))

# Extract all countries/regions with data for this indicator
cur_countries <- cur_data[rowSums(is.na(cur_data)) != length(cur_years), "Country.Code"]  # only keep countries/regions with at least one non-missing value

# Dataframe with start/end year and list of all countries/regions
cur_years_geo <- data.frame(id = c("first_year", "last_year", "countries"), 
                            value = c(min(cur_years), max(cur_years), paste(cur_countries, collapse = "; ")))

# Add series id and cur_years_geo to metadata
metadata_extracted <- rbind(data.frame(id = "series_id", value = indic_code), 
                            ((((cur_metadata$source)[[3]][[1]])[[2]][[1]])[[2]][[1]]),
                            cur_years_geo)

# Collect metadata for each indicator
for (i in 2:length(all_indicators_id)) {
  #for (i in 2:3) {
  print(i)
  # current indicator code
  indic_code <- all_indicators_id[i]
  
  # API url for metadata
  url_meta_JSON <- paste0(api_meta_prefix, indic_code, api_meta_suffix)
  
  # Get metadata from API
  cur_metadata <- jsonlite::fromJSON(url_meta_JSON)
  
  # Get data for current indicator - subset data for current indicator
  cur_data <- WDI_data %>% filter(Indicator.Code == indic_code)
  
  # Extract first year with data and last year with data for each indicator
  cur_data <- remove_empty(cur_data, which = "cols")
  
  # All years with at least one non-missing value
  cur_years <- as.numeric(substring(colnames(cur_data)[which(startsWith(colnames(cur_data), "X"))], 2, 5))
  
  # Extract all countries/regions with data for this indicator
  cur_countries <- cur_data[rowSums(is.na(cur_data)) != length(cur_years), "Country.Code"]  # only keep countries/regions with at least one non-missing value
  
  # Dataframe with start/end year and list of all countries/regions
  cur_years_geo <- data.frame(id = c("first_year", "last_year", "countries"), 
                              value = c(min(cur_years), max(cur_years), paste(cur_countries, collapse = "; ")))
  
  # Add series id and cur_years_geo to metadata
  cur_metadata_extracted <- rbind(data.frame(id = "series_id", value = indic_code), 
                                  ((((cur_metadata$source)[[3]][[1]])[[2]][[1]])[[2]][[1]]),
                                  cur_years_geo)
  cur_metadata_extracted
  
  colnames(cur_metadata_extracted) <- c("id", paste0("value_", i))
  
  # Add metadata for current indicator to metadata_extracted
  metadata_extracted <- merge(metadata_extracted, cur_metadata_extracted, by = "id", all = TRUE)
}

dim(metadata_extracted)
#View(metadata_extracted)

metadata_extracted_t <- t(metadata_extracted)
colnames(metadata_extracted_t) <- metadata_extracted_t[1,]
metadata_extracted_t <- metadata_extracted_t[-1,]

metadata_extracted_t <- as.data.frame(metadata_extracted_t)

dim(metadata_extracted_t)
#View(metadata_extracted_t)

dbid = "WDI"
api_data = "https://api.worldbank.org/v2/countries/all/indicators/"
api_meta = "https://api.worldbank.org/v2/sources/2/series/"

# Read country metadata
country_meta <- read.csv("C:/Users/wb460271/OneDrive - WBG/Documents/WDI - links and docs/WDI_metadata_schema/WDI_CSV_20230927/WDICountry.csv")
dim(country_meta)
# add column Type (country/region)
country_meta$Type <- "Country"
country_meta[country_meta$Region == "" , "Type"] <- "Region"
table(country_meta$Type)
#View(country_meta[, c("Short.Name", "Type")])

country_meta_short <- country_meta[, c("Short.Name", "Country.Code", "Type")]
colnames(country_meta_short) <- c("name", "code", "type")

##### Get list of missing indicators to be added ###############################
list_projects <- metadataeditr::list_projects(limit = 2000)
list_projects$status_code

table(metadata_extracted_t$series_id %in% list_projects$response$projects$study_idno)
metadata_to_be_added <- metadata_extracted_t[which(!(metadata_extracted_t$series_id %in% list_projects$response$projects$study_idno)),]
metadata_to_be_added[, c("series_id", "IndicatorName")]

# Create entry for each indicator
for (i in 1:nrow(metadata_to_be_added)) {
  #for (i in 1:1) {
  # Generate series metadata
  
  indic_code <- metadata_to_be_added[i, "series_id"]
  
  this_series <- list(
    series_description = list(
      idno                   = metadata_to_be_added[i, "series_id"],
      name                   = metadata_to_be_added[i, "IndicatorName"],
      database_id            = dbid,
      languages              = list(list(name = "English", code = "EN")),
      measurement_unit       = metadata_to_be_added[i, "Unitofmeasure"],
      dimensions             = list(list(label = "Year"),
                                    list(label = "Geographic area", description = "Country/economy, country group, region")),
      periodicity            = metadata_to_be_added[i, "Periodicity"],
      base_period            = metadata_to_be_added[i, "BasePeriod"],
      #definition_short       = "",
      definition_long        = metadata_to_be_added[i, "Longdefinition"],
      methodology            = metadata_to_be_added[i, "Statisticalconceptandmethodology"],
      limitation             = metadata_to_be_added[i, "Limitationsandexceptions"],
      topics                 = list(list(name = metadata_to_be_added[i, "Topic"],
                                         vocabulary = "WDI topics" )), #list(list(name = series$Topic[i], vocabulary = "WDI topics")),
      relevance              = metadata_to_be_added[i, "Developmentrelevance"],
      time_periods           = list(list(start = metadata_to_be_added[i, "first_year"],
                                         end = metadata_to_be_added[i, "last_year"])),
      #geographic_units       = as.list(strsplit(metadata_to_be_added[i,"countries"], "; ")[[1]]), #geo_l,
      aggregation_method     = metadata_to_be_added[i, "Aggregationmethod"],
      license                = list(list(name = "CC BY-4.0", uri = "https://creativecommons.org/licenses/by/4.0/")),
      links                  = list(
        list(type="WDI API", description = "Data in JSON",
             uri = paste0(api_data, tolower(indic_code), "?date=", metadata_to_be_added[i, "first_year"], ":",
                          metadata_to_be_added[i, "last_year"], "&format=json")),
        list(type="WDI API", description = "Data in XML",
             uri = paste0(api_data, tolower(indic_code), "?date=", metadata_to_be_added[i, "first_year"], ":",
                          metadata_to_be_added[i, "last_year"])),
        list(type="WDI API", description = "Metadata in JSON",
             uri = paste0(api_meta, tolower(indic_code), "/metadata?format=json")),
        list(type="WDI API", description = "Metadata in XML",
             uri = paste0(api_meta, tolower(indic_code), "/metadata"))),
      api_documentation      = list(
        list(
          description = "Developer Information webpage (detailed documentation of the WDI API)",
          uri = "https://datahelpdesk.worldbank.org/knowledgebase/topics/125589-developer-information")
      ),
      sources                = list(list(name = metadata_to_be_added[i, "Source"])),
      #sources_note           = metadata_to_be_added[i, "Notesfromoriginalsource"],
      #keywords               = keyw,
      notes                  = metadata_to_be_added[i, "Generalcomments"]
    )
  )
  
  #result<-nada_http_post(
  #  url = "editor/create/timeseries",
  #  options = this_series)
  
  
  url = "https://metadataeditorqa.worldbank.org/index.php/api/editor/create/timeseries"
  #url = "https://dev.ihsn.org/editor/index.php/api/editor/create/timeseries"
  result <- POST(url,
                 add_headers("X-API-KEY" = my_keys[9,1]),
                 body = this_series,
                 content_type_json(),
                 encode="json",
                 accept_json()
  )
  
  content(result,"text")
}

########### List all collections and projects in catalog #######################
# List all collections in catalog
all_collections <- GET("https://metadataeditorqa.worldbank.org/index.php/api/collections",
                       add_headers("X-API-KEY" = my_keys[9,1]), 
                       content_type_json(),
                       encode="json", 
                       accept_json())
all_collections$status_code
all_collections_unpacked <- jsonlite::fromJSON(content(all_collections,"text"))
all_collections_unpacked$collections

# List all projects in catalog
# Specify limit
all_projects <- GET("https://metadataeditorqa.worldbank.org/index.php/api/editor?limit=2000",
                    add_headers("X-API-KEY" = my_keys[9,1]),
                    encode="json",
                    accept_json())

#fromJSON(content(all_projects,"text"))
all_projects_unpacked <- jsonlite::fromJSON(content(all_projects,"text"))
View(all_projects_unpacked$projects)
dim(all_projects_unpacked$projects)
colnames(all_projects_unpacked$projects)
class(all_projects_unpacked$projects)
all_projects_df <- all_projects_unpacked$projects

# 1477 indicators on December 15, 2023
table(all_projects_df[, "study_idno"] %in% metadata_extracted_t$series_id)
table(metadata_extracted_t$series_id %in% all_projects_df[, "study_idno"])

# Add project id (id) and idno to metadata_extracted
metadata_extracted_t <- merge(metadata_extracted_t, all_projects_df[, c("study_idno", "id", "idno")], 
                              by.x = "series_id", by.y = "study_idno", 
                              all.x = T, all.y = F)
dim(metadata_extracted_t)

##### Update projects ##########################################################
i=5
for(i in 1:nrow(metadata_extracted_t)){
  print(i)
  project_id   <- metadata_extracted_t[i, "id"] 
  indic_code <- metadata_extracted_t[i, "series_id"]
  print(indic_code)
  indic_name <- metadata_extracted_t[i, "IndicatorName"]
  indic_name
  
  # Country and geo areas objects
  country_region_list_cur <- str_split_1(metadata_extracted_t[i, "countries"], "; ")
  region_meta <- country_meta_short %>% filter(code %in% country_region_list_cur)
  country_meta <- region_meta %>% filter(type == "Country") %>% select(name, code)
  
  # res_1 <- GET(paste0("https://metadataeditorqa.worldbank.org/index.php/api/editor?keywords=\"", indic_code, "\""),
  #     add_headers("X-API-KEY" = my_keys[9,1]))
  # res_1_unpacked <- fromJSON(content(res_1,"text"))
  # res_1_unpacked$total
  
  # updated_series <- list(
  #   series_description = list(definition_long  = metadata_extracted_t[i, "Longdefinition"]
  #   )
  # )
  
  # Need to specify all fields otherwise get overwritten in editor
  updated_series <- list(
    series_description = list(
      idno                   = metadata_extracted_t[i, "series_id"],
      name                   = metadata_extracted_t[i, "IndicatorName"],
      database_id            = dbid,
      languages              = list(list(name = "English", code = "EN")),
      measurement_unit       = metadata_extracted_t[i, "Unitofmeasure"],
      dimensions             = list(list(label = "Year"),
                                    list(label = "Geographic area", description = "Country/economy, country group, region")),
      periodicity            = metadata_extracted_t[i, "Periodicity"],
      base_period            = metadata_extracted_t[i, "BasePeriod"],
      definition_short       = "",
      definition_long        = metadata_extracted_t[i, "Longdefinition"],
      methodology            = metadata_extracted_t[i, "Statisticalconceptandmethodology"],
      limitation             = metadata_extracted_t[i, "Limitationsandexceptions"],
      topics                 = list(list(name = metadata_extracted_t[i, "Topic"],
                                         vocabulary = "WDI topics" )),
      relevance              = metadata_extracted_t[i, "Developmentrelevance"],
      time_periods           = list(list(start = metadata_extracted_t[i, "first_year"],
                                         end = metadata_extracted_t[i, "last_year"])),
      ref_country            = apply(country_meta, 1, as.list),
      geographic_units       = apply(region_meta, 1, as.list), #as.list(strsplit(metadata_extracted_t[i,"countries"], "; ")[[1]]), #geo_l,
      aggregation_method     = metadata_extracted_t[i, "Aggregationmethod"],
      license                = list(list(name = "CC BY-4.0", uri = "https://creativecommons.org/licenses/by/4.0/")),
      links                  = list(
        list(type="WDI API", description = "Data in JSON",
             uri = paste0(api_data, tolower(indic_code), "?date=", metadata_extracted_t[i, "first_year"], ":",
                          metadata_extracted_t[i, "last_year"], "&format=json")),
        list(type="WDI API", description = "Data in XML",
             uri = paste0(api_data, tolower(indic_code), "?date=", metadata_extracted_t[i, "first_year"], ":",
                          metadata_extracted_t[i, "last_year"])),
        list(type="WDI API", description = "Metadata in JSON",
             uri = paste0(api_meta, tolower(indic_code), "/metadata?format=json")),
        list(type="WDI API", description = "Metadata in XML",
             uri = paste0(api_meta, tolower(indic_code), "/metadata"))),
      api_documentation      = list(list(
        description = "Developer Information webpage (detailed documentation of the WDI API)",
        uri = "https://datahelpdesk.worldbank.org/knowledgebase/topics/125589-developer-information")),
      sources                = list(list(name = metadata_extracted_t[i, "Source"])),
      #sources_note           = metadata_extracted_t[i, "Notesfromoriginalsource"],
      #keywords               = keyw,
      notes                  = metadata_extracted_t[i, "Generalcomments"]
    )
  )
  
  
  # Endpoint: /editor/update/timeseries/{id}
  url_update = paste0("https://metadataeditorqa.worldbank.org/index.php/api/editor/update/timeseries/", project_id)
  result <- POST(url_update,
                 add_headers("X-API-KEY" = my_keys[9,1]),
                 body = updated_series,
                 content_type_json(),
                 encode="json",
                 accept_json()
  )
  content(result,"text")
  if (result$status_code != 200){
    break
  }
}
