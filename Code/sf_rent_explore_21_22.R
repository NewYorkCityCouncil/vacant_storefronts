library(data.table)
library(sf)
library(leaflet)
library(censusapi)
library(dplyr)
library(councildown)
source("~/utils/unzip_files.R")

vs <- fread("data/missing_storefront_2.csv")

# vacant storefronts - median rent ----------------------------------------

# rentgrp <- listCensusMetadata(
#   name = "acs/acs5",
#   vintage = 2021,
#   type = "variables", 
#   group = "B25061")

# get median rent by ct 
rent_acs <- getCensus(
  # must add a census api key
  key = "8cdccf85df4c4c7cf6fadaca4006860333f8e592", 
  name = "acs/acs5",
  vintage = 2021,
  vars = "B25064_001E", 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

# ct shapes
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyct2020_22a.zip"
ct_shp <- sf::read_sf(unzip_sf(url)) %>%
  mutate(
    # match county numbers with those from the acs data 
    county = case_when(
      BoroCode == "1" ~ "061", 
      BoroCode == "2" ~ "005", 
      BoroCode == "3" ~ "047", 
      BoroCode == "4" ~ "081", 
      BoroCode == "5" ~ "085" 
    ), 
    # create GEO_ID to match acs data 
    GEO_ID = paste0("1400000US36", county, CT2020)
  )

rent_acs <- rent_acs %>%
  mutate(GEOID = paste0(state, county, tract)) %>%
  left_join(ct_shp %>% select(!c("county", "BoroCT2020", "BoroCode")), by = "GEOID") %>% 
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84")




