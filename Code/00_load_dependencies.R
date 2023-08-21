## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor", "lubridate",
                      "leaflet", "leaflet.extras", "htmlwidgets",
                      "skimr", "htmltools", "ggiraph", "gt", "gtExtras", 
                      "data.table", "sf", "leaflet", "cluster", "httr2",
                      "ggthemes", "zoo", "data.table", "mapview", "vroom",
                      "here"
)

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#councildown.check <- "councildown" %in% installed.packages()[,"Package"]
councilverse.check <- "councilverse" %in% installed.packages()[,"Package"]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
#lapply(list.of.packages, require, character.only = TRUE)
lapply(c(list.of.packages,"councilverse"), require, character.only = TRUE)

# remove created variables for packages
rm(list.of.packages,new.packages)

## Functions -----------------------------------------------

# geocoding function ------- 

Gcode <- function(Address, Boro){
  
 # require(RCurl)
  p1=Address
  p1=gsub(" ","%20",p1)
  p2=Boro
  p2=gsub(" ","%20",p2)
  u1=c('https://geosearch.planninglabs.nyc/v2/autocomplete?text=')
  u4="&size=1"
  url=paste(c(u1,p1,"%2C",p2,u4),collapse="")
  
  resp <- request(url) %>% 
    req_retry(backoff = ~ 5) %>% 
    req_perform(verbosity = 1) %>% 
    resp_body_json(simplifyVector = T)
  # 
  # XY = c(resp, ff)
  # 
  # names(XY)<-c("response", "features")
  
  return(resp)
  
}

## Common Datasets --------------------------------
# read in tracts
ct.shp <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Census_Tracts_for_2020_US_Census/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson") %>% clean_names()

# read in boro boundaries
boro.shp <- read_sf("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% # set to nyc projection
  st_simplify(dTolerance = .00001) # less precise to speed rendering

# Storefronts Reported Vacant or Not - https://data.cityofnewyork.us/City-Government/Storefronts-Reported-Vacant-or-Not/92iy-9c3n
vacant_dataset <- read.csv("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=999999999") %>% clean_names() 

# read in old Council district boundaries
council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>% 
  st_read() %>% 
  st_transform(st_crs(4326))