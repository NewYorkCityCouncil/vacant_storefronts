# reese
# 6-13-22

# setup
source("00_load_dependencies.R")
library(vroom)
# Storefront Registration Class 2 and 4 Statistics
storereg_24 <- vroom("https://data.cityofnewyork.us/resource/dxru-eun8.csv?$limit=99999999999")
# Storefront Registration Statistics for Designated Class One
storereg_1 <- vroom("https://data.cityofnewyork.us/resource/x3n4-h56k.csv?$limit=99999999999")

# table stuff
total_19 <- storereg_24[storereg_24$aggregate_level_citywide == 'CITYWIDE',] %>% 
  group_by(reporting_year) %>% summarise(sum_storefronts = sum(total_storefronts))
total_20_21 <- storereg_24[storereg_24$aggregate_level_citywide == 'Census Tract',] %>% 
  group_by(reporting_year) %>% summarise(sum_storefronts = sum(total_storefronts))
reg_24_by_yr <- rbind(total_19, total_20_21)
reg_24_by_yr$type <- "class 2 and 4"
rm(total_19)
rm(total_20_21)

# table stuff
total_19_2 <- storereg_1[storereg_1$aggregate_level_citywide == 'CITYWIDE',] %>% 
  group_by(reporting_year) %>% summarise(sum_storefronts = sum(total_storefronts))
total_20_21_2 <- storereg_1[storereg_1$aggregate_level_citywide == 'Citywide',] %>% 
  group_by(reporting_year) %>% summarise(sum_storefronts = sum(total_storefronts))
reg_1_by_yr <- rbind(total_19_2, total_20_21_2)
reg_1_by_yr$type <- "class 1"
rm(total_19_2)
rm(total_20_21_2)

# table of all registrations (citywide) by year
all_regs <- rbind(reg_24_by_yr, reg_1_by_yr) # basically nearly all regs were 2/4

storereg_24$aggregate_level_id <- as.numeric(storereg_24$aggregate_level_id)
storereg_24_cd <- storereg_24[grepl("DISTRICT",storereg_24$aggregate_level_citywide, ignore.case = TRUE),]

# messy cleanup merging 2/4 registrations w/ council district sf
temp <- storereg_24_cd[storereg_24_cd$reporting_year==2021,]
temp$pct_change <- temp$total_storefronts
temp <- temp[,c("pct_change","aggregate_level_id")]
temp <- temp %>% full_join( storereg_24_cd,
                             by=c('aggregate_level_id'))
temp$pct_change <- temp$pct_change/temp$total_storefronts
temp$pct_change <- temp$pct_change*100
# temp[grepl("DISTRICT",temp$aggregate_level_citywide, ignore.case = TRUE),]$aggregate_level_citywide <- "CounDist"
names(temp)[names(temp) == 'aggregate_level_id'] <- 'CounDist'
temp$CounDist <- as.integer(temp$CounDist)
temp_shp <- temp %>% full_join(y = council_districts, by = 'CounDist') %>%
  st_as_sf() %>% st_transform(crs = '+proj=longlat +datum=WGS84')
rm(temp)
# map 2/4 registration growth from 2019/2020 to 2021
# darker spots = more growth
pal = colorBin(
  palette = rev(nycc_pal("cool")(100)),
  domain = as.numeric(temp_shp[temp_shp$reporting_year==2019 | temp_shp$reporting_year==2020,]$pct_change),
  bins = 10,
  na.color = "White",
  reverse = TRUE)
leaflet() %>% addCouncilStyle(add_dists = TRUE) %>%
  addPolygons(
    data = temp_shp[temp_shp$reporting_year==2019,],
    col =  ~ pal(as.numeric(pct_change)),
    weight = 1,
    fillOpacity = .4,
    group="2019",
    popup = paste0(
      "<strong>Percent of 2021: </strong>",
      round(temp_shp[temp_shp$reporting_year==2019,]$pct_change,2),
      "% <br>"
    )) %>%
  addPolygons(
    data = temp_shp[temp_shp$reporting_year==2020,],
    col =  ~ pal(as.numeric(pct_change)),
    weight = 1,
    fillOpacity = .4,
    group="2020",
    popup = paste0(
      "<strong>Percent of 2021: </strong>",
      round(temp_shp[temp_shp$reporting_year==2020,]$pct_change,2),
      "% <br>"
    )) %>%
  addLegend("topleft", pal = pal, values = paste0(unique(temp_shp$pct_change), "%"), title="% of 2021 Total Storefronts") %>%
  addLayersControl(
    # baseGroups = c("Num"),
    overlayGroups = c("2019", "2020"),
    options = layersControlOptions(collapsed = FALSE)
  )

# map 1 registration growth from 2019/2020 to 2021
# storereg 1 has incomplete, very little data - do later if helpful