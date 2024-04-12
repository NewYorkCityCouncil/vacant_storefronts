library(driscoll)
library(geojsonsf)
source("round_2/code/00_load_dependencies.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/11/2024
#
# This file creates maps of vacant storefronts
########################################################################################


################################################################################
# get vacancy data in spatial format
################################################################################

vacant_dataset = vacant_dataset %>%
  filter(!is.na(latitude), # NA's are 2% of the dataset
         reporting_year == "2022 and 2023") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  mutate(vacant_on_12_31 = ifelse(vacant_on_12_31 == "YES", "vacant", "filled"))


################################################################################
# plot individual points of vacant and not vacant - NOT USEFUL 
################################################################################

pal = colorFactor(
  palette = pal_nycc("double"),
  domain = vacant_dataset$vacant_on_12_31
) 

# map individual dots - not useful
map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                 zoomControl = FALSE, 
                                 minZoom = 10, 
                                 maxZoom = 15)) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addCircles(data = vacant_dataset, weight = 3, radius = 135, 
             fillColor = ~pal(vacant_on_12_31), opacity = 0, fillOpacity = 0.8) %>%
  addLegend_decreasing(position = "topleft", pal, 
                       values = vacant_dataset$vacant_on_12_31, 
                       opacity = 1)


################################################################################
# plot council district level percentages - SEMI USEFUL 
################################################################################

vacant_count = st_intersects(council_districts, vacant_dataset %>% 
                               filter(vacant_on_12_31 == "vacant"))
vacant_count = sapply(vacant_count, length)

total_count = st_intersects(council_districts, vacant_dataset)
total_count = sapply(total_count, length)

council_districts$num_vacant = vacant_count
council_districts$num_total = total_count
council_districts = council_districts %>% 
  mutate(perc_vacant = num_vacant/num_total)

pal = colorNumeric(
  palette = pal_nycc("warm"),
  domain = c(0, council_districts$perc_vacant*100)
) 

pal = colorBin(
  palette = pal_nycc("warm"),
  bins = 5,
  domain = c(0, council_districts$perc_vacant*100)
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addPolygons(data = council_districts, fillColor = ~pal(perc_vacant*100), 
              weight = 0, fillOpacity = 1, smoothFactor = 0) %>%
  addCouncilStyle(add_dists = TRUE, highlight_dists = 4) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = council_districts$perc_vacant*100, 
                       opacity = 1, 
                       title = "% of Storefronts reporting <br> vacant on Dec 31st 2023") 

saveWidget(map, file=file.path("visuals", "vacant_cd_2023_map.html"))
mapview::mapshot(map, file=file.path("visuals", "vacant_cd_2023_map.pdf"))


################################################################################
# plot tract level percentages - LOOKS WEIRD because there's so many 100's
################################################################################


tracts = st_read("https://data.cityofnewyork.us/resource/63ge-mke6.geojson?$limit=5000")

vacant_count = st_intersects(tracts, vacant_dataset %>% 
                               filter(vacant_on_12_31 == "vacant"))
vacant_count = sapply(vacant_count, length)

total_count = st_intersects(tracts, vacant_dataset)
total_count = sapply(total_count, length)

tracts$num_vacant = vacant_count
tracts$num_total = total_count
tracts = tracts %>% 
  mutate(perc_vacant = num_vacant/num_total)

pal = colorBin(
  palette = "warm",
  bins = 5,
  domain = c(0, tracts$perc_vacant*100)
) 

map = leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addPolygons(data = tracts, fillColor = ~pal(perc_vacant*100), 
              weight = 0, fillOpacity = 1, smoothFactor = 0) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addLegend_decreasing(position = "topleft", pal,
                       values = tracts$perc_vacant*100, 
                       opacity = 1, 
                       title = "% of Storefronts reporting <br> vacant on Dec 31st 2023") 

saveWidget(map, file=file.path('visuals', 
                               "vacant_tract_2023_map.html"))
mapview::mapshot(map, file=file.path('visuals', 
                            "vacant_tract_2023_map.pdf"))

################################################################################
# identify streets with high % of vacancy
################################################################################

streets = st_read("https://data.cityofnewyork.us/resource/fwpa-qxaf.geojson?$limit=150000") %>%
  filter(category %in% c("Global", "Regional", "Neighborhood")) %>%
  group_by(category, boroname, street) %>%
  summarise(geometry = st_union(geometry))

pal = colorFactor(
  palette = c("lightgrey", "#2F56A6"),
  domain = c("existing pool", 
             "area that is both >15 minute walk from a pool <br>&emsp;&emsp;and an Environmental Justice area")
) 

breaks = classInt::classIntervals(no_use$new_users, n = 5, style = 'jenks')$brks
breaks[length(breaks)] = breaks[length(breaks)] + 5000
reds = c("#ff0000", "#ff8080", "#ffbfbf", "#fff6f6")

pal2 = colorBin(
  palette = rev(colorRampPalette(reds)(5)),
  bins = c(0, 25000, 50000, 75000, 100000, 155000),#round(breaks/5000) * 5000,
  domain = no_use$new_users
) 

map = leaflet::leaflet(options = leafletOptions(attributionControl=FALSE, 
                                       zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 15)) %>%
  addPolygons(data = interest_area, weight = 0, col = 'grey', 
              fillOpacity = 0.15) %>%
  addCouncilStyle(add_dists = TRUE) %>%
  addCircles(data = pools, weight = 3, radius = 75, col = '#2F56A6', 
             opacity = 1, fillOpacity = 1, popup = ~name) %>%
  addCircles(data = no_use[no_use$new_users > 100000, ], radius = 130, 
            fillOpacity = 1, fillColor = ~pal2(new_users), 
            opacity = 1, color = "#660000", weight = 0.5,
            popup = ~label, 
            group = "Potential sites where >100k people would gain pool access") %>%
  addCircles(data = no_use[no_use$new_users > 75000, ], radius = 130, 
             fillOpacity = 1, fillColor = ~pal2(new_users), 
             opacity = 1, color = "#660000", weight = 0.5,
             popup = ~label, 
             group = "Potential sites where >75k people would gain pool access") %>% 
  addCircles(data = no_use[no_use$new_users > 50000, ], radius = 130, 
             fillOpacity = 1, fillColor = ~pal2(new_users), 
             opacity = 1, color = "#660000", weight = 0.5,
             popup = ~label, 
             group = "Potential sites where >50k people would gain pool access") %>% 
  addCircles(data = no_use[no_use$new_users > 25000, ], radius = 130, 
             fillOpacity = 1, fillColor = ~pal2(new_users), 
             opacity = 1, color = "#660000", weight = 0.5,
             popup = ~label, 
             group = "Potential sites where >25k people would gain pool access") %>% 
  addCircles(data = no_use, radius = 130, 
             fillOpacity = 1, fillColor = ~pal2(new_users), 
             opacity = 1, color = "#660000", weight = 0.5,
             popup = ~label, 
             group = "All potential sites") %>% 
  addLegend_decreasing(position="topleft", pal, 
                       values = c("existing pool", 
                                  "area that is both >15 minute walk from a pool <br>&emsp;&emsp;and an Environmental Justice area"), 
                       opacity = 1) %>%
  addLegend_decreasing(position="topleft", pal2, values = no_use$new_users, 
                       opacity = 1, decreasing = T, 
                       title = paste0("People who would gain access if a <br>", 
                                      "pool were built at this location")) %>%
  addLayersControl(options = layersControlOptions(collapsed = F), 
                   baseGroups = c("Potential sites where >100k people would gain pool access", 
                                  "Potential sites where >75k people would gain pool access", 
                                  "Potential sites where >50k people would gain pool access", 
                                  "Potential sites where >25k people would gain pool access", 
                                  "All potential sites"))

saveWidget(map, file=file.path('visuals', 
                               "potential_pool_locations.html"))
mapview::mapshot(map, 
        file = file.path("visuals", "potential_pool_locations.pdf"),
        remove_controls = c("homeButton", "layersControl", "zoomControl"), vwidth = 1000, vheight = 850)


