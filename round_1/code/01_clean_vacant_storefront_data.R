### RUN DEPENDENCIES ## ---------
source(file.path(getwd(),"code/00_load_dependencies.R"))

## Data quality notes ## ------------

# # check data
# summary(ct.shp)
# summary(vacant_dataset)
# skim(ct.shp)
# skim(vacant_dataset)
# 
# # there are 277 storefronts with incorrect census tract label of 0 in 2020-2021 reporting year, 0.3% of all storefronts
ck <- vacant_dataset %>%
arrange(census_tract) %>%
  filter(census_tract %in% c("", "0", NA)) %>%
  group_by(reporting_year) %>% count()
# 
# # 0.3% missing latitude
ck2 <- vacant_dataset %>%
  arrange(census_tract) %>%
  filter(is.na(council_district)==T) %>%
  group_by(reporting_year) %>% count()

# can we match previous period where address was not vacant to get there business activity?
ck3 <- vacant_dataset %>% filter(vacant_on_12_31 == "NO") %>% 
  group_by(reporting_year, primary_business_activity) %>% 
  count()

# class 1 vs class 2 & 3 reporting vacant_6_30_or_date_sold -- can we get that info?

# # checked pluto dataset to get missing lat & long
# pluto <- vroom("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=999999999&$select=bbl,latitude,longitude,bctcb2020")
# 
ck3 <- vacant_dataset %>%
  arrange(census_tract) %>%
  filter(is.na(vacant_dataset$latitude)==T)
# 
# vacant_pluto <- ck3 %>% 
#   
#   left_join(pluto, by=c('bbl'))

# there are non-matching bbls, will geocode addresses instead for missing lat/lons

# run geocoding function `Gcode(addr, boro)` -----
G=c()
r=list()


# test 1 to see if it works
 Gcode(ck3$property_street_address_or[1], ck3$borough[1])

# run on unique addresses 
unique_addr = ck3[,c('property_street_address_or', 
                     'borough')] %>% 
  unique() 

# loop
for (i in 1:dim(unique_addr)[1])
{ 
  G=Gcode(unique_addr[[1]][i], unique_addr[[2]][i])
  r[i]=G[3]
}
#

coords=c()
bbls=c()

for (i in 1:length(r)) {
  if(is.null(r[[i]]$geometry)==F) {
coords[i]= r[[i]]$geometry$coordinates
bbls[i]= r[[i]]$properties$addendum$pad$bbl }
else{
  coords[i]=NA
  bbls[i]=NA }
}


geosearch_results <- as.data.frame(cbind(bbls, coords)) %>% 
  unnest_wider(coords, names_sep = "_")

ck3 <- ck3 %>% left_join(geosearch_results, b=c('bbl'='bbls'))


# filter out for now -------
vacant_dataset <- vacant_dataset %>%  
  filter(!is.na(vacant_dataset$latitude)==T)  

# make vacant dataset into points shapefile based on lat/lon
vacant_dataset_pts.shp <- vacant_dataset %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% st_set_crs(4326)

vacant_ct.shp <- st_join(vacant_dataset_pts.shp, 
                     ct.shp %>% select(geoid, ct_label,
                                       boro_ct2020,nta2020, nta_name),
                     st_intersects) 
vacant_ct <- vacant_ct.shp %>% st_drop_geometry() %>%  as.data.frame() 


### CREATE TRACT VACANCY COUNTS DATASET & SHAPEFILE ### ----

# keeping reporting year to make year to year comparisons, see if there have been improvement

ct_vacant <- vacant_ct %>% 
  group_by(reporting_year, geoid, boro_ct2020,vacant_on_12_31) %>% 
  summarize(count_vacant=n()) %>% 
  mutate(perc_vacant=count_vacant/sum(count_vacant), 
         perc_vacant=round(perc_vacant,2),
         total_storefronts=sum(count_vacant)) %>% 
  #filter(vacant_on_12_31=="YES") %>% 
  ungroup() %>% as.data.frame()



# make dataset into shapefile
ct_vacant.shp <- ct.shp %>% select(geoid, geometry) %>% 
  right_join(ct_vacant, by='geoid')


### SAVE OUTPUTS ### -------------run if needed
#write_csv(ct_vacant, "data/output/vacant_storefronts_tracts_2020-2021.csv")
#st_write(ct_vacant.shp, "data/output/vacant_storefronts_tracts_2020-2021.geojson", driver='GeoJSON', delete_dsn=TRUE)

