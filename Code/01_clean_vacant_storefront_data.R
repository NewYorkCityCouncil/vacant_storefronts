### RUN DEPENDENCIES ## ---------
source(file.path(getwd(),"code/00_load_dependencies.R"))

## Data quality notes ## ------------

# there are 277 storefronts with incorrect census tract label of 0 in 2020-2021 reporting year, 0.3% of all storefronts
ck <- vacant_dataset %>% 
arrange(census_tract) %>% 
  filter(census_tract %in% c("", "0")) %>% 
  group_by(reporting_year) %>% count()

# filter out for now
vacant_dataset <- vacant_dataset %>%  
  filter(!census_tract %in% c("", "0")) 

# ---- Anne's suggestion:
# In the chunk starting row 55 we're dropping the decimal in order to be able to merge with the storefront data - which only provides the round numbers for the locations. It's BANANAS to me that they provide the data that way, by removing the decimals they are deciding to use old census tract designations, but only for tracts where there has been enough growth to merit them being divided. Frankly it makes no sense that they did that, but given that we're trying to make it work, I've at least condensed our spatial file. Previously we were merging each storefront to all tracts that it could match (ie if the storefront is reported to be in CT 2, we would merge it with both CT 2.01 and CT 2.02) now I've combined the shapes so that there will only be one existing shape labelled 2. We could use the lat lon to find the correct 2020 tract but there are some missing obs. The spatial difference is less bad than I had thought it might be - places where you see a red line are where I've combined the two (or more) tracts.

# ^ The 2020 map doesn't implement this to try to best preserve the original map in the replication.

### CLEAN VACANT DATASET ### ---- 

# clean census tract column for matching to the ct.shp later on
vacant_dataset$boro_ct <- str_extract(vacant_dataset$borough_block_lot, "^\\d{1}") #regex get borocode

# nas introduced because of comma, clean first
vacant_dataset$census_tract_2 <- as.numeric(
  gsub(",","",vacant_dataset$census_tract))*100 # add trailing zeros

vacant_dataset$census_tract_2 <- str_pad(vacant_dataset$census_tract_2,
                                            width=6, pad="0") # leading zeros

vacant_cleaned <- transform(vacant_dataset,
                               boro_ct=paste0(boro_ct,census_tract_2)) # create clean ct


### CLEAN CT SHAPFILE ### ----

# round the CTs to match with vacant CT formatting, same zero padding as above
# the census tracts are now 2020, believe previously they were 2010, not sure if that may have some issues
ct.shp_cleaned <- ct.shp %>% 
  mutate(ct_label_round = round(as.numeric(ct_label), digits = 0),
         ct_label_round = as.numeric(ct_label_round)*100,
         ct_label_round = str_pad(ct_label_round, width=6, pad="0")) %>% 
  transform(boro_ct2020= as.character(paste0(boro_code, ct_label_round))) %>% 
  rename(boro_ct = boro_ct2020) %>% 
  group_by(boro_ct) %>% 
  summarize(geometry = st_union(geometry))  # combining rounded shapes into one, per Anne's CR suggestion -- this prevents multiple matches error when joining later on

### CREATE TRACT VACANCY COUNTS DATASET & SHAPEFILE ### ----

# keeping reporting year to make year to year comparisons, see if there have been improvement

ct_vacant <- vacant_cleaned %>% 
  group_by(reporting_year, boro_ct, vacant_on_12_31) %>% 
  summarize(count_vacant=n()) %>% 
  #complete(vacant_on_12_31, fill = list(count_vacant = 0)) %>%  code not changing anything?
  mutate(perc_vacant=count_vacant/sum(count_vacant), 
         perc_vacant=round(perc_vacant,2),
         total_storefronts=sum(count_vacant)) %>% 
  #filter(vacant_on_12_31=="YES") %>% 
  left_join(ct.shp_cleaned,  by = c("boro_ct")) %>% ungroup() %>% 
  as.data.frame()


# make dataset into shapefile
ct_vacant.shp <- ct_vacant %>% 
  st_as_sf() %>% st_transform('+proj=longlat +datum=WGS84')


### SAVE OUTPUTS ### -------------run if needed
#write_csv(ct_vacant, "data/output/vacant_storefronts_tracts_2020-2021.csv")
#st_write(ct_vacant.shp, "data/output/vacant_storefronts_tracts_2020-2021.geojson", driver='GeoJSON', delete_dsn=TRUE)

