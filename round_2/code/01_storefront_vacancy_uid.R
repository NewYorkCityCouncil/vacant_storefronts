# read in storefront vacancy dataset
vacancy <- vroom::vroom("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=999999999")

# create unique storefront bbl id
v_temp_tots <- vacancy %>% 
  mutate(property_street_address_or = toupper(property_street_address_or)) %>% #clean addresses
  distinct() %>% #remove duplicate rows
  drop_na(latitude) %>%  # geocode missing lat/long based on bbl later
  mutate(uid = paste(property_street_address_or, bbl, 
                     bin,latitude, longitude, unit)) %>% #create unique id
  group_by(uid, reporting_year, bbl, bin,
           census_tract, council_district) %>%  
  count()

#checks
subset <- v_temp_tots %>% filter(n==1) 
quantile(v_temp_tots$n, seq(0,1,0.1))

write_csv(v_temp_tots, "round_2/data/output/uid_storefront_vacancy.csv")

# how does it look to track storefronts across time
v_temp <- vacancy %>% 
  select(-c(sold_date, construction_reported, vacant_6_30_or_date_sold,
            borough, borough_block_lot, zip_code, property_number,
            property_street, primary_business_activity)) %>% #remove unnecessary & duplicated columns 
  mutate(property_street_address_or = toupper(property_street_address_or)) %>% #clean addresses
  distinct() %>% #remove duplicate rows
  drop_na(latitude) %>%  # geocode missing lat/long based on bbl later
  mutate(uid = paste(property_street_address_or, bbl, 
                     bin,latitude, longitude, unit)) %>% #create unique id
  pivot_wider(
    names_from = reporting_year, 
    values_from = vacant_on_12_31 ) %>% # make time variables
  filter(uid %in% subset$uid==T) # subset to one business per uid reporting period
  unnest(everything()) %>% 

#linkage time! lets join our datasets
v_temp_tots$tax_class = rep("NA", nrow(v_temp_tots))