# read in storefront vacancy dataset
vacancy <- vroom::vroom("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=999999999")

# create unique storefront bbl id
v_uniq <- vacancy %>% 
  select(-c(sold_date, construction_reported, vacant_6_30_or_date_sold,
            property_street_address_or, borough, borough_block_lot, zip_code, 
            primary_business_activity)) %>% #remove unnecessary & duplicated columns 
  distinct() %>% #remove duplicate rows
  drop_na(latitude) %>%  # geocode missing lat/long based on bbl later
  #mutate(uid = paste(property_number,property_street)) %>% #create unique id
  mutate(uid2 = paste(bin, bbl))  #create alt unique id
  
v_tots <- v_uniq %>% 
  group_by(uid2, reporting_year, bbl, bin, paste(property_number,property_street),
           census_tract, council_district) %>%
  count()

#uid of `bin, bbl` = 127608 *lets us this
#uid of `lat, long` = 123772
#uid of `property_number, property_street` = 125349
#uid of all ids = 127608
#unit doesnt change anything 

#checks
subset <- v_tots %>% filter(n==1) 
quantile(v_temp_tots$n, seq(0,1,0.1)) # 90%!

#write_csv(v_tots, "round_2/data/output/uid_storefront_vacancy.csv")

# how does it look to track storefronts across time
v_temporal <- v_uniq %>% 
  mutate(vacant_on_12_31 = case_when(vacant_on_12_31 == "YES" ~ 1,
                                     TRUE ~ 0)) %>% # for easier calculations later on
  pivot_wider(
    names_from = reporting_year, names_prefix = 'vacant_',
    values_from = vacant_on_12_31) %>% # make time variables
  unnest(everything()) %>% 
  janitor::clean_names() %>% 
  filter(uid2 %in% subset$uid2 == T) %>% # subset to one business per uid reporting period
  # count how many times storefront was vacant
  mutate(vacant_times = rowSums(
    across(vacant_2022_and_2023:vacant_2019_and_2020),na.rm = T))

# linkage time! lets join our datasets