
# ---- load data ----
commercial_evictions <- vroom("https://data.cityofnewyork.us/resource/6z8x-wfk4.csv?$limit=9999999&$where=residential_commercial_ind=%27Commercial%27") %>%
  distinct()  
# create uid2 to merge
commercial_evictions <- commercial_evictions %>% mutate(uid2 = paste(bin, bbl))
# now ready to merge on uid2, but consider getting rid of all the double NAs
# lat/long is available if you want to get bin, bbl from that; but don't know how reliable it is