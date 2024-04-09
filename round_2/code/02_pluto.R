
pluto_22 <- vroom::vroom("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=9999999&$select=bbl,comarea,retailarea") # not much else useful not available in tax info
                                                                                                                    # though this is also probably avail in tax info...
# no bin for uid, ready to merge otherwise