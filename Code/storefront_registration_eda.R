# reese
# 6-13-22

source("00_load_dependencies.R")
library(vroom)
# Storefront Registration Class 2 and 4 Statistics
storereg_24 <- vroom("https://data.cityofnewyork.us/resource/dxru-eun8.csv?$limit=99999999999")
# Storefront Registration Statistics for Designated Class One
storereg_1 <- vroom("https://data.cityofnewyork.us/resource/dxru-eun8.csv?$limit=99999999999")

total_19 <- storereg_24[storereg_24$aggregate_level_citywide == 'CITYWIDE',] %>% group_by(reporting_year) %>% summarise(sum_24_storefronts = sum(total_storefronts))
total_20_21 <- storereg_24[storereg_24$aggregate_level_citywide == 'Census Tract',] %>% group_by(reporting_year) %>% summarise(sum_24_storefronts = sum(total_storefronts))
reg_24_by_yr <- rbind(total_19, total_20_21)
rm(total_19)
rm(total_20_21)

# same number of class 1 registractions as 2/4...?

