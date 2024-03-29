library(tidyverse)
library(sf)
library(mapview)
library(councildown)

t <- read_sf('data/input/bk_vacancy_hotspot.geojson')
t1 <- read_sf('data/input/mn_vacancy_hotspot.geojson')
t2 <- read_sf('data/input/bx_vacancy_hotspot.geojson')
t3 <- read_sf('data/input/si_vacancy_hotspot.geojson')
t5 <- read_sf('data/input/qns_vacancy_hotspot.geojson')

mapview(t) + mapview(t1) + mapview(t2) + mapview(t3) +
  mapview(t5)

all <- bind_rows(t,t1,t2,t3,t5)
write_sf(all, 'data/output/SBS_hotspot_vacancy_23.geojson')