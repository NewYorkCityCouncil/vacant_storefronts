list.of.packages <- c("tidyverse", "janitor", "lubridate",  "ggplot2", 
                      "leaflet", "leaflet.extras", "htmlwidgets",
                      "skimr", "htmltools", "ggiraph", "gt", "gtExtras", 
                      "data.table", "sf", "leaflet"
)

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)