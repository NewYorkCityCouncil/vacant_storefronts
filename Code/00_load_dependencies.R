## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
councildown.check <- "councildown" %in% installed.packages()[,"Package"]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)
if(councildown.check == FALSE) remotes::install_github("newyorkcitycouncil/councildown")
  
# packages are loaded
lapply(c(list.of.packages,"councildown"), require, character.only = TRUE)

# remove created variables for packages
rm(list.of.packages,new.packages)

## Functions -----------------------------------------------
