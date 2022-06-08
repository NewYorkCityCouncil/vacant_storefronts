library(classInt)
library(tidyverse)

# get classification bins for carto map on grant/loans


g <- read_csv('data/grant_top5naics_zipcode_ll94_sbs_erg_sbcl_report_employee_reten.csv') 


l <- read_csv("data/loan_top5naics_zip_ll94_sbs_erg_sbcl_report_small_business_cont.csv")


summary(g$sumgrant)
summary(l$sumloan)

plot(density(g$sumgrant, na.rm = T))
plot(density(l$sumloan, na.rm = T))

#carto has classification by jenks

int_g <- classIntervals(na.omit(g$sumgrant), n = 5, style = 'jenks')
int_l <- classIntervals(na.omit(l$sumloan), n = 5, style = 'jenks')
