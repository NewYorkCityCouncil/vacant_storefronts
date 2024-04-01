# Property Valuation and Assessment Data Tax Classes 1,2,3,4

# ---- read in 2023 data (2024, 2025 also available) ---- 
tax_class <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272023%27&$select=extracrdt,boro,block,lot,appt_date,units,owner,pytaxclass,curtaxflag,fintaxclass,fintaxflag")
# some definitions : 
# extracrdt = data extract date
# appt_date = date of the most recent appointment
# pytaxclass = property tax class [Values as of the Final Roll for the Previous Tax Year ]
# curtaxflag = current taxable flag [T' = Taxable,  'A' = Actual or blank]
# fintaxclass = property tax class [Most current Values for this property]
# fintaxflag = final taxable flag [T' = Taxable,  'A' = Actual or blank]

# make bbl variable
tax_class$bbl <- paste0(tax_class$boro,
                       str_pad(tax_class$block, 5, pad = "0"),
                       str_pad(tax_class$lot, 4, pad = "0"))

# grab latest extracrdt by bbl to eliminate dups
# tax_class %>% group_by(extracrdt) %>% summarise(n = n(), pct = n()/nrow(tax_class))
latest_tax_class <- tax_class %>% 
  group_by(bbl) %>%
  slice(which.max(as.Date(extracrdt, '%Y-%m-%d'))) %>%
  distinct()
rm(tax_class) # free up space

# now ready to merge on bbl # bin not available unfortunately to make uid2