# Property Valuation and Assessment Data Tax Classes 1,2,3,4

# edit: pull in 2022

# ---- read in 2022 data (2024, 2025 also available) ---- 
# tax_class <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272023%27&$select=extracrdt,boro,block,lot,appt_date,units,owner,pytaxclass,fintaxclass,bldg_class,owner,corner,num_bldgs,yrbuilt,gross_sqft,fintaxflag,curtaxflag")
tax_class_1 <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272022%27&$select=extracrdt,boro,block,lot,appt_date,units,owner,pytaxclass,fintaxclass,bldg_class")
tax_class_2 <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272022%27&$select=extracrdt,boro,block,lot,corner,num_bldgs,yrbuilt,gross_sqft,fintaxflag,curtaxflag")
# had to break into two, was getting error selecting on that many cols?

# some definitions : 
# extracrdt = data extract date
# appt_date = date of the most recent appointment
# pytaxclass = property tax class [Values as of the Final Roll for the Previous Tax Year ]
# curtaxflag = current taxable flag [T' = Taxable,  'A' = Actual or blank]
# fintaxclass = property tax class [Most current Values for this property]
# fintaxflag = final taxable flag [T' = Taxable,  'A' = Actual or blank]

# make bbl variable
make_bbl <- function(df) {
  df$bbl <- paste0(df$boro,
                         str_pad(df$block, 5, pad = "0"),
                         str_pad(df$lot, 4, pad = "0"))
  return(df)
}
tax_class_1 <- make_bbl(tax_class_1)
tax_class_2 <- make_bbl(tax_class_2)

# grab latest extracrdt by bbl to eliminate dups
# tax_class %>% group_by(extracrdt) %>% summarise(n = n(), pct = n()/nrow(tax_class))
get_latest <- function(df) {
  df_sub <- df %>%
    group_by(bbl) %>%
    slice(which.max(as.Date(extracrdt, '%Y-%m-%d'))) %>%
    distinct()
  return(df_sub)
}
tax_class_1 <- get_latest(tax_class_1)
tax_class_2 <- get_latest(tax_class_2)

# merge together on bbl
tax_class <- tax_class_1 %>%
  left_join(tax_class_2 %>% select (-c(boro, block, lot)), by = c("bbl", "extracrdt")) 
# clear space
rm(tax_class_2, tax_class_1)

# now ready to merge on bbl # bin not available unfortunately to make uid2
# write.csv(pluto_22, "../data/tax_class.csv")
