######## Local Law 157 - Citywide Basic Stats ##################
################################################################


# get tax class info to link 'vacant storefront or not' with 'Class 1 & Class 2,4 Aggregated Statistics' datasets  

# using Reese's helper functions - move to load dependencies later --------
make_bbl <- function(df) {
  df$bbl <- paste0(df$boro,
                   str_pad(df$block, 5, pad = "0"),
                   str_pad(df$lot, 4, pad = "0"))
  return(df)
}

get_latest <- function(df) {
  df_sub <- df %>%
    group_by(bbl) %>%
    slice(which.max(as.Date(extracrdt, '%Y-%m-%d'))) %>%
    distinct()
  return(df_sub)
}

# read in datasets ------------

# first dataset
setwd(here::here('round_2'))
source('code/00_load_dependencies.R') #'storefront vacant or not' is loaded in

# second datasets

class2_4 <- read_csv('https://data.cityofnewyork.us/resource/dxru-eun8.csv?$limit=99999999')

class2_4$type <- rep('Class 2 or 4', nrow(class2_4))

class1 <- read_csv('https://data.cityofnewyork.us/resource/x3n4-h56k.csv?$limit=9999')

class1$type <- rep('Class 1', nrow(class1))

both_classes_stats <- bind_rows(class1, class2_4)


# third dataset
tax_class <- readRDS("data/output/tax_class.csv")

# run the below tax class datasets if not already saved locally -------

# # no 2019 but can use 2020, which had extract_dates of 2019
# # tax_class_19 <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272019%27&$select=boro,block,lot,pytaxclass,extracrdt") %>%
# #   make_bbl() %>%  get_latest()
# 
# #2020
# tax_class_20 <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272020%27&$select=boro,block,lot,pytaxclass,extracrdt") %>%  
#   make_bbl() %>%  get_latest()
# 
# tax_class_20$year <- rep(2020, nrow(tax_class_20))
# 
# #2019
# tax_class_19 <- tax_class_20
# 
# tax_class_19$year <- rep(2019, nrow(tax_class_20))
# 
# #2021
# tax_class_21 <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272021%27&$select=boro,block,lot,pytaxclass,extracrdt") %>%  
#   make_bbl() %>%  get_latest()
# 
# tax_class_21$year <- rep(2021, nrow(tax_class_21))
# 
# #2022
# tax_class_22 <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272022%27&$select=boro,block,lot,pytaxclass,extracrdt") %>%  
#   make_bbl() %>%  get_latest()
# 
# tax_class_22$year <- rep(2022, nrow(tax_class_22))
# 
# 
# #2023
# tax_class_23 <- vroom::vroom("https://data.cityofnewyork.us/resource/8y4t-faws.csv?$limit=999999999&$where=year=%272023%27&$select=boro,block,lot,pytaxclass,extracrdt") %>%  
#   make_bbl() %>%  get_latest()
# 
# tax_class_23$year <- rep(2023, nrow(tax_class_23))
# 
# # combine
# tax_class_all <- bind_rows(tax_class_19, tax_class_20, tax_class_21, 
#                            tax_class_22, tax_class_23)
# tax_class_all$tax_id <- paste(tax_class_all$year, tax_class_all$bbl)
# 
# # clear 
# rm(tax_class_19, tax_class_20,tax_class_21,tax_class_22, tax_class_23)

# saveRDS(tax_class_all, "data/output/tax_class.csv")


# build dataset for gt table & charts -------------

vacancy_rate <- vacant_dataset %>% 
  select(reporting_year, vacant_on_12_31)  %>% 
  group_by(reporting_year) %>% 
  count(name = "Total Storefronts") %>% 
  mutate(`Vacant on 12/31` = substr(reporting_year, 1,4),
         `Total Storefronts` = scales::comma(`Total Storefronts`)) 

rate <- vacant_dataset %>%
  group_by(reporting_year, vacant_on_12_31) %>% 
  count() %>% 
  group_by(reporting_year) %>% 
  reframe(`Total Vacant` = n,
    `Percent Vacant` = round(`Total Vacant` / sum(`Total Vacant`) * 100,1),
           vacant_on_12_31 = vacant_on_12_31) %>%
  filter(vacant_on_12_31 == "YES")

class_counts <- both_classes_stats %>% 
  mutate(aggregate_level_citywide = toupper(aggregate_level_citywide)) %>% 
  filter(aggregate_level_citywide == "CITYWIDE") %>% 
  mutate(total_storefronts = gsub(",","", total_storefronts),
         storefronts_with_leases_ending = gsub(",","",storefronts_with_leases_ending),
         number_whose_lease_is_due = gsub(",","",number_whose_lease_is_due)) %>% 
  group_by(reporting_year, type) %>% 
  select(reporting_year, type, total_storefronts) %>% 
  pivot_wider(names_from = type, values_from = total_storefronts) %>% 
  ungroup() %>% as.data.frame() %>% 
  arrange(reporting_year) %>% 
  mutate(reporting_year = as.character(reporting_year))

table_1 <- vacancy_rate %>%  left_join(rate %>% select(!vacant_on_12_31)) %>% 
  select(reporting_year, `Vacant on 12/31`, `Percent Vacant`, 
         `Total Vacant`, `Total Storefronts`) %>% 
  left_join(class_counts, by = c('Vacant on 12/31'='reporting_year')) %>% 
  rename(`Reporting Year` = reporting_year) %>% 
  ungroup() %>% as.data.frame()


table <- gt(table_1) %>% 
  tab_header(title = "Citywide Overview of Local Law 57 Storefront Vacancy Reporting") %>%
  gt_theme_nytimes() %>% 
  # cols_align(align = c( "center"),
  #            columns = c(2:4)) %>% 
  tab_style(style = cell_text(color = "#777777"), 
            locations = cells_column_labels()) %>% 
  tab_options(column_labels.font.weight = "bolder") 

table

gtsave(table, "visuals/citywide_overview_stats_ll157.png")


##### # FYI storefront totals citywide differ from the `aggregate` datasets and the `vacant_or_not` dataset 

#reference data dictionary to understand column names
#
both_classes_stats_together <- both_classes_stats %>% 
  mutate(aggregate_level_citywide = toupper(aggregate_level_citywide)) %>% 
  filter(aggregate_level_citywide == "CITYWIDE") %>% 
  mutate(total_storefronts = gsub(",","", total_storefronts),
         storefront_leased_to_tenants = gsub(",","",storefront_leased_to_tenants),
         storefront_reported_occupied = gsub(",","",storefront_reported_occupied),
         storefront_reported_not_leased = gsub(",","",storefront_reported_not_leased)) %>% 
  select(-c(aggregate_level_id, aggregate_level_citywide,
            storefront_under_construction:average_years_for_construction, storefront_leased_and_not_operating_on_12_31,
            median_years_lease_term:average_years_not_leased))

names(both_classes_stats_together)[c(3:10,13)] <- c("median_sf", "avg_sf",
                                              "storefront_leased",
                                              "storefront_owner_occupied",
                                              "storefront_vacant",
                                              "Tax_Class",
                                              "median_monthly_rent_sf",
                                              "avg_monthly_rent_sf",
                                              "leases_ending_12_31")

both_classes_stats_together <- both_classes_stats_together %>% 
  mutate(rent_med = as.numeric(median_monthly_rent_sf) *
           as.numeric(median_sf),
         rent_avg = as.numeric(avg_monthly_rent_sf) * 
           as.numeric(avg_sf),
         lease_ending = as.numeric(leases_ending_12_31)/as.numeric(total_storefronts)) 
both_part_1 <- both_classes_stats_together  %>% #select(reporting_year:Tax_Class) %>% 
  filter(Tax_Class == "Class 2 or 4") %>% select(-Tax_Class) %>% 
  mutate(percent_vacant = as.numeric(storefront_vacant)/as.numeric(total_storefronts),
         `Vacant (%)` = scales::percent(percent_vacant),
         per_leased = as.numeric(storefront_leased)/as.numeric(total_storefronts),
         per_owner = as.numeric(storefront_owner_occupied)/as.numeric(total_storefronts),
         `Leased (%)` = scales::percent(per_leased),
         `Owner-Occupied (%)` = scales::percent(per_owner)) %>% 
  select(-c(percent_vacant,per_leased,per_owner,storefront_vacant,storefront_leased,storefront_owner_occupied)) %>% 
  arrange(reporting_year) %>% 
  rename(`median\nmonthly_rent_sf` = median_monthly_rent_sf,
         `average\nmonthly_rent_sf`= avg_monthly_rent_sf,
         `median\nyears_leased`= median_years_leased,
         `average\nyears_leased`= average_years_leased,
         `leases_ending\n12_31`= leases_ending_12_31,
         `total\nstorefronts`=total_storefronts,
         `reporting\nyear` = reporting_year)


table <- gt(both_part_1) %>% 
  tab_header(title = "Class 2 and 4 Storefronts Citywide Statistics") %>%
  gt_theme_nytimes() %>% 
  # cols_align(align = c( "center"),
  #            columns = c(2:4)) %>% 
  tab_style(style = cell_text(color = "#777777"), 
            locations = cells_column_labels()) %>% 
  tab_options(column_labels.font.weight = "bolder") 

table

gtsave(table, "visuals/citywide_class2-4_stats_ll157.png")


#####
class_type <- vacant_dataset %>% 
  mutate(year = substr(reporting_year, 1,4),
         tax_id = paste(year, bbl)) %>% 
  left_join(tax_class, by= 'tax_id') %>% 
  select(reporting_year, vacant_on_12_31, pytaxclass) %>%
  mutate(class_type = case_when(pytaxclass %in% c('1', '1B', '1C') ~ 'Class 1',
                                TRUE ~ 'Class 2 and 4')) %>% 
  # mutate(reporting_year = case_when(reporting_year == "2023" ~ 
  #                                     "2022 and 2023",
  #                                   TRUE ~ reporting_year)) %>% 
  group_by(reporting_year, vacant_on_12_31, class_type) %>%
  summarise(count = n() ) %>%
  group_by(reporting_year, class_type)  %>% 
  reframe( percent = round(count / sum(count) * 100,1),
           vacant_on_12_31 = vacant_on_12_31) %>%
  filter(vacant_on_12_31 == "YES")


