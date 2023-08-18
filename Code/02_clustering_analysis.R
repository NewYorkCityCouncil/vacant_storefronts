source(file.path(getwd(),"code/01_clean_vacant_storefront_data.R"))


# set up dataset for clustering --------
ct_vacant_clusters <- ct_vacant %>% 
  filter(vacant_on_12_31=="YES") %>% #focusing on tract with vacancies
  filter(!boro_ct %in% c('1000000', '4038300')) %>% #remove park areas(central/flushing)
  arrange(reporting_year)

### DECIDE HOW MANY CLUSTERS ### ----

# run cluster analysis for each reporting period
wss=c()
u=unique(ct_vacant_clusters$reporting_year)
cols=ct_vacant_clusters[,c("reporting_year","perc_vacant", 
                           "total_storefronts")] %>% 
  ungroup() %>% as.data.frame()
all_wss = list()

#loop
for (i in 1:length(u)){
# within sum of squares
set.seed(124) # guarantees that the same random values are produced each time you run the code
wss <- sapply(1:10, function(k){
                kmeans(cols %>% 
                         filter(reporting_year==u[i]) %>% 
                         select(-1), 
                       k, nstart = 50, iter.max = 15)$tot.withinss})
all_wss[[i]] <- c(wss) 
}
# check elbow plot
plot(1:10, all_wss[[1]],type="b", xlab= "k values", ) #2019-2020
points(1:10, all_wss[[2]],type="b",col="blue") #2020-2021
points(1:10, all_wss[[3]],type="b",col="red") #2021-2022
abline(v=4)
legend("right", legend=c("2019-2020", "2020-2021", "2021-2022"),
       col=c("black","blue", "red"), lty=1, cex=0.8, title = "Reporting years")


# kmeans - choosing 3 clusters 
# repeat for each reporting period
km=c()
u=unique(ct_vacant_clusters$reporting_year)
all_km = list()

for (i in 1:length(u)){
  set.seed(124) # guarantees that the same random values are produced each time you run the code
  # kmeans - choosing 3 clusters
  km <- kmeans(x = cols %>% filter(reporting_year==u[i]) %>% select(-1), 
               centers = 3)
  all_km[[i]] <- c(km) 
}

# add the cluster in which the vacant storefront census tract falls in 
ct_vacant_clusters$cluster <- c(all_km[[1]]$cluster, 
                                all_km[[2]]$cluster, 
                                all_km[[3]]$cluster)
#table(ct_vacant_clusters$reporting_year,ct_vacant_clusters$cluster) 
#table(all_km[[3]][["cluster"]]) check if matches km output

# recode cluster break offs into differnt grouping order 1,2,3
ct_vacant_clusters <- ct_vacant_clusters %>% 
  mutate(cluster_recoded=ifelse(cluster==1,1,
                                ifelse(cluster==2,3,2)))

# make shapefile
ct_vacant_clusters.shp <- ct_vacant_clusters %>% 
  left_join(ct.shp_cleaned, by = c("boro_ct")) %>% 
  ungroup() %>% st_as_sf() %>% st_transform('+proj=longlat +datum=WGS84')

### save outputs ### ------


### CLUSTERS COMPARING NO VACANCIES to LOW VACANCIES ### -------

# remove any tracts with zero percent if any
ct_vacant_clusters_no_zero <- ct_vacant_clusters %>% filter(perc_vacant!=0)

# run cluster analysis for each reporting period
wss=c()
u=unique(ct_vacant_clusters_no_zero$reporting_year)
cols=ct_vacant_clusters_no_zero [,c("reporting_year","perc_vacant", 
                           "total_storefronts")] %>% 
  ungroup() %>% as.data.frame()
all_wss = list()

# loop
for (i in 1:length(u)){
  set.seed(125) # guarantees that the same random values are produced each time you run the code
  # within sum of squares
  wss <- sapply(1:10, function(k){
    kmeans(cols %>% 
             filter(reporting_year==u[i]) %>% 
             select(-1), 
           k, nstart = 50, iter.max = 15)$tot.withinss})
  all_wss[[i]] <- c(wss) 
}
# check elbow plot
plot(1:10, all_wss[[1]],type="b", xlab= "k values", ) #2019-2020
points(1:10, all_wss[[2]],type="b",col="blue") #2020-2021
points(1:10, all_wss[[3]],type="b",col="red") #2021-2022
abline(v=4)
legend("right", legend=c("2019-2020", "2020-2021", "2021-2022"),
       col=c("black","blue", "red"), lty=1, cex=0.8, title = "Reporting years")




# kmeans - choosing 2 clusters
# repeat for each reporting period
km=c()
u=unique(ct_vacant_clusters_no_zero$reporting_year)
all_km = list()

for (i in 1:length(u)){
  set.seed(124) # guarantees that the same random values are produced each time you run the code
  # kmeans - choosing 3 clusters
  km <- kmeans(x = cols %>% filter(reporting_year==u[i]) %>% select(-1), 
               centers = 3)
  all_km[[i]] <- c(km) 
}

# add the cluster in which the vacant storefront census tract falls in
ct_vacant_clusters_no_zero$cluster <- c(all_km[[1]]$cluster, 
                                        all_km[[2]]$cluster, 
                                        all_km[[3]]$cluster)

ct_vacant_clusters$cluster <- 0  ## ?
ct_vacant_clusters <- ct_vacant_clusters %>% 
  filter(perc_vacant==0) %>% bind_rows(ct_vacant_clusters_no_zero) 
  
# make shapefile
ct_vacant_clusters.shp_2 <- ct_vacant_clusters %>% 
  st_as_sf() %>% st_transform('+proj=longlat +datum=WGS84')

# save outputs