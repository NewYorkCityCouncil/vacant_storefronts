

### MAPPING CLUSTERS ### -----

# 2021: prep datasets to visualize each cluster group
# group into one shapefile
df_cluster_1 <- ct_vacant_clusters.shp_2 %>% filter(cluster_recoded==1) 
df_cluster_2 <- ct_vacant_clusters.shp_2 %>% filter(cluster_recoded==2) 
df_cluster_3 <- ct_vacant_clusters.shp_2 %>% filter(cluster_recoded==3) 
df_cluster_4_2021 <- ct.shp_cleaned %>% 
  anti_join(ct_vacant.shp %>% as.data.frame() %>% 
              filter(reporting_year=="2020 and 2021"), 
            by=c("boro_ct")) # outline tracts with no storefronts in 2020-2021

# 2021: prep datasets to visualize each cluster group
# group into one shapefile
df_cluster_4_2022 <- ct.shp_cleaned %>% 
  anti_join(ct_vacant.shp %>% data.frame() %>% 
              filter(reporting_year=="2021 and 2022"), 
            by=c("boro_ct")) # outline tracts with no storefronts in 2021-2022

# map
cluster_map <- leaflet() %>%
  addCouncilStyle(add_dists = F) %>% 
  addPolygons(data= df_cluster_1 %>% 
                filter(reporting_year=="2020 and 2021"),
              fillOpacity = 1,
              fillColor = "#d6a33a", #orange
              color="#666666",
              stroke = T, weight = 0.5) %>% # cluster group #1
  addPolygons(data=boro.shp, stroke = T, 
              fill=F, color = "#666666", fillOpacity = 1,
              weight = 1) %>%  # boro boundaries
  addPolygons(data = df_cluster_3 %>% filter(reporting_year=="2020 and 2021"),
              fillColor = "#fda6b4", #pink
              color="#666666", fillOpacity = 1,
              stroke = T, weight = 0.5) %>%  # cluster group #2
  addPolygons(data = df_cluster_2 %>% filter(reporting_year=="2020 and 2021"),
              fillColor = "#d31819", #red
              stroke = T, weight = 0.5,
              color="#666666", fillOpacity = 1) %>%  # cluster group #3
  addPolygons(data = df_cluster_4_2021, 
              stroke = T, weight = 0.5,
              fillColor = "white",
              color="#666666",
              fillOpacity = 1) %>%  # cluster group #4
  addLayersControl(
    baseGroups = c("2020", "2021"),
    options = layersControlOptions(collapsed = FALSE)
  )
  addLegend(position ="topleft", 
            colors = c("red", "gold", "pink"),
            labels = c("vacant: 15.24% storefronts: 221", 
                       "vacant: 10.94%%  storefronts: 89", 
                       "vacant: 13.72% storefronts: 27"),
            title = "Cluster Means", opacity = 1) 
  
  
### save output ### ------
saveWidget(merged_map, file=file.path('../visuals', 
  "cluster_map_2022.html"))
mapshot(merged_map, file = "../visuals/cluster_map_2022.png")
