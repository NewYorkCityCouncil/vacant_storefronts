source("Code/00_load_dependencies.R")
# income and vacancy correlation 
# load recent storefront data

storereg_24 <- fread("https://data.cityofnewyork.us/resource/92iy-9c3n.csv?$limit=99999999")
sr <- storereg_24[reporting_year %in% "2021 and 2022", ]
sr[, bbl := as.character(bbl)]

sr <- sr %>% 
mutate(county = case_when(
  borough == "MANHATTAN" ~ "061", 
  borough == "BRONX" ~ "005", 
  borough == "BROOKLYN" ~ "047", 
  borough == "QUEENS" ~ "081", 
  borough == "STATEN ISLAND" ~ "085" 
))
sr[, ct := as.numeric(census_tract) * 100]
sr[, ct := str_pad(ct, 6, "left", pad = "0")]
sr[, geoid := paste0(county, ct)]

# get median income data --------------------------------------------------
library(censusapi)
group_B19013 <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2021,
  type = "variables",
  group = "B19013")

# View(group_B19013)
med_inc <- getCensus(
  # must add a census api key
  key = "8cdccf85df4c4c7cf6fadaca4006860333f8e592", 
  name = "acs/acs5",
  vintage = 2021,
  vars = group_B19013$name, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

setDT(med_inc)
med_inc[, geoid := paste0(county, tract)]
sr_med <- merge(sr, med_inc, by = "geoid")

# merge with storefronts data
incsub <- unique(sr_med[,.(geoid, reporting_year, borough_block_lot, 
                        vacant_on_12_31, estimate = B19013_001E)])

incsub[, n_sf := .N, by = "geoid"]
incsub2 <- incsub[estimate > 0, ]
incwd <- dcast(geoid + estimate ~ vacant_on_12_31, data = incsub2)
incwd[, totalsf := NO + YES]
incwd2 <- incwd[totalsf > 1, ]
incwd2[, prop_vac := YES/totalsf]

# check 
hist(incwd2$estimate)

# let's look at the number of storefronts 
ggplot(incwd2, aes(x=totalsf)) + geom_histogram() + theme_bw()
# dist of vacancy
ggplot(incwd2, aes(x=prop_vac)) + geom_histogram() + theme_bw()
summary(incwd2$prop_vac)
summary(incwd2$totalsf)

options(scipen = 999)

plot <- 
  ggplot(data = incwd2, 
         aes(x=estimate, y=prop_vac, color=estimate)) +
  geom_point_interactive(
    tooltip = paste0(
      "Median Income: $", scales::comma(incwd2$estimate), 
      "\n", 
      "Vacancy Rate: ", round(incwd2$prop_vac*100, 2), "%", 
      "\n", 
      incwd2$census_tract) ) + 
  geom_vline(xintercept = 72058, na.rm=TRUE,
             color ="#666666",linetype = "solid") +
  #scale_linetype_manual(name = NULL, values = 4) +
  scale_x_continuous(label = scales::comma_format(prefix = "$"),
                     breaks = seq(0, 300000, 40000)) +
  scale_y_continuous(labels = scales::label_percent(scale=1)) +
  #ggtitle("") +
  labs(
    x = "Median Income",
    y = "Vacancy Rate",
    color = "Median Income",
    caption = expression(paste(italic("Source: Census ACS; NYC DOF; Storefronts Reported Vacant or Not (Filing Year 2020 - 2021)")))
  ) +
  scale_color_gradientn(
    labels = scales::dollar_format(),
    colours = c('#800000','#DD6C54',"#e5cccc",
                '#AFB3D1'),
    values = scales::rescale(seq(0,150000,20000)) )  +
  
  theme(legend.position="none", legend.text = element_text(size=8),
        legend.title = element_text(size=10, family = 'Georgia'),
        
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Georgia",size = 14),
        axis.title.y = element_text(size = 11, 
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 11, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(size = 11, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, 
                                    margin = margin(t = 10, r = 0, b = 0, l = 0))) 

tooltip_css <- "background-color:#CACACA;"

plot_interactive <- girafe(ggobj = plot,  
                           width_svg = 9,
                           height_svg = 5, 
                           options = list(
                             opts_tooltip(css = tooltip_css)
                           )
)

save_html(plot_interactive, "visuals/sf_vacancy_vs_median_income_21_22.html")
# let's look at the number of 













