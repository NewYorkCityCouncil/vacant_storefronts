## LIBRARIES -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor", "lubridate",  "ggplot2", 
                      "leaflet", "leaflet.extras", "htmlwidgets",
                      "skimr", "htmltools", "ggiraph", "gt", "gtExtras"
)

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)


### read in data
m <- read.csv('data/prop_vac_med_inc.csv') %>% 
  mutate(prop_vac_label = round(prop_vac *100,2),
         estimate = as.numeric(estimate) ) %>% 
  arrange(estimate)

### Correlation Plots ------------

options(scipen = 999)

plot <- 
  ggplot(data = m, 
         aes(x=estimate, y=prop_vac_label, color=estimate)) +
  geom_point_interactive(
    tooltip = paste0(
      "Median Income: $", scales::comma(m$estimate), 
      "\n", 
      "Vacancy Rate: ", m$prop_vac_label, "%", 
      "\n", 
      m$census_tract) ) + 
  geom_vline(xintercept = 40000, na.rm=TRUE,
             color ="#666666",linetype = "solid") +
  #scale_linetype_manual(name = NULL, values = 4) +
  scale_x_continuous(label = scales::comma_format(prefix = "$"),
                     breaks = seq(0,150000,20000)) +
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

htmltools::save_html(plot_interactive, "visuals/income_vacancy.html")
