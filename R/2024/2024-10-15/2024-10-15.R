


rm(list = ls())
gc()



# load libraries ---------

library(data.table)
library(ggplot2)
library(ggtext)
library(sf)
library(giscoR)  


# data cleaning ----------

tuesdata <- tidytuesdayR::tt_load('2024-10-15')


orcas <- tuesdata$orcas


df <- orcas[, c("year","encounter_number", "location", "begin_latitude","begin_longitude", "end_latitude" ,"end_longitude")] |>
  setDT()


df_map <- df[, c("year","encounter_number", "location")]



# latest four years
latest_years <- sort(unique(df$year), decreasing = TRUE)[1:4]

df_filtered <- df[year %in% latest_years]



 # average lng and lat
df_filtered$lng <- rowMeans(df_filtered[, .(begin_longitude, end_longitude)], na.rm = TRUE)

df_filtered$lat <- rowMeans(df_filtered[, .(begin_latitude, end_latitude)], na.rm = TRUE)

df_filtered <- df_filtered[!is.na(lng) & !is.na(lat) & !is.na(encounter_number)]



# geographical boundaries
map_data <- gisco_get_countries(country = c("USA", "Canada"), resolution = 1)



# plot --------

p <- ggplot() +
    
    geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
    
    geom_point(data = df_filtered, aes(x = lng, y = lat, size = encounter_number), color = "#8491B4", alpha = 0.6) + 
    
    scale_size(range = c(0.05, 3), guide = "legend") +  
    
    labs(title = "Whale Encounters from 2021 to 2024",
         subtitle = "Average Locations of Encounters",
         
         x = "Longitude",
         y = "Latitude",
         
         caption = paste0(
             "Source: <b>Center for Whale Research (CWR)</b> | ",
             "Graphic: <b>Natasa Anastasiadou</b>"
         )) +
    
    coord_sf(xlim = c(-125.5, -122.5), ylim = c(47.8, 49.5), expand = FALSE) +
    
    facet_wrap(~ year) +
    
    theme_minimal() +
    
    theme(
        
        # legend.position = "none",
        
        axis.title.x = element_text(size = 10, hjust = 0.5, vjust = -1, family = "Candara"),
        axis.title.y = element_text(size = 10, hjust = 0.5, family = "Candara"),
        
        axis.text.x =  element_text(size = 7, family = "Candara", angle = 90, hjust = 1),
        axis.text.y =  element_text(size = 7, family = "Candara"),
        
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 10, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey94", color = NA)
    )




ggsave(
  plot = p, filename = "Rplot.png",
  width = 14, height = 10, units = "in", dpi = 600
)  

