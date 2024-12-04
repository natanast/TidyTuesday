

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(sf)
# library(usmap)
# library(extrafont)
# library(ggtext)
# library(tidyverse)
# library(shadowtext)


# load data --------

A64_traffic <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv')


# clean data -------


sensor_sf <- st_as_sf(A64_traffic, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)




# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(readr)

# Load the United Kingdom map
uk_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
    filter(sovereignt == "United Kingdom")

# Load traffic data (replace with actual path to the CSV file)
# traffic_data <- read_csv("A64_traffic.csv")

# Convert traffic data to spatial format
traffic_sf <- traffic_data %>%
    filter(Status == "Active") %>%  # Filter for active sensors
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# Create the map
ggplot() +
    # Base map of the UK
    geom_sf(data = uk_map, fill = "grey85", color = "white") +
    
    # Plot traffic sensors, size and color by total volume
    geom_sf(data = traffic_sf, aes(size = `Total Volume`, color = `Avg mph`), alpha = 0.7) +
    
    
    # coord_sf(xlim = c(-2, 3), ylim = c(53, 56)) +  # Set UK bounds
    
    theme_minimal() +
    
    # Adjust color scale for average speed
    scale_color_gradient(low = "blue", high = "red", name = "Avg Speed (mph)") +
    
    # Adjust size scale for total volume
    scale_size(range = c(2, 10), name = "Total Volume") +
    
    labs(
        title = "Traffic Data in the United Kingdom",
        subtitle = "Road sensor data showing total volume and average speed",
        caption = "Data: A64_traffic.csv"
    ) +
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "right"
    )











# Recording --------

# library(camcorder)
# 
# gg_record(
#     dir = file.path("recording"),
#     device = "png",
#     width = 10,
#     height = 10,
#     units = "in",
#     dpi = 600
# )





# plot -------


ggplot() +
    
    geom_sf(data = sensor_sf, fill = "lightgrey", color = "darkgrey") 

# ggplot(map_data) +
#     
#     geom_sf(data = map, fill = "grey95", linewidth = 0.05) +
#     
#     geom_sf(aes(fill = encounter_count), color = "gray25", linewidth = 0.2) +  # Fill based on encounters
#     
#     # Add text with a shadow (stroke effect)
#     geom_shadowtext(
#         aes(x = centroid_x, y = centroid_y, label = state),
#         size = 3.5,
#         family = "Candara",
#         color = "gray10",
#         bg.color = "grey93",
#         bg.r = 0.06
#     ) +
#     
#     scale_fill_stepsn(
#         colors = c("#7d7ca9","#abaad9","#ffffe0","#ff9a92","#b24745"),
#         na.value = "grey55",
#         labels = scales::comma,
#         guide = guide_colorsteps(
#             title = "Encounters",
#             barheight = unit(0.7, "lines"),
#             barwidth = unit(16, "lines")
#             
#         )
#     ) +
#     
#     labs(
#         title = "State-by-State Customs and Border Protection (CBP) Encounters",
#         subtitle = "A comprehensive view of encounters reported by U.S. CBP for each state. <br> States with missing encounter data are displayed in <b><span style='color: grey35; font-weight: bold;'> grey </span></b>.",
#         caption = "Source: <b> U.S. Customs and Border Protection (CBP) Encounter Data</b> | Graphic: <b>Natasa Anastasiadou</b>"
#     ) +
#     
#     theme_void() +  
#     
#     theme(
#         
#         legend.position = "bottom",
#         legend.title.position = "top",
#         
#         legend.title = element_text(size = 9, face = "bold", family = "Candara", color = "grey30", hjust = .5),
#         legend.text = element_text(size = 7, family = "Candara", color = "grey30"),
#         
# 
#         plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 25, l = 50)),
#         plot.subtitle = element_markdown(size = 15, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 10, l = 50)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.03),
#         
#         plot.margin = margin(20, 20, 20, 20),
#         
#         plot.background = element_rect(fill = "grey93", color = NA)
#     )

 
ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)


# gg_stop_recording()
# 
# gg_playback(
#     name = "Rplot_gif.gif",
#     first_image_duration = 8,
#     last_image_duration = 20,
#     frame_duration = 0.45, 
#     width = 4800, # Match or scale up to the recorded dimensions (8 inches * 600 dpi)
#     height = 4800
# )







# # Load required libraries
# library(sf)
# library(rnaturalearth)
# library(ggplot2)
# 
# # Load UK map data
# uk_map <- ne_countries(scale = "medium", returnclass = "sf")
# 
# # Filter for the United Kingdom
# uk_map_filtered <- uk_map[uk_map$sovereignt == "United Kingdom", ]
# 
# # Plot the map of the United Kingdom
# ggplot() +
#     geom_sf(data = uk_map_filtered, fill = "lightblue", color = "black") +
#     coord_sf(xlim = c(-10, 5), ylim = c(49, 61), expand = FALSE) + # Set limits for UK
#     labs(
#         title = "Map of the United Kingdom",
#         caption = "Source: Natural Earth"
#     ) +
#     theme_minimal()











