

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(sf)
library(usmap)
library(extrafont)
library(ggtext)
library(tidyverse)
library(shadowtext)


# load data --------

# cbp_resp <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_resp.csv')
cbp_state <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_state.csv')


# clean data -------

# Prepare map
map <- us_map() |> 
    select(state = abbr) |> 
    st_as_sf()

# Filter and prepare data
data <- cbp_state[, .(fiscal_year, state, encounter_count)]

data_agg <- data[, .(encounter_count = sum(encounter_count, na.rm = TRUE)), by = state]

# Merge data with map
map_data <- map |> 
    left_join(data_agg, by = c("state" = "state"))


# Calculate centroids for state labels
centroids <- st_centroid(map_data)
centroids_coords <- st_coordinates(centroids)
map_data$centroid_x <- centroids_coords[, 1]
map_data$centroid_y <- centroids_coords[, 2]



# plot -------

gr = ggplot(map_data) +
    
    geom_sf(data = map, fill = "grey95", linewidth = 0.05) +
    
    geom_sf(aes(fill = encounter_count), color = "gray25", linewidth = 0.2) +  # Fill based on encounters
    
    # Add text with a shadow (stroke effect)
    geom_shadowtext(
        aes(x = centroid_x, y = centroid_y, label = state),
        size = 3.5,                    
        family = "Candara",            
        color = "gray10",               
        bg.color = "grey93",            
        bg.r = 0.06                     
    ) +
    
    scale_fill_stepsn(
        colors = c("#7d7ca9","#abaad9","#ffffe0","#ff9a92","#b24745"),
        na.value = "grey55",
        labels = scales::comma,
        guide = guide_colorsteps(
            title = "Encounters",
            barheight = unit(0.7, "lines"),
            barwidth = unit(16, "lines")
            
        )
    ) +
    
    labs(
        title = "State-by-State Customs and Border Protection (CBP) Encounters",
        subtitle = "A comprehensive view of encounters reported by U.S. CBP for each state. <br> States with missing encounter data are displayed in <b><span style='color: grey35; font-weight: bold;'> grey </span></b>.",
        caption = "Source: <b> U.S. Customs and Border Protection (CBP) Encounter Data</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme_void() +  
    
    theme(
        
        legend.position = "bottom",
        legend.title.position = "top",
        
        legend.title = element_text(size = 9, face = "bold", family = "Candara", color = "grey30", hjust = .5),
        legend.text = element_text(size = 7, family = "Candara", color = "grey30"),
        

        plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 25, l = 50)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 10, l = 50)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.03),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )
ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)

