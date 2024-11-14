


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(paletteer)
library(dplyr)
library(patchwork)


# load data ------------

countries <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv')
country_subdivisions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/country_subdivisions.csv')


# data cleaning
world <- ne_countries(scale = "small", returnclass = "sf")




# Subdivision counts and predominant types per country
subdiv_count <- country_subdivisions |>
    group_by(alpha_2) |>
    summarise(subdivisions_count = n())

predominant_subdiv <- country_subdivisions |>
    group_by(alpha_2, type) |>
    summarise(count = n()) |>
    arrange(alpha_2, desc(count)) |>
    slice(1) |>
    select(alpha_2, predominant_type = type)



# Data preparation for stacked bar plot
df <- country_subdivisions |>
    count(alpha_2, type) 


world_with_subdiv <- world |>
    left_join(subdiv_count, by = c("iso_a2" = "alpha_2")) |>
    left_join(df, by = c("iso_a2" = "alpha_2"))



world_with_subdiv[, c("type.x", "type.y")]

# Top 10 countries by subdivision count


col <- c('#5773cc', '#5e77ce', '#647bcf', '#6b7fd1', '#7083d2', '#7687d3', '#7b8bd4', '#818fd5', '#8594d6', '#8a98d7', '#8e9dd7', '#92a2d8', '#96a6d8', '#99abd8', '#9cb0d7', '#9eb6d6', '#9fbbd4', '#9fc2d1', '#9cc8cc', '#fed38e', '#fccb8b', '#f9c288', '#f6ba85', '#f3b281', '#f0a97e', '#eca17a', '#e89976', '#e49272', '#e08a6e', '#db8269', '#d77b65', '#d27360', '#cd6c5c', '#c86457', '#c25d53', '#bd564e', '#b84e4a', '#b24745')
col_2 <- c('#3a5cbc','#b9b8e7','#b24745', '#6F99AD', '#FFDC91', '#20854E','#FFDC91','#6F99AD', '#F39B7F','#D0DFE6')


# Map plot
map <- ggplot(world_with_subdiv) +
    geom_sf(aes(fill = type.y), color = "gray80") +
    # scale_fill_manual(values = col) +
    labs(
        title = "Predominant Subdivision Type by Country",
        fill = "Subdivision Type"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-55, 85)) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
    )

map



top_countries <- country_subdivisions |>
    count(alpha_2, sort = TRUE) |>
    top_n(10, wt = n) |>
    inner_join(countries, by = "alpha_2")  


# Data preparation for stacked bar plot
top_country_subdivisions <- country_subdivisions |>
    filter(alpha_2 %in% top_countries$alpha_2) |>
    count(alpha_2, type) |>
    inner_join(countries, by = "alpha_2")  

# Create the stacked bar plot
p_stacked <- ggplot(top_country_subdivisions, aes(x = reorder(name, n), y = n, fill = type)) +
    geom_bar(stat = "identity") +   
    scale_fill_manual(values = col) +
    coord_flip() +
    labs(
        title = "Top 10 Countries by Number and Type of Subdivisions",
        x = "Country",
        y = "Number of Subdivisions",
        fill = "Subdivision Type"
    ) +
    theme_minimal() +
    theme(
        
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
    )





# Combine map with inset bar plot
final_plot <- map +
    inset_element(p_stacked, left = 0, bottom = 0.03, right = 0.4, top = 0.4)

# Display the final plot
final_plot



# # Start recording ---------------------------------------------------------
# 
# gg_record(
#     dir = file.path(".", "recording"),
#     device = "png",
#     width = 7,
#     height = 6,
#     units = "in",
#     dpi = 300
# )
# 
# 
# # Save gif ----------------------------------------------------------------
# 
# ggsave(
#     file.path(".", paste0("20241105", ".png")),
#     bg = "#e4e4e3",
#     width = 7,
#     height = 6
# )
# 
# gg_playback(
#     name = file.path(".", paste0("20241105", ".gif")),
#     first_image_duration = 4,
#     last_image_duration = 20,
#     frame_duration = .25,
#     background = "#e4e4e3"
# )


