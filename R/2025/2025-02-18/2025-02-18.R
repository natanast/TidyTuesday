

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)


# load data --------

agencies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')


# data cleaning ------

df <- agencies[!is.na(agencies$latitude) & !is.na(agencies$longitude), ]

df <- df[latitude >= 24 & latitude <= 49 & longitude >= -125 & longitude <= -66]

# Base map with state borders
usa_states <- map_data("state")


# plot ---------


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)


# library(ggplot2)
# library(dplyr)
# library(maps)
# 
# # Load data
# agencies <- read.csv("agencies.csv")
# 
# # Fix longitude column name if needed
# colnames(agencies)[colnames(agencies) == "longitude"] <- "longitude"
# 
# 
# # Plot the agencies on the map
# ggplot() +
#   geom_polygon(data = usa_states, aes(x = long, y = lat, group = group),
#                fill = "gray90", color = "white") +
#   geom_point(data = df, aes(x = longitude, y = latitude, color = is_nibrs),
#              alpha = 0.6, size = 1) +
#   scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue"),
#                      labels = c("Not in NIBRS", "NIBRS Participant")) +
#   theme_minimal() +
#   labs(title = "Law Enforcement Agencies in the U.S.",
#        subtitle = "Colored by NIBRS Participation",
#        x = "Longitude", y = "Latitude",
#        color = "NIBRS Status") +
#   theme(legend.position = "bottom")

