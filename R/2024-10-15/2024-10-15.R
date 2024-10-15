


rm(list = ls())
gc()

# load libraries -----------

library(leaflet)
library(data.table)

# data cleaning ------------

tuesdata <- tidytuesdayR::tt_load('2024-10-15')

orcas <- tuesdata$orcas

colnames(orcas)

df <- orcas[, c("year","encounter_number", "location", "begin_latitude","begin_longitude", "end_latitude" ,"end_longitude")] |>
  setDT()


df <- df[year == 2024, ]

df_map <- df[, c("year","encounter_number", "location")]


df_map$lng <- rowMeans(df[, .(begin_longitude, end_longitude)], na.rm = TRUE)
df_map$lat <- rowMeans(df[, .(begin_latitude, end_latitude)], na.rm = TRUE)






df_plot |>
  leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  setView(-124, 49, zoom = 6.5) |>
  addCircleMarkers(
    data = df_map,
    lng = ~lng, 
    lat = ~lat,
    clusterOptions = markerClusterOptions(),
    stroke = TRUE, 
    fill = TRUE, 
    color = "#033c73", 
    fillColor = "#2fa4e7",
    radius = 5, weight = .5,
    opacity = 1, 
    fillOpacity = 1)

# Make a list of icons. We'll index into it based on name.
oceanIcons <- iconList(
  orca = makeIcon(
    "https://cdn-icons-png.flaticon.com/512/235/235443.png",
    18,
    18
  )
)

df_icon <- df_plot[, c("lng", "lat")]


leaflet(df) %>% addTiles() %>%
  # Select from oceanIcons based on df$type
  addMarkers(icon = ~oceanIcons[type])

# library(dplyr, warn.conflicts = FALSE)
# library(tidygeocoder)
# 
# # create a dataframe with addresses
# some_addresses <- tibble::tribble(
#   ~name,                  ~addr,
#   "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
#   "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
#   "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
# )
# 
# # geocode the addresses
# lat_longs <- some_addresses %>%
#   geocode(addr, method = 'osm', lat = latitude , long = longitude)
# 
# 
# library(ggplot2)
# 
# ggplot(df_map, aes(lng, lat), color = "grey99") +
#   borders("state") + 
#   geom_point() +
#   theme_void()
