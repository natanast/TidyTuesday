


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

# Remove rows with NA values in lng or lat
df_map <- df_map[!is.na(lng) & !is.na(lat) & !is.na(encounter_number)]



library(sf)
library(giscoR)  


# Get geographical boundaries for USA and Canada
map_data <- gisco_get_countries(country = c("USA", "Canada"), resolution = 1)


# Add a column for the orca image URL (or local file path)
df_map$orca_image <- "https://static.thenounproject.com/png/4370877-200.png"




# Plot with orca icons sized by the encounter_number
ggplot() +
  geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  # Plot the map boundaries
  geom_image(data = df_map, aes(x = lng, y = lat, image = orca_image, size = encounter_number)) +  # Use orca icon with size
  scale_size(range = c(1, 10)) +  # Adjust size range; higher values make the icons larger
  theme_minimal() +
  labs(title = "Whale Encounters in 2024",
       subtitle = "Locations of Encounters with Proportional Icon Size",
       x = "Longitude",
       y = "Latitude") +
  coord_sf(xlim = c(-125.5, -122.5), ylim = c(47.8, 49.5), expand = FALSE)  # Adjust limits for the area






ggplot() +
  geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  # Plot the map boundaries
  geom_point(data = df_map, aes(x = lng, y = lat, size = encounter_number), color = "#2fa4e7", alpha = 0.6) +  # Plot whale encounters with size based on encounter_number
  scale_size(range = c(2, 10), guide = "none") +  # Adjust size range; you can change these values as needed
  theme_minimal() +
  labs(title = "Whale Encounters in 2024",
       subtitle = "Average Locations of Encounters in the USA and Canada",
       x = "Longitude",
       y = "Latitude") +
  coord_sf(xlim = c(-125.5, -122.5), ylim = c(47.8, 49.5), expand = FALSE)



# Create a new column for the size based on encounter_number with a smaller range
df_map[, icon_size := scales::rescale(encounter_number, to = c(0.01, 0.07))]  # Adjust the size range

# Plot with orca icons sized by the encounter_number
ggplot() +
  geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  # Plot the map boundaries
  geom_image(data = df_map, aes(x = lng, y = lat, image = orca_image), size = df_map$icon_size) +  # Use orca icon with size
  theme_minimal() +
  labs(title = "Whale Encounters in 2024",
       subtitle = "Locations of Encounters with Proportional Icon Size",
       x = "Longitude",
       y = "Latitude") +
  coord_sf(xlim = c(-125.5, -122.5), ylim = c(47.8, 49.5), expand = FALSE)  # Adjust limits for the area



# # leaflet() ---------------
#
# df_map |>
#   leaflet() |>
#   addProviderTiles("CartoDB.Positron") |>
#   setView(-124, 49, zoom = 6.5) |>
#   addCircleMarkers(
#     data = df_map,
#     lng = ~lng, 
#     lat = ~lat,
#     # clusterOptions = markerClusterOptions(),
#     stroke = TRUE, 
#     fill = TRUE, 
#     color = "#033c73", 
#     fillColor = "#2fa4e7",
#     radius = 5, weight = .5,
#     opacity = 1, 
#     fillOpacity = 1)
# 
# # Make a list of icons. We'll index into it based on name.
# oceanIcons <- iconList(
#   orca = makeIcon(
#     "https://cdn-icons-png.flaticon.com/512/235/235443.png",
#     18,
#     18
#   )
# )
# 
# df_icon <- df_map[, c("lng", "lat")]
# 
# df_icon$type <- "orca"
# 
# df_icon$geometry <- paste0("POINT (", df_icon$lng," ", df_icon$lat, ")")
# 
# df_icon$lng <- NULL
# df_icon$lat <- NULL


