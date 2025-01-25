

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(sf)
library(giscoR)  
library(patchwork)
library(ggtext)
library(extrafont)


# load data --------

peaks_tidy <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv')


# data cleaning ------

# Data cleaning: keep only relevant columns
peaks <- peaks_tidy[, .(PHOST_FACTOR, HIMAL_FACTOR, OPEN, PKNAME)]

# Filter for peaks that are open for expeditions
open_peaks <- peaks[OPEN == TRUE, .(N = uniqueN(PKNAME)), by = .(PHOST_FACTOR, HIMAL_FACTOR)]

# Keep only combinations with more than 5 peaks
open_peaks <- open_peaks[N > 5]

# Modify the PHOST_FACTOR column
open_peaks[PHOST_FACTOR == "Nepal only", PHOST_FACTOR := "Nepal"]
open_peaks[PHOST_FACTOR == "Nepal & China", PHOST_FACTOR := "China"]
open_peaks[PHOST_FACTOR == "Nepal & India", PHOST_FACTOR := "India"]

# geographical boundaries for map
map_data <- gisco_get_countries(
    country = c("Nepal", "China", "India"), 
    resolution = 1
)


# Plot 1 ---------

peaks <- open_peaks[, .(Total_Peaks = sum(N)), by = PHOST_FACTOR]

peaks$PHOST_FACTOR <- factor(peaks$PHOST_FACTOR, levels = c("India", "Nepal", "China"))


# bar plot
bar_plot <- ggplot(peaks, aes(x = PHOST_FACTOR, y = Total_Peaks, fill = PHOST_FACTOR)) +
    
    geom_bar(stat = "identity", width = 0.4, alpha = 0.9) + 
    
    geom_text(aes(label = Total_Peaks), vjust = -0.5, size = 4, family = "Candara") +
    
    theme_minimal() +
    
    theme(
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_markdown(size = 12, margin = margin(t = -12), family = "Candara"),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
    
    scale_fill_manual(values = c("India" = "#6F99AD", "Nepal" = "#B24745", "China" = "#0072B5"))  # Customize colors



bar_plot


# map -----------

map = ggplot() +
    
    geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
    
    coord_sf(xlim = c(67, 97), ylim = c(25, 42), expand = FALSE) +
    
    theme_minimal()




spacer <- ggplot() + 
    
    labs(
        title = "Mapping the Himalayan Peaks.",
        subtitle = "Bar plot shows the total number of <b>open</b> peaks for <b>expeditions</b> across India, Nepal, and China.",
        caption = paste0(
            "Source: <b> Himalayan Dataset </b> | ",
            "Graphic: <b>Natasa Anastasiadou</b>"
        )
        ) +

    theme_void() + 
    
    theme(plot.margin = margin(0, 1, 5, 1),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 50)),
          plot.subtitle = element_markdown(size = 16, hjust = 0.5, family = "Candara", color = "grey30"),
          plot.caption  = element_markdown(size = 10, family = "Candara", vjust = 1, margin = margin(b = 100))
    )



# Now adjust the map with the wider plot
final_map <- spacer +
    
    inset_element(map, left = 0.01, bottom = 0.01, right = 0.99, top = 0.99) +
    
    inset_element(bar_plot, left = 0.25, bottom = 0.21, right = 0.75, top = 0.95) +
    
    theme(
        plot.margin = margin(30, 30, 30, 30)
    )

# Display the final map with the bar plot
final_map


ggsave(
    plot = final_map, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)



# 
# 
# 
# china <- open_peaks[PHOST_FACTOR == "China",]
# 
# # Summarize the total number of peaks for China
# china_total <- china[, .(Total_Peaks = sum(N))]
# 
# 
# # Create a lollipop plot for China with a thicker blue segment
# china_plot <- ggplot(china_total, aes(x = "China", y = Total_Peaks)) +
#     
#     geom_bar(stat = "identity", width = 0.7, fill = "#0072B5", alpha = 0.9) +
#     
#     geom_text(aes(label = Total_Peaks), vjust = 10, size = 6) +
#     
#     # # Lollipop point
#     # geom_point(size = 7, color = "#0072B5") +
#     # 
#     # # Lollipop stem with thicker width and blue color
#     # geom_segment(aes(x = "China", xend = "China", y = 10, yend = 115), 
#     #              color = "#0072B5", linewidth = 2) +
#     
# 
#     
#     # Add labels
#     # geom_text(aes(label = Total_Peaks), vjust = -1.5, size = 4) +
#     
#     # Minimal theme
#     theme_minimal() +
#     
#     labs(
#         x = "",
#         y = ""
#     ) +
#     
#     theme(
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#     )
# 
# # Display the plot
# china_plot
# 
# 
# 
# 
# 
# # plot 2 -----------
# 
# nepal <- open_peaks[PHOST_FACTOR == "Nepal",]
# 
# # Summarize the total number of peaks for China
# nepal_total <- nepal[, .(Total_Peaks = sum(N))]
# 
# 
# # Create a lollipop plot for China with a thicker blue segment
# nepal_plot <- ggplot(nepal_total, aes(x = "Nepal", y = Total_Peaks)) +
#     
#     geom_bar(stat = "identity", width = 0.7, fill = "#B24745", alpha = 0.9) +
#     
#     geom_text(aes(label = Total_Peaks), vjust = 10, size = 6) +
#     
#     # 
#     # # Lollipop point
#     # geom_point(size = 7, color = "#B24745") +
#     # 
#     # # Lollipop stem with thicker width and blue color
#     # geom_segment(aes(x = "Nepal", xend = "Nepal", y = 10, yend = 222), 
#     #              color = "#B24745", linewidth = 2) +
#     # 
#     # # Add labels
#     # geom_text(aes(label = Total_Peaks), vjust = -1.5, size = 4) +
#     
#     # Minimal theme
#     theme_minimal() +
#     
#     labs(
#         x = "",
#         y = ""
#     ) +
#     
#     theme(
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#     )
# 
# # Display the plot
# nepal_plot
# 
# 
# 
# 
# # plot 3 -----------
# 
# india <- open_peaks[PHOST_FACTOR == "India",]
# 
# # Summarize the total number of peaks for China
# india_total <- india[, .(Total_Peaks = sum(N))]
# 
# 
# # Create a lollipop plot for China with a thicker blue segment
# india_plot <- ggplot(india_total, aes(x = "India", y = Total_Peaks)) +
#     
#     geom_bar(stat = "identity", width = 0.7, fill = "#6F99AD", alpha = 0.9) +
#     
#     geom_text(aes(label = Total_Peaks), vjust = 10, size = 6) +
#     
#     # # Lollipop point
#     # geom_point(size = 3, color = "#6F99AD") +
#     # 
#     # # Lollipop stem with thicker width and blue color
#     # geom_segment(aes(x = "India", xend = "India", y = 0, yend = 16), 
#     #              color = "#6F99AD", linewidth = 0.5) +
#     # 
#     # # Add labels
#     # geom_text(aes(label = Total_Peaks), vjust = -1.5, size = 4) +
#     # 
#     # Minimal theme
#     theme_minimal() +
#     
#     labs(
#         x = "",
#         y = ""
#     ) +
#     
#     theme(
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()
#     )
# 
# # Display the plot
# india_plot
# 






# # final plot -------



# Embed the plots into the maps using inset_element
# gr1 = map + inset_element(india_plot, .05, .01, .85, .51)
# 





# # Add all three lollipop plots to the map along the same latitude (30°N)
# final_map <- map +
#     inset_element(india_plot, left = 0.25, bottom = 0.3, right = 0.35, top = 0.5) +   # Adjusted position for India
#     inset_element(nepal_plot, left = 0.45, bottom = 0.3, right = 0.55, top = 0.8) +   # Adjusted position for Nepal
#     inset_element(china_plot, left = 0.75, bottom = 0.3, right = 0.85, top = 0.8)     # Adjusted position for China
# 
# # Display the final map with insets
# final_map




# gr2 = map2 + inset_element(nepal_plot, .15, .01, .85, .71)
# gr3 = map3 + inset_element(india_plot, .15, .01, .85, .71)

# Combine the maps and plots
# multi = gr1 | gr2 | gr3
# multi

# ggsave(
#     plot = gr, filename = "Rplot.png",
#     width = 10, height = 10, units = "in", dpi = 600
# )



# map1 -----------
# map1 = ggplot() +
#     
#     geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
#     
#     coord_sf(xlim = c(80, 100), ylim = c(30, 40), expand = FALSE) +
#     
#     theme_minimal()
# 
# # map2 -----------
# map2 = ggplot() +
#     
#     geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
#     
#     coord_sf(xlim = c(80, 90), ylim = c(25, 30), expand = FALSE) +
#     
#     theme_minimal()
# 
# 
# # map2 -----------
# map3 = ggplot() +
#     
#     geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
#     
#     coord_sf(xlim = c(70, 88), ylim = c(20, 33), expand = FALSE) +
#     
#     theme_minimal()
