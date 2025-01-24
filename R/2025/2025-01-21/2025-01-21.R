

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(sf)
library(giscoR)  

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

china <- open_peaks[PHOST_FACTOR == "China",]

# Summarize the total number of peaks for China
china_total <- china[, .(Total_Peaks = sum(N))]


# Create a lollipop plot for China with a thicker blue segment
china_plot <- ggplot(china_total, aes(x = "China", y = Total_Peaks)) +
    
    # Lollipop point
    geom_point(size = 7, color = "#0072B5") +
    
    # Lollipop stem with thicker width and blue color
    geom_segment(aes(x = "China", xend = "China", y = 10, yend = 115), 
                 color = "#0072B5", linewidth = 2) +
    
    # Add labels
    geom_text(aes(label = Total_Peaks), vjust = -1.5, size = 4) +
    
    # Minimal theme
    theme_minimal() +
    
    labs(
        x = "",
        y = ""
    ) +
    
    theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

# Display the plot
china_plot





# plot 2 -----------

nepal <- open_peaks[PHOST_FACTOR == "Nepal",]

# Summarize the total number of peaks for China
nepal_total <- nepal[, .(Total_Peaks = sum(N))]


# Create a lollipop plot for China with a thicker blue segment
nepal_plot <- ggplot(nepal_total, aes(x = "Nepal", y = Total_Peaks)) +
    
    # Lollipop point
    geom_point(size = 7, color = "#B24745") +
    
    # Lollipop stem with thicker width and blue color
    geom_segment(aes(x = "Nepal", xend = "Nepal", y = 10, yend = 222), 
                 color = "#B24745", linewidth = 2) +
    
    # Add labels
    geom_text(aes(label = Total_Peaks), vjust = -1.5, size = 4) +
    
    # Minimal theme
    theme_minimal() +
    
    labs(
        x = "",
        y = ""
    ) +
    
    theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

# Display the plot
nepal_plot




# plot 3 -----------

india <- open_peaks[PHOST_FACTOR == "India",]

# Summarize the total number of peaks for China
india_total <- india[, .(Total_Peaks = sum(N))]


# Create a lollipop plot for China with a thicker blue segment
india_plot <- ggplot(india_total, aes(x = "India", y = Total_Peaks)) +
    
    # Lollipop point
    geom_point(size = 3, color = "#6F99AD") +
    
    # Lollipop stem with thicker width and blue color
    geom_segment(aes(x = "India", xend = "India", y = 0, yend = 16), 
                 color = "#6F99AD", linewidth = 0.5) +
    
    # Add labels
    geom_text(aes(label = Total_Peaks), vjust = -1.5, size = 4) +
    
    # Minimal theme
    theme_minimal() +
    
    labs(
        x = "",
        y = ""
    ) +
    
    theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

# Display the plot
india_plot



# map -----------

map = ggplot() +
    
    geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
    
    coord_sf(xlim = c(70, 95), ylim = c(20, 35), expand = FALSE) +
    
    theme_minimal()





# # final plot -------

library(patchwork)


# Embed the plots into the maps using inset_element
gr1 = map + inset_element(india_plot, .05, .01, .85, .51)



gr2 = map2 + inset_element(nepal_plot, .15, .01, .85, .71)
gr3 = map3 + inset_element(india_plot, .15, .01, .85, .71)

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

