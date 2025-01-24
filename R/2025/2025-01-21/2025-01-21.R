

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

peaks <- peaks_tidy[, c("PHOST_FACTOR", "HIMAL_FACTOR", "OPEN")]

# Filter for peaks that are open for expeditions
open_peaks <- peaks[OPEN == TRUE, .N, by = .(PHOST_FACTOR, HIMAL_FACTOR)]

open_peaks <- open_peaks[N > 5,]

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

china_plot = ggplot(china, aes(x = reorder(HIMAL_FACTOR, N), y = N, fill = HIMAL_FACTOR)) +
    
    # geom_segment( aes(x = reorder(HIMAL_FACTOR, N), xend = reorder(HIMAL_FACTOR, N), y = 0, yend = N)) +
    # 
    # geom_point(size = 2, color="red", fill = alpha("orange", 0.3), alpha = 0.7, shape = 21, stroke = 2)+
    # 
    
    geom_bar(stat = "identity", position = "dodge") +

    theme_minimal() 
    # labs(
    #     title = "Open Peaks by Host Country and Mountain Range",
    #     x = "Host Country",
    #     y = "Number of Open Peaks",
    #     fill = "Mountain Range"
    # )




# plot 2 -----------

nepal <- open_peaks[PHOST_FACTOR == "Nepal",]

nepal_plot = ggplot(nepal, aes(x = reorder(HIMAL_FACTOR, N), y = N, fill = HIMAL_FACTOR)) +
    
    # geom_segment( aes(x = reorder(HIMAL_FACTOR, N), xend = reorder(HIMAL_FACTOR, N), y = 0, yend = N)) +
    # 
    # geom_point(size = 2, color="red", fill = alpha("orange", 0.3), alpha = 0.7, shape = 21, stroke = 2)+
    # 
    
    geom_bar(stat = "identity", position = "dodge") +
    
    theme_minimal() 



# plot 3 -----------

india <- open_peaks[PHOST_FACTOR == "India",]

india_plot = ggplot(india, aes(x = reorder(HIMAL_FACTOR, N), y = N, fill = HIMAL_FACTOR)) +
    
    # geom_segment( aes(x = reorder(HIMAL_FACTOR, N), xend = reorder(HIMAL_FACTOR, N), y = 0, yend = N)) +
    # 
    # geom_point(size = 2, color="red", fill = alpha("orange", 0.3), alpha = 0.7, shape = 21, stroke = 2)+
    # 
    
    geom_bar(stat = "identity", position = "dodge") +
    
    theme_minimal() 









# map1 -----------
map1 = ggplot() +
    
    geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
    
    coord_sf(xlim = c(80, 100), ylim = c(30, 40), expand = FALSE) +
    
    theme_minimal()

# map2 -----------
map2 = ggplot() +
    
    geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
    
    coord_sf(xlim = c(80, 90), ylim = c(25, 30), expand = FALSE) +
    
    theme_minimal()


# map2 -----------
map3 = ggplot() +
    
    geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
    
    coord_sf(xlim = c(70, 88), ylim = c(20, 33), expand = FALSE) +
    
    theme_minimal()





# # final plot -------
library(patchwork)


# Embed the plots into the maps using inset_element
gr1 = map + inset_element(china_plot, .35, .01, .85, .71)
gr2 = map2 + inset_element(nepal_plot, .15, .01, .85, .71)
gr3 = map3 + inset_element(india_plot, .15, .01, .85, .71)

# Combine the maps and plots
multi = gr1 | gr2 | gr3
multi

# ggsave(
#     plot = gr, filename = "Rplot.png",
#     width = 10, height = 10, units = "in", dpi = 600
# )


