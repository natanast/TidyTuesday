

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
    
    scale_fill_manual(values = c("India" = "#6F99AD", "Nepal" = "#B24745", "China" = "#0072B5")) 


# map -----------

map = ggplot() +
    
    geom_sf(data = map_data, fill = "grey", alpha = 0.3) +  
    
    coord_sf(xlim = c(67, 97), ylim = c(25, 42), expand = FALSE) +
    
    theme_minimal()

# space for title
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



# final plot ------------
final_map <- spacer +
    
    inset_element(map, left = 0.01, bottom = 0.01, right = 0.99, top = 0.99) +
    
    inset_element(bar_plot, left = 0.25, bottom = 0.21, right = 0.75, top = 0.95) +
    
    theme(
        plot.margin = margin(30, 30, 30, 30)
    )


ggsave(
    plot = final_map, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
