

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(waffle)
# library(stringr)
# library(extrafont)
# library(colorspace)
# library(ggtext)
# library(paletteer)
# library(shadowtext)

# load data --------

palms <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')


# data cleaning ------

# plot ---------
# Filter data for a specific species (e.g., "Elaeis guineensis")
brahea_data <- palms %>%
    filter(acc_genus == "Brahea")  # Replace with the species of interest

# Scatter plot for max_rachis_length_m vs max_petiole_length_m
ggplot(brahea_data, aes(x = max__rachis__length_m, y = max__petiole_length_m)) +
    geom_point(aes(color = acc_species), size = 3) +  # Add color by species
    labs(
        title = "Comparison of Rachis and Petiole Length for Genus Brahea",
        x = "Max Rachis Length (m)",
        y = "Max Petiole Length (m)"
    ) +
    theme_minimal() +
    theme(legend.title = element_blank())  # Remove legend title for clarity

# save ---------

# ggsave(
#    plot = gr, filename = "Rplot.png",
#    width = 9, height = 7, units = "in", dpi = 600
#)


