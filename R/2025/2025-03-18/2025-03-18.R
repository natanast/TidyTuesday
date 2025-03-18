

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(waffle)
library(stringr)
# library(extrafont)
# library(colorspace)
# library(ggtext)
# library(paletteer)
# library(shadowtext)

# load data --------

palms <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')


# data cleaning ------

df <- palms[, .(acc_genus, acc_species, 
                blade_length = max__petiole_length_m, 
                rachis_length = max__rachis__length_m)]

df <- df[!is.na(blade_length) & !is.na(rachis_length)]

df <- df[acc_genus == "Basselinia"]

df$acc_species <- str_to_title(df$acc_species)

# Convert to long format
df_long <- melt(df, id.vars = "acc_species", 
                measure.vars = c("blade_length", "rachis_length"),
                variable.name = "Measurement_Type",
                value.name = "Size")

# Adjust values: Fruit width goes negative
df_long[, Size := ifelse(Measurement_Type == "rachis_length", -Size, Size)]


# plot ------

# Mirrored bar plot
ggplot(df_long, aes(x = Size, y = acc_species, fill = Measurement_Type)) +
    # Bars
    geom_col(width = 0.6, alpha = 0.8) +
    
    # Center vertical line
    geom_vline(xintercept = 0, color = "grey20", linetype = "dashed", size = 0.55) +
    
    # Reverse the y-axis order
    scale_y_discrete(limits = rev(unique(df_long$acc_species))) +
    
    scale_x_continuous(labels = abs) +
    
    # Custom colors
    scale_fill_manual(values = c("blade_length" = "#7E6148", 
                                 "rachis_length" = "#79AF97")) +
    
    # Labels and theme
    labs(
        # title = "Mirrored Bar Plot of Fruit Length and Width in Basselinia Species",
        x = "Size (cm)",
        y = "",
        fill = "Measurement Type"
    ) +
    
    theme_minimal() +
    
    theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom"
    )







# save ---------

# ggsave(
#    plot = gr, filename = "Rplot.png",
#    width = 9, height = 7, units = "in", dpi = 600
#)


