

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
# library(colorspace)
library(ggtext)
# library(paletteer)
# library(shadowtext)

# load data --------

palms <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')


# data cleaning ------

df <- palms[, .(acc_genus, acc_species, 
                blade_length = max__blade__length_m, 
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
df_long[, Size := ifelse(Measurement_Type == "blade_length", -Size, Size)]


# plot ------

# Mirrored bar plot
ggplot(df_long, aes(x = Size, y = acc_species, fill = Measurement_Type)) +
    
    # Bars
    geom_col(width = 0.6, alpha = 0.8) +
    
    geom_vline(xintercept = 0, color = "grey20", linetype = "dashed", size = 0.55) +
    
    scale_y_discrete(limits = rev(unique(df_long$acc_species))) +
    
    scale_x_continuous(limits = c(-4, 4), labels = abs) +
    
    scale_fill_manual(
        values = c(
            "blade_length" = "#79AF97", 
            "rachis_length" = "#7E6148"
        )
    ) +
    

    labs(
        title = "Fruit Length and Width in Basselinia Species",
        subtitle = "",
        caption = "Source: <b>  Long Beach Animal Shelter Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Size (cm)",
        y = "",
        fill = "Measurement Type"
    ) +
    
    theme_minimal() +
    
    theme(
        axis.text.x = element_text(size = 12, family = "Candara"),
        axis.text.y = element_text(size = 12, family = "Candara"),
        
        legend.position = "bottom",
        
        
        legend.title = element_text(size = 10, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_line(linewidth = 0.45, color = "grey85"),
        
        plot.background = element_rect(fill = "grey93", color = NA),

        plot.title = element_markdown(size = 12, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.3),
        
        plot.margin = margin(20, 20, 20, 20)
    )







# save ---------

# ggsave(
#    plot = gr, filename = "Rplot.png",
#    width = 9, height = 7, units = "in", dpi = 600
#)


