

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(ggtext)


# load data --------

palms <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')


# data cleaning ------

df <- palms[, .(acc_genus, acc_species, 
                blade_length = max__blade__length_m, 
                rachis_length = max__rachis__length_m)]

df <- df[!is.na(blade_length) & !is.na(rachis_length)]

df <- df[acc_genus == "Basselinia"]

df$acc_species <- str_to_title(df$acc_species)


df_long <- melt(df, id.vars = "acc_species", 
                measure.vars = c("blade_length", "rachis_length"),
                variable.name = "Measurement_Type",
                value.name = "Size")


df_long[, Size := ifelse(Measurement_Type == "blade_length", -Size, Size)]


# plot ------

gr = ggplot(df_long, aes(x = Size, y = acc_species, fill = Measurement_Type)) +

    geom_col(width = 0.5, alpha = 0.8) +
    
    geom_vline(xintercept = 0, color = "grey20", linetype = "dashed", size = 0.55) +
    
    scale_y_discrete(limits = rev(unique(df_long$acc_species))) +
    
    scale_x_continuous(limits = c(-4, 4), labels = abs) +

    scale_fill_manual(
        values = c("blade_length" = "#79AF97", "rachis_length" = "#7E6148"),
        labels = c("blade_length" = "Blade", "rachis_length" = "Rachis")
    ) +

    labs(
        title = "Comparison of Leaf Blade and Rachis Lengths in Basselinia Species",
        subtitle = "Visualizing the structural diversity of *Basselinia* palms through their maximum <span style='color:#79AF97;'><b>Blade</b></span> and <span style='color:#7E6148;'><b>Rachis</b></span> lengths.",
        caption = "Source: <b> {palmtrees} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Size (m)",
        y = "",
        fill = "Length"
    ) +
    
    theme_minimal() +
    
    theme(
        axis.text.x = element_text(size = 12, family = "Candara"),
        axis.text.y = element_text(size = 12, family = "Candara"),
        
        legend.position = "bottom",
        
        axis.title.x = element_text(size = 14, family = "Candara"),
        
        legend.title = element_text(size = 12, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 10, family = "Candara", color = "grey30"),
        
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_line(linewidth = 0.45, color = "grey85"),
        
        plot.background = element_rect(fill = "grey93", color = NA),

        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 10, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 20, t = 2)),
        plot.caption = element_markdown(margin = margin(t = 22), size = 10, family = "Candara", hjust = 1.03),
        
        plot.margin = margin(20, 20, 20, 20)
    )


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 10, units = "in", dpi = 600
)


