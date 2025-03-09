

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggstream)
library(extrafont)
library(colorspace)
library(ggtext)
library(paletteer)
library(shadowtext)
library(tidyr)


# load data --------

longbeach <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')


# data cleaning ------

df_heatmap <- longbeach[, .N, by = .(outcome_type, animal_type)]

df_heatmap <- df_heatmap[!is.na(outcome_type), ]

df_heatmap$outcome_type <- str_to_title(df_heatmap$outcome_type)
df_heatmap$animal_type <- str_to_title(df_heatmap$animal_type)

df_heatmap$animal_type <- factor(df_heatmap$animal_type, levels = rev(sort(unique(df_heatmap$animal_type))))

# fills missing ones with 0
df_heatmap <- df_heatmap |> 
    complete(outcome_type, animal_type, fill = list(N = 0))  


# plot --------

gr = ggplot(df_heatmap, aes(x = outcome_type, y = animal_type, fill = N)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
    
    geom_shadowtext(
        aes(label = ifelse(N > 0, N, "")),
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .1, 
        size = 3
    ) +

    scale_fill_stepsn(
        colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(1, 10, 100, 1000, 3000), 
        transform = "log10",  
        labels = scales::comma,
        name = "No. of Animals",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(8, "lines"), 
            barwidth = unit(0.25, "lines")
        )  # Centers the title
    ) +

    theme_minimal() +
    
    labs(title = "Fate of Shelter Animals at Long Beach Animal Shelter",
         subtitle = "This heatmap illustrates the distribution of different outcomes for animals at the Long Beach Animal Shelter.",
         caption = "Source: <b>  Long Beach Animal Shelter Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
         x = "Outcome Type",
         y = "Animal Type"
         ) +

    theme(
        
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 8, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        axis.title.x = element_text(size = 8, family = "Candara"),
        axis.title.y = element_text(size = 8, family = "Candara"),
        
        axis.text.x = element_text(size = 6, family = "Candara", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 6, family = "Candara"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 12, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.3),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )  

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 7, units = "in", dpi = 600
)


