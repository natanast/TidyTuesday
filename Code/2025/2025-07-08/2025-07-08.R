

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)

library(extrafont)
library(ggtext)


# load data ------

# answers <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/answers.csv')
color_ranks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv')
# users <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/users.csv')


# clean data ------

color_ranks[, hex := tolower(hex)]

top50_colors <- color_ranks[order(rank)][1:50]

top50_colors[, row := rep(1:5, each = 10)]
top50_colors[, col := rep(1:10, times = 5)]


# plot ------

gr = ggplot(top50_colors, aes(x = col, y = -row)) +
    
    geom_tile(
        aes(fill = hex), 
        color = "grey20",
        linewidth = .35,
        width = 0.85, 
        height = 0.85
    ) +
    
    scale_fill_identity() +
    
    coord_fixed() +
    
    theme_void(base_family = "Candara") +
    
    labs(
        title = "XKCD’s Favorite 50 Colors",
        subtitle = "Exploring the names from the XKCD color naming survey — each tile shows a color.",
        caption = "Source: <b> xkcd Color Survey</b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme(
        
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5,  color = "grey30", margin = margin(b = 15)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "grey90", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr
 
ggsave(
    plot = gr, filename = "plot.png",
    width = 8, height = 6, units = "in", dpi = 600
)





