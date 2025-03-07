

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


# load data --------

longbeach <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')


# data cleaning ------
library(ggplot2)
library(data.table)


# Count occurrences of each outcome type per animal type
df_heatmap <- longbeach[, .N, by = .(outcome_type, animal_type)]

df_heatmap <- df_heatmap[!is.na(outcome_type), ]

# Define custom color scale
colors =  c('#00429d', '#73a2c6', 'grey96', '#f4777f', '#93003a')

# Plot the heatmap
ggplot(df_heatmap, aes(x = outcome_type, y = animal_type, fill = N)) +
    geom_tile(color = "grey20") +
    # scale_fill_gradientn(colors = colors, name = "Count") +  # Custom gradient

    
    scale_fill_stepsn(
        colors =  c('#2c5769', '#6F99AD', 'grey96', '#ff9992', '#b24745'),
        breaks = c(1, 10, 100, 1000, 3000),  # Log-spaced breaks
        transform = "log10",  # Apply log transformation
        labels = scales::comma,
        guide = guide_colorsteps(
            barheight = unit(16, "lines"), 
            barwidth = unit(.5, "lines")
        )
    ) +


    # 
    # scale_color_stepsn(
    #     colors = c("#00429d","#73a2c6","#ffffe0","#f4777f","#93003a") |> darken(.75),
    #     # breaks = c(1000, 2000, 3000, 5000),
    #     # transform = "log10",
    #     guide = "none"
    # ) +
    # 
    theme_minimal() +
    
    labs(title = "Outcome Types by Animal Type",
         # subtitle = "Average perfume ratings of popular brands with over 20 years of releases and significant reviews (>500 Rating Count).",
         caption = "Source: <b> Parfumo Fragrance Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
         x = "Outcome Type",
         y = "Animal Type"
         # fill = "Outcome Type"
         ) +


    theme(
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "right",
        
        legend.title = element_text(size = 10, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        axis.title.x = element_text(size = 12, family = "Candara"),
        axis.title.y = element_text(size = 12, family = "Candara"),
        
        # axis.text.x = element_text(size = 12, family = "Candara"),
        axis.text.y = element_text(size = 12, family = "Candara"),
        
        panel.grid.major = element_line(linewidth = .4, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5, )),
        plot.subtitle = element_markdown(size = 14, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, family = "Candara", hjust = 1.3),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
          )  # Rotate x-axis labels



# # Custom order of outcome types
# custom_order <- c("adoption", "transfer", 
#                   "return to owner", "shelter, neuter, return", 
#                   "died", "community cat", "return to wild habitat",
#                   "transport", "foster to adopt", "homefirst", 
#                   "disposal", "missing", "trap, neuter, release", 
#                   "return to rescue", "duplicate", "foster",
#                   "rescue", "euthanasia")
# 
# 
# # Custom order of outcome types
# custom_order <- c("adoption", "transfer", "return to owner",
#                   "died", "return to wild habitat",
#                   "return to rescue", 
#                   "rescue", "euthanasia")
# plot ---------


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 11, height = 9, units = "in", dpi = 600
)


