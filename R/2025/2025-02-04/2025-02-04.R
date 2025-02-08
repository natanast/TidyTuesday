

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)


# load data --------

# simpsons_characters <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')
simpsons_episodes <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')
# simpsons_locations <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv')
# simpsons_script_lines <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv')



# data cleaning ------

d = simpsons_episodes[, .(number_in_season, imdb_rating, season)]

d$season = d$season |> as.character()

# Filter out season 28
d <- d[season != "28"]  # or d <- d[!season %in% "28"]




# plot -------

col = c('#60608b', '#6c6c98', '#7978a4', '#8584b1', '#9291be', '#9e9ecb', '#acabd8', '#b9b8e5', '#c6c5f2', '#ffeacf', '#ffd5be', '#fcc1ad', '#f7ad9c', '#f09a8c', '#e7877d', '#dc756e', '#d0645f', '#c15451')

col = c('#60608b', '#9291be', '#b9b8e5', '#ffeacf','#fcc1ad', '#e7877d', '#c15451')


gr = ggplot(d, aes(x = season, y = imdb_rating)) +
    
    # geom_point(aes(x = season, y = imdb_rating))
    # Add points (jittered)
    geom_jitter(aes(fill = season), size = 3, width = 0.05, shape = 21, stroke = 0.5, alpha = 0.9, color = "black") +
    
    # Violin plot with transparency
    geom_violin(aes(fill = season), trim = FALSE, show.legend = TRUE, alpha = 0.6, adjust = 0.7,
                color = "black") +
    
    scale_fill_manual(values = col) +
    
    theme_minimal() +
    
    labs(
        title = "<b><span style='color: #EEA236; font-weight: bold;'>The Simpsons</span></b> 
                     IMDb Ratings Across Seasons",
        subtitle = "<b>Each point</b> represents an episode's IMDb rating, while the 
                    <b>violin plots</b> show the rating distribution per season.",
        caption = "Source: <b> Simpsons Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Season",
        y = "IMDb Rating"
        
    ) +
    
    theme(
        legend.position = "none",
        
        plot.margin = margin(20, 20, 20, 20),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        axis.text.x = element_markdown(hjust = 1, vjust = 0.5, family = "Candara", size = 13),
        axis.text.y = element_markdown(hjust = 1, vjust = 0.5, family = "Candara", size = 13),
        
        # axis.title.x = element_markdown(family = "Candara", size = 14, margin = margin(t = 14)),
        axis.title.y = element_markdown(family = "Candara", size = 14, margin = margin(r = 14)),
        
        plot.title = element_markdown(size = 21, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption = element_markdown(margin = margin(t = 35), size = 11, family = "Candara", hjust = 1),
        
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
