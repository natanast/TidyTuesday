

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

# col = c('#60608b', '#6c6c98', '#7978a4', '#8584b1', '#9291be', '#9e9ecb', '#acabd8', '#b9b8e5', '#c6c5f2', '#ffeacf', '#ffd5be', '#fcc1ad', '#f7ad9c', '#f09a8c', '#e7877d', '#dc756e', '#d0645f', '#c15451')

col = c('#60608b', '#9291be', '#b9b8e5', '#ffd5be','#f09a8c', '#dc756e', '#c15451')


gr = ggplot(d, aes(x = season, y = imdb_rating)) +
    
    # geom_point(aes(x = season, y = imdb_rating))
    # Add points (jittered)
    geom_jitter(aes(fill = season), size = 3, width = 0.05, shape = 21, stroke = 0.5, alpha = 0.9, color = "black") +
    
    # Violin plot with transparency
    geom_violin(aes(fill = season), trim = FALSE, show.legend = TRUE, alpha = 0.5, adjust = 1,
                color = "black") +
    

    theme_minimal() +
    
    scale_fill_manual(values = col)



ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)





