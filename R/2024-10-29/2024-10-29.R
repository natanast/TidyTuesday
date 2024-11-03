


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(stringr)
library(ggplot2)

library(ggstream)

library(colorspace)
library(ggtext)


# load data ------------

monster_movie_genres <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movie_genres.csv')
monster_movies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movies.csv')


# data cleaning --------

dt1 <- merge(monster_movie_genres, monster_movies[, c("tconst", "year")], by = c("tconst"))

x <- dt1[, by = .(year, genres), .N]


order <- x[, .(total_N = sum(N)), by = genres]

index <- order[order(-total_N)]$genres

x$genres <- factor(x$genres, levels = index)



top_genres <- order[order(-total_N)][1:4, genres]


label_data <- order[genres %in% top_genres]


cols <- c("Horror"      = "#BC3C29", 
          "Comedy"      = "#9dced6", 
          "Documentary" = "#a28ee8", 
          "Drama"       = "#0072B5", 
          "Adventure"   = "gray70", 
          "Sci-Fi"      = "gray70", 
          "Action"      = "gray70", 
          "Animation"   = "gray70", 
          "Fantasy"     = "gray70", 
          "Family"      = "gray70",
          "Thriller"    = "gray70", 
          "Crime"       = "gray70", 
          "Short"       = "gray70", 
          "Mystery"     = "gray70", 
          "Biography"   = "gray70", 
          "Romance"     = "gray70",
          "Music"       = "gray70", 
          "History"     = "gray70", 
          "Musical"     = "gray70", 
          "Western"     = "gray70", 
          "Sport"       = "gray70", 
          "War"         = "gray70"
          )

# plot ---------


p <- ggplot(x, aes(x = year, y = N, fill = genres)) +
    
  geom_stream(color = "white", linewidth = 0.075, type = "proportional", bw = 0.75) +
  
  # Custom annotations for top genres
  annotate("text", x = 2015, y = 0.95, label = "Horror", color = "white", fontface = "bold", size = 6) +
  annotate("text", x = 2017, y = 0.87, label = "Comedy", color = "white", fontface = "bold", size = 6) +
  annotate("text", x = 2010, y = 0.73, label = "Documentary", color = "white", fontface = "bold", size = 6) +
  annotate("text", x = 2020, y = 0.71, label = "Drama", color = "white", fontface = "bold", size = 6) +
  
  scale_fill_manual(values = cols) +
  
  scale_x_continuous(expand = c(0, 0), breaks = seq(1990, 2024, by = 5), limits = c(1990, 2024)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  labs(
    title = "Trends in Monster Movies Genres Over Time",
    subtitle = "Popularity of different genres from 1990 to 2024",
    caption = paste0(
      "Source: <b>Internet Movie Database</b> | ",
      "Graphic: <b>Natasa Anastasiadou</b>"
    )
  ) +
  
  theme_minimal() +
  
  theme(    
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, color = "grey30"),
    axis.text.y = element_blank(),
            
    legend.position = "none",
        
    panel.grid.major = element_line(linewidth = .35, color = "grey85"),
    panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
            
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Candara"),
    plot.subtitle = element_markdown(size = 14, hjust = 0.5, family = "Candara", color = "grey30"),
    plot.caption  = element_markdown(margin = margin(t = 25), size = 10, family = "Candara", hjust = 1),
            
    plot.margin = margin(20, 20, 20, 20),
            
    plot.background = element_rect(fill = "grey95", color = NA)
  )

p

ggsave(
  plot = p, filename = "Rplot.png",
  width = 14, height = 10, units = "in", dpi = 600
)  

