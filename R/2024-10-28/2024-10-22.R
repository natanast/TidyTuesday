


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(stringr)
library(ggplot2)



# load data ------------

monster_movie_genres <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movie_genres.csv')
monster_movies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-29/monster_movies.csv')


# data cleaning --------


# Join dt2 to dt1 by "id" and "country", and add the value2 column
dt1 <- merge(monster_movie_genres, monster_movies[, c("tconst", "year")], by = c("tconst"))

x <- dt1[, by = .(year, genres), .N]

index <- which(x$N > 5)

x <- x[index, ]

# plot ---------

ggplot(x) +
    
    geom_area(aes(x = year, y = N, fill = genres, color = genres, linetype = genres), 
              linewidth = .8, alpha = .5, position = "identity") +
    
    
    scale_y_continuous(labels = scales::comma) +
    # scale_x_continuous(expand = c(0, 0), breaks = seq(1923, 2023, by = 25), limits = c(1923, 2023)) +
    # 
    # scale_color_manual(values = c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")) +
    # scale_fill_manual(values = c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")) +  
    # 
    theme_minimal() +
    
    theme(
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        
        legend.position = "right",
        
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)),
        
        plot.margin = margin(20, 20, 20, 20)
        
    ) 
    # 
    # labs(
    #     y = "Total Emissions MtCO2",
    #     x = "Years",
    #     title = "Total Emissions Over Time by Parent Entity",
    # ) 



library(ggstream)

ggplot(x, aes(x = year, y = N, fill = genres, label = genres)) +
    
    geom_stream(bw = .85) +
    geom_stream_label(size = 4) +
    
    scale_x_continuous(expand = c(0, 0), breaks = seq(2003, 2024, by = 5), limits = c(2003, 2024)) +
    
    scale_color_manual(values = c("#BC3C29", "#0072B5", "#E18727")) +
    scale_fill_manual(values = c("#BC3C29", "#0072B5", "#E18727")) +
    
    theme_minimal() +
    
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = bg, color = NA),
        panel.grid.major.x = element_line(linewidth = 0.5, color = lighten(bg, 0.1)),
        panel.grid.minor = element_line(linewidth = .25, color = "grey65", linetype = "dashed"),
        axis.text.x = element_text(color = lighten(bg, 0.8), size = 10),
        axis.text.y = element_text(color = lighten(bg, 0.8), size = 10),
        axis.title.y = element_text(color = lighten(bg, 0.8), margin = margin(r = 15)),
        axis.title.x = element_text(color = lighten(bg, 0.8), margin = margin(t = 15)),
        plot.margin = margin(20, 20, 20, 20),
        legend.title = element_text(color = "grey85")) +
    
    labs(y = "Number of papers")




