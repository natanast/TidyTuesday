

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(extrafont)
library(ggtext)
library(waffle)


# load data --------

nsf_terminations <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')


# clean data ------

df = nsf_terminations[, .(directorate)]


df = df[!is.na(directorate), .N, by = directorate]

df$directorate <- df$directorate |> str_wrap(width = 15)

# plot ---------

df |> 
    ggplot(aes(fill = directorate, values = N)) +
    
    geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
    
    facet_wrap(~directorate, nrow = 1, strip.position = "bottom") +
    
    
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.text.x = element_blank(),
        
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill = "grey93", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )




