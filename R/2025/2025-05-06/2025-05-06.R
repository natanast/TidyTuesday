

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



# One point per termination
df_expanded <- df[, .(directorate = rep(directorate, times = N))]

# Create grid layout: each directorate starts at (0,0)
df_expanded[, id := seq_len(.N), by = directorate]
df_expanded[, `:=`(
    x = (id - 1) %% 10,
    y = (id - 1) %/% 10  # negative y to grow downward
)]



# plot ---------


df_expanded |> 
    
    ggplot(aes(x = x, y = y)) +
    
    geom_point(aes(fill = directorate), size = 3, shape = 21, color = "white", stroke = 0.3) +
    
    facet_wrap(~directorate, nrow = 1, strip.position = "bottom") +

    coord_equal() +

    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.text.x = element_blank(),
        
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill = "grey93", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )



ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)

