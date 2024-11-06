


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(stringr)
library(ggplot2)


# load data ------------

democracy_data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')


# data cleaning --------



# plot ---------

library(ggstream)

ggplot(x, aes(x = year, y = N, fill = genres, label = genres)) +
    
    geom_stream(color = "white", linewidth = .065, type = "ridge", bw = 1) + 
   
    # annotate("text", x = 2019, y = 10, label = "Triller", color = "gray9", fontface = "bold") +
    # annotate("text", x = 2020.5, y = 130, label = "Family", color = "white", fontface = "bold") +
    # annotate("text", x = 2020, y = 170, label = "Drama", color = "white", fontface = "bold") +
    # annotate("text", x = 2021, y = 250, label = "Comedy", color = "white", fontface = "bold") +
    
    # geom_stream_label(size = 4) +
    # 
    scale_x_continuous(expand = c(0, 0), breaks = seq(1990, 2024, by = 5), limits = c(1990, 2024)) +
    scale_y_continuous(expand = c(0,0)) +

    # scale_color_manual(values = c("#BC3C29", "#0072B5", "#E18727")) +
    # scale_fill_manual(values = c("#BC3C29", "#0072B5", "#E18727")) +
    # 
    theme_minimal()

    # 
    # theme(
    #     # legend.position = "none"
    #     # plot.background = element_rect(fill = bg, color = NA),
    #     # panel.grid.major.x = element_line(linewidth = 0.5, color = lighten(bg, 0.1)),
    #     # panel.grid.minor = element_line(linewidth = .25, color = "grey65", linetype = "dashed"),
    #     # axis.text.x = element_text(color = lighten(bg, 0.8), size = 10),
    #     # axis.text.y = element_text(color = lighten(bg, 0.8), size = 10),
    #     # axis.title.y = element_text(color = lighten(bg, 0.8), margin = margin(r = 15)),
    #     # axis.title.x = element_text(color = lighten(bg, 0.8), margin = margin(t = 15)),
    #     # plot.margin = margin(20, 20, 20, 20),
    #     # legend.title = element_text(color = "grey85")
    # )



