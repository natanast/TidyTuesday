

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)

# load data --------

spells <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-17/spells.csv')


# data cleaning ------




# plot --------


gr

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
