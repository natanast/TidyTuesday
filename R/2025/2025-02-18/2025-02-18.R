

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

# cdc_datasets <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/cdc_datasets.csv')


# data cleaning ------



ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
