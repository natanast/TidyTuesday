

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggplotify)

library(extrafont)
library(ggtext)

# load data --------

parfumo_data_clean <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv')
