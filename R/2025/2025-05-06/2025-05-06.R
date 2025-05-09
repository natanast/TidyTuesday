

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(extrafont)
library(ggtext)


# load data --------


nsf_terminations <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')
