
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

agencies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')

# data cleaning ------
