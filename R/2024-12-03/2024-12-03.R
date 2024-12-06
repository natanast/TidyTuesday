

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)


# load data --------

A64_traffic <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv')


# clean data -------

m <- A64_traffic[]
