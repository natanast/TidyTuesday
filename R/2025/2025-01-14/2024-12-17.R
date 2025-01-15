

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)

# load data --------



# data cleaning ------




# plot --------


gr

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
