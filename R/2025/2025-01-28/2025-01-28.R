

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)


# load data --------

water_insecurity_2022  <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')
water_insecurity_2023  <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')


# data cleaning ------

df1 <- water_insecurity_2022[, .(name, year, total_pop, plumbing)]
df2 <- water_insecurity_2023[, .(name, year, total_pop, plumbing)]


df1$state <- str_split(df1$name, ", ", simplify = TRUE)[, 2]
df2$state <- str_split(df2$name, ", ", simplify = TRUE)[, 2]


df1 <- df1[which(!is.na(plumbing))]
df2 <- df2[which(!is.na(plumbing))]


merged <- df1 |> merge(df2, by = c("name", "state"), suffixes = c("_2022", "_2023"), all = FALSE)

# Calculate the change in plumbing
p <- merged[, .(plumbing_change = sum(plumbing_2023) - sum(plumbing_2022)), by = state]


# plot -------

ggplot(p, aes(x = state, y = plumbing_change)) +
    
    geom_bar(stat = "identity", position = "dodge") +
    
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    
    labs(
        title = "Percent of Households Lacking Plumbing by State: 2022 vs 2023",
        x = "State", 
        y = "Percent of Households Lacking Plumbing"
    )         





