


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(stringr)
library(ggplot2)
library(ggtext)
library(dplyr)


# load data ------------

countries <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv')
country_subdivisions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/country_subdivisions.csv')



# Count the subdivisions per type for each country
subdiv_count <- country_subdivisions[, .(subdivision_count = .N), by = .(alpha_2, type)]

# For each country, find the most frequent subdivision type
predominant_subdiv <- subdiv_count[, .SD[which.max(subdivision_count)], by = alpha_2]

# Merge with the 'countries' dataset to get official country names

data <- merge(predominant_subdiv, countries[, .(alpha_2, name)], by = "alpha_2", all.x = TRUE)


# Bubble plot
ggplot(data) +
    
    geom_point(
        aes(x = subdivision_count, y = name,
            size = subdivision_count, 
            fill = type
        ),
        
        shape = 21, , stroke = .15,
        alpha = 0.75
    ) +
    
    scale_size(range = c(0.5, 8)) +
        

    theme_minimal() +

    labs(title = "Country Subdivision Counts and Types",
         x = "Number of Subdivisions", 
         y = "Country",
         # subtitle = "<b>Female</b> leaders are a rare sight among the longest-serving monarchs and presidents, with only <b>3</b> appearing in the top 20.",
         caption = "Source: <b>Democracy and Dictatorship Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>"
         ) +
    
    # labs(title = "Top 20 Longest-Serving Rulers (Monarchs & Presidents)",
    #      subtitle = "<b>Female</b> leaders are a rare sight among the longest-serving monarchs and presidents, with only <b>3</b> appearing in the top 20.",
    #      caption = "Source: <b>Democracy and Dictatorship Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
    #      x = "", y = "") +

    theme(
        legend.position = "none",
        # legend.title = element_blank(),
        # legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 8, family = "Candara"), 
        axis.text.y = element_markdown(size = 8, family = "Candara"), # Enable markdown for color
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_markdown(size = 5, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption = element_markdown(margin = margin(t = 25), size = 10, family = "Candara", hjust = 1),
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )

