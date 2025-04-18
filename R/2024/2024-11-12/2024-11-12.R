

rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggthemes)
library(paletteer)



# load data ------------

countries <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/countries.csv')
country_subdivisions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-12/country_subdivisions.csv')


# clean data --------
# Count the subdivisions per type for each country
subdiv_count <- country_subdivisions[, .(subdivision_count = .N), by = .(alpha_2, type)]

# For each country, find the most frequent subdivision type
predominant_subdiv <- subdiv_count[, .SD[which.max(subdivision_count)], by = alpha_2]



# Merge with the 'countries' dataset to get official country names
data <- merge(predominant_subdiv, countries[, .(alpha_2, name)], by = "alpha_2", all.x = TRUE)


data <- data[subdivision_count > 20]

data <- data[order(name)]
data[, name := factor(name, levels = rev(unique(name)))]


colors <- paletteer_d("ggthemes::Tableau_20")

# Bubble plot
p = ggplot(data) +
    
    geom_point(
        aes(x = subdivision_count, y = name,
            size = subdivision_count, 
            fill = type
        ),
        
        shape = 21, , stroke = .15,
        alpha = 0.75
    ) +
    
    scale_size(range = c(2, 10), guide = "none") +
    scale_fill_manual(values = colors) +
    

    theme_minimal() +

    labs(title = "Country Subdivision Counts and Types",
         x = "Number of Subdivisions", 
         y = "Country",
         subtitle = "Count of Subdivisions with the <b>predominant</b> type highlighted for each country. <br> Only countries with more than 20 subdivisions are shown.",
         caption = "Source: <b> {ISOcodes} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
         fill = "Type"
         ) +
    
    theme(
        # legend.position = c(.8, .7),
        legend.position = "right",
        legend.title = element_markdown(size = 12, family = "Candara", color = "grey30"),
        legend.text = element_markdown(size = 12, family = "Candara", color = "grey30"),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown(size = 12, family = "Candara"),
        axis.text.x = element_markdown(size = 12, family = "Candara"), 
        axis.text.y = element_markdown(size = 12, family = "Candara"), # Enable markdown for color
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption = element_markdown(margin = margin(t = 12), size = 10, family = "Candara", hjust = 2),
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    ) 


ggsave(
    plot = p, filename = "Rplot.png",
    width = 12, height = 12, units = "in", dpi = 600
)    
