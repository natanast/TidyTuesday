

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggplotify)

library(extrafont)
library(ggtext)

# load data --------

A64_traffic <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv')


# clean data -------

h <- A64_traffic[, c("Report Date", "0 - 10 mph", "11 - 15 mph",
                     "16 - 20 mph", "21 - 25 mph", "26 - 30 mph", "31 - 35 mph", 
                     "36 - 40 mph", "41 - 45 mph", "46 - 50 mph", "51 - 55 mph",
                     "56 - 60 mph", "61 - 70 mph", "71 - 80 mph", "80+ mph")]

# Compute daily averages
daily_averages <- h[, lapply(.SD, mean, na.rm = TRUE), by = `Report Date`]

# Convert to long format for ggplot
long_data <- melt(daily_averages, id.vars = "Report Date", variable.name = "Speed Range", value.name = "Avg Traffic Volume")



# Create the heatmap using ggplot2
gr = ggplot(long_data, aes(x = `Speed Range`, y = `Report Date`, fill = `Avg Traffic Volume`)) +
    
    geom_tile(linewidth = .15, color = "grey20") +

    scale_fill_gradientn(colors = c("#7d7ca9", "#abaad9", "#ffffe0", "#ff9a92", "#b24745")) +
    
    theme_minimal() +
    
    labs(
        title = "A64 Traffic Heatmap",
        subtitle = "Average traffic volume by speed range across different dates.",
        caption = "Source: <b> A64 Traffic Data</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    

    theme(
        legend.position = "right",
        legend.title.position = "left",
        
        
        legend.title = element_text(size = 10, face = "bold", family = "Candara", color = "grey30", angle = 90, hjust = .5),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        axis.text.x = element_text(size = 12, family = "Candara", angle = 45, hjust = 1, margin = margin(t = -20)),
        axis.text.y = element_text(size = 12, family = "Candara"),
        
        plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, family = "Candara", hjust = 1.3),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    ) 

gr


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 9, units = "in", dpi = 600
)

