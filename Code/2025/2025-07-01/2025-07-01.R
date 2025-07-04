

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)

library(extrafont)


# load data ------

gas_data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')


# clean data ------

gas_data[, date := as.Date(date)]

gas <- gas_data[fuel == "gasoline" & grade != "all" & formulation != "all"]

gas[, year := as.integer(format(date, "%Y"))]

yearly_gas <- gas[, .(price = mean(price, na.rm = TRUE)), by = .(year, grade)]

yearly_gas[, decade := paste0(floor(year / 10) * 10, "s")]



# plot --------


ggplot(yearly_gas, aes(x = factor(year), y = price, fill = grade)) +
    
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.9) +
    
    facet_wrap(~ decade, scales = "free_x") +

    labs(
        title = "Yearly Average Gasoline Prices",
        subtitle = "Each bubble represents a bookshelf. <b>Larger </b> bubbles mean more books belong to that category.",
        caption = "Source: <b> {gutenbergr} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Price per Gallon (USD)",
        fill = "Grade"
    ) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        # plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        # plot.subtitle = element_markdown(size = 14, hjust = 0.5,  color = "grey30"),
        # plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )



# 
# ggsave(
#     plot = p, filename = "plot.png",
#     width = 8.5, height = 9, units = "in", dpi = 600
# )





