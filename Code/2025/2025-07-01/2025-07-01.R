

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)

library(extrafont)
library(ggtext)


# load data ------

gas_data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')


# clean data ------

gas_data[, date := as.Date(date)]

gas <- gas_data[fuel == "gasoline" & grade != "all" & formulation != "all"]

gas[, year := as.integer(format(date, "%Y"))]

yearly_gas <- gas[, .(price = mean(price, na.rm = TRUE)), by = .(year, grade)]

yearly_gas[, decade := paste0(floor(year / 10) * 10, "s")]

yearly_gas$grade <- yearly_gas$grade |> factor(levels = c("regular", "midgrade", "premium"))



# plot --------


col = c('#3a5cbc','#b9b8e7', '#b24745')

gr = ggplot(yearly_gas, aes(x = factor(year), y = price, fill = grade)) +
    
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.9) +
    
    facet_wrap(~ decade, scales = "free_x") +
    
    scale_fill_manual(values = col) +

    labs(
        title = "Gasoline Prices Across Grades and Decades in the U.S.",
        subtitle = "Annual average retail prices for regular, midgrade, and premium gasoline from 1990 to 2025",
        caption = "Source: <b> Weekly US Gas Prices</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Price per Gallon (USD)",
        fill = "Grade"
    ) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        axis.title.y = element_text(size = 12, margin = margin(r = 15)),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5,  color = "grey30", margin = margin(b = 15)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "grey90", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr

ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 10, units = "in", dpi = 600
)





