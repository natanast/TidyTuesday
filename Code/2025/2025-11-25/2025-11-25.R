

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)

library(shadowtext)


# load data ------

spi_indicators <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv')


# clean data ------

df <- spi_indicators[year >= 2016 & region == "Europe & Central Asia", ]

df_clean <- df[!is.na(overall_score)]

# order 
latest_order <- df_clean[year == 2023][order(overall_score)]$country
df_clean$country <- factor(df_clean$country, levels = latest_order)


# plot ------

gr = ggplot(df_clean, aes(x = year, y = country, fill = overall_score)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
     
    geom_shadowtext(
        aes(label = round(overall_score, 1)),
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .1, 
        size = 3
    ) +
    
    scale_fill_stepsn(
        colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(30, 60, 70, 80, 90),
        name = "SPI",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(0.25, "lines"), 
            barwidth = unit(8, "lines")
        )  
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    
    labs(
        title = "Statistical Performance Index (SPI) in Europe, 2016–2023",
        subtitle = "Overall SPI scores across years for European countries (World Bank SPI dataset)",
        caption = "Source: <b>Euroleague dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    
    theme(
        legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(size = 8, angle = 0, hjust = .5, face = "bold", color = "grey30"),
        legend.text = element_text(size = 8, color = "grey30"),
        
        axis.title = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5, color = "grey30"),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
    )

  

gr

ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 12, units = "in", dpi = 600
)

