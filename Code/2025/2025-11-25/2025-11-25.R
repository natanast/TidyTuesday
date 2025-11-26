

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


# plot ------

ggplot(df_clean, aes(x = year, y = country, fill = overall_score)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
     
    geom_shadowtext(
        # aes(label = ifelse(overall_score > 0, overall_score, "")),
        aes(label = round(overall_score, 1)),
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .1, 
        size = 2
    ) +
    
    scale_fill_stepsn(
        colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(35, 45, 75, 85, 100),
        # transform = "log10",  
        # labels = round(overall_score),
        name = "SPI",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(8, "lines"), 
            barwidth = unit(0.25, "lines")
        )  # Centers the title
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        legend.title = element_text(size = 8, angle = 90, hjust = .5, face = "bold", color = "grey30"),
        legend.text = element_text(size = 8, color = "grey30"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    )



# plot ------ 

    
    # labs(
    #     title = "Road to Glory: EuroLeague Teams’ Final Four and Titles (1988–2025)",
    #     subtitle = "<b><span style='color: #426b7b; font-weight: bold;'>Darker</span></b> dots indicate <b><span style='color: #426b7b; font-weight: bold;'>Championship victories</span></b>, 
    #                 while <b><span style='color: #81a7ba; font-weight: bold;'>lighter</span></b> dots indicate  <b><span style='color: #81a7ba; font-weight: bold;'>Final Four appearances</span></b>.",
    #     caption = "Source: <b>Euroleague dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
    #     x = "Year",
    #     y = "",
    #     fill = ""
    # ) +
    
  

gr

ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)

