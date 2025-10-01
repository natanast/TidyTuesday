

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)


# load data ------



# clean data ------


# plot -------

gr = ggplot() +
    
    
    geom_polygon(data = aus_map, aes(x = long, y = lat, group = group),
                 fill = "#fffaf9", color = "grey70", alpha = 0.65) +
    
    geom_point(
        data = frogs_merged, 
        aes(x = decimalLongitude, y = decimalLatitude),
        alpha = 0.5, 
        size = 1, 
        stroke = .25,
        color = "#6F99AD" |> darken(.25), 
        fill = "#6F99AD" |> lighten(.25),
        shape = 21
    ) +
    
    coord_fixed(1.3) +
    
    facet_wrap(~season) +
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "Seasonal Distribution of Frog Observations in Australia",
        subtitle = "FrogID data provide a continent-wide view of frog activity across time and space.",
        caption = "Source: <b>FrogID data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Longitude",
        y = "Latitude"
    ) +
    
    theme(
        plot.title = element_markdown(size = 16, face = "bold", hjust = .5, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.6, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.5),
        
        panel.grid.major = element_line(linewidth = 0.25, color = "grey80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = .4),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e8e8e7", color = NA)
    )


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 10, units = "in", dpi = 600
)

