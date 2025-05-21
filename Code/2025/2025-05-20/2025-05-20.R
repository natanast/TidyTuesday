

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)
library(paletteer)


# load data --------

nsf_terminations <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')


# clean data ------



# plot ---------

col = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 9)


gr = df_expanded |> 
    
    ggplot(aes(x = x, y = y)) +
    
    geom_point(aes(fill = directorate), size = 3, shape = 21, color = "white", stroke = .15) +
    
    facet_wrap(~directorate, nrow = 1, strip.position = "bottom") +

    coord_equal() +
    
    scale_fill_manual(values = col) +

    labs(
        title = "Grants Terminated Across Directorates",
        subtitle = "<b>Each dot</b> represents a <b>terminated award</b> within a U.S. National Science Foundation (NSF) directorate.",
        caption = "*Science, Technology, Engineering, and Mathematics <br> <br>Source: <b>U.S. NSF Grant Terminations data </b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.title = element_blank(),
        
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9.5),
        
        strip.text = element_text(size = 9.5),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),

        plot.background = element_rect(fill = "grey93", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr

ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)

