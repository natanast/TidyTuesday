

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)
library(ggstream)


# load data ------

# clean data ------

# plot -------

gr = ggplot(df_filtered, aes(x = year, y = mean_bacteria, fill = council)) +
    
    geom_stream(
        extra_span = 0.2,
        bw = .9,
        lwd = 0.15,
        color = "grey20"
    ) +

    geom_stream(
        extra_span = 0.2, 
        true_range = "none",
        bw = .9,
        alpha = 0.2
    ) +
    
    scale_fill_manual(values = col) +
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "Coastal Water Quality Across NSW Councils Over Years",
        subtitle = "Top 10 councils by mean enterococci bacteria levels by council, measured in CFU per 100mL",
        caption = "Source: <b>Water Quality at Sydney Beaches data </b> | Graphic: <b>Natasa Anastasiadou</b>", 
        x = "",
        y = "Mean Enterococci (CFU/100ml)",
        fill = "Council"
    ) +
    
    theme(
        
        legend.position = "right",
        legend.key.size = unit(.75, "lines"),      
        legend.text = element_text(size = 9),      
        legend.title = element_text(size = 10),
        
        axis.text = element_text(size = 11),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),

        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = .5),

        plot.background = element_rect(fill = "grey93", color = NA),
        plot.margin = margin(20, 20, 20, 20)

    )


gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)

