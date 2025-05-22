

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)


# load data --------

water_quality <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/water_quality.csv')


# clean data ------

water_quality[, year := year(date)]


df <- water_quality[, 
    .(mean_bacteria = mean(enterococci_cfu_100ml, na.rm = TRUE)),
    by = .(year, council)
]


df$mean_bacteria <- df$mean_bacteria |> round(2)


df$council <- df$council |> str_remove_all("Council") |> str_squish()

df[council == "The of the Municipality of Hunters Hill", council := "Hunters Hill"]


top_councils <- df[, .(avg_bacteria = mean(mean_bacteria, na.rm = TRUE)), by = council]

top_councils <- top_councils[order(-avg_bacteria)][1:10, council]

df_filtered <- df[council %in% top_councils]



# plot -------

col = c('#396375', '#5a8192', '#7f9faa', '#a7bec0', '#d2ded1', '#febaad', '#f49992', '#e37b78', '#cc5f5e', '#b24745')

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
    
    # scale_fill_viridis_d(option = "turbo", direction = -1) + 
    scale_fill_manual(values = col) +
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "Streamgraph of Mean Enterococci Bacteria Levels by Council Over Years",
        subtitle = "<b>Each dot</b> represents a <b>terminated award</b> within a U.S. National Science Foundation (NSF) directorate.",
        caption = "Source: <b>U.S. NSF Grant Terminations data </b> | Graphic: <b>Natasa Anastasiadou</b>", 
        x = "",
        y = "Mean Enterococci (CFU/100ml)",
        fill = "Council"
    ) +
    
    theme(
        
        legend.position = "right",
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),

        plot.title = element_markdown(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),

        plot.background = element_rect(fill = "grey93", color = NA),
        plot.margin = margin(20, 20, 20, 20)

    )

     
#     theme(
#         legend.position = "none",
#         
#         axis.title = element_blank(),
#         
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 9.5),
#         
#         strip.text = element_text(size = 9.5),
#         
#         panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
# 
#         plot.background = element_rect(fill = "grey93", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# 


gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)

