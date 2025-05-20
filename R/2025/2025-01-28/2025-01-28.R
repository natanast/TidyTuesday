
rm(list = ls())
gc()

# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)

# load data --------

water_insecurity_2022  <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')
water_insecurity_2023  <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')


# data cleaning ------

df1 <- water_insecurity_2022[, .(name, year, total_pop, plumbing)]
df2 <- water_insecurity_2023[, .(name, year, total_pop, plumbing)]


df1$state <- str_split(df1$name, ", ", simplify = TRUE)[, 2]
df2$state <- str_split(df2$name, ", ", simplify = TRUE)[, 2]


df1 <- df1[which(!is.na(plumbing))]
df2 <- df2[which(!is.na(plumbing))]


merged <- df1 |> merge(df2, by = c("name", "state"), suffixes = c("_2022", "_2023"), all = FALSE)

p <- merged[, .(plumbing_change = sum(plumbing_2023) - sum(plumbing_2022)), by = state]

p$type <- ifelse(p$plumbing_change > 0, "Positive", "Negative")


# plot -------

gr = ggplot(p, aes(x = state, y = plumbing_change)) +
    
    geom_segment(
        aes(x = state, xend = state,
            y = 0 , yend = plumbing_change),
        color = "grey35", linewidth = 0.45, lineend = "round"
    ) +
    
    geom_point( aes(fill = type, color = type), size = 4.5, shape = 21, stroke = .15) +
    
    # Define custom colors for each type
    scale_fill_manual(values = c("Negative" = "#B24745", "Positive" = "#79AF97")) +
    scale_color_manual(values = c("Negative" = "#B24745", "Positive" = "#79AF97") |> darken(.25), guide = "none") +
    

    theme_minimal() +

    labs(
        title = "Number of Households Lacking Plumbing by State: 2022 vs 2023",
        subtitle = "<b><span style='color: #79AF97; font-weight: bold;'>Positive</span></b> 
                    values represent states with an <b>increase</b> in the number of households lacking plumbing from 2022 to 2023, 
                    <br>while <b><span style='color: #B24745;'>negative</span></b> values indicate a <b>decrease</b>.</br>",
        caption = "Source: <b>Water insecurity data</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme(
        legend.position = "none",
        
        plot.margin = margin(20, 20, 20, 20),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        axis.text.x = element_markdown(angle = 90, hjust = 1, vjust = 0.5, family = "Candara", size = 12),
        
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        
        plot.title = element_markdown(size = 19, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, family = "Candara", hjust = 1),

        
        plot.background = element_rect(fill = "grey93", color = NA)
    )



ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)





