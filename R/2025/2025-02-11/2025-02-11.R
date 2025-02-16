

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

cdc_datasets <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/cdc_datasets.csv')


# data cleaning ------

df <- cdc_datasets[, .N, by = category][N > 15]

# Order categories by count
df <- df[order(-N)]

# Filter out the specific category
df <- df[df$category != "This dataset has not been categorized", ]

# color palette
col = c('#60608b', '#6c6c98', '#9291be', '#9e9ecb', '#b9b8e5', 
        '#c6c5f2', '#ffeacf', '#ffd5be', '#f7ad9c', '#f09a8c', 
        '#dc756e', '#d0645f', '#c15451')


# Adding '*'
df$category <- ifelse(df$category == "NNDSS", "*NNDSS", 
                      ifelse(df$category == "NCHS", "**NCHS", df$category))



df$category <- str_wrap(df$category, width = 30)

df$category <- factor(df$category, levels = df$category)


# plot -------
gr = ggplot(data = df) +
    geom_point(
        aes(x = N, y = reorder(category, N),
            size = N, 
            fill = category
        ),
        shape = 21, stroke = .25, alpha = 0.8
    ) +
    scale_size_continuous(range = c(6, 13)) +
    scale_fill_manual(values = col) +  # Use the reversed ordered color palette
    theme_minimal() +
    labs(
        title = "Tracking CDC Datasets Removed from Public Access and Preserved in Archives",
        subtitle = "Only categories with more than 15 datasets are displayed",
        caption = "Source: <b> CDC Datasets</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Number of datasets",
        y = ""
    ) +
    theme(
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        axis.text.x = element_text(hjust = 1, vjust = 0.5, family = "Candara", size = 13),
        axis.text.y = element_text(hjust = 1, vjust = 0.5, family = "Candara", size = 14),
        axis.title.x = element_markdown(family = "Candara", size = 14, margin = margin(t = 14)),
        axis.title.y = element_markdown(family = "Candara", size = 14, margin = margin(r = 14)),
        
        plot.title = element_markdown(size = 19, face = "bold", hjust = 1, family = "Candara"),
        plot.subtitle = element_markdown(size = 15, hjust = 0.05, family = "Candara", color = "grey30"),
        plot.caption = element_markdown(margin = margin(t = 35), size = 11, family = "Candara", hjust = 1),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    ) +
    
    annotate(
        "text", x = min(df$N), y = -Inf, label = "* NNDSS: National Notifiable Diseases Surveillance System\n     ** NCHS: National Center for Health Statistics",
        hjust = -0.4, vjust = -1.8, size = 4, family = "Candara", color = "black"
    )


ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
