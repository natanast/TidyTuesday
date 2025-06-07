

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)
# library(ggstream)
# library(ggforce)


# load data ------

# gutenberg_authors <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_authors.csv')
# gutenberg_languages <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_languages.csv')
gutenberg_metadata <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_metadata.csv')
# gutenberg_subjects <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_subjects.csv')


# clean data ------

df <- gutenberg_metadata[
    , .(bookshelf = unlist(str_split(gutenberg_bookshelf, "/"))), 
    by = gutenberg_id
]

df <- df[, .N, by = bookshelf][order(-N)]

# df1 <- df[1:200]

df$bookshelf <- df$bookshelf |> str_remove("^Browsing: ")


# Process the data 
df_plot <- df[, {
    
    dat.egg <- circleProgressiveLayout(N)
    dat.egg <- circleLayoutVertices(dat.egg, npoints = 100)
    
    cbind(dat.egg, .SD[dat.egg$id])
}]


df_plot_l <- df_plot |>
    group_by(bookshelf, N, id) |>
    summarise(
        x = (min(x) + max(x)) / 2,
        y = (min(y) + max(y)) / 2
    ) |>
    setDT()


df_plot_l <- df_plot_l[order(-N)][1:20]

df_plot_l$lbl = df_plot_l$bookshelf |> str_wrap(width = 10)




p <- df_plot |>
    
    ggplot(aes(x, y, group = id)) +
    
    geom_polygon(aes(fill = bookshelf), fill = "#7f9faa", color = "grey30", linewidth = .25) +
    
    geom_shadowtext(
        data = df_plot_l, 
        aes(x, y, label = lbl, size = N),
        inherit.aes = FALSE,
        color = "grey1", 
        bg.color = "#d9e3f1", 
        bg.r = .05,
        family = "Candara"
    ) +
    
    
    # scale_fill_manual(
    #     values = col,
    #     name = "Team"
    #     
    # ) +
    
    scale_size_continuous(guide = "none", range = c(2, 8)) +
    
    
    # labs(
    #     title = "Expectations vs Performance: Top NCAA Men's March Madness Teams in 2024",
    #     subtitle = "<b> Bubble size</b> represents each team's <b>championship likelihood</b> — bigger size indicated bigger likelihood. <br> <b>Label</b> shows their Performance Against Seed Expectations <b>(PASE)</b> — larger PASE indicates better performance.",
    #     caption = "Source: <b> NCAA Men's March Madness</b> | Graphic: <b>Natasa Anastasiadou</b>"
    # ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        legend.position = "none",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        # plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5,  color = "grey30"),
        # plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1.25),
        # 
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )

p



ggsave(
    plot = p, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)




