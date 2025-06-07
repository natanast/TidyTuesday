

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

df1 <- df[1:100]

df1$bookshelf <- df1$bookshelf |> str_remove("^Browsing: ")




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


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)






# Process the data 
df_plot <- df1[, {
    
    dat.egg <- circleProgressiveLayout(N)
    dat.egg <- circleLayoutVertices(dat.egg, npoints = 100)
    
    cbind(dat.egg, .SD[dat.egg$id])
}]


df_plot_l <- df_plot |>
    group_by(bookshelf, N, id) |>
    summarise(
        x = (min(x) + max(x)) / 2,
        y = (min(y) + max(y)) / 2
    )


df_plot_l$lbl = df_plot_l$bookshelf




p <- df_plot |>
    
    ggplot(aes(x, y, group = id)) +
    
    geom_polygon(aes(fill = bookshelf), color = "grey30", linewidth = .25) +
    
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
    
    scale_size_continuous(guide = "none", range = c(5, 9)) +
    

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

