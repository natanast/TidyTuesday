

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)


# load data ------

# movies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
shows <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')


# clean data ------

reports_order <- sort(unique(shows$report))
first_report <- reports_order[1]
last_report <- reports_order[length(reports_order)]

views_first <- shows[report == first_report, .(title, views_first = views)]
views_last <- shows[report == last_report, .(title, views_last = views)]

views_compare <- merge(views_first, views_last, by = "title", all = FALSE)
top_titles <- views_compare[order(-views_last)][1:10]


top_titles$title <- top_titles$title |> str_extract("^[^/]+") |> str_squish()


# plot --------

ggplot(top_titles) +

    geom_segment(
        aes(x = views_first, xend = views_last, y = reorder(title, views_last),
        yend = reorder(title, views_last)),
        color = "grey25", 
        size = .75
    ) +
    
    geom_point(
        aes(x = views_first, y = reorder(title, views_last)),
        color = "#7ca6bb" |> darken(.55),  
        fill = "#7ca6bb" |> lighten(.25), 
        shape = 21, 
        stroke = .95,
        size = 3.5
    ) +
    
    geom_point(
        aes(x = views_last, y = reorder(title, views_last)),
        color = "#ed7b76" |> darken(.55),  
        fill = "#ed7b76" |> lighten(.25), 
        shape = 21, 
        stroke = .95,
        size = 3.5
    ) +
    
    labs(
        title = "Change in Views from First to Last Report",
        subtitle = "Exploring who created the Netflix series",
        caption = "Source: <b>Netflix data</b> • Graphic: <b>Natasa Anastasiadou</b>",
        x = "Views"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title.y = element_blank(),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .2, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.65, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.35),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )


# Save the plot
ggsave(
    plot = gr, filename = "plot.png",
    width = 8, height = 8, units = "in", dpi = 600
)



