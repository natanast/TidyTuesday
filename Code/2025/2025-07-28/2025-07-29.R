

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

reports_order <- shows$report |> unique() |> sort()
first_report <- reports_order[1]
last_report <- reports_order[length(reports_order)]


views_first <- shows[report == first_report, .(title, views_first = views)]
views_last <- shows[report == last_report, .(title, views_last = views)]


views_compare <- merge(views_first, views_last, by = "title", all = FALSE)
# top_titles <- views_compare[order(-views_last)][1:10]


# top_titles$title <- top_titles$title |> str_extract("^[^/]+") |> str_squish()



# Calculate difference for all shows
views_compare[, diff_views := views_last - views_first]

# Clean titles
views_compare[, title := str_extract(title, "^[^/]+") |> str_squish()]

# Get top 5 increases
top_increases <- views_compare[diff_views > 0][order(-diff_views)][1:5]

# Get top 5 decreases
top_decreases <- views_compare[diff_views < 0][order(diff_views)][1:5]

# Add a change_type column
top_increases[, change_type := "Increase"]
top_decreases[, change_type := "Decrease"]

# Combine both sets
top_changes <- rbind(top_increases, top_decreases)




# plot --------

gr = ggplot(top_changes) +

    geom_segment(
        aes(
            x = views_first, xend = views_last,  
            y = reorder(title, diff_views), yend = reorder(title, diff_views)
        ),
        color = "grey25", 
        size = .55
    ) +
    
    geom_point(
        aes(x = views_first, y = reorder(title, diff_views)),
        color = "#7ca6bb" |> darken(.35),  
        fill = "#7ca6bb" |> lighten(.25), 
        shape = 21, 
        stroke = .45,
        size = 3.5
    ) +
    
    # geom_point(
    #     aes(
    #         x = views_last, y = reorder(title, diff_views), 
    #         color = change_type, fill = change_type
    #     ),
    #     shape = 21, 
    #     stroke = .45,
    #     size = 3.5
    # ) +
    # 
    # scale_color_manual(
    #     values = c(
    #         "Increase" = "forestgreen" |> darken(.35), 
    #         "Decrease" = "#ed7b76" |> darken(.35) 
    #     )
    # ) +
    
    # scale_fill_manual(
    #     values = c(
    #         "Increase" = "forestgreen" |> lighten(.25), 
    #         "Decrease" = "#ed7b76" |> lighten(.25)
    #     )
    # ) +
    
    geom_point(
        aes(x = views_last, y = reorder(title, diff_views)),
        color = "#ed7b76" |> darken(.35),
        fill = "#ed7b76" |> lighten(.25),
        shape = 21,
        stroke = .45,
        size = 3.5
    ) +

    labs(
        title = "Top 10 Netflix Shows by Increase and Decrease in Views",
        subtitle = "Change between first and last report periods",
        caption = "Source: <b>Netflix data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Views"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title.y = element_blank(),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .2, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.2, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 9, height = 9, units = "in", dpi = 600
)


