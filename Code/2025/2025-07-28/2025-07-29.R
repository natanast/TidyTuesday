

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)
library(scales)


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


# Calculate difference for all shows
views_compare[, diff_views := views_last - views_first]

# Clean titles
views_compare[, title := str_extract(title, "^[^/]+") |> str_squish()]

# Get top 5 increases
top_increases <- views_compare[diff_views > 0][order(-diff_views)][1:5]

# Get top 5 decreases
top_decreases <- views_compare[diff_views < 0][order(diff_views)][1:5]

# Add a change_type column
top_increases[, change_type := "Increase (2025)"]
top_decreases[, change_type := "Decrease (2025)"]

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
        aes(
            x = views_first, 
            y = reorder(title, diff_views), 
            fill = "First Report (2023)", 
            color = "First Report (2023)"
        ),
        shape = 21,
        stroke = .45,
        size = 3.5
    ) +
    # geom_point(
    #     aes(
    #         x = views_first, y = reorder(title, diff_views)
    #         # fill = change_type, color = change_type
    #     ),
    #     fill = "#7ca6bb" |> lighten(0.25),
    #     color = "#7ca6bb" |> darken(0.35),
    #     shape = 21, 
    #     stroke = .45,
    #     size = 3.5
    # ) +
    
    geom_point(
        aes(
            x = views_last, y = reorder(title, diff_views), 
            fill = change_type, color = change_type
        ),
        shape = 21, 
        stroke = .45,
        size = 3.5
    ) +
    
    # scale_color_manual(
    #     name = NULL,
    #     values = c(
    #         "First Report (2023)" = "#7ca6bb" |> darken(0.35),
    #         "Increase" = "#5cb85c",
    #         "Decrease" = "#d9534f"
    #     )
    # ) +
    # scale_fill_manual(
    #     name = NULL,
    #     values = c(
    #         "First Report (2023)" = "#7ca6bb" |> lighten(0.25),
    #         "Increase" = "#5cb85c" |> lighten(0.25),
    #         "Decrease" = "#d9534f" |> lighten(0.25)
    #     )
    # ) +
    
    scale_color_manual(
        name = NULL,
        values = c(
            "First Report (2023)" = "#7ca6bb" |> darken(0.35),
            "Increase (2025)" = "#5cb85c",
            "Decrease (2025)" = "#d9534f"
        )
    ) +
    scale_fill_manual(
        name = NULL,
        values = c(
            "First Report (2023)" = "#7ca6bb" |> lighten(0.25),
            "Increase (2025)" = "#5cb85c" |> lighten(0.25),
            "Decrease (2025)" = "#d9534f" |> lighten(0.25)
        )
    ) +
    
    # scale_color_manual(
    #     name = NULL,
    #     values = c(
    #         "Increase" = "#4CAF50",  # green
    #         "Decrease" = "#F44336"   # red
    #     )
    # ) +
    # scale_fill_manual(
    #     name = NULL,
    #     values = c(
    #         "Increase" = "#A5D6A7",  # light green
    #         "Decrease" = "#EF9A9A"   # light red
    #     )
    # ) +

    # geom_point(
    #     aes(
    #         x = views_first, y = reorder(title, diff_views), 
    #         fill = "First Report (2023)", color = "First Report (2023)"
    #     ),
    #     shape = 21, 
    #     stroke = .45,
    #     size = 3.5
    # ) +
    # 
    # geom_point(
    #     aes(
    #         x = views_last, y = reorder(title, diff_views), 
    #         fill = "Last Report (2025)", color = "Last Report (2025)"
    #     ),
    #     shape = 21,
    #     stroke = .45,
    #     size = 3.5
    # ) +

    # scale_color_manual(
    #     name = NULL,
    #     values = c(
    #         "First Report (2023)" = "#7ca6bb" |> darken(0.35),
    #         "Last Report (2025)"  = "#ed7b76" |> darken(0.35)
    #     )
    # ) +
    # 
    # scale_fill_manual(
    #     name = NULL,
    #     values = c(
    #         "First Report (2023)" = "#7ca6bb" |> lighten(0.25),
    #         "Last Report (2025)"  = "#ed7b76" |> lighten(0.25)
    #     )
    # ) +
    
    scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
    
    facet_wrap(~change_type, scales = "free_y", ncol = 1) +
    
    labs(
        title = "Netflix Shows with the Largest Shifts in Popularity",
        subtitle = "Top 5 increases and decreases in view counts between the first and last reporting periods",
        caption = "Source: <b>Netflix data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Views"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        legend.text = element_text(size = 11),
        
        axis.text = element_text(size = 11),
        axis.title.y = element_blank(),
        
        strip.text = element_text(size = 12),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .15, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.65, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.45),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 9, height = 9, units = "in", dpi = 600
)


