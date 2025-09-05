

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)


# load data ------

frogID_data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv')
frog_names <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv')


# clean data ------

frogs_merged <- merge(frogID_data, frog_names, by = "scientificName", all.x = TRUE)

# Add month & season
frogs_merged[, month := month(eventDate)]
frogs_merged[, season := fcase(
    month %in% c(12, 1, 2), "Summer",
    month %in% c(3, 4, 5), "Autumn",
    month %in% c(6, 7, 8), "Winter",
    month %in% c(9, 10, 11), "Spring"
)]


library(maps)

aus_map <- map_data("world", region = "Australia")

gr = ggplot() +
    
    
    geom_polygon(data = aus_map, aes(x = long, y = lat, group = group),
                 fill = "grey90", color = "white") +
    
    geom_point(data = frogs_merged, 
               aes(x = decimalLongitude, y = decimalLatitude),
               alpha = 0.3, size = 0.5, color = "#6F99AD") +
    
    coord_fixed(1.3) +
    
    facet_wrap(~season) +
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "FrogID observations across Australia",
        subtitle = "Top 5 increases and decreases in view counts between the first and last reporting periods",
        caption = "Source: <b>FrogID data</b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme(
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey95", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey94", color = NA)
    )


gr

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

    geom_point(
        aes(
            x = views_last, y = reorder(title, diff_views), 
            fill = change_type, color = change_type
        ),
        shape = 21, 
        stroke = .45,
        size = 3.5
    ) +

    
    scale_color_manual(
        name = NULL,
        values = c(
            "First Report (2023)" = "#7ca6bb" |> darken(0.35),
            "Increase (2025)" = "#3DA873" |> darken(0.35),
            "Decrease (2025)" = "#d9534f" |> darken(0.35)
        )
    ) +
    scale_fill_manual(
        name = NULL,
        values = c(
            "First Report (2023)" = "#7ca6bb" |> lighten(0.25),
            "Increase (2025)" = "#3DA873" |> lighten(0.25),
            "Decrease (2025)" = "#ed7b76" |> lighten(0.25)
        )
    ) +


    scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
    
    facet_wrap(
        ~change_type, 
        scales = "free_y", 
        ncol = 1,
        labeller = labeller(
            change_type = c(
                "Increase (2025)" = "Increase",
                "Decrease (2025)" = "Decrease"
            )
        )
    ) +
    
    labs(
        title = "Netflix Shows with the Largest Shifts in Popularity",
        subtitle = "Top 5 increases and decreases in view counts between the first and last reporting periods",
        caption = "Source: <b>Netflix data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Views"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        legend.text = element_text(size = 10),
        
        axis.text = element_text(size = 11),
        axis.title.y = element_blank(),
        
        strip.text = element_text(size = 12),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .5, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.6, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.85),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 9, height = 9, units = "in", dpi = 600
)


