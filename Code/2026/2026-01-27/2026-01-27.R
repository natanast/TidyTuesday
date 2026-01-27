

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
# library(extrafont)
# library(colorspace)
library(ggtext)


# load data ------

companies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/companies.csv')
legal_nature <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/legal_nature.csv')
qualifications <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/qualifications.csv')
size <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/size.csv')


# clean data ------

d <- companies[, .(legal_nature, capital_stock, company_size)]

d1 <- d[, .N, by = .(company_size, legal_nature, capital_stock)]


plot_data <- d[, .(
    median_capital = median(capital_stock, na.rm = TRUE),
    n_companies = .N    # .N is a special symbol in data.table for "count"
), by = .(company_size, legal_nature)][n_companies > 10]

# plot -------

ggplot(plot_data) +
    
    geom_point(
        aes(x = median_capital, y = legal_nature, size = n_companies)
    ) +
    
    scale_x_log10(
        
        labels = label_number(scale_cut = cut_short_scale(), prefix = "R$ ")
    ) +
    
    scale_size_continuous(range = c(3, 7)) +
    
    facet_grid(
        rows = vars(company_size),
        scales = "free_y",
        space = "free_y"
    ) +
    
    # labs(
    #     title = "Seasonal Distribution of Frog Observations in Australia",
    #     subtitle = "FrogID data provide a continent-wide view of frog activity across time and space.",
    #     caption = "Source: <b>FrogID data</b> | Graphic: <b>Natasa Anastasiadou</b>",
    #     x = "Longitude",
    #     y = "Latitude"
    # ) +
    
    theme_minimal() +
    
    theme(
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .5, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.6, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.5),
        
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        
        panel.grid.major = element_line(linewidth = .4),
        
        panel.border = element_rect(fill = NA, linewidth = .4),
        axis.ticks = element_line(linewidth = .4),
        
        plot.margin = margin(20, 20, 20, 20)
    )

    
    
    
    

ggplot(d, aes(x = company_size, y = capital_stock)) +
    geom_violin() +

    geom_polygon(data = aus_map, aes(x = long, y = lat, group = group),
                 fill = "#fffaf9", color = "grey70", alpha = 0.65) +
    
    geom_point(
        data = frogs_merged, 
        aes(x = decimalLongitude, y = decimalLatitude),
        alpha = 0.5, 
        size = 1, 
        stroke = .25,
        color = "#6F99AD" |> darken(.25), 
        fill = "#6F99AD" |> lighten(.25),
        shape = 21
    ) +
    
    coord_fixed(1.3) +
    
    facet_wrap(~season) +
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "Seasonal Distribution of Frog Observations in Australia",
        subtitle = "FrogID data provide a continent-wide view of frog activity across time and space.",
        caption = "Source: <b>FrogID data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Longitude",
        y = "Latitude"
    ) +
    
    theme(
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .5, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.6, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.5),
        
        panel.grid.major = element_line(linewidth = 0.25, color = "grey80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = .4),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e8e8e7", color = NA)
    )

gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 10, units = "in", dpi = 600
)

