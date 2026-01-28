

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
# library(colorspace)
library(ggtext)
library(scales)


# load data ------

companies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/companies.csv')
# legal_nature <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/legal_nature.csv')
# qualifications <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/qualifications.csv')
# size <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-27/size.csv')


# clean data ------

d <- companies[, .(legal_nature, capital_stock, company_size)]

d1 <- d[, .(median_capital = median(capital_stock, na.rm = TRUE), n_companies = .N), by = .(company_size, legal_nature)]

d1 <- d1[n_companies > 10]


d1$legal_nature <- factor(d1$legal_nature, levels = sort(unique(d1$legal_nature), decreasing = TRUE))

col = c('#3a5cbc','#6F99AD','#F39B7F')


# plot -------

gr = ggplot(d1) +
    
    geom_point(
        aes(x = median_capital, y = legal_nature, size = n_companies, fill = company_size),
        shape = 21, stroke = .25
    ) +
    
    scale_x_log10(
        labels = label_number(scale_cut = cut_short_scale(), prefix = "")
    ) +

    scale_size_continuous(
        range = c(3, 7),
        name = "No. of Companies"
    ) +
    
    facet_grid(
        rows = vars(company_size),
        scales = "free_y",
        space = "free_y"
    ) +
    
    scale_fill_manual(
        values = col,
        guide = "none"
    ) +
    
    labs(
        title = "Median Capital Stock of Brazilian Companies",
        subtitle = "Median capital (BRL) by legal nature and size category.",
        caption = "Source: <b>Brazilian Companies data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Media Capital Stock",
        y = NULL
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .5, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.6, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.5),
        
        panel.grid.major = element_line(linewidth = 0.25, color = "grey80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = .4),
        
        plot.background = element_rect(fill = "#fffaf9", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    )

    
    
gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 10, units = "in", dpi = 600
)

