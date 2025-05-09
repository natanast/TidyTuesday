

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)
library(paletteer)


# load data --------

nsf_terminations <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')


# clean data ------

df = nsf_terminations[, .(directorate)]


df = df[!is.na(directorate), .N, by = directorate]

df$directorate <- df$directorate |> str_remove_all('^"|"$') |> str_wrap(width = 15)


# Sort by number of terminations descending
setorder(df, -N)
df[, directorate := factor(directorate, levels = directorate)]


# One point per termination
df_expanded <- df[, .(directorate = rep(directorate, times = N))]

# Create grid layout: each directorate starts at (0,0)
df_expanded[, id := seq_len(.N), by = directorate]
df_expanded[, `:=`(
    x = (id - 1) %% 10,
    y = (id - 1) %/% 10  # negative y to grow downward
)]



# plot ---------

col = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 9)


gr = df_expanded |> 
    
    ggplot(aes(x = x, y = y)) +
    
    geom_point(aes(fill = directorate), size = 3, shape = 21, color = "white", stroke = .15) +
    
    facet_wrap(~directorate, nrow = 1, strip.position = "bottom") +

    coord_equal() +
    
    scale_fill_manual(values = col) +

    labs(
        title = "Number of Households Lacking Plumbing by State: 2022 vs 2023",
        subtitle = "<b><span style='color: #79AF97; font-weight: bold;'>Positive</span></b>
                    values represent states with an <b>increase</b> in the number of households lacking plumbing from 2022 to 2023,
                    <br>while <b><span style='color: #B24745;'>negative</span></b> values indicate a <b>decrease</b>.</br>",
        caption = "Source: <b>Water insecurity data</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        # plot.title = element_markdown(size = 19, face = "bold", hjust = 0.5, family = "Candara"),
        # plot.subtitle = element_markdown(size = 14, hjust = 0.5, family = "Candara", color = "grey30"),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 10, family = "Candara", hjust = 1),

        plot.background = element_rect(fill = "grey93", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr

ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)

