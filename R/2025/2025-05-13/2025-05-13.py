
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *


# Load data --------

vesuvius = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')


# clean data ------

df = vesuvius[['event_id', 'duration_magnitude_md', 'depth_km', 'year']]

df['year'] = df['year'].astype(str)

df['md_bin'] = (df['duration_magnitude_md'] / 0.25).round() * 0.25


agg_df = (
    df.groupby(['year', 'md_bin'])
    .size()
    .reset_index(name='count')
)


col = ['#2c5769', '#366072', '#3f697c', '#497285', '#527c8f', '#5c8599', '#658fa3', '#f2a39b', '#e69289', '#d98079', '#cb6f68', '#be5d58', '#b14c49', '#a33a3a']

# Plot --------

plot_bin = (
    ggplot(
        agg_df, 
        aes(x = 'factor(year)', y = 'md_bin', size = 'count', fill = 'year')
    ) +

    geom_point(alpha = .8, shape = 'o', color = '#768188', stroke = .15) +

    scale_size_continuous(
        range = (1, 8)
    ) +

    scale_fill_manual(values = col) +

    guides(fill = False) +

    labs(
        title = "Seismic Activity at Mount Vesuvius Over Time",
        subtitle = "Each point marks a cluster of events by duration magnitude and year, with size indicating the number of occurrences.",
        caption = "Source: Mount Vesuvius Dataset | Graphic: Natasa Anastasiadou",
        x = 'Year',
        y = 'Duration Magnitude',
        size = "Event Count"
    ) +

    theme_minimal(base_family = "Candara") +

    theme(

        axis_text_x = element_text(hjust = 0.5, size = 10),
        axis_text_y = element_text(size = 10),
        axis_title = element_text(size = 12),

        plot_title = element_text(size = 14, weight = 'bold', ha = 'center'),
        plot_subtitle = element_text(size = 12, ha = 'center'),
        plot_caption = element_text(size = 8, hjust = .5, ha = 'right'),

        panel_grid_major=element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype = "dashed"),
        panel_grid_minor = element_blank(),

        plot_background=element_rect(fill = '#e6e6e6', color = '#e6e6e6'),
        
        legend_title = element_text(size = 9),
        legend_text = element_text(size = 8),
        legend_key_size = 2,

        figure_size = (10, 6)

    )
)


plot_bin

#  Save the plot with custom size and resolution
ggsave(plot_bin, "plot.png", width = 11, height = 7, dpi = 600)
