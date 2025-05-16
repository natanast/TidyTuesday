
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

# Plot --------

plot_bin = (
    ggplot(
        agg_df, 
        aes(x = 'factor(year)', y = 'md_bin', size = 'count', fill = 'year')
    ) +

    geom_point(alpha = .8, stroke = .15) +

    scale_size_continuous(
        range = (1, 9)
    ) +

    guides(fill = False) +

    labs(
        title = "Seismic Duration Magnitude per Year",
        subtitle = "Tracking the shift from rural to urban living in selected countries (1960–2023)",
        caption = "Source:  | Graphic: Natasa Anastasiadou",
        x = 'Year',
        y = 'Duration Magnitude (Md)'
    ) +

    theme_minimal(base_family = "Candara") +

    theme(

        axis_text_x=element_text(hjust = 0.5, size = 8),
        axis_text_y=element_text(size = 8),
        axis_title=element_text(size = 10),

        plot_title = element_text(size = 12, weight = 'bold', ha = 'center'),
        plot_subtitle = element_text(size = 10, ha = 'center'),
        plot_caption = element_text(size = 6, ha = 'right'),

        panel_grid_major=element_line(color='#c9c9c9', alpha=0.75, size=0.65, linetype="dashed"),
        panel_grid_minor = element_blank(),

        plot_background=element_rect(fill='#e6e6e6', color='#e6e6e6'),
        
        legend_title = element_text(size = 8),
        legend_text = element_text(size = 7),

        figure_size=(10, 6)

    )
)


plot_bin

#  Save the plot with custom size and resolution
ggsave(plot_bin, "plot.png", width = 10, height = 6, dpi = 600)
