
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *


# Load data --------

vesuvius = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')


# clean data ------

df = vesuvius[['event_id', 'duration_magnitude_md', 'depth_km', 'year']]

df['md_bin'] = (df['duration_magnitude_md'] / 0.25).round() * 0.25


agg_df = (
    df.groupby(['year', 'md_bin'])
    .size()
    .reset_index(name='count')
)

# Plot --------

plot_bin = (
    ggplot(agg_df, aes(x='factor(year)', y='md_bin', size='count')) +

    geom_point(alpha=0.8, color="#1d3557") +

    scale_size_continuous(range=(1, 9)) +

    labs(
        title='Binned Seismic Duration Magnitude per Year',
        x='Year',
        y='Duration Magnitude (Md)',
        size='Event Count'
    ) +

    theme_minimal(base_family = "Candara") +

    theme(
        axis_text_x=element_text(rotation=45, hjust=1, size=8),
        axis_text_y=element_text(size = 8),
        axis_title=element_text(size = 10),
        plot_title=element_text(size=12, weight='bold', ha='center'),
        panel_grid_major=element_line(color='#c9c9c9', alpha=0.75, size=0.65, linetype="dashed"),
        plot_background=element_rect(fill='#f8f8f8', color='#f8f8f8'),
        legend_title=element_text(size=8),
        legend_text=element_text(size=7),
        figure_size=(10, 6)
    )
)


#  Save the plot with custom size and resolution
ggsave(plot_bin, "plot.png", width = 10, height = 6, dpi = 600)




# g = (
#     ggplot(df_filtered) +

#     aes(x='year', y='Percentage', fill='Population_Type') + 

#     geom_area(alpha=0.7) +

#     facet_wrap('~country') +  

#     scale_fill_manual(values={"Urban": "#6F99AD", "Rural": "#BC3C29"}) + # Custom colors for each quadrant

#     labs(
#         title = "Urbanization Over Time Across the Globe",
#         subtitle = "Tracking the shift from rural to urban living in selected countries (1960–2023)",
#         caption = "30DayChartChallenge 2025: Day 20 | Source: Urbanization & Climate Metrics Insights (Kaggle) | Graphic: Natasa Anastasiadou",
#         x = '',
#         y = 'Population (%)',
#         fill = 'Type'
#     ) +

#     theme_minimal() +

#     theme(
#         axis_text = element_text(family = 'Candara', size = 8),
#         axis_title = element_text(family = 'Candara', size = 10),
        
#         plot_title = element_text(size = 12, weight = 'bold', ha = 'center'),
#         plot_subtitle = element_text(size = 10, ha = 'center'),
#         plot_caption = element_text(size = 6, ha = 'right'),
        
#         panel_grid_major = element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype="dashed"),
#         plot_background = element_rect(fill = '#f8f8f8', color = '#f8f8f8'),

#         legend_title=element_text(size = 8),
#         legend_text=element_text(size = 7),
        
#         figure_size=(10, 6)
#     )
# )


