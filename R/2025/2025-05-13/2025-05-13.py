
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *


# Load data --------

vesuvius = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')


# clean data ------

# Parse the datetime column
vesuvius['time'] = pd.to_datetime(vesuvius['time'])


# Extract useful features for plotting
vesuvius['year'] = vesuvius['time'].dt.year
vesuvius['month'] = vesuvius['time'].dt.month
vesuvius['hour'] = vesuvius['time'].dt.hour


df = vesuvius[['event_id', 'duration_magnitude_md', 'depth_km', 'year']]

# Plot --------

plot1 = (
    ggplot(df, aes(x='year')) +
    geom_bar(fill="#e63946") +
    labs(title="Seismic Events per Year at Mount Vesuvius",
         x="Year", y="Number of Events") +
    theme_minimal()
)


plot2 = (
    ggplot(df, aes(x='duration_magnitude_md', y='depth_km')) +
    geom_point(alpha=0.4, color="#1d3557") +
    # geom_smooth(method='lm', color="red") +
    labs(title="Depth vs Duration Magnitude",
         x="Depth (km)", y="Duration Magnitude (Md)") +
    theme_minimal() +
        theme(
        axis_text = element_text(family = 'Candara', size = 8),
        axis_title = element_text(family = 'Candara', size = 10),
        
        plot_title = element_text(size = 12, weight = 'bold', ha = 'center'),
        plot_subtitle = element_text(size = 10, ha = 'center'),
        plot_caption = element_text(size = 6, ha = 'right'),
        
        panel_grid_major = element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype="dashed"),
        plot_background = element_rect(fill = '#f8f8f8', color = '#f8f8f8'),

        legend_title=element_text(size = 8),
        legend_text=element_text(size = 7),
        
        figure_size=(10, 6)
    )
)



plot2

g = (
    ggplot(df_filtered) +

    aes(x='year', y='Percentage', fill='Population_Type') + 

    geom_area(alpha=0.7) +

    facet_wrap('~country') +  

    scale_fill_manual(values={"Urban": "#6F99AD", "Rural": "#BC3C29"}) + # Custom colors for each quadrant

    labs(
        title = "Urbanization Over Time Across the Globe",
        subtitle = "Tracking the shift from rural to urban living in selected countries (1960–2023)",
        caption = "30DayChartChallenge 2025: Day 20 | Source: Urbanization & Climate Metrics Insights (Kaggle) | Graphic: Natasa Anastasiadou",
        x = '',
        y = 'Population (%)',
        fill = 'Type'
    ) +

    theme_minimal() +

    theme(
        axis_text = element_text(family = 'Candara', size = 8),
        axis_title = element_text(family = 'Candara', size = 10),
        
        plot_title = element_text(size = 12, weight = 'bold', ha = 'center'),
        plot_subtitle = element_text(size = 10, ha = 'center'),
        plot_caption = element_text(size = 6, ha = 'right'),
        
        panel_grid_major = element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype="dashed"),
        plot_background = element_rect(fill = '#f8f8f8', color = '#f8f8f8'),

        legend_title=element_text(size = 8),
        legend_text=element_text(size = 7),
        
        figure_size=(10, 6)
    )
)

# Show the plot
g

#  Save the plot with custom size and resolution
ggsave(g, "20_day.png", width = 10, height = 6, dpi = 600)


def classify_event(row):
    if row['depth_km'] < 1.5:
        return 'low depth'
    elif row['depth_km'] >= 1.5 and row['duration_magnitude_md'] < 0.5:
        return 'deep & low magnitude'
    elif row['depth_km'] >= 1.5 and row['duration_magnitude_md'] >= 0.5:
        return 'deep & high magnitude'
    else:
        return 'other'

df['group'] = df.apply(classify_event, axis=1)


from plotnine import *

plot_colored = (
    ggplot(df, aes(x='duration_magnitude_md', y='depth_km', color='group')) +
    geom_point(alpha=0.6) +
    scale_color_manual(
        values={
            'low depth': 'grey',
            'deep & low magnitude': '#457b9d',
            'deep & high magnitude': '#e63946'
        }
    ) +
    labs(title="Depth vs Duration Magnitude by Seismic Event Type",
         x="Duration Magnitude (Md)", y="Depth (km)", color="Event Group") +
    theme_minimal() +
    theme(
        axis_text=element_text(family='Candara', size=8),
        axis_title=element_text(family='Candara', size=10),
        plot_title=element_text(size=12, weight='bold', ha='center'),
        panel_grid_major=element_line(color='#c9c9c9', alpha=0.75, size=0.65, linetype="dashed"),
        plot_background=element_rect(fill='#f8f8f8', color='#f8f8f8'),
        legend_title=element_text(size=8),
        legend_text=element_text(size=7),
        figure_size=(10, 6)
    )
)
