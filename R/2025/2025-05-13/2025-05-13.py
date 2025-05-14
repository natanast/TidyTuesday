
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


# Plot --------

plot1 = (
    ggplot(vesuvius, aes(x='year')) +
    geom_bar(fill="#e63946") +
    labs(title="Seismic Events per Year at Mount Vesuvius",
         x="Year", y="Number of Events") +
    theme_minimal()
)


plot2 = (
    ggplot(vesuvius, aes(x='depth_km', y='duration_magnitude_md')) +
    geom_point(alpha=0.4, color="#1d3557") +
    geom_smooth(method='lm', color="red") +
    labs(title="Depth vs Duration Magnitude",
         x="Depth (km)", y="Duration Magnitude (Md)") +
    theme_minimal()
)


plot3 = (
    ggplot(vesuvius, aes(x='factor(month)')) +
    geom_bar(fill="#457b9d") +
    labs(title="Monthly Seismic Events at Mount Vesuvius",
         x="Month", y="Number of Events") +
    theme_minimal()


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

