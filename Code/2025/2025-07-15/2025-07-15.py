
import numpy as np
import pandas as pd
import seaborn as sns
from plotnine import *


# Load data --------

df = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv')

# clean data ------

# font family
# plt.rcParams["font.family"] = "Candara"

# Select relevant columns
stats = ['attack', 'defense', 'speed']
selected_types = ['fire', 'water', 'grass', 'electric']

col = ['#f4cd2c', '#C03028', '#78C850', '#6890F0']


df = pokemon_df[['pokemon', 'type_1'] + stats]
df = df[df['type_1'].isin(selected_types)].dropna()


# plot --------

g = (
    ggplot(df_long)

    + aes(x = "stat", y = "value", fill = "type_1")

    + geom_violin(size = 0.25, alpha = 0.4, show_legend = False, trim = False)

    + geom_jitter(size = 2.5, width = 0.1, height = 0, alpha = 0.9, color = 'white', show_legend = False, stroke=0.2)

    + facet_wrap('~type_1')

    + scale_fill_manual(values=col)

    + theme_minimal()

    + labs(
        title = "Pokémon Stat Comparison Across Types",
        subtitle = "A closer look at key stats (Attack, Defense, and Speed) for various Pokémon types (Electric, Fire, Grass, Water).",
        caption = "Source: {pokemon} R package | Graphic: Natasa Anastasiadou"
    )

    + theme(

        legend_position= "none", 
        axis_title = element_blank(),

        axis_text_x = element_text(margin={'t': 40, 'units': 'pt'}, family="Candara", size = 8),
        axis_text_y = element_text(margin={'r': 40, 'units': 'pt'}, family="Candara", size = 8),

        plot_title = element_text(size = 12, color = 'black', weight = 'bold', hjust = 0.5, family="Candara"),
        plot_subtitle = element_text(size= 10, color = 'black', hjust = 0.5, family="Candara"),
        plot_caption =  element_text(size= 7, color = 'black', hjust = 1, family="Candara"),

        plot_background = element_rect(fill='white', color='white'),
        panel_background = element_rect(fill='white', color='white'),

        panel_grid_major_y = element_line(color = '#e5e5e5', alpha = 0.9, size = 0.75),
        panel_grid_major_x = element_line(color = '#e5e5e5', alpha = 0.9, size = 0.75),
        panel_border = element_rect(color = '#e5e5e5', alpha = 0.7, size = 0.5),

        strip_text_x = element_text(size = 8, family="Candara"),
        axis_ticks = element_line(color='#e5e5e5', alpha = 0.7),

        figure_size = (8, 4.5)

    ) 
)

g

col = ['#6890F0', '#C03028']


# Plot: Real vs Nominal Funding
plot1 = (
    ggplot(df, aes(x="year")) +

    geom_line(
        aes(y = "nominal_gbp_millions", color = "nominal_gbp_millions"),
        size = 1,
        linejoin = "round"
    ) +
    
    geom_line(
        aes(y = "total_y2000_gbp_millions", color ='"Inflation-adjusted (£M, Y2000)"'), 
        size = 1,
        linejoin = "round"
    ) +

    geom_point(
        aes(y = "nominal_gbp_millions", color = '"Nominal (£M)"'),
        size = 3,
        shape = 'o',
        stroke = 0.55,
        fill = "white"
    ) +

    geom_point(
        aes(y = "total_y2000_gbp_millions", color ='"Inflation-adjusted (£M, Y2000)"'),
        size = 3,
        shape = 'o',
        stroke = 0.55,
        fill = "white"
    ) +
    scale_color_manual(
        values = col,
        labels={
        "Nominal (£M)": "Nominal",
        "Inflation-adjusted (£M, Y2000)": "Inflation-adjusted"
    }
    ) +
    
    theme_minimal(base_family = "Candara") +

    labs(
        title = "British Library Total Funding Over Time",
        subtitle = "Inflation-adjusted funding (year 2000 GBP) shows a declining trend",
        caption = "Source: {pokemon} R package | Graphic: Natasa Anastasiadou",
        y = "Funding (£ Millions)"
    ) +

    theme(

        plot_title = element_text(size = 12, color = 'black', weight = 'bold', hjust = 0.5),
        plot_subtitle = element_text(size = 10, color = 'black', hjust = 0.5),
        plot_caption =  element_text(size = 7, color = 'black', hjust = 1),

        plot_background = element_rect(fill = 'white', color = 'white'),
        panel_background = element_rect(fill = 'white', color = 'white')
    )
)

plot1

# Save the plot with custom size and resolution
ggsave(plot1, "plot.png", width = 10, height = 6, dpi = 300)



import pandas as pd
from plotnine import *

# Reshape your data from wide to long
df_long = pd.melt(
    df,
    id_vars = 'year',
    value_vars = ['nominal_gbp_millions', 'total_y2000_gbp_millions'],
    var_name = 'type',
    value_name = 'funding'
)

# Create mapping for display labels and colors
label_map = {
    'nominal_gbp_millions': 'Nominal',
    'total_y2000_gbp_millions': 'Inflation-adjusted'
}

color_map = {
    'nominal_gbp_millions': '#6890F0',
    'total_y2000_gbp_millions': '#C03028'
}

# Plot
plot1 = (
    ggplot(df_long, aes(x = 'year', y = 'funding', color = 'type')) +
    
    geom_line(size = 1, linejoin = 'round') +

    geom_point(size = 3, shape = 'o', stroke = 0.55, fill = 'white') +
    
    scale_color_manual(values = color_map, labels = label_map) +

    scale_fill_manual(values = color_map, labels = label_map) +

    theme_minimal(base_family = 'Candara') +

    labs(
        title = "British Library Total Funding Over Time",
        subtitle = "Inflation-adjusted funding (year 2000 GBP) shows a declining trend",
        caption = "Source: {pokemon} R package | Graphic: Natasa Anastasiadou",
        y = "Funding (£ Millions)",
        color = "",  # Remove "type" legend title
        fill = ""
    ) +

    theme(
        plot_title = element_text(size = 12, weight = 'bold', hjust = 0.5),
        plot_subtitle = element_text(size = 10, hjust = 0.5),
        plot_caption = element_text(size = 7, hjust = 1),
        plot_background = element_rect(fill = 'white', color = 'white'),
        panel_background = element_rect(fill = 'white', color = 'white')
    )
)

# Save
plot1.save("plot.png", width = 10, height = 6, dpi = 300)
