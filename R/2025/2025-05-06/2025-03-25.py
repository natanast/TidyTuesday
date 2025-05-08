
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.ndimage import gaussian_filter1d

# Load and clean data
report_words_clean = pd.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-25/report_words_clean.csv"
)

# Define keywords related to data science
ds_keywords = ["data", "website", "cloud", "analysis", "software", "performance", "technology", "processing"]

# Filter and aggregate word counts by year
ds_data = report_words_clean[report_words_clean["word"].isin(ds_keywords)]
word_counts_by_year = ds_data.groupby(["year", "word"]).size().unstack(fill_value=0)

# Apply Gaussian smoothing
smoothed_df = word_counts_by_year.apply(lambda x: gaussian_filter1d(x, sigma=0.6))

# Define colors
colors = ["#a33a3a", "#ff8c86", "#ffaba4", "#ffcac3", "#acd6ec", "#8db7cc", "#6F99AD", "#2c5769"]

# custom order
words = ["data", "analysis", "cloud", "performance", "processing", "software", "website", "technology"]

smoothed_df = smoothed_df[words]

smoothed_df = smoothed_df[smoothed_df.columns[::-1]]  # Reverse column order


# Create figure
fig, ax = plt.subplots(figsize=(10, 6))

# Plot stacked area chart with smoothed data
ax.stackplot(
    smoothed_df.index, 
    smoothed_df.T, 
    labels=smoothed_df.columns, 
    colors=colors, 
    alpha=0.8, 
    edgecolor='white', 
    linewidth=0.35
)



# Remove top, right, and left borders
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)


# Start x-axis from 2005
ax.set_xlim(2005, 2023)


ax.set_xlabel("Year", fontsize = 6)
ax.set_ylabel("Word Frequency", fontsize = 6)

# X-axis settings
ax.set_xticks([2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023])  
ax.set_xticklabels([2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019, 2021, 2023], fontsize = 5)
ax.set_yticklabels(ax.get_yticks(), fontsize=5)


# legend
handles, labels = ax.get_legend_handles_labels()
ax.legend(handles[::-1], labels[::-1], loc='center left', bbox_to_anchor=(1.02, 0.5), fontsize = 5)  


plt.title(
    "Data Science Terminology in Amazon Annual Reports", 
    fontsize = 9,
    pad = 35,
    x = 0.5,
    y = 0.99
    )

# Add the subtitle for clarification
plt.text(x = 0.5, y = 1.1, 
        s = "Tracking the Evolution of Data Science Keywords Over Time (2005–2023)", 
        ha = 'center', 
        va = 'center', 
        fontsize = 7, 
        style = 'italic', 
        color = "#8C8380",
        transform = plt.gca().transAxes)

# Add a caption to the plot
plt.text(
    x = 0.98, y = -0.15,  # Adjust x, y to position the caption below 
    s = "Source:  Amazon's annual reports | Graphic: Natasa Anastasiadou", 
    ha = 'center', 
    va = 'center', 
    fontsize = 5, 
    style = 'italic', 
    color = "#8C8380",
    transform = plt.gca().transAxes
)



# Display the plot
plt.tight_layout()
plt.show()


plt.savefig("plot.png", dpi = 600, bbox_inches='tight')  # Save with high resolution
