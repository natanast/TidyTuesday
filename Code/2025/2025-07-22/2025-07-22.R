

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(tidyr)
library(shadowtext)
library(ggtext)
library(scales)



# load data ------

mta_art <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-22/mta_art.csv')
station_lines <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-22/station_lines.csv')


# clean data ------

df = mta_art[, .N , by = artist]

df = df[order(-N)]


df1 = mta_art[, .N, by = station_name]

df1 = df1[order(-N)]

# plot ------

gr = ggplot(top50_colors, aes(x = col, y = -row)) +
    
    geom_tile(
        aes(fill = hex), 
        color = "grey20",
        linewidth = .35,
        width = 0.85, 
        height = 0.85
    ) +
    
    scale_fill_identity() +
    
    coord_fixed() +
    
    theme_void(base_family = "Candara") +
    
    labs(
        title = "XKCD’s Favorite 50 Colors",
        subtitle = "Exploring the names from the XKCD color naming survey — each tile shows a color.",
        caption = "Source: <b> xkcd Color Survey</b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme(
        
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5,  color = "grey30", margin = margin(b = 15)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "grey90", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr
 
ggsave(
    plot = gr, filename = "plot.png",
    width = 8, height = 6, units = "in", dpi = 600
)










# Load data
mta_art <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/mta_art.csv")

# Count artworks per artist and agency
df_heatmap <- mta_art[, .N, by = .(artist, agency)]

# Keep top 15 artists overall
top_artists <- df_heatmap[, .(total = sum(N)), by = artist][order(-total)][1:30, artist]
df_heatmap <- df_heatmap[artist %in% top_artists]

# Fill missing combinations with 0
df_heatmap <- df_heatmap |> 
    complete(artist, agency, fill = list(N = 0))

# # Sort artists by total works for display
# artist_order <- df_heatmap[, .(total = sum(N)), by = artist][order(total)]$artist
# df_heatmap$artist <- factor(df_heatmap$artist, levels = artist_order)

# Plot
gr <- ggplot(df_heatmap, aes(x = agency, y = artist, fill = N)) +
    
    geom_tile(color = "grey20", linewidth = 0.25) +
    
    geom_shadowtext(
        aes(label = ifelse(N > 0, N, "")),
        color = "black",
        family = "Candara",
        bg.color = "grey95",
        bg.r = 0.1,
        size = 3
    ) +
    
    scale_fill_stepsn(
        colors = c('#2c5769', '#6F99AD', '#ffb5ac', '#a33a3a'),
        breaks = c(1, 2, 3, 5),
        transform = "log10",
        labels = comma,
        name = "No. of Artworks",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(8, "lines"),
            barwidth = unit(0.25, "lines")
        )
    ) +
    
    theme_minimal() +
    
    labs(
        title = "Most Prolific Artists in the MTA Permanent Art Collection",
        subtitle = "Heatmap showing the top 15 artists by number of artworks, across MTA agencies",
        caption = "Source: <b>MTA Permanent Art Catalog</b> • Graphic: <b>Natasa Anastasiadou</b>",
        x = "Agency",
        y = "Artist"
    ) +
    
    theme(
        
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 8, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        axis.title.x = element_text(size = 8, family = "Candara"),
        axis.title.y = element_text(size = 8, family = "Candara"),
        
        axis.text.x = element_text(size = 6, family = "Candara", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 6, family = "Candara"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 12, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.3),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr

# Save the plot
ggsave(
    plot = gr, filename = "mta_artists_heatmap.png",
    width = 9, height = 7, units = "in", dpi = 600
)
