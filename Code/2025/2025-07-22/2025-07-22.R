

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

df_heatmap <- mta_art[, .N, by = .(artist, agency)]


top_artists <- mta_art[, .N , by = artist]
top_artists = top_artists[order(-N)][1:20, artist]


df_heatmap <- df_heatmap[artist %in% top_artists]

# Fill missing combinations with 0
df_heatmap <- df_heatmap |> 
    complete(artist, agency, fill = list(N = 0))


df_heatmap$artist <- df_heatmap$artist |> factor(levels = rev(unique(df_heatmap$artist)))


# plot ------

gr <- ggplot(df_heatmap, aes(x = agency, y = artist, fill = N)) +
    
    geom_tile(color = "grey20", linewidth = 0.25) +
    
    geom_shadowtext(
        aes(label = ifelse(N > 0, N, "")),
        color = "black",
        family = "Candara",
        bg.color = "grey95",
        bg.r = 0.1,
        size = 4
    ) +
    
    scale_fill_stepsn(
        colors = c('#295466', '#7ca6bb', '#ffd1c9', '#ed7b76', '#942d2f'),
        breaks = c(1, 1.5, 2, 3, 5),
        transform = "log10",
        labels = comma,
        name = "No. of Artworks",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(8, "lines"),
            barwidth = unit(0.25, "lines")
        )
    ) +
    
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "Where the Art Lives Across the MTA",
        subtitle = "Exploring who created the MTA’s permanent artworks and where they’re displayed",
        caption = "Source: <b>MTA Permanent Art Catalog</b> • Graphic: <b>Natasa Anastasiadou</b>",
        x = "Agency",
        y = "Artist"
    ) +
    
    theme(
        
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 10.5, angle = 90, hjust = .5, face = "bold", color = "grey30"),
        legend.text = element_text(size = 8.5, color = "grey30"),
        
        # axis.title.x = element_text(size = 8),
        # axis.title.y = element_text(size = 8),
        axis.title = element_blank(),
        
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = .2, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.65, color = "grey30", margin = margin(b = 25, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.35),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr

# Save the plot
ggsave(
    plot = gr, filename = "plot.png",
    width = 8, height = 8, units = "in", dpi = 600
)



