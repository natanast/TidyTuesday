

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)

library(shadowtext)


# load data ------

qatarcars <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-09/qatarcars.csv')


# clean data ------


colnames(qatarcars)


# df <- qatarcars[, .(length, width, height,
#                     mass, horsepower,
#                     economy, performance,
#                     trunk, seating)] 
# 

# df_pca <- qatarcars[
#     ,
#     .(horsepower, performance, economy)
# ]

df_pca <- qatarcars[
    ,
    .(mass, horsepower, economy, performance, height)
]

df_pca <- na.omit(df_pca)

pca <- prcomp(df_pca, scale. = TRUE)
summary(pca)

# Remove rows with NA
df_pca <- na.omit(df)

# Run PCA (scaled!)
pca <- prcomp(as.matrix(df_pca), scale. = TRUE)


summary(pca)

scores <- as.data.table(pca$x)

complete_idx <- complete.cases(df)


# Add grouping variable (choose what you want here)
scores[, Group := qatarcars[complete_idx, enginetype]]



prop_var <- summary(pca)$importance[2, ]  # Extract Proportion of Variance
pc1_label <- paste0("PC1 (", round(prop_var[1] * 100, 2), "%)")
pc2_label <- paste0("PC2 (", round(prop_var[2] * 100, 2), "%)")


library(ggforce)
gr = ggplot(scores, aes(PC1, PC2)) +
    
    geom_mark_ellipse(aes(fill = Group, label = Group), alpha = .1, expand = unit(1.5, "mm")) +
    
    geom_point(aes(fill = Group), shape = 21, size = 3, stroke = .25, color = "white") +  
    
    scale_fill_manual(
        values = c(
            "Hybrid" = "#990000",
            "Petrol" = "#004d99"
            
        )
    ) +
    
    # geom_text(aes(label = Sample)) +
    
    scale_x_continuous(limits = c(-40, 40)) +
    scale_y_continuous(limits = c(-35, 30)) +
    
    
    theme_minimal() +
    
    theme(
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)
    ) +
    
    labs(x = pc1_label, y = pc2_label)



gr



# plot ------

gr = ggplot(df_clean, aes(x = year, y = country, fill = overall_score)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
     
    geom_shadowtext(
        aes(label = round(overall_score, 1)),
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .085, 
        size = 2.8
    ) +
    
    scale_fill_stepsn(
        colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(30, 60, 70, 80, 90),
        name = "SPI",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(0.25, "lines"), 
            barwidth = unit(8, "lines")
        )  
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    
    labs(
        title = "Tracking Statistical Performance in Europe & Central Asia (2016–2023)",
        subtitle = "Overall SPI scores summarizing the strength of national statistical systems across time.",
        caption = "Source: <b>Statistical Performance Indicators dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    
    theme(
        legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(size = 10, angle = 0, hjust = .5, face = "bold", color = "grey30"),
        legend.text = element_text(size = 8, color = "grey30"),
        
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = .1, margin = margin(b = 10)),
        plot.subtitle = element_markdown(size = 14, hjust = 1, color = "grey30", margin = margin(b = 10)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "#e8e8e7", color = NA)
    )

  

gr

ggsave(
    plot = gr, filename = "plot.png",
    width = 9, height = 10, units = "in", dpi = 600
)

