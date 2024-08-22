
rm(list = ls())
gc()



worlds_fairs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-13/worlds_fairs.csv')


library(ggplot2)
library(tidyverse)

#library(patchwork)
#library(ggtext)
library(data.table)


# Data 1

worlds_fairs <- as.data.table(worlds_fairs)

df <- worlds_fairs


df$cost_per_vis <- df$cost / df$visitors

df$cost_per_vis <- round(df$cost_per_vis, 1)



df <- df[!is.na(cost)]


# Create a new column for 10-year intervals
df$decade <- paste0(floor(df$start_year / 10) * 10, "s")

# Aggregate data by decade
df_agg <- df[, .(avg_cost_per_vis = mean(cost_per_vis, na.rm = TRUE)), by = decade]


df_agg <- df_agg[order(-avg_cost_per_vis)] 

df_agg$color <- ifelse(
  seq_len(nrow(df_agg)) <= 5, "Top 5", "Other"
)


# Create a label for the top 5
df_agg[, top_label := ifelse(rank(-avg_cost_per_vis) <= 5, paste0("Top ", rank(-avg_cost_per_vis)), "Others")]


# df_agg$Group <- ifelse(
  # df_agg$color == "Top 5", "1", "2"
# )


# Plot 1---------


library(packcircles)
library(ggplot2)
library(shadowtext)

library(paletteer)
library(extrafont)

library(ggtext)


# Process the data 
df_plot <- df_agg[, {
  dat.egg <- circleProgressiveLayout(avg_cost_per_vis)
  dat.egg <- circleLayoutVertices(dat.egg, npoints = 100)
  
  cbind(dat.egg, .SD[dat.egg$id])
}]


df_plot_l <- df_plot |>
  group_by(decade, avg_cost_per_vis, id) |>
  summarise(
    x = (min(x) + max(x)) / 2,
    y = (min(y) + max(y)) / 2
  )

df_plot_l$lbl = df_plot_l$avg_cost_per_vis |> round(digits = 2) |> paste0(" M")



#my_col = c('#00429d', '#325da9', '#4e78b5', '#6694c1', '#80b1cc', '#9dced6', '#c0eade', '#ffdac4', '#ffb3a7', '#fb8a8c', '#eb6574', '#d5405e', '#BC3C29','#b9b8e7','#3a5cbc', '#b9b8e7','#BC3C29','#E18727','#0072B5', '#dddaea', '#20854E','#FFDC91','#6F99AD', '#80b1cc', '#9dced6','#91D1C2','#F39B7F','#b24745', '#B09C85','#9dced6', '#c0eade', '#ffdac4')


# Define colors for the top 5 and others
colors = c("Top 1" = "#b24745", "Top 2" = "#6A6599", "Top 3" = "#9dced6", "Top 4" = "#F39B7F", "Top 5" = "#FFDC91", "Others" = "grey70")



p <- df_plot |>
  ggplot(aes(x, y, group = id)) +
  geom_polygon(aes(fill = top_label), color = "grey30", linewidth = .25) +
  
  geom_shadowtext(
    data = df_plot_l, inherit.aes = FALSE,
    aes(x, y, label = lbl, size = avg_cost_per_vis * 5),  
    color = "grey1", bg.color = "#d9e3f1", bg.r = .05
  ) +
  
  scale_fill_manual(
    values = colors
  ) +
  
  scale_size_continuous(guide = "none", range = c(2, 12)) +
  
  #facet_grid(cols = vars(Group), switch = "y") +
  
  coord_equal() +
  
  labs(title = "World's Fairs: Average Cost per Visitor by Decade in Millions",
       
       subtitle = paste0(
         "Highlighting the Top 5 Most Expensive Expositions"
       ),
       
       caption = paste0(
         "Source: <b>Wikipedia's list of world expositions</b> | ",
         "Graphic: <b>Natasa Anastasiadou</b>"
       )
       ) +
  
  theme_minimal() +
  
  theme(

    axis.title = element_blank(),
    axis.text = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    
    panel.grid.major = element_line(linewidth = .35, color = "grey85"),
    panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
    
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Candara"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, family = "Candara", color = "grey30"),
    plot.caption  = element_markdown(margin = margin(t = 25), size = 10, family = "Candara", hjust = 1.3),
    
    
    plot.margin = margin(20, 20, 20, 20),
    
    plot.background = element_rect(fill = "#e4e4e3", color = NA)
    
  )

p
  

ggsave(
  plot = p, filename = "Rplot.png",
  width = 12, height = 10, units = "in", dpi = 600
)    


