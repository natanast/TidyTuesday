
rm(list = ls())
gc()


power_rangers_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_episodes.csv')
power_rangers_seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_seasons.csv')


library(data.table)

df <-  power_rangers_seasons |> as.data.table()

df$season_num <- df$season_num |> as.character()



# Plot 1---------------

colors <- c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")


# Plot
library(ggplot2)


# Plot
ggplot(df, aes(x = season_num, y = IMDB_rating)) +
  
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  
  coord_flip() +
  
  #scale_fill_manual(values = colors) +
  
  labs(
    title = "Top 5 Parent Entities by Total CO2 Emissions",
    x = "Parent Entity",
    y = "Total MtCO2"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    
    #axis.line = element_line(linewidth = 0.55),
    #axis.ticks = element_line(linewidth = 0.55),
    
    plot.title = element_text(hjust = 0.5),
    
    plot.margin = margin(20, 20, 20, 20),
    
    plot.background = element_rect(fill = "#e4e4e3", color = NA)
  )


p
  
ggsave(
  plot = p, filename = "Rplot.png",
  width = 12, height = 10, units = "in", dpi = 600
)    
