
rm(list = ls())
gc()

tuesdata <- tidytuesdayR::tt_load('2024-05-21')

emissions <- tuesdata$emissions

library(dplyr)
library(ggplot2)
library(tidyverse)

library(ggstream)
library(colorspace)
library(scales)
library(cowplot)


# Plot 1---------------
# Group by parent_entity and sum CO2
data_1 <- emissions |>
  group_by(parent_entity) |>
  summarise(total_CO2 = sum(total_emissions_MtCO2e))

# Keep the top 10 rows based on total_CO2
top_5_data <- data_1 %>%
  arrange(desc(total_CO2)) %>%
  slice_head(n = 4)

# specific colors
specific_colors <- c("#6F99AD", "#CCA852", "#BC3C29", "#6A6599", "#7E6148", "#E18727", "#E69F00", "#0072B5", "#008280", "#E7969C")

colors <- c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")


# Plot
p1 <- ggplot(top_5_data, aes(x = reorder(parent_entity, total_CO2), y = total_CO2, fill = parent_entity)) +
  
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  
  coord_flip() +
  
  scale_fill_manual(values = colors) +
  
  labs(
    title = "Top 5 Parent Entities by Total CO2 Emissions",
    x = "Parent Entity",
    y = "Total MtCO2"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    
    axis.line = element_line(linewidth = 0.55),
    axis.ticks = element_line(linewidth = 0.55),
    
    plot.title = element_text(hjust = 0.5),
    
    plot.margin = margin(20, 20, 20, 20),
    
    plot.background = element_rect(fill = "#e4e4e3", color = NA)
  )




# Plot 2-----------------

# Group by parent_entity and sum CO2
data_2 <- emissions |>
  filter(parent_entity %in% c(
                            "China (Coal)", "Former Soviet Union", "Saudi Aramco",
                            "Chevron"
                            #"ExxonMobil"
                            #"Gazprom", "BP", "Shell",
                            # "Coal India", "National Iranian Oil Co."
                            )) |>
  group_by(parent_entity, year) |>
  summarise(MtCO2_per_year = sum(total_emissions_MtCO2e))
  

 p2 <- ggplot(data_2) +
  
  geom_area(aes(x = year, y = MtCO2_per_year, fill = parent_entity, color = parent_entity, linetype = parent_entity), 
            linewidth = .8, alpha = .5, position = "identity") +
  
  
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1923, 2023, by = 50), limits = c(1923, 2023)) +

  scale_color_manual(values = c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")) +
  scale_fill_manual(values = c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")) +  
      
  theme_minimal() +
  
  theme(
    plot.background = element_rect(fill = "#e4e4e3"),
    
    legend.position = "left",
    
    panel.grid.major = element_line(linewidth = .35, color = "grey85"),
    panel.grid.minor = element_line(linewidth = .25, color = "grey85", linetype = "dashed"),
    
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    
    plot.margin = margin(20, 20, 20, 20)
    
  ) +
  
  labs(
    y = "MtCO2",
    x = "Years",
    title = "Total Emission Over Time by Parent Entity",
  )
  
  #facet_wrap(~parent_entity, ncol = 1)


combined_plot <- (p1 | p2) +  
  patchwork::plot_layout(
    guides = "collect",
    widths = c(1, 2)
  )

combined_plot

