
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
top_10_data <- data_1 %>%
  arrange(desc(total_CO2)) %>%
  slice_head(n = 10)

# specific colors
specific_colors <- c("#6F99AD", "#CCA852", "#BC3C29", "#6A6599", "#7E6148", "#E18727", "#E69F00", "#0072B5", "#008280", "#E7969C")

# Plot
ggplot(top_10_data, aes(x = reorder(parent_entity, total_CO2), y = total_CO2, fill = parent_entity)) +
  
  geom_bar(stat = "identity", alpha = 0.8, width = 0.7) +
  
  coord_flip() +
  
  scale_fill_manual(values = specific_colors) +
  
  labs(
    title = "Top 10 Parent Entities by Total CO2 Emissions",
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
                            "Chevron", "ExxonMobil"
                            #"Gazprom", "BP", "Shell",
                            # "Coal India", "National Iranian Oil Co."
                            )) |>
  group_by(parent_entity, year) |>
  summarise(MtCO2_per_year = sum(total_emissions_MtCO2e))
  


ggplot(data_2) +
  
  geom_area(aes(x = year, y = MtCO2_per_year, fill = parent_entity, color = parent_entity, linetype = parent_entity), 
            linewidth = .6, alpha = .6, position = "identity") +
  
  
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1923, 2023, by = 20), limits = c(1923, 2023)) +

  #scale_color_manual(values = c("#6F99AD", "#CCA852", "#BC3C29", "#6A6599", "#7E6148", "#656566", "#E69F00","#008280", "#E7969C", "#0072B5")) +
  #scale_fill_manual(values = c("#6F99AD", "#CCA852", "#BC3C29", "#6A6599", "#7E6148", "#656566", "#E69F00","#008280", "#E7969C", "#0072B5")) +
  scale_color_manual(values = c("#7E6148", "#e3c6c3", "#6A6599", "#7AA6DC", "#BC3C29")) +
  scale_fill_manual(values = c("#7E6148", "#e3c6c3", "#6A6599", "#7AA6DC", "#BC3C29")) +
  
  theme_minimal() +
  
  theme(
    plot.background = element_rect(fill = "#e4e4e3"),
    
    legend.position = "bottom",
    
    panel.grid.major = element_line(linewidth = .35, color = "grey85"),
    panel.grid.minor = element_line(linewidth = .25, color = "grey85", linetype = "dashed"),
    
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  labs(
    y = "MtCO2",
    x = "Years"
  ) 
