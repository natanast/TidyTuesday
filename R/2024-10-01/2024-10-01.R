
rm(list = ls())
gc()


# load data-----
tuesdata <- tidytuesdayR::tt_load('2024-10-01')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 40)

chess <- tuesdata$chess



# data cleaning ------
df = chess[, c("winner", "white_rating", "black_rating")]

df$higher_rating <- ifelse(df$white_rating > df$black_rating, "white_player", "black_player")
# plot--------

library(data.table)


# Create the dumbbell plot
p <- ggplot(df_plot, aes(y = reorder(king_name, difference))) + 
  
  geom_point(aes(x = king_age, color = "King/Queen"), size = 3, shape = 19) +
  
  geom_point(aes(x = consort_age, color = "Consort"), size = 3, shape = 19) + 
  
  geom_segment(aes(x = king_age, xend = consort_age, 
                   y = king_name, yend = king_name), 
                   color = "grey50", size = 0.5, linetype = "solid") +
  
  
  geom_label_repel(
    #data = df_names,
    aes(x = consort_age, y = king_name, label = consort_name),
    nudge_x = 0.25,  # Adjust the horizontal position
    nudge_y = 0.25,
    max.overlaps = Inf, 
    label.size = NA, 
    fill = alpha("#e4e4e3", .65),
    size = 3.75, 
    family = "Candara"
    #position = "identity"
  ) +
  
  labs(title = "Age Gaps Between Kings/Queens and Consorts",
       x = "Age (Years)",
       y = "",
       color = "Person",
       
       subtitle = paste0(
         "Age gaps in descending order after the year 1600"
       ),
       
       caption = paste0(
         "Source: <b>English Monarchs and Marriages</b> | ",
         "Graphic: <b>Natasa Anastasiadou</b>"
       )
       ) +  
  
  scale_color_manual(values = c("King/Queen" = "#0072B5", "Consort" = "#BC3C29")) +
  
  theme_minimal() +
    
  theme(
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    
    axis.text.x = element_text(size = 14, face = "bold", family = "Candara"), 
    axis.text.y = element_text(size = 14, face = "bold", family = "Candara"),
    
    panel.grid.major = element_line(linewidth = .35, color = "grey85"),
    panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
    
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Candara"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Candara", color = "grey30"),
    plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Candara", hjust = 1),
    
    plot.margin = margin(20, 20, 20, 20),
    
    plot.background = element_rect(fill = "#e4e4e3", color = NA)
      
    )

p
  
ggsave(
  plot = p, filename = "Rplot.png",
  width = 12, height = 10, units = "in", dpi = 600
)    


library(ggplot2)
# Load necessary libraries
library(ggplot2)
library(tidyr)



# data cleaning ------
df = chess[, c("winner", "white_rating", "black_rating")]

#df$higher_rating <- ifelse(df$white_rating > df$black_rating, "white_player", "black_player")

chess_l <- df |>
  pivot_longer(cols = c(white_rating, black_rating),
               names_to = "player", values_to = "rating")


# Gather ratings into a long format for plotting
chess_long <- df %>%
  pivot_longer(cols = c(white_rating, black_rating),
               names_to = "player", values_to = "rating") %>%
  mutate(winner = ifelse(player == "white_rating", winner, 
                         ifelse(winner == "white", "black", "white")))

# Create the box plot
ggplot(chess_l, aes(x = winner, y = rating, fill = winner)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Ratings by Winner",
       x = "Winner", y = "Rating") +
  scale_fill_manual(values = c("white" = "lightblue", "black" = "lightgreen", "draw" = "lightgrey"))
