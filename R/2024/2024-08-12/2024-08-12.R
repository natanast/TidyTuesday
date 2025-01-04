
rm(list = ls())
gc()


library(data.table)
library(rvest)
library(stringr)


# load data -----

english_monarchs_marriages_df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-20/english_monarchs_marriages_df.csv')


# url to scrape:
root <- "https://www.ianvisits.co.uk/articles/a-list-of-monarchs-by-marriage-6857/"


df <-  english_monarchs_marriages_df
df <- df |> as.data.table()

# Plot 1---------------


df$king_age <- str_replace_all(df$king_age, "\\?", NA_character_)
df$king_age <- ifelse(df$king_age == "", NA, df$king_age)
df$king_age <- ifelse(df$king_age == "–", NA, df$king_age)

df <- df[!is.na(df$king_age), ]


df$consort_age <- str_replace_all(df$consort_age, "\\?", NA_character_)
df <- df[!is.na(df$consort_age), ]


df$king_age <- as.numeric(df$king_age)
df$consort_age <- as.numeric(df$consort_age)



df$year_of_marriage <- str_replace_all(df$year_of_marriage, "\\?", NA_character_)
df <- df[!is.na(df$year_of_marriage), ]
df$year_of_marriage <- as.numeric(df$year_of_marriage)

df$difference <- abs(df$king_age - df$consort_age)


# Reorder kings by the absolute age difference, from largest to smallest
df_plot$king_name <- reorder(df_plot$king_name, df_plot$difference)


df_plot <- df[df$year_of_marriage > 1600,]

df_plot <- df_plot[order(df_plot$difference),]

df_plot$king_name <- reorder(df_plot$king_name, df_plot$difference)
#df_plot$king_name <- df[df_plot$king_name != "Henry VIII",]


colors <- c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")


# Plot
library(ggplot2)
library(ggrepel)
library(extrafont)
library(ggtext)

# Import system fonts
font_import() # This might take some time the first time you run it

# List available fonts
fonts()

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
