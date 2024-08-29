
rm(list = ls())
gc()


power_rangers_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_episodes.csv')
power_rangers_seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-27/power_rangers_seasons.csv')


library(data.table)

df <-  power_rangers_seasons |> as.data.table()

df$season_num <- df$season_num |> as.character()

df <- df[order(IMDB_rating)]

# Create a new column for categorizing top 5 and bottom 5
df[, category := "Other"]
df[1:5, category := "Top 5"]
df[(.N-4):.N, category := "Bottom 5"]



# df$x <- paste0(df$season_title, " (Season ", df$season_num, ")")

# Create a combined column that adds season number only if not already in the title
df$x <- ifelse(grepl("\\(Season \\d+\\)", df$season_title),
               df$season_title,
               paste0(df$season_title, " (Season ", df$season_num, ")"))


df$x <- factor(df$x, levels = df$x)



# Plot 1---------------

colors <- c("#6A6599", "#BC3C29", "#DF8F44", "#79AF97", "#0072B5")


# Plot
library(ggplot2)
library(extrafont)
library(ggtext)

# Plot
ggplot(df, aes(x = x, y = IMDB_rating, fill = category)) +
  
  geom_bar(stat = "identity", alpha = 0.65, width = 0.7) +
  
  geom_text(aes(label = IMDB_rating), 
            hjust = -0.1,  # Positioning the text just outside the bar
            color = "black", 
            size = 3.5) +
  
  coord_flip() +
  
  scale_fill_manual(values = c("Top 5" = "#0072B5", "Bottom 5" = "#BC3C29", "Other" = "gray50")) +
  
  labs(
    title = "IMDB rating of the Power Rangers seasons",
    subtitle = "IMDB rating of the Power Rangers seasons",
    
    x = "Season",
    y = "IMDB rating",
    
    caption = paste0(
      "Source: <b>NationalDayCalendar.com</b> | ",
      "Graphic: <b>Natasa Anastasiadou</b>"
    )
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Candara"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Candara", color = "grey30"),
    plot.caption  = element_markdown(margin = margin(t = 25), size = 8, family = "Candara", hjust = 1),
    
    plot.margin = margin(20, 20, 20, 20),
    
    plot.background = element_rect(fill = "grey94", color = NA)
  )


p
  
ggsave(
  plot = p, filename = "Rplot.png",
  width = 12, height = 10, units = "in", dpi = 600
)    
