

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(wordcloud2)
library(ggwordcloud)


# load data --------

conf2023 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv')
conf2024 <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv')



# data cleaning ------

k1 <- conf2023$block_track_title |>
    tolower() |>
    str_split(" ") |>
    unlist() |>
    unique()


k2 <- conf2024$track |>
    tolower() |>
    str_split(" ") |>
    unlist() |>
    unique()



c <- intersect(k1, k2)
c

# Calculate frequency of all words in 2023
freq_2023 <- conf2023$block_track_title |>
    tolower() |>
    str_remove_all("[^a-z ]") |>  # Remove non-alphabetic characters
    str_split(" ") |> 
    unlist() |>
    table() |>
    as.data.table()

setnames(freq_2023, c("word", "freq_2023"))

# Calculate frequency of all words in 2024
freq_2024 <- conf2024$track |>
    tolower() |>
    str_remove_all("[^a-z ]") |>  # Remove non-alphabetic characters
    str_split(" ") |> 
    unlist() |>
    table() |>
    as.data.table()

setnames(freq_2024, c("word", "freq_2024"))

# Filter for common keywords and merge frequencies
common_freq <- merge(freq_2023, freq_2024, by = "word", all = FALSE)
common_freq[, total_freq := freq_2023 + freq_2024]
common_freq <- common_freq[order(-total_freq)]





common_freq <- common_freq[, .(word, total_freq)]

# Create a word cloud with ggplot2
ggplot(common_freq, aes(label = word, size = total_freq, color = word)) +
  geom_text_wordcloud(
    family = "Caldara",   # Set font
    # color = "black",      # Word color
    background = "white" # Background color
    # shape = "cardioid"    # Shape of the word cloud
    # word_size_range = c(10000, 10000000)  # Word size range based on frequency
  ) +
    scale_size_area(max_size = 20) +
  theme_minimal() + 
  ggtitle("Word Cloud of Common Keywords", subtitle = "Based on Track Titles in 2023 and 2024") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14))



# plot --------
# Create a word cloud using the total frequencies
wordcloud2(data = common_freq[, .(word, total_freq)],
           size = 0.7,         # Adjust size of words
           color = "random-light", # Use random light colors
           backgroundColor = "white",
           shape = 'pentagon',
           fontFamily = "Caldara") # Set background color



gr

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
