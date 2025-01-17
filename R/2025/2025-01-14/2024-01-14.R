

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
# library(wordcloud2)
library(ggwordcloud)
library(extrafont)
library(ggtext)


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



common_freq_1 <- common_freq[, .(word, total_freq)]



# Words to exclude
exclude_words <- c("", "a", "and", "in", "is", "it", "its", "not", "of", "or", "the", "to", "up", "with", "your")

# Filter out excluded words
common_freq_filtered <- common_freq[!word %in% exclude_words]



# plot --------


ggplot(common_freq_filtered, aes(label = word, size = total_freq, color = word)) +
    geom_text_wordcloud(
        family = "Candara"   # Set font
        # color = "black",      # Word color
        # background = "white" # Background color
        # shape = "cardioid"    # Shape of the word cloud
        # word_size_range = c(10000, 10000000)  # Word size range based on frequency
    ) +

    scale_size_area(max_size = 20) +
    
    theme_minimal() + 
    
    labs(
        title = "Word Cloud of Common Keywords",
        subtitle = "Based on Track Titles in 2023 and 2024.",
        caption = "Source: <b> posit::conf talks from 2023 and 2024</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme(
        plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5, )),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5, face = "bold", family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )



gr

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
