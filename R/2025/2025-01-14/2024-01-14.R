

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggwordcloud)
library(extrafont)
library(ggtext)
library(paletteer)


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

common_freq_filtered$angle <- sample(c(0, 45, 60, 90, 120, 180), nrow(common_freq_filtered), replace = TRUE)



# plot --------

col = c('#0072b5', '#2b78b9', '#417ebe', '#5284c2', '#618bc6', '#6f91cb', '#7c97cf', '#899ed3', '#95a4d8', '#a1abdc', '#adb1e1', '#f1a494', '#eb9a8b', '#e59183', '#df887b', '#d87f73', '#d2766b', '#cc6c63', '#c6635b', '#bf5a54', '#b9514c', '#b24745')

gr = ggplot(common_freq_filtered, aes(label = word, size = total_freq, color = word)) +
    geom_text_wordcloud(
        family = "Candara"
    ) +

    scale_size_area(max_size = 25) +
    
    scale_color_manual(values = col) +  # Set custom colors
    
    theme_minimal() + 
    
    labs(
        title = "Keyword Trends: Common words from Track Titles in Posit::Conf Talks",
        subtitle = "A word cloud visualization of shared keywords from track titles in Posit::Conf talks of 2023 and 2024. <br> These trends showcase key topics and evolving themes from the conference.",
        caption = "Source: <b> posit::conf talks from 2023 and 2024</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme(
        plot.title = element_markdown(size = 19, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 38)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, face = "bold", family = "Candara", color = "grey30", margin = margin(b = 5, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )



gr

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 8.5, height = 8, units = "in", dpi = 600
)

