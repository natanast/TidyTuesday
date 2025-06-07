

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)
# library(ggstream)
# library(ggforce)


# load data ------

gutenberg_authors <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_authors.csv')
gutenberg_languages <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_languages.csv')
gutenberg_metadata <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_metadata.csv')
gutenberg_subjects <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_subjects.csv')


# clean data ------
df <- gutenberg_metadata[
    , .(bookshelf = unlist(str_split(gutenberg_bookshelf, "/"))), 
    by = gutenberg_id
]

df <- df[, .N, by = bookshelf][order(-N)]


ggplot(df[1:20], aes(x = reorder(bookshelf, N), y = N)) +
    geom_col(fill = "#1f78b4") +
    # coord_polar() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Top 30 Gutenberg Bookshelves", caption = "#TidyTuesday")





library(ggplot2)
library(ggwordcloud)

ggplot(df[1:20], aes(label = bookshelf, size = N)) +
    geom_text_wordcloud(area_corr = TRUE, family = "serif", color = "darkblue") +
    scale_size_area(max_size = 12) +
    theme_minimal() +
    labs(
        title = "Most Common Subjects in Project Gutenberg",
        subtitle = "Based on Library of Congress Subject Headings",
        caption = "#TidyTuesday | Data: Project Gutenberg"
    )



gr = ggplot(df_filtered, aes(x = year, y = mean_bacteria, fill = council)) +
    
    geom_stream(
        extra_span = 0.2,
        bw = .9,
        lwd = 0.15,
        color = "grey20"
    ) +

    geom_stream(
        extra_span = 0.2, 
        true_range = "none",
        bw = .9,
        alpha = 0.2
    ) +
    
    scale_fill_manual(values = col) +
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "Coastal Water Quality Across NSW Councils Over Years",
        subtitle = "Top 10 councils by mean enterococci bacteria levels by council, measured in CFU per 100mL",
        caption = "Source: <b>Water Quality at Sydney Beaches data </b> | Graphic: <b>Natasa Anastasiadou</b>", 
        x = "",
        y = "Mean Enterococci (CFU/100ml)",
        fill = "Council"
    ) +
    
    theme(
        
        legend.position = "right",
        legend.key.size = unit(.75, "lines"),      
        legend.text = element_text(size = 9),      
        legend.title = element_text(size = 10),
        
        axis.text = element_text(size = 11),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),

        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = .5),

        plot.background = element_rect(fill = "grey93", color = NA),
        plot.margin = margin(20, 20, 20, 20)

    )


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)


