

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(extrafont)
library(ggtext)
library(ggstream)
library(paletteer)


# load data --------

parfumo_data_clean <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv')


# data cleaning -----

d = parfumo_data_clean[, c("Name", "Brand", "Release_Year", "Rating_Value", "Rating_Count", "Perfumers" )]


d <- d[!is.na(d$Release_Year) & !is.na(d$Rating_Value) & !is.na(d$Rating_Count), ]

d1 <- d[Rating_Count > 500]


# Calculate the longevity of each brand
brand_longevity <- d1[, .(
    Min_Year = min(Release_Year),
    Max_Year = max(Release_Year),
    Active_Years = max(Release_Year) - min(Release_Year) + 1
), by = Brand]


brand_longevity <- brand_longevity[Active_Years > 20]


d2 <- d1[Brand %in% brand_longevity$Brand, ]

stream_data <- d2[, .(Release_Year, Rating_Value, Brand)]


stream_data$Release_Year <- stream_data$Release_Year |> as.numeric() 


# Aggregate the ratings by Release_Year and Brand to get the average rating
stream_data_avg <- stream_data[, .(Avg_Rating = mean(Rating_Value, na.rm = TRUE)), by = .(Release_Year, Brand)]


stream_data_avg <- stream_data_avg[Release_Year > 1980,]


# Recording --------

library(camcorder)

gg_record(
    dir = file.path("recording"),
    device = "png",
    width = 12,
    height = 12,
    units = "in",
    dpi = 600
)


# plot ---------

col = c('#60608b', '#6c6c98', '#7978a4', '#8584b1', '#9291be', '#9e9ecb', '#acabd8', '#b9b8e5', '#c6c5f2', '#ffeacf', '#ffd5be', '#fcc1ad', '#f7ad9c', '#f09a8c', '#e7877d', '#dc756e', '#d0645f', '#c15451')

ggplot(stream_data_avg, aes(x = Release_Year, y = Avg_Rating, fill = Brand)) +
    
    geom_stream(
        type = "mirror",
        color = "grey85",
        linewidth = .35,
        extra_span = .20
    ) +
    
    labs(
        title = "Trends in Perfume Ratings Across Decades",
        subtitle = "Average perfume ratings of popular brands with over 20 years of releases and significant reviews (>500 Rating Count).",
        caption = "Source: <b> Parfumo Fragrance Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Release Year",
        y = "Average Rating",
        fill = "Brand"
    ) +

    theme_minimal() +
    
    scale_fill_manual(values = col) +

    theme(
        legend.position = "right",

        legend.title = element_text(size = 10, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),

        axis.title.x = element_text(size = 12, family = "Candara"),
        axis.title.y = element_text(size = 12, family = "Candara"),

        axis.text.x = element_text(size = 12, family = "Candara"),
        axis.text.y = element_text(size = 12, family = "Candara"),

        panel.grid.major = element_line(linewidth = .4, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),

        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5, )),
        plot.subtitle = element_markdown(size = 14, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, family = "Candara", hjust = 1.25),

        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )


# ggsave(
#     plot = gr, filename = "Rplot.png",
#     width = 11, height = 9, units = "in", dpi = 600
# )


gg_stop_recording()

gg_playback(
    name = "Rplot_gif.gif",
    first_image_duration = 8,
    last_image_duration = 5,
    frame_duration = 0.5, 
    width = 2000, # Match or scale up to the recorded dimensions (8 inches * 600 dpi)
    height = 600
)

