

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


ggplot(stream_data_avg, aes(x = Release_Year, y = Avg_Rating, fill = Brand)) +
    
    geom_stream(
        type = "mirror",
        color = "grey85",
        linewidth = .02,
        # bw = .75
        extra_span = .20
        # true_range = "both"
        # sorting = "inside_out"
    ) +
    
    labs(
        title = "Average Perfume Ratings Over Time by Brand",
        x = "Release Year",
        y = "Average Rating",
        fill = "Brand"
    ) +
    
    theme_minimal() +
    
    scale_fill_manual(values = paletteer_c("ggthemes::Sunset-Sunrise Diverging", n = 30)) +

    theme(
        legend.position = "right"
    )


