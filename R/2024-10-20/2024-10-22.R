


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(ggplot2)
library(ggtext)
library(sf)
library(giscoR)  


# data cleaning ------------
tuesdata <- tidytuesdayR::tt_load('2024-10-22')

cia_factbook <- tuesdata$cia_factbook



# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the dataset
cia_factbook <- read.csv("cia_factbook.csv")

# Calculate internet users per square kilometer
cia_factbook <- cia_factbook %>%
  mutate(internet_users_density = internet_users / area) %>%
  filter(!is.na(internet_users_density))  # Remove missing data

# Plot top 10 countries with highest internet user density
top_countries_density <- cia_factbook %>%
  arrange(desc(internet_users_density)) %>%
  top_n(10, internet_users_density)

ggplot(top_countries_density, aes(x = reorder(country, internet_users_density), y = internet_users_density)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries by Internet Users per Square Kilometer",
       x = "Country", y = "Internet Users per Square Kilometer") +
  theme_minimal()
