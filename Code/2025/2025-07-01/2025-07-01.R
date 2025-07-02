

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)


# load data ------

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')


# clean data ------
library(data.table)
library(ggplot2)

df[, date := as.Date(date)]




# Create a 'year-month' column
gas_data[, year_month := as.Date(format(date, "%Y-%m-01"))]

# Calculate monthly average price per grade
monthly_gas <- gas_data[, .(price = mean(price, na.rm = TRUE)), by = .(year_month, grade)]

# Add the decade column again
monthly_gas[, decade := paste0(floor(as.integer(format(year_month, "%Y")) / 10) * 10, "s")]

ggplot(monthly_gas, aes(x = year_month, y = price, fill = grade)) +
  geom_area(position = "identity", alpha = 0.7) +
  facet_wrap(~ decade, scales = "free_x") +
  labs(
    title = "Monthly U.S. Gasoline Prices by Grade and Decade",
    subtitle = "Averaged by month to smooth weekly fluctuations",
    x = "Year",
    y = "Price per Gallon (USD)",
    fill = "Grade"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal(base_size = 13)



library(ggridges)
ggplot(df[fuel == "gasoline" & grade == "regular"], aes(x = price, y = decade, fill = decade)) +
    geom_density_ridges(scale = 3, alpha = 0.7) +
    labs(title = "Distribution of Regular Gasoline Prices by Decade")

# Plot -------
# 
# p = ggplot(df_plot) +
#     
#     geom_segment(aes(x = commission_date, xend = termination_date, y = full_name, 
#                      yend = full_name), 
#                  color = "grey70", linewidth = .75) +
#     
#     geom_point(aes(x = commission_date, y = full_name), color = "#0072B5", size = 3) +
#     
#     geom_point(aes(x = termination_date, y = full_name), color = "#b24745", size = 3) +
#     
#     scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
#    
#     labs(
#         title = "Most Popular Bookshelves in the Gutenberg Project",
#         subtitle = "Each bubble represents a bookshelf. <b>Larger </b> bubbles mean more books belong to that category.",
#         caption = "Source: <b> {gutenbergr} R package</b> | Graphic: <b>Natasa Anastasiadou</b>"
#     ) +
#     
#     facet_wrap(vars(gender)) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         # panel.grid.major.y = element_blank(),
#         axis.text.y = element_text(size = 9),
#         axis.text.x = element_text(size = 9, angle = 90),
#         axis.title = element_blank(),
#         
#         
#         panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
#         panel.grid.minor = element_blank(),
#     
#         panel.border = element_rect(fill = NA, linewidth = .4),
#         
#         plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
#         plot.subtitle = element_markdown(size = 14, hjust = 0.5,  color = "grey30"),
#         plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
#         
#         plot.margin = margin(20, 20, 20, 20),
#         plot.background = element_rect(fill = "#e4e4e3", color = NA)
#         
#     )
# 
# 
# ggsave(
#     plot = p, filename = "plot.png",
#     width = 8.5, height = 9, units = "in", dpi = 600
# )
