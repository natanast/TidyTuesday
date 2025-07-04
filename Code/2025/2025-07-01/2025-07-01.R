

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)

library(extrafont)


# load data ------

gas_data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')


# clean data ------

gas_data[, date := as.Date(date)]


# # Create a 'year-month' column
# gas_data[, year_month := as.Date(format(date, "%Y-%m-01"))]
# 
# # Calculate monthly average price per grade
# monthly_gas <- gas_data[, .(price = mean(price, na.rm = TRUE)), by = .(year_month, grade)]
# 
# # Add the decade column again
# monthly_gas[, decade := paste0(floor(as.integer(format(year_month, "%Y")) / 10) * 10, "s")]

gas <- gas_data[fuel == "gasoline" & grade != "all" & formulation != "all"]


# plot --------


gas[, year := as.integer(format(date, "%Y"))]

yearly_gas <- gas[, .(price = mean(price, na.rm = TRUE)), by = .(year, grade)]

yearly_gas[, decade := paste0(floor(year / 10) * 10, "s")]


ggplot(yearly_gas, aes(x = factor(year), y = price, fill = grade)) +
    
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.9) +

    # geom_col(position = "dodge", alpha = 0.8, width = .75) +
    
    facet_wrap(~ decade, scales = "free_x") +

    
    labs(
        title = "Yearly Average Gasoline Prices",
        subtitle = "Each bubble represents a bookshelf. <b>Larger </b> bubbles mean more books belong to that category.",
        caption = "Source: <b> {gutenbergr} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Price per Gallon (USD)",
        fill = "Grade"
    ) +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        # plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        # plot.subtitle = element_markdown(size = 14, hjust = 0.5,  color = "grey30"),
        # plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )




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





