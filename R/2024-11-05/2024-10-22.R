


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(stringr)
library(ggplot2)

library(ggrepel)
library(ggtext)
library(extrafont)



# load data ------------

democracy_data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')



# data cleaning --------

df <- democracy_data[, .(
    country_name, year, regime_category, is_monarchy, monarch_name, 
    monarch_accession_year,  is_democracy, is_presidential, president_name,
    president_accesion_year, is_colony, is_female_monarch, is_female_president
)]



# # Monarchs duration calculation
# monarch_duration <- df[!is.na(monarch_name) & !is.na(monarch_accession_year), .(
#     leader_name = monarch_name,
#     country = country_name,
#     start_year = monarch_accession_year,
#     end_year = max(year, na.rm = TRUE),
#     duration = max(year, na.rm = TRUE) - monarch_accession_year
# ), by = .(country_name, monarch_name)]
# 
# 
# # Presidents duration calculation
# president_duration <- df[!is.na(president_name) & !is.na(president_accesion_year), .(
#     leader_name = president_name,
#     country = country_name,
#     start_year = president_accesion_year,
#     end_year = max(year, na.rm = TRUE),
#     duration = max(year, na.rm = TRUE) - president_accesion_year
# ), by = .(country_name, president_name)]



monarch_duration <- df[!is.na(monarch_name) & !is.na(monarch_accession_year) &
                           !(monarch_name == "Elizabeth II" & country_name != "United Kingdom"), .(
                               leader_name = monarch_name,
                               country = country_name,
                               start_year = min(monarch_accession_year, na.rm = TRUE),
                               end_year = max(year, na.rm = TRUE),
                               duration = max(year, na.rm = TRUE) - min(monarch_accession_year, na.rm = TRUE),
                               female = unique(is_female_monarch)
                           ), by = .(country_name, monarch_name)]



president_duration <- df[!is.na(president_name) & !is.na(president_accesion_year), .(
    leader_name = president_name[1],       
    country = country_name[1],             
    start_year = min(president_accesion_year, na.rm = TRUE),  
    end_year = max(year, na.rm = TRUE),                       
    duration = max(year, na.rm = TRUE) - min(president_accesion_year, na.rm = TRUE),
    female = unique(is_female_president)
), by = .(country_name, president_name)] 



df2 <- rbindlist(list(
    monarch_duration[, .(leader_name, country, start_year, end_year, duration, female, type = "Monarch")],
    president_duration[, .(leader_name, country, start_year, end_year, duration, female, type = "President")]
))



df2 <- df2[order(-duration)]



# Keeping only the top 20 longest-serving rulers
top_rulers <- df2[1:20]

# Make sure leader_name is a factor with levels ordered by duration
top_rulers$leader_name <- factor(top_rulers$leader_name, 
                                 levels = top_rulers$leader_name[order(top_rulers$duration, decreasing = FALSE)])


# plot -----
library(colorspace)


ggplot(top_rulers, aes(x = duration,  y = leader_name)) + 
    
    geom_segment(aes(x = start_year, xend = end_year, 
                     y = leader_name, yend = leader_name), 
                 color = "grey75", size = 0.75, linetype = "solid", alpha = 0.6) +
    
    
    geom_point(aes(x = start_year, fill = "start_year", color = "start_year"), size = 3, shape = 21, stroke = 0.25) +
    
    geom_point(aes(x = end_year, fill = "end_year", color = "end_year"), size = 3, shape = 21, stroke = 0.25) +
    
    # Custom colors for start and end year points
    scale_fill_manual(values = c("start_year" = "#0072B5", "end_year" = "#b24745")) +
    scale_color_manual(values = c("start_year" = "#0072B5", "end_year" = "#b24745") |> darken(0.25), guide = "none") +
    
    # geom_point(aes(x = start_year, color = "start_year"), size = 3, shape = 19, alpha = 0.8) +
    
    # geom_point(aes(x = end_year, color = "end_year"), size = 3, shape = 19, alpha = 0.8) + 
    
    geom_label_repel(
        aes(x = end_year, y = leader_name, label = country),
        nudge_x = 0.25,
        nudge_y = 0.25,
        max.overlaps = Inf,
        label.size = NA,
        fill = alpha("#e4e4e3", .65),
        size = 3.75,
        family = "Candara"
    ) +
    
    labs(title = "Top 20 Longest-Serving Rulers (Monarchs & Presidents)",
         x = "",
         y = "",
         
         subtitle = paste0(
             "Rulers with the longest durations"
         ),
         
         caption = paste0(
             "Source: <b>Democracy and Dictatorship Dataset</b> | ",
             "Graphic: <b>Natasa Anastasiadou</b>"
         )
    ) +  
    
    
    theme_minimal() +
    
    theme(
        
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        
        axis.text.x = element_text(size = 12, face = "bold", family = "Candara"), 
        axis.text.y = element_text(size = 12, face = "bold", family = "Candara"),
        
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
        
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 10, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )


p

ggsave(
    plot = p, filename = "Rplot.png",
    width = 12, height = 10, units = "in", dpi = 600
)    




