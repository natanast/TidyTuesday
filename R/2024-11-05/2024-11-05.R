

# load libraries -----------

library(data.table)
library(stringr)
library(ggplot2)

library(ggrepel)
library(ggtext)
library(extrafont)
library(colorspace)




# load data ------------

democracy_data <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv')



# data cleaning --------

df <- democracy_data[, .(
    country_name, year, regime_category, is_monarchy, monarch_name, 
    monarch_accession_year,  is_democracy, is_presidential, president_name,
    president_accesion_year, is_colony, is_female_monarch, is_female_president
)]


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


df2$country_female <- ifelse(df2$female == TRUE, df2$country, "")
    


df2 <- df2[order(-duration)]



# Keeping only the top 20 longest-serving rulers
top_rulers <- df2[1:20]

# Make sure leader_name is a factor with levels ordered by duration
top_rulers$leader_name <- factor(top_rulers$leader_name, 
                                 levels = top_rulers$leader_name[order(top_rulers$duration, decreasing = FALSE)])

name_colors <- setNames(ifelse(top_rulers$female, "grey20", "grey60"), top_rulers$leader_name)


# Callout text for female leaders
callout <- paste0(
    "<b>Female leaders</b> stand out<br>
     with <b>darker names</b> and <b>lines.</b>"
)


# plot -----

p <- ggplot(top_rulers, aes(x = duration, y = leader_name)) + 
    
    geom_segment(aes(x = start_year, xend = end_year, 
                     y = leader_name, yend = leader_name, 
                     color = ifelse(female, "Female", "Male")),
                 size = 1, linetype = "solid", alpha = 0.6) +
    
    geom_point(aes(x = start_year, fill = "Start", color = "Start"), size = 5, shape = 21, stroke = 0.25) +
    
    geom_point(aes(x = end_year, fill = "End", color = "End"), size = 5, shape = 21, stroke = 0.25) +
    
    scale_fill_manual(values = c("Start" = "#0072B5", "End" = "#b24745")) +
    scale_color_manual(values = c("Female" = "grey15", "Male" = "grey75")) +
    
    geom_label_repel(
        aes(x = start_year, y = leader_name, label = country_female),
        nudge_x = -6, nudge_y = 0, max.overlaps = Inf,
        label.size = NA, fill = alpha("#e4e4e3", .65),
        size = 5, family = "Candara", color = "grey20"
    ) +
    
    geom_richtext(
        aes(x = 1850, y = 14, label = callout), stat = "unique",
        family = "Candara", size = 6, lineheight = 1.2,
        color = "grey20", hjust = 0, vjust = 1.03, fill = NA, label.color = NA
    ) +
    
    labs(title = "Top 20 Longest-Serving Rulers (Monarchs & Presidents)",
         subtitle = "<b>Female</b> leaders are a rare sight among the longest-serving monarchs and presidents, with only <b>3</b> appearing in the top 20.",
         caption = "Source: <b>Democracy and Dictatorship Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
         x = "", y = "") +
    
    
    scale_y_discrete(labels = function(x) {
        sapply(x, function(name) {
            paste0("<span style='color:", name_colors[name], "'>", name, "</span>")
        })
    }) +
    
    theme_minimal() +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 18, face = "bold", family = "Candara"), 
        axis.text.y = element_markdown(size = 18, family = "Candara", face = "bold"), # Enable markdown for color
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
        plot.title = element_text(size = 26, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_markdown(size = 18, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption = element_markdown(margin = margin(t = 25), size = 14, family = "Candara", hjust = 1),
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )



ggsave(
    plot = p, filename = "Rplot.png",
    width = 16, height = 12, units = "in", dpi = 600
)    

