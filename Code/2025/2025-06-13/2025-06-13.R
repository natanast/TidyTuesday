

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)

# load data ------
 
judges_appointments <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_appointments.csv')
judges_people <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_people.csv')


# clean data ------

df1 <- judges_appointments[, .(judge_id, commission_date, termination_date, termination_reason)]

df2 <- judges_people[, .(judge_id, name_first, name_last, gender)]


# Merge datasets 
df <- df1 |>
    merge(df2, by = "judge_id", all = TRUE)


df <- df[!is.na(commission_date) & !is.na(termination_date)]

df$full_name = paste0(df$name_first, " ", df$name_last)



df[, commission_date := as.Date(commission_date, format = "%m/%d/%Y")]
df[, termination_date := as.Date(termination_date, format = "%m/%d/%Y")]
df[, duration := as.numeric(difftime(termination_date, commission_date, units = "days")) / 365.25]


# top 10 Male
df_M <- df[gender == "M", ]
df_top10_M <- df_M[order(-duration)][1:10]


# top 10 Female
df_F <- df[gender == "F", ]
df_top10_F <- df_F[order(-duration)][1:10]


# Combine both
df_plot <- rbind(df_top10_F, df_top10_M)




# 3. Plot
ggplot(df_plot) +
    
    geom_segment(aes(x = commission_date, xend = termination_date, y = full_name, yend = full_name), 
                 color = "#619CFF", linewidth = 1) +
    
    geom_point(aes(x = commission_date, y = full_name), color = "#F8766D", size = 4) +
    
    geom_point(aes(x = termination_date, y = full_name), color = "#00BFC4", size = 4) +
    
    scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
    
    labs(
        title = "Top 20 Longest-Serving Female Judges in U.S. Federal Courts",
        subtitle = "Commission to Termination Dates",
        x = "Years",
        y = NULL,
        caption = "Source: #TidyTuesday | Data: Federal Judicial Center"
    ) +
    
    facet_wrap(vars(gender)) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        # panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 9),
        
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
        plot.caption = element_text(size = 10, margin = margin(t = 10))
    )


# plot --------

p <- df_plot |>
    
    ggplot(aes(x, y, group = id)) +
    
    geom_polygon(
        aes(fill = bookshelf),
        fill = "#79a3b7", 
        color = "grey30", 
        linewidth = .35
    ) +
    
    geom_shadowtext(
        data = df_plot_l, 
        aes(x, y, label = lbl, size = N),
        inherit.aes = FALSE,
        color = "grey1", 
        bg.color = "#d9e3f1", 
        bg.r = .05,
        family = "Candara"
    ) +

    scale_size_continuous(guide = "none", range = c(2, 8)) +
    
    
    labs(
        title = "Most Popular Bookshelves in the Gutenberg Project",
        subtitle = "Each bubble represents a bookshelf. <b>Larger </b> bubbles mean more books belong to that category.",
        caption = "Source: <b> {gutenbergr} R package</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        legend.position = "none",

        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5,  color = "grey30"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),

        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )



ggsave(
    plot = p, filename = "plot.png",
    width = 8.5, height = 9, units = "in", dpi = 600
)




