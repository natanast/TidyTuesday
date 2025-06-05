

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(stringr)
library(ggplot2)
library(extrafont)
library(ggtext)
library(ggstream)
library(ggforce)


# load data ------

monsters <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')

# clean data ------

monsters <- monsters[hp_number > 250]

df <- monsters[, .(name, ac, initiative, str,dex,con, int, cha)]

df <- df[complete.cases(df)]




# Prepare data for heatmap: melt to long format
df_long <- melt(df, id.vars = "name")

ggplot(df_long, aes(x = name, y = variable, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c() +
    # coord_polar() +
    theme_minimal() +
    labs(x = "Stat", y = "Monster", fill = "Value") +
    theme(axis.text.y = element_text(size = 4))






library(ggplot2)
library(data.table)

df <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')

df_clean <- df[, .(alignment, hp_number)]

ggplot(df_clean, aes(x = alignment, y = hp_number)) +
    # geom_boxplot(alpha = 0.7) +
    geom_jitter(aes(color = alignment), size = 2, width = 0.1, height = 0.2) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = "HP Number Distribution by Monster Alignment",
         x = "Alignment",
         y = "HP Number")



ggplot(df_clean, aes(x = hp_number, fill = alignment)) +
    geom_histogram(bins = 30, alpha = 0.7) +
    facet_wrap(~ alignment, scales = "free_y") +
    theme_minimal() +
    labs(title = "HP Number Distribution per Alignment",
         x = "HP Number", y = "Count")


library(data.table)
library(ggplot2)

# Filter complete cases
df_stat_avg <- df[, .(alignment, hp_number, ac, initiative, str, dex, con, int, cha)]
df_stat_avg <- df_stat_avg[complete.cases(df_stat_avg)]

# Melt and compute mean per group
df_melted <- melt(df_stat_avg, id.vars = "alignment", variable.name = "Stat")


df_melted <- df_melted[, .(value = mean(value, na.rm = TRUE)), by = .(alignment, Stat)]

# Plot heatmap
ggplot(df_melted, aes(x = Stat, y = alignment, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c() +
    theme_minimal(base_size = 14) +
    labs(title = "Average Monster Stats by Alignment", x = "Stat", y = "Alignment")



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


gr


ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)

