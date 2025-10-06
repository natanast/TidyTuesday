

rm(list = ls())
gc()


# Libraries ------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)

# load data ------

euroleague_basketball <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv')

# clean data ------

df1 <- euroleague_basketball[, .(Team, FinalFour_Appearances, Titles_Won, Years_of_FinalFour_Appearances, Years_of_Titles_Won)]


# Separate Final Four years
finalfour_long <- df1[, .(
    Year = unlist(str_split(Years_of_FinalFour_Appearances, ",\\s*")),
    Status = "Final Four"
), by = Team]

# Separate Title years
titles_long <- df1[, .(
    Year = unlist(str_split(Years_of_Titles_Won, ",\\s*")),
    Status = "Championship"
), by = Team]



df_long <- rbind(finalfour_long, titles_long, fill = TRUE)


df_long$Year <- ifelse(df_long$Year == "None", "", df_long$Year)

df_long <- df_long[Year != ""]

df_long[, Year := as.integer(Year)]


# make sure Team is a factor and ordered by number of Titles (optional)
team_order <- df_long[Status == "Championship", .N, by = Team][order(-N)]$Team
team_order <- unique(c(team_order, df_long$Team))
df_long[, Team := factor(Team, levels = rev(team_order))]  



# plot ------ 

col = c("#426b7b", "#9fc6d9")


gr = ggplot() +
    
    geom_point(
        data = df_long, 
        aes(x = Year, y = Team, color = Status, fill = Status), 
        size = 4.5, 
        stroke = .35,
        # alpha = 0.85,
        shape = 21
        
    ) +
    
    scale_color_manual(values = col |> darken(.25) ) +
    scale_fill_manual(values = col |> lighten(.25) ) +
    
    # Make x-axis continuous
    scale_x_continuous(
        limits = c(1988, 2025),          # set range
        breaks = seq(1988, 2025, by = 2) # put a tick every 2 years
    ) +
    
    labs(
        title = "Road to Glory: EuroLeague Teams’ Final Four and Titles (1988–2025)",
        subtitle = "<b><span style='color: #426b7b; font-weight: bold;'>Darker</span></b> dots indicate <b><span style='color: #426b7b; font-weight: bold;'>Championship victories</span></b>, 
                    while <b><span style='color: #81a7ba; font-weight: bold;'>lighter</span></b> dots indicate  <b><span style='color: #81a7ba; font-weight: bold;'>Final Four appearances</span></b>.",
        caption = "Source: <b>Euroleague dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year",
        y = "",
        fill = ""
    ) +
    
    guides(color = "none") +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.title = element_markdown(size = 15, face = "bold", hjust = .4, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = .1, color = "grey30", margin = margin(b = 5, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 15), size = 8, hjust = 1),
        
        panel.grid.major = element_line(linewidth = 0.25, color = "grey80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        axis.title.x = element_text(size = 11, margin = margin(t = 15)),
        
        axis.text = element_text(size = 10),
        
        legend.position = "top",
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e8e8e7", color = NA)
    )

gr

ggsave(
    plot = gr, filename = "plot.png",
    width = 10, height = 8, units = "in", dpi = 600
)




