


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(ggplot2)



# data cleaning ------------
tuesdata <- tidytuesdayR::tt_load('2024-10-22')

cia_factbook <- tuesdata$cia_factbook



x <- as.data.table(cia_factbook)


x <- x[, .(country, area, population)]

x = x[order(-area), ]


x$population_density = x$population / x$area

x <- x[1:10]




# Plot 1---------


library(packcircles)
library(ggplot2)
library(shadowtext)

library(paletteer)
library(extrafont)

library(ggtext)


# Process the data 
df_plot <- x[, {
  dat.egg <- circleProgressiveLayout(area)
  dat.egg <- circleLayoutVertices(dat.egg, npoints = 100)
  
  cbind(dat.egg, .SD[dat.egg$id])
}]


setDT(df_plot)

df_plot_l <- df_plot[, .(x = (min(x) + max(x)) / 2, 
                         y = (min(y) + max(y)) / 2, 
                         population = first(population)),
                     by = .(country, id)]



df_plot_l$lbl <- round(df_plot_l$population / 1e7, 1)


col = c('#BC3C29','#b9b8e7','#E18727', '#B09C85','#FFDC91','#5E4FA2','#0072B5','#20854E','#9dced6','#91D1C2')




p = df_plot |>
  ggplot(aes(x, y, group = id)) +
  
  geom_polygon(
    aes(fill = country), 
    color = "grey30", 
    linewidth = .25
  ) +
  
  
  geom_shadowtext(
    data = df_plot_l, 
    inherit.aes = FALSE,
    aes(x, y, label = lbl), 
    color = "grey1", 
    bg.color = "#d9e3f1", 
    bg.r = .05,
    size = 8
  ) +
  
  scale_fill_manual(
    values = col
  ) +
  
  scale_size_continuous(guide = "none", range = c(2, 12)) +
  
  
  coord_equal() +
  
  labs(title = "Top 10 Countries by Area and Population",
       
       subtitle = "Bubble Size Represents Area; Population Numbers Inside Bubbles (in Tens of Millions)",
       
       
       caption = paste0(
         "Source: <b>CIA World Factbook</b> | ",
         "Graphic: <b>Natasa Anastasiadou</b>"
       )
  ) +
  
  theme_minimal() +
  
  theme(
    
    axis.title = element_blank(),
    axis.text = element_blank(),
    
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    
    panel.grid.major = element_line(linewidth = .35, color = "grey85"),
    panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
    
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, family = "Candara"),
    plot.subtitle = element_markdown(size = 17, hjust = 0.5, family = "Candara", color = "grey30"),
    plot.caption  = element_markdown(margin = margin(t = 25), size = 12, family = "Candara", hjust = 1),
    
    plot.margin = margin(20, 20, 20, 20),
    
    plot.background = element_rect(fill = "grey95", color = NA)
    
  )


ggsave(
  plot = p, filename = "Rplot.png",
  width = 14, height = 10, units = "in", dpi = 600
)  




