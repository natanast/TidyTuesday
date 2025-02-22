

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)

library(giscoR)  
library(sf)
library(circlize)

# load data --------

agencies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')


# data cleaning ------

df <- agencies[!is.na(agencies$agency_type) & agency_type != "Unknown", ]


# Aggregate by state and agency type
df1 <- df[, .N, by = .(state, agency_type)]


# Convert data to a matrix format for chordDiagram
chord_matrix <- dcast(df1, agency_type ~ state, value.var = "N", fill = 0)

# Store row names before converting to matrix
agency_types <- chord_matrix$agency_type

# Remove agency_type column
chord_matrix$agency_type <- NULL

# Convert to a matrix (this will lose row names)
chord_matrix <- as.matrix(chord_matrix)

# Reassign the row names after conversion
rownames(chord_matrix) <- agency_types


# my_col = c('#3a5cbc', '#b9b8e7','#BC3C29','#E18727','#0072B5', '#dddaea', '#20854E','#FFDC91','#6F99AD', '#F39B7F','#D0DFE6', '#b24745')
# 

# Create a named vector where each agency type is assigned a color
color_map <-  c(
    "City"                  = "#b24745", 
    "County"                = "#b9b8e7", 
    "University or College" = "#3a5cbc",
    "Other"                 = "#6F99AD",
    "Other State Agency"    = "#66C2A5", 
    "Tribal"                = "#F39B7F", 
    "State Police"          = "#5E4FA2"
)




# plot ---------

png("Rplot.png", width = 3000, height = 3000, res = 200) 



par(bg = "grey93")  # Adjust bottom margin (first value)


circos.clear()
circos.par(
    start.degree = 270,
    canvas.xlim = c(-1.1, 1.1),
    canvas.ylim = c(-1.1, 1.1)
    
)


# Draw the chord diagram with custom settings
chordDiagram(chord_matrix, 
             annotationTrack = c("grid", "names"), 
             grid.col = color_map,
             annotationTrackHeight = c(0.01, 0.001),
             preAllocateTracks = list(track.height = 0.1))




# labs
title("FBI Crime Data: Agency types in U.S. states.",
      cex.main = 2,  
      font.main = 1,
      line = -2)


mtext("A chord diagram representing the distribution of Agency types across U.S. states",
      side = 3, line = -4, cex = 1.2)

mtext("Source: FBI Crime Data API | Graphic: Natasa Anastasiadou",
      side = 3, line = -73, cex = 1, adj = 1)



# Customize labels on the diagram
circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
                cex = 0.85)
}, bg.border = NA)


dev.off()

