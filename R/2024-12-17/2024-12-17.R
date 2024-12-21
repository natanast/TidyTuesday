

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)

library(tidygraph)

library(ggrepel)
library(ggraph)
library(ggnewscale)
library(shadowtext)

library(extrafont)
library(ggtext)


# load data --------

spells <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-17/spells.csv')


# data cleaning ------

df <- spells[range_type %in% c("self", "touch") & level > 6, ]

# Create an edge list for classes and spells
edges <- melt(
    df, 
    id.vars = "name", 
    measure.vars = patterns("bard|cleric|druid|paladin|ranger|sorcerer|warlock|wizard"),
    variable.name = "class", 
    value.name = "can_cast"
)[can_cast == TRUE, .(spell = name, class)]





# Prepare node labels
edges[, spell := str_wrap(spell, width = 15)]


# Generate the graph object
graph <- edges[, .(from = spell, to = class)] %>%
    as_tbl_graph()

# Add node attributes for Degree and Level
layout <- graph |>
    mutate(
        Degree = centrality_degree(mode = "all"),
        Level = ifelse(name %in% edges$class, "Class", "Spell")
    ) |>
    create_layout(layout = "igraph", algorithm = "kk")



# plot --------
gr = ggraph(layout) + 
    
    # geom_edge_link(color = "#97A1A7", edge_width = .05) +
    geom_edge_bundle_force(
        n_cycle = 2.5,
        threshold = 0.5,
        color = "#97A1A7",
        edge_width = .4
    ) +
    
    
    geom_node_label(
        aes(label = name, fill = Level),
        fontface = "bold",
        family = "Candara",
        color = "grey25",
        hjust = 0.5,
        vjust = 0.5,
        size = 5
        ) +

    # geom_node_point(
    #     aes(size = Degree, fill = Level), shape = 21,
    #     stroke = .2, color = "grey96"
    # ) +
    
    # scale_size_continuous(
    #     range = c(6, 18), 
    #     # guide = guide_legend(
    #     #     title = "No. of connections",
    #     #     override.aes = list(color = "grey10", stroke = .35)
    #     # )
    # ) +
    
    scale_fill_manual(
        values = c(
            "Class" = "#acabd8",
            "Spell" = "#ffeacf"
        ),
        guide = "none" 
    ) +
    
    new_scale("size") +
    
    
    scale_size_continuous(range = c(6, 10), guide = "none") +
    
    theme_graph() +
    
    labs(
        title = "Exploring Spellcasting Across Classes in Dungeons & Dragons (2024 Edition)",
        subtitle = "High-level (Level > 6) spellcasting: <span style='color:#60608b; font-weight: bold;'>Classes</span> capable of casting touch and self-range <span style='color:#f7ad9c; font-weight: bold;'>spells</span>.",
        caption = "Source: <b>Dungeons & Dragons Free Rules (2024 Edition)</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme(
        plot.title = element_markdown(size = 22, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5, )),
        plot.subtitle = element_markdown(size = 18, hjust = 0.5, face = "bold", family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 12, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 12, height = 12, units = "in", dpi = 600
)
