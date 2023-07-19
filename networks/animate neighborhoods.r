# Load required libraries
library(igraph)
library(ggplot2)
library(ggraph)
library(gganimate)

# Generate a random graph
set.seed(123)
g <- sample_gnp(10, 0.3)

# Convert graph to data frame for plotting
df <- data.frame(get.edgelist(g))

# Calculate layout
layout <- layout_nicely(g)
vertices <- data.frame(id = V(g), x = layout[,1], y = layout[,2])

# Create plots for each node highlighting its neighborhood
plots <- lapply(1:vcount(g), function(i) {
  edges <- E(g)[.inc(i)]
  df$color <- ifelse(get.edgelist(g)[,1] %in% ends(g, edges)[,1] & get.edgelist(g)[,2] %in% ends(g, edges)[,2], "red", "black")
  vertices$color <- ifelse(vertices$id == i, "red", "black")
  vertices$size <- ifelse(vertices$id == i, 5, 3)
  
  p <- ggplot() +
    geom_edge_link(data = df, aes(x = x, y = y, xend = xend, yend = yend, color = color), 
                   arrow = grid::arrow(length = unit(0.2,"cm")), end_cap = circle(0.07, 'cm'), 
                   edge_alpha = 0.8, edge_width = 0.7) +
    geom_point(data = vertices, aes(x = x, y = y, color = color, size = size)) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(1,1,1,1, "cm"),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("Neighborhood of node", i))
  
  return(p)
})

# Combine all plots into a single animation
animation <- gganimate::transition_states(list = plots, 1:length(plots), 
                                          transition_length = 2, state_length = 1)
animate(animation, nframes = 200, width = 400, height = 300, renderer = gifski_renderer())
