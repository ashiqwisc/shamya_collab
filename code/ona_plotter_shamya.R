library(tidyverse)

make.ona.plot <- function(set, 
                          plot_name, 
                          flip_x = FALSE
) {
  color <- c("red")
  grand_mean <- plot(set, title = "Grand Mean ONA plot") %>%
    edges(
      weights = set$line.weights,
      # edge_size_multiplier = 0.5,
      edge_color = c("red")) %>%
    nodes(
      # node_size_multiplier = 0.5,
      self_connection_color = c("red")) %>%
    units(
      points=set$points,
      points_color = c("red"),
      show_mean = TRUE, show_points = TRUE, with_ci = TRUE) 
  print(grand_mean)
}