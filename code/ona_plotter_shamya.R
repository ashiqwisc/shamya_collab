library(tidyverse)

make.ona.plot <- function(set, 
                          plot_name, 
                          flip_x = FALSE
) {
  grand_mean <- plot(set, title = "Grand Mean ONA plot") %>%
    edges(
      weights = set$line.weights,
      edge_size_multiplier = 0.8,
      edge_saturation_multiplier = 1,
      edge_color = c("black")) %>%
    nodes(
      # node_size_multiplier = 0.5,
      self_connection_color = c("black")) %>%
    units(
      points=set$points,
      points_color = c("black"),
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) 
  print(grand_mean)
  
  # plot_periods <- plot(set, title = "Period ONA Scores") %>%
  #   units(
  #     points = set$points[set$points$periodID == 1,],
  #     points_color = c("red"),
  #     show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
  # units(
  #   points = set$points[set$points$periodID == 2,],
  #   points_color = c("orange"),
  #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
  # units(
  #   points = set$points[set$points$periodID == 3,],
  #   points_color = c("green"),
  #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE)  %>%
  # units(
  #   points = set$points[set$points$periodID == 4,],
  #   points_color = c("blue"),
  #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
  # units(
  #   points = set$points[set$points$periodID == 5,],
  #   points_color = c("purple"),
  #   show_mean = TRUE, show_points = FALSE, with_ci = TRUE) 
  # print(plot_periods)
  
  
  # color <- c("red", "orange", "green", "blue", "purple")
  # for(period in c(1,2,3,4,5)) {
  #   plot_period <- plot(set, title = paste0("Period ", period)) %>%
  #     units(
  #       points = set$points[set$points$periodID == period,],
  #       points_color = c(color[period]),
  #       show_mean = TRUE, show_points = TRUE, with_ci = TRUE) %>%
  #     edges(
  #       weights = set$line.weights[set$line.weights$periodID == period,],
  #       edge_size_multiplier = 2,
  #       edge_saturation_multiplier = 2,
  #       # edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
  #       # node_position_multiplier = node_position_multiplier,
  #       edge_color = c(color[period])) %>%
  #     nodes(
  #       # node_size_multiplier = node_size_multiplier,
  #       # node_position_multiplier = node_position_multiplier,
  #       self_connection_color= c(color[period]))
  #   print(plot_period)
  # }
  
  
}