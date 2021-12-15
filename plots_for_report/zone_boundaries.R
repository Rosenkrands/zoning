source('2d-instance.R')

instance = generate_2d_instance(no_of_points = 200)

solution <- solve_kmeans(instance, no_of_centers = 8)

solution$clusters <- solution$clusters %>% mutate(`Centroid id` = factor(row_number()))

ggplot(
  solution$instance
) +
  geom_point(aes(x,y,color=`Centroid id`)) +
  geom_point(
    data = solution$clusters, 
    aes(x, y, color=`Centroid id`), shape = 10, size = 5
  ) +
  geom_voronoi(
    data = solution$clusters,
    aes(x, y, fill = `Centroid id`),
    alpha = .25,
    # geom="path",
    outline = data.frame(
      x = 1.1*c(-10,-10,10,10),
      y = 1.1*c(-10,10,10,-10)
    )
  ) +
  # stat_voronoi(
  #   data = solution$clusters,
  #   aes(x, y),
  #   geom="path",
  #   outline = data.frame(
  #     x = 1.1*c(-10,-10,10,10),
  #     y = 1.1*c(-10,10,10,-10)
  #   )
  # ) +
  theme_void() + theme(legend.position = "none") + coord_fixed()

ggsave('./plots_for_report/voronoi_centroid_zones.pdf', width = 5, height = 5)

ggplot(
  solution$instance
) +
  geom_voronoi(
    data = solution$instance,
    aes(x, y, fill = `Centroid id`),
    alpha = .25,
    # geom="path",
    outline = data.frame(
      x = 1.1*c(-10,-10,10,10),
      y = 1.1*c(-10,10,10,-10)
    )
  ) +
  # stat_voronoi(
  #   data = solution$instance,
  #   aes(x, y),
  #   geom="path",
  #   outline = data.frame(
  #     x = 1.1*c(-10,-10,10,10),
  #     y = 1.1*c(-10,10,10,-10)
  #   )
  # ) +
  geom_point(aes(x,y,color=`Centroid id`)) +
  geom_point(
    data = solution$clusters, 
    aes(x, y, color=`Centroid id`), shape = 10, size = 5
  ) +
  theme_void() + theme(legend.position = "none") + coord_fixed()

ggsave('./plots_for_report/voronoi_point_zones.pdf', width = 5, height = 5)