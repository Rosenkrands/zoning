source("2d-instance.R")

instance <- generate_2d_instance(no_of_points = 10)

ggplot(
  instance$data %>%
    mutate(`Demand point id` = paste0("x[",`Demand point id`,"]"))
) +
  geom_text(aes(x,y,label=`Demand point id`), parse=T) +
  geom_path(data = tibble(x = c(-10,10,10,-10,-10), 
                          y = c(-10,-10,10,10,-10)) %>%
              mutate(across(c(x,y), ~.x*1.1)),
               aes(x,y)) + 
  theme_void()

ggsave('plots_for_report/example_instance.pdf',width = 4, height = 4)

ggplot(
  instance$data %>%
    mutate(`Demand point id` = paste0("x[",`Demand point id`,"]"),
           Zone = as.character(c(1,2,3,1,1,3,3,2,3,3)))
           # Zone = c("z[1]", "z[2]", "z[3]", "z[1]", "z[1]", "z[3]", "z[3]", "z[2]", "z[3]", "z[3]"))
) +
  geom_text(aes(x,y,label=`Demand point id`, color = Zone), parse=T, show.legend=F) +
  geom_point(aes(x*100,y*100,color = Zone))+
  geom_path(data = tibble(x = c(-10,10,10,-10,-10), 
                          y = c(-10,-10,10,10,-10)) %>%
              mutate(across(c(x,y), ~.x*1.1)),
            aes(x,y)) +
  scale_color_manual(values = 2:4, labels = c(parse(text="z[1]"), parse(text="z[2]"), parse(text="z[3]"))) +
  scale_x_continuous(limits=(c(-11,11))) +
  scale_y_continuous(limits=(c(-11,11))) +
  theme_void()

ggsave('plots_for_report/example_instance_w_zone.pdf',width = 4.5, height = 4)

centroids <- grid_centroids(instance, dimension = 3)

ggplot(
  instance$data %>%
    mutate(`Demand point id` = paste0("x[",`Demand point id`,"]"))
) +
  geom_text(aes(x,y,label=`Demand point id`), parse=T) +
  geom_path(data = tibble(x = c(-10,10,10,-10,-10), 
                          y = c(-10,-10,10,10,-10)) %>%
              mutate(across(c(x,y), ~.x*1)),
            aes(x,y)) +
  geom_point(
    data = centroids$locations, aes(x, y), shape = 10, size = 5
  ) +
  theme_void()

ggsave('plots_for_report/example_instance_centroids.pdf',width = 4, height = 4)

solution <- solve_ga(instance, centroids, no_of_centers = 3, obj = "TOT", miter = 10)

assignment <- centroids$distances %>%
  inner_join(solution$centroids, by = "Centroid id") %>%
  group_by(`Demand point id`) %>%
  filter(Distance == min(Distance)) %>%
  ungroup() %>%
  select(-x, -y)

instance_w_assignment <- instance$data %>%
  inner_join(assignment, by = "Demand point id")

ggplot(
  instance_w_assignment %>%
    mutate(`Demand point id` = paste0("x[",`Demand point id`,"]"))
) +
  geom_segment(data = solution$instance, aes(x = x, y = y, xend = x.centroid, yend = y.centroid),
               color = "gray") +
  geom_text(aes(x,y,label=`Demand point id`,color=`Centroid id`), parse=T) +
  geom_path(data = tibble(x = c(-10,10,10,-10,-10), 
                          y = c(-10,-10,10,10,-10)) %>%
              mutate(across(c(x,y), ~.x*1.1)),
            aes(x,y)) +
  geom_point(
    data = solution$centroids, 
    aes(x, y, color=`Centroid id`), shape = 10, size = 5
  ) +
  theme_void() +
  theme(legend.position = "none")

ggsave('plots_for_report/example_instance_selected_centroids.pdf',width = 4, height = 4)

solution_free <- solve_kmeans(instance, no_of_centers = 3)

centroids <- solution_free$instance %>% 
  select(`Centroid id`, x.centroid, y.centroid) %>%
  distinct()

ggplot(solution_free$instance %>%
         mutate(`Demand point id` = paste0("x[",`Demand point id`,"]"))
  ) +
  geom_segment(aes(x = x, y = y, xend = x.centroid, yend = y.centroid),
               color = "gray") +
  geom_text(aes(x,y,label=`Demand point id`,color=`Centroid id`), parse=T) +
  geom_path(data = tibble(x = c(-10,10,10,-10,-10), 
                          y = c(-10,-10,10,10,-10)) %>%
              mutate(across(c(x,y), ~.x*1.1)),
            aes(x,y)) +
  geom_point(
    data = centroids,
    aes(x.centroid, y.centroid, color=`Centroid id`), shape = 10, size = 5
  ) +
  theme_void() +
  theme(legend.position = "none") 

ggsave('plots_for_report/example_instance_selected_centroids_free.pdf',width = 4, height = 4)
