source('./2d-instance.R')

set.seed(110520)
## ILLUSTRATION OF THE FLIGHT CONFIGURATION

r_plot_data <- readRDS('./solution-results.rds') %>% filter(Objective == "TOT", `Grid dimension` == 8)

plot_range_constraint <- function(num_uav, sol_method = "GA:TOT", n = 0) {
  # First choose a level for each of the factors
  # num_uav = "low"; 
  arv = "low"
  
  filtered <- r_plot_data %>%
    filter(`Number of UAVs` == num_uav, 
           `Arrival rate variance` == arv,
           `Solution method` == sol_method)
  
  instances <- unique(filtered$instance)
  selected_instance <- instances[10]#sample(1:length(instances),1)]  
  
  plot_data <- filtered %>% filter(instance == selected_instance)
  
  solution = readRDS(paste0('./solution_for_simulation/',(plot_data %>% filter(`Solution method` == sol_methods[1]))$file))
  
  centroids <- solution$instance %>% 
    select(`Centroid id`, x.centroid, y.centroid) %>%
    distinct() %>%
    mutate(r = (solution$instance %>%
      mutate(distance = sqrt((x - x.centroid)^2 + (y - y.centroid)^2)) %>%
      summarise(distance = max(distance)))$distance
    )
  
  ggplot(solution$instance) +
    geom_segment(aes(x = x, y = y, xend = x.centroid, yend = y.centroid),
                 color = "grey") +
    ggforce::geom_circle(
      data = centroids %>% 
        left_join(tibble(n = c(0,0.2,0.5)), by = character()) %>%
        mutate(r = r + n*switch(num_uav, low = 5, medium = 10, high = 15),
               `Scaling factor` = factor(n)),
      aes(x0 = x.centroid, y0 = y.centroid, r = r, fill = `Centroid id`, linetype = `Scaling factor`),
      alpha = .1, color = "grey"
    ) +
    geom_point(aes(x,y, color = `Centroid id`)) +
    geom_point(
      data = centroids, 
      aes(x.centroid, y.centroid, color=`Centroid id`), shape = 10, size = 5
    ) + theme_void() + coord_fixed() + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
}

plot_range_constraint(num_uav = "low")
ggsave('./plots_for_report/free-flight_range_constraint.pdf', width = 6.5, height = 4.5)

## SIMULATION RESULTS FOR THE DIFFERENT FLIGHT CONFIGURATIONS

ff_plot_data <- readRDS('./free-flight-simulation.rds')

ff_plot_data %>%
  ggplot(aes(x = `Arrival rate variance`, y = `Mean response`, fill = `Flight config`)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~`Number of UAVs`, labeller = label_both)

ff_plot_data %>%
  ggplot(aes(x = `Arrival rate variance`, y = `90th percentile response`, fill = `Flight config`)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~`Number of UAVs`, labeller = label_both)

ff_plot_data %>%
  ggplot(aes(x = `Arrival rate variance`, y = `Fulfillment ratio`, fill = `Flight config`)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~`Number of UAVs`, labeller = label_both)
