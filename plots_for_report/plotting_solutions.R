source("2d-instance.R")

simulation_result <- readRDS("./simulation-results.rds")

# First choose a level for each of the factors
num_uav = "low"; arv = "high"

# Choose solution methods to display solutions for
sol_methods = c("GA:TOT", "GA:SAFE", "WKM:WWCSS", "KM:WCSS")

filtered <- simulation_result %>%
  filter(`Number of UAVs` == num_uav, 
         `Arrival rate variance` == arv,
         `Solution method` %in% sol_methods)

instances <- unique(filtered$instance)
selected_instance <- instances[25]  

plot_data <- filtered %>% filter(instance == selected_instance)

plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',(plot_data %>% filter(`Solution method` == sol_methods[1]))$file))
) + coord_fixed()

ggsave('./plots_for_report/example_of_ga_tot.pdf', width = 5, height = 4)
  
plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',(plot_data %>% filter(`Solution method` == sol_methods[2]))$file))
) +  coord_fixed()  

ggsave('./plots_for_report/example_of_ga_safe.pdf', width = 5, height = 4)

filtered <- simulation_result %>%
  filter(`Number of UAVs` == num_uav, 
         `Arrival rate variance` == arv,
         `Solution method` %in% sol_methods)

instances <- unique(filtered$instance)
selected_instance <- instances[3]  

plot_data <- filtered %>% filter(instance == selected_instance)

plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',(plot_data %>% filter(`Solution method` == sol_methods[4]))$file))
) + coord_fixed()

ggsave('./plots_for_report/example_of_km_wcss_instance_3.pdf', width = 5, height = 4)


plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',(plot_data %>% filter(`Solution method` == sol_methods[1]))$file))
) + coord_fixed()

ggsave('./plots_for_report/example_of_ga_tot_instance_3.pdf', width = 5, height = 4)

plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',(plot_data %>% filter(`Solution method` == sol_methods[3]))$file))
) +  coord_fixed()  

ggsave('./plots_for_report/example_of_wkm_wwcss_instance_3.pdf', width = 5, height = 4)
