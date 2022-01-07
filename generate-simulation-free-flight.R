library(parallel)
source('2d-instance.R')
source('simulation-function.R')
source('solution-functions.R')

### GENERATION OF SIMULATION ###
# Find the best solution
sol_files <- list.files('./solution_for_simulation')

result <- do.call(
  bind_rows,
  pbapply::pblapply(sol_files %>% as.list(), calc_obj2)
)

# result_filtered <- result %>%
#   filter(method == 'GA', obj == "TOT", ar_var %in% c(20,80), no_of_centers %in% c(5,15),
#          grid_dimension == 8)

result_filtered <- result %>%
  filter(method == 'GA', obj == "TOT", ar_var %in% c(20,50,80), no_of_centers %in% c(5,10,15),
         grid_dimension == 8)

# Load the solutions into a list, with method specified
solutions <- pbapply::pblapply(
  result_filtered$file %>% split(1:nrow(result_filtered)),
  function(file) readRDS(paste0('./solution_for_simulation/',file))
)
names(solutions) <- str_c("sim_free-flight_max_", result_filtered$file)

distances <- tibble(max_dist = c("0", ".2", ".5", "No constraint"))

params <- as_tibble_col(solutions) %>%
  rename(Solution = value) %>%
  mutate(`Simulation file` = str_c("sim_free-flight_distance-placeholder_", result_filtered$file)) %>%
  full_join(distances, by = character()) %>%
  mutate(`Simulation file` = str_replace(`Simulation file`, 'distance-placeholder', max_dist))

params_list <- split(params, 1:nrow(params))

# Perform simulation for solutions, in parallel
run_simulation <- function(param) {
  # Determine solution method
  if (is.null(param$Solution[[1]][["ga"]])) {
    method = "KMeans"
    flight = "free"
  } else {
    method = "GA"
    flight = "free"
  }
  file = paste0('./simulations_free_flight/',param$`Simulation file`)
  if (file.exists(file)) {cat("File exists, continuing...\n"); return()}

  if (param$max_dist != "No constraint") {
    max_dist <- param$Solution[[1]]$instance %>%
      mutate(distance = sqrt((x - x.centroid)^2 + (y - y.centroid)^2)) %>%
      summarise(distance = max(distance)) %>%
      mutate(distance = distance + as.numeric(param$max_dist) * nrow(param$Solution[[1]]$centroids)) %>% # TRY RUNNING WITH HIGHER THAN .1 AND SEE IF THERE IS A DIFFERENCE
      as.numeric()
  } else {
    max_dist <- 100000
  }

  rslt <- simulation(param$Solution[[1]], method = method, flight = flight, max_dist = max_dist)
  saveRDS(rslt, file = file)
}


# pbapply::pblapply(params_list,run_simulation)

system.time({
# set up of parallel computation
num_cores <- detectCores(logical = F)
cl <- makeCluster(num_cores)

parallel::clusterExport(cl, c('solutions', 'simulation', 'euclid_norm'))
invisible(parallel::clusterEvalQ(cl, {library(dplyr); library(tidyr)}))

pbapply::pblapply(
  params_list,
  run_simulation,
  cl = cl
)
})
# 
### ANALYZING THE RESULTS ###
sol_files <- list.files('./solution_for_simulation')

result <- do.call(
  bind_rows,
  pbapply::pblapply(sol_files %>% as.list(), calc_obj2)
)

result_filtered <- result %>%
  filter(grid_dimension == 8)

result_filtered <- result_filtered %>% mutate(
  `Solution method` = factor(paste0(method,':',obj),
                             levels = c("GA:ARV", "GA:SAFE", "GA:TOT", "KM:WCSS", "WKM:WWCSS")),
  `Number of UAVs` = factor(as.numeric(no_of_centers),
                            levels = c(5, 10, 15),
                            labels = c("low", "medium", "high")),
  `Arrival rate variance` = factor(ar_var,
                                   levels = c(20,50,80),
                                   labels = c("low", "medium", "high")),
) %>%
  select(-c(method, obj, no_of_centers, ar_var)) %>%
  filter(`Solution method` == 'GA:TOT', `Arrival rate variance` %in% c("low", "medium","high"), `Number of UAVs` %in% c("low","medium","high"))

# Read simulations into list
sim_files <- list.files('./simulations_free_flight')

sim_result <- pbapply::pblapply(
  sim_files %>% as.list(),
  function(f) readRDS(paste0('./simulations_free_flight/',f))
)
names(sim_result) <- sim_files

result_table <- as_tibble(sim_result) %>%
  mutate(type = c("metric", "utilization")) %>%
  pivot_longer(cols = -c(type), names_to = "file") %>%
  pivot_wider(id_cols = c(type, file), names_from = type)

simulation_result <- result_table %>%
  rename(`Simulation file` = file) %>%
  rowwise() %>%
  mutate(`Solution file` = paste(str_split(`Simulation file`, '_')[[1]][4:8], collapse="_"),
         `Flight config` = str_split(`Simulation file`, '_')[[1]][3]) %>%
  filter(`Flight config` %in% c("0", ".2", ".5", "1", "No constraint")) %>%
  left_join(result_filtered, by = c("Solution file" = "file"))

response_time_metrics <- simulation_result %>%
  select(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`, metric) %>%
  mutate(responseTime = map(metric, ~.x$responseTimePerformance)) %>%
  unnest(cols = responseTime) %>%
  group_by(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`) %>%
  summarise(`Mean response` = mean(responseTime),
            #`Median response` = median(responseTime),
            `90th percentile response` = quantile(responseTime, probs = c(.9)))

fulfillment_metrics <- simulation_result %>%
  select(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`, metric) %>%
  mutate(demandPerformance = map(metric, ~.x$demandPerformance)) %>%
  unnest(cols = demandPerformance) %>%
  group_by(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`) %>%
  summarise(`Fulfillment ratio` = mean(nCovered/nGenerated, na.rm = TRUE))

distance_metrics <- simulation_result %>%
  select(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`, metric) %>%
  mutate(distanceSummary = map(metric, ~.x$distanceSummary)) %>%
  unnest(cols = distanceSummary) %>%
  mutate(name = str_c("distance ", str_replace(name, "1th", "1st"))) %>%
  pivot_wider(id_cols = c(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`))

regression_data <- inner_join(
  response_time_metrics, fulfillment_metrics,
  by = c("instance", "Solution method", "Arrival rate variance", "Number of UAVs", "Flight config")
) %>%
  inner_join(distance_metrics,
             by = c("instance", "Solution method", "Arrival rate variance", "Number of UAVs", "Flight config"))

### Temporary plot without zoning, real plot is made in ./plots_for_report/free-flight_distance_constraint.R

ff_plot_data <- regression_data %>%
  group_by(`Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`) %>%
  summarise(across(c(`Mean response`, `90th percentile response`, `Fulfillment ratio`), mean)) %>%
  mutate(`Flight configuration` = factor(
    `Flight config`,
    levels = c("0", ".2", ".5", "No constraint"),
    labels = c("Scaling factor: 0", "Scaling factor: 0.2", "Scaling factor: 0.5", "No distance constraint")
  )
  )

ff_plot_data %>%
  pivot_longer(cols = c(`Mean response`, `Fulfillment ratio`), names_to = "Measure") %>%
  mutate(Measure = factor(Measure, levels = c("Mean response", "Fulfillment ratio"))) %>%
  ggplot(aes(x = `Arrival rate variance`, y = value, fill = `Flight configuration`)) +
  geom_col(position = position_dodge(), color = "black") +
  facet_grid(Measure~`Number of UAVs`, scales = "free", labeller = label_both) +
  theme_bw() + labs(y="")

# OUR PICK FOR FREE FLIGHT FLIGHT CONFIG IS 0
saveRDS(
  regression_data %>%
    filter(`Flight config` == "0") %>%
    select(-`Flight config`) %>%
    mutate(`Solution method` = "Free-flight"),
  './free-flight-data.rds'
)

regression_data_zoned <- readRDS("./regression-data.rds") %>%
  filter(`Solution method` == "GA:TOT",
         `Arrival rate variance` %in% c("low", "medium", "high"),
         `Number of UAVs` %in% c("low", "medium", "high")) %>%
  mutate(`Flight config` = "Zoned")

# regression_data <- readRDS('./free-flight-data.rds')

plot_data <- bind_rows(regression_data, regression_data_zoned) %>%
  mutate(`Flight config` = factor(`Flight config`, levels = c("Zoned", "0", ".2", ".5", "1", "No constraint"))) %>%
  group_by(`Solution method`, `Arrival rate variance`, `Number of UAVs`, `Flight config`) %>%
  summarise(across(c(`Mean response`, `90th percentile response`, `Fulfillment ratio`), mean))

saveRDS(plot_data, './free-flight-simulation.rds')

plot_data %>%
  ggplot(aes(x = `Arrival rate variance`, y = `Mean response`, fill = `Flight config`)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~`Number of UAVs`, labeller = label_both)

plot_data %>%
  ggplot(aes(x = `Arrival rate variance`, y = `90th percentile response`, fill = `Flight config`)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~`Number of UAVs`, labeller = label_both)

plot_data %>%
  ggplot(aes(x = `Arrival rate variance`, y = `Fulfillment ratio`, fill = `Flight config`)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~`Number of UAVs`, labeller = label_both)

# plot_data %>%
#   filter(`Number of UAVs` %in% c("medium", "high"),
#          `Arrival rate variance` == c("low", "medium"),
#          `Flight config` %in% c("Zoned", "0", ".2")) %>%
#   pivot_longer(cols = c(`Mean response`, `90th percentile response`, `Fulfillment ratio`)) %>%
#   filter(name %in% c("Mean response", "Fulfillment ratio")) %>%
#   ggplot(aes(x = ``))

## OLD
# Join the simulation results on the file version

# result_table_max <- as_tibble(sim_result[1:40]) %>%
#   mutate(type = c("metric", "utilization")) %>%
#   pivot_longer(cols = -c(type), names_to = "file") %>%
#   pivot_wider(id_cols = c(type, file), names_from = type)
#
# result_table_no <- as_tibble(sim_result[41:80]) %>%
#   mutate(type = c("metric", "utilization")) %>%
#   pivot_longer(cols = -c(type), names_to = "file") %>%
#   pivot_wider(id_cols = c(type, file), names_from = type)
#
# simulation_result_max <- result %>% inner_join(result_table_max, by = "file")
# simulation_result_no <- result %>% inner_join(result_table_no, by = "file")
#
# response_time_metrics_max <- simulation_result_max %>%
#   select(`Solution method`, `Arrival rate variance`, `Number of UAVs`, metric) %>%
#   mutate(responseTime = map(metric, ~.x[[1]]$responseTimePerformance)) %>%
#   unnest(cols = responseTime) %>%
#   group_by(`Solution method`, `Arrival rate variance`, `Number of UAVs`) %>%
#   summarise(`Mean response` = mean(responseTime),
#             #`Median response` = median(responseTime),
#             `90th percentile response` = quantile(responseTime, probs = c(.9)))
#
# fulfillment_metrics_max <- simulation_result_max %>%
#   select(`Solution method`, `Arrival rate variance`, `Number of UAVs`, metric) %>%
#   mutate(demandPerformance = map(metric, ~.x[[1]]$demandPerformance)) %>%
#   unnest(cols = demandPerformance) %>%
#   group_by(`Solution method`, `Arrival rate variance`, `Number of UAVs`) %>%
#   summarise(`Fulfillment ratio` = mean(nCovered/nGenerated, na.rm = TRUE))
#
# response_time_metrics_no <- simulation_result_no %>%
#   select(`Solution method`, `Arrival rate variance`, `Number of UAVs`, metric) %>%
#   mutate(responseTime = map(metric, ~.x[[1]]$responseTimePerformance)) %>%
#   unnest(cols = responseTime) %>%
#   group_by(`Solution method`, `Arrival rate variance`, `Number of UAVs`) %>%
#   summarise(`Mean response` = mean(responseTime),
#             #`Median response` = median(responseTime),
#             `90th percentile response` = quantile(responseTime, probs = c(.9)))
#
# fulfillment_metrics_no <- simulation_result_no %>%
#   select(`Solution method`, `Arrival rate variance`, `Number of UAVs`, metric) %>%
#   mutate(demandPerformance = map(metric, ~.x[[1]]$demandPerformance)) %>%
#   unnest(cols = demandPerformance) %>%
#   group_by(`Solution method`, `Arrival rate variance`, `Number of UAVs`) %>%
#   summarise(`Fulfillment ratio` = mean(nCovered/nGenerated, na.rm = TRUE))
#
# regression_data_max <- inner_join(
#   response_time_metrics_max, fulfillment_metrics_max,
#   by = c("Solution method", "Arrival rate variance", "Number of UAVs")
# )
#
# regression_data_no <- inner_join(
#     response_time_metrics_no, fulfillment_metrics_no,
#     by = c("Solution method", "Arrival rate variance", "Number of UAVs")
#   )
#
#
# regression_data_zoned <- regression_data %>% filter(`Solution method` == "KM:WCSS",
#                            `Arrival rate variance` %in% c("low", "high"),
#                            `Number of UAVs` %in% c("low", "high"))
#
# plot_data <- bind_rows(
#   regression_data_max %>% mutate(range_constraint = "max"),
#   regression_data_no %>% mutate(range_constraint = "no"),
#   regression_data_zoned %>% mutate(range_constraint = "zoned")
# ) %>%
#   mutate(range_constraint = factor(range_constraint,
#                                    levels = c("no", "max", "zoned"),
#                                    labels = c("free-flight no limit", "free-flight limit", "zoned")))
#
# plot_data %>%
#   ggplot(aes(x = `Arrival rate variance`, y = `Mean response`, fill = range_constraint)) +
#   geom_col(position = position_dodge()) +
#   facet_wrap(~`Number of UAVs`, labeller = label_both)
#
# plot_data %>%
#   ggplot(aes(x = `Arrival rate variance`, y = `90th percentile response`, fill = range_constraint)) +
#   geom_col(position = position_dodge()) +
#   facet_wrap(~`Number of UAVs`, labeller = label_both)
#
# plot_data %>%
#   ggplot(aes(x = `Arrival rate variance`, y = `Fulfillment ratio`, fill = range_constraint)) +
#   geom_col(position = position_dodge()) +
#   facet_wrap(~`Number of UAVs`, labeller = label_both)
#
# plot_data %>% filter(range_constraint %in% c("max", "zoned"))
