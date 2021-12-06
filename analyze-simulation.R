library(parallel)
source('2d-instance.R')
source('simulation-function.R')
source('solution-functions.R')

# Get solution files to join on
sol_files <- list.files('./solution_for_simulation')

result <- do.call(
  bind_rows, 
  pbapply::pblapply(sol_files %>% as.list(), calc_obj2)
)

result <- result %>% mutate(
  `Solution method` = factor(paste0(method,':',obj),
                             levels = c("GA:ARV", "GA:SAFE", "GA:TOT", "KM:WCSS")),
  `Number of UAVs` = factor(as.numeric(no_of_centers),
                            levels = c(5, 10, 15),
                            labels = c("low", "medium", "high")),
  `Arrival rate variance` = factor(ar_var, 
                                   levels = c(20,50,80),
                                   labels = c("low", "medium", "high")),
) %>%
  select(-c(method, obj, no_of_centers, ar_var)) %>%
  filter(grid_dimension == 8)

# Read simulations into list
sim_files <- list.files('./simulations')

sim_result <- pbapply::pblapply(
  sim_files %>% as.list(),
  function(f) readRDS(paste0('./simulations/',f))
)

# Join the simulation results on the file version
names(sim_result) <- result$file

result_table <- as_tibble(sim_result) %>% 
  mutate(type = c("metric", "utilization")) %>%
  pivot_longer(cols = -c(type), names_to = "file") %>%
  pivot_wider(id_cols = c(type, file), names_from = type)

simulation_result <- result %>% inner_join(result_table, by = "file")

saveRDS(simulation_result, file = "./simulation-results.rds")

response_time_metrics <- simulation_result %>%
  select(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, metric) %>%
  mutate(responseTime = map(metric, ~.x[[1]]$responseTimePerformance)) %>%
  unnest(cols = responseTime) %>%
  group_by(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`) %>%
  summarise(`Mean response` = mean(responseTime),
            #`Median response` = median(responseTime),
            `90th percentile response` = quantile(responseTime, probs = c(.9)))

fulfillment_metrics <- simulation_result %>%
  select(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`, metric) %>%
  mutate(demandPerformance = map(metric, ~.x[[1]]$demandPerformance)) %>%
  unnest(cols = demandPerformance) %>%
  group_by(instance, `Solution method`, `Arrival rate variance`, `Number of UAVs`) %>%
  summarise(`Fulfillment ratio` = mean(nCovered/nGenerated, na.rm = TRUE))

regression_data <- inner_join(
  response_time_metrics, fulfillment_metrics,
  by = c("instance", "Solution method", "Arrival rate variance", "Number of UAVs")
)

saveRDS(regression_data, file = './regression-data.rds')

# safety distances
ggplot() +
  geom_histogram(data = data$metric[3][[1]][[1]]$distances %>% mutate(obj = "TOT"),
                 aes(x = distance)) +
  geom_histogram(data = data$metric[6][[1]][[1]]$distances %>% mutate(obj = "WCSS"),
                 aes(x = distance)) +
  facet_wrap(~obj, nrow = 2)
  
plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',data$file[3]))
) + ggtitle("obj = TOT")

plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',data$file[6]))
) + ggtitle("obj = WCSS")

# effectiveness
mean_coverage <- function(x) {
  mc <- do.call(
    bind_rows,
    lapply(
      x,
      function(z) z$demandPerformance %>% 
        summarise(demand_coverage = sum(nCovered)/sum(nGenerated))
    )
  )
  as.numeric(mc)
}

# test <- data %>%
#   rowwise() %>%
#   mutate(mean_coverage = mean_coverage(metric)) %>%
#   select(instance, method, obj, no_of_centers, mean_coverage)

simulation_result %>%
  filter(`Solution method` %in% c("GA:TOT", "KM:WCSS"),
         `Number of UAVs` %in% c("low","medium","high"),
         `Arrival rate variance` %in% c("low","medium","high")) %>%
  rowwise() %>%
  mutate(mean_coverage = mean_coverage(metric)) %>%
  select(-c(metric, utilization)) %>%
  group_by(`Solution method`, `Number of UAVs`, `Arrival rate variance`) %>%
  summarise(t = (sd(mean_coverage)/sqrt(20))*pt(c(.975), 1),
            mean_coverage = mean(mean_coverage)) %>%
  ggplot(aes(x = `Arrival rate variance`,
             y = mean_coverage,
             fill = `Solution method`)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_coverage - t, ymax = mean_coverage + t),
                position = position_dodge(.9), width = .2) +
  facet_wrap(~ `Number of UAVs`, nrow = 1)
  
simulation_result %>%
  filter(`Solution method` %in% c("GA:TOT", "KM:WCSS"),
         `Number of UAVs` %in% c("low","medium","high"),
         `Arrival rate variance` %in% c("low","medium","high")) %>%
  rowwise() %>%
  mutate(mean_response = mean_response(metric)) %>%
  select(-c(metric, utilization)) %>%
  group_by(`Solution method`, `Number of UAVs`, `Arrival rate variance`) %>%
  summarise(t = (sd(mean_response)/sqrt(20))*pt(c(.975), 1),
            mean_response = mean(mean_response)) %>%
  ggplot(aes(x = `Arrival rate variance`,
             y = mean_response,
             fill = `Solution method`)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_response - t, ymax = mean_response + t),
                position = position_dodge(.9), width = .2) +
  facet_wrap(~ `Number of UAVs`, nrow = 1)

# Response rate distribution
rrd_data <- simulation_result %>%
  unnest(cols = metric) %>%
  mutate(metric = map(metric, ~.x$responseTimePerformance)) %>%
  unnest(cols = metric) %>%
  filter(`Solution method` %in% c("GA:TOT","KM:WCSS"),
         `Number of UAVs` %in% c("low","medium", "high"),
         `Arrival rate variance` %in% c("low", "medium", "high"))

rrd_mean_data <- rrd_data %>%
  group_by(`Solution method`, `Number of UAVs`, `Arrival rate variance`) %>%
  summarise(mean_responseTime = mean(responseTime),
            median_responseTime = median(responseTime),
            t = (sd(responseTime)/sqrt(20))*pt(c(.975), 1))

rrd_data %>%
  ggplot(aes(x=responseTime,
             fill = `Solution method`)) +
  geom_density(color = 'black', alpha = .4) +
  # geom_vline(data = rrd_mean_data, aes(xintercept = mean_responseTime, color = `Solution method`)) +
  facet_grid(`Number of UAVs`~`Arrival rate variance`, labeller = label_both) +
  theme_bw()

rrd_mean_data <-