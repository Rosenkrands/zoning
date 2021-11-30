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
                            levels = c(5, 10, 15, 20),
                            labels = c("low", "medium", "high", "20")),
  `Arrival rate variance` = factor(ar_var, 
                                   levels = c(20,50,80),
                                   labels = c("low", "medium", "high")),
) %>%
  select(-c(method, obj, no_of_centers, ar_var))

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

data <- result %>% inner_join(result_table, by = "file")

saveRDS(data, file = "./simulation-results.rds")

# prepare data for utilization plot
# data %>%
#   unnest(cols = utilization) %>%
#   unnest(cols = utilization) %>%
#   filter(`Solution method` %in% c("GA:TOT","KM:WCSS"),
#          `Number of UAVs` %in% c("low"),
#          `Arrival rate variance` %in% c("high")) %>%
#   ggplot(aes(x=time, 
#              y=inUse/time, 
#              group = file, 
#              color = `Solution method`)) + 
#   geom_line() +
#   facet_wrap(`Number of UAVs`~`Arrival rate variance`) +
#   theme_bw()
  

#' calculate metrics for the simulation results, this should be done for each
#' repetition of the simulation and for each of the simulations
# 
# plot_data_6_tot <- do.call(
#   bind_rows, 
#   lapply(seq_along(data$log[3][[1]]),
#          function(i) data$log[3][[1]][[i]] %>% mutate(rep_id = i))
# ) %>% 
#   select(id, rep_id, status, time) %>%
#   mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
#   group_by(rep_id, time) %>%
#   summarise(inUse = mean(inUse)) %>%
#   mutate(inUse = cumsum(inUse))
# 
# plot_data_6_wcss <- do.call(
#   bind_rows, 
#   lapply(seq_along(data$log[6][[1]]),
#          function(i) data$log[6][[1]][[i]] %>% mutate(rep_id = i))
# ) %>% 
#   select(id, rep_id, status, time) %>%
#   mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
#   group_by(rep_id, time) %>%
#   summarise(inUse = mean(inUse)) %>%
#   mutate(inUse = cumsum(inUse))
# 
# plot_data <- bind_rows(
#   plot_data_6_tot %>% mutate(obj = "TOT"),
#   plot_data_6_wcss %>% mutate(obj = "WCSS"),
# )
# 
# plot_data %>%
#   ggplot() +
#   geom_line(aes(x = time, y = inUse/time, group = as.character(rep_id)),
#             color = "gray") +
#   geom_line(data = plot_data %>%
#               group_by(obj, time) %>%
#               summarise(inUse = mean(inUse)),
#             aes(x = time, y = inUse/time, group = obj)) +
#   geom_hline(yintercept = 1, linetype="dashed") +
#   facet_wrap(~obj) +
#   theme_bw()

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

mean_response <- function(x) {
  mc <- do.call(
    bind_rows,
    lapply(
      x,
      function(z) z$demandPerformance %>%
        filter(nCovered != 0) %>%
        mutate(mean_response = totalResponseTime/nCovered) %>%
        summarise(mean_response = mean(mean_response))
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

# sample_m <- mean(test$mean_coverage[2][[1]]$demand_coverage)
# t <- (sd(test$mean_coverage[2][[1]]$demand_coverage)/sqrt(19))*pt(c(.975), 19)
# 
# 
# mean(test$mean_coverage[4][[1]]$demand_coverage)
# sd(test$mean_coverage[4][[1]]$demand_coverage)/sqrt(19)
# 
# tibble(
#   sample_mean = c(
#     mean(test$mean_coverage[3][[1]]$demand_coverage),
#     mean(test$mean_coverage[6][[1]]$demand_coverage) 
#   ),
#   t = c(
#     (sd(test$mean_coverage[3][[1]]$demand_coverage)/sqrt(1))*pt(c(.975), 1),
#     (sd(test$mean_coverage[6][[1]]$demand_coverage)/sqrt(1))*pt(c(.975), 1)
#   ),
#   obj = c("TOT", "WCSS")
# ) %>%
#   ggplot(aes(x=obj, y=sample_mean, color=obj)) +
#   geom_pointrange(aes(ymin=sample_mean - t, ymax=sample_mean + t))
# 
# 
# # Checking the random seeds
# data$metric[3][[1]][[1]]$demandPerformance %>% summarise(nGenerated = sum(nGenerated))
# data$metric[6][[1]][[1]]$demandPerformance %>% summarise(nGenerated = sum(nGenerated))
