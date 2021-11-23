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

# Read simulations into list
sim_files <- list.files('./simulations')

sim_result <- pbapply::pblapply(
  sim_files %>% as.list(),
  function(f) readRDS(paste0('./simulations/',f))
)

# Join the simulation results on the file version
names(sim_result) <- result$file[1:4]

result_table <- as_tibble(sim_result) %>% 
  mutate(type = c("metric", "log")) %>%
  pivot_longer(cols = -c(type), names_to = "file") %>%
  pivot_wider(id_cols = c(type, file), names_from = type)

data <- result %>% inner_join(result_table, by = "file")

#' calculate metrics for the simulation results, this should be done for each
#' repetition of the simulation and for each of the simulations

plot_data_6_arv <- do.call(
  bind_rows, 
  lapply(seq_along(data$log[2][[1]]),
         function(i) data$log[2][[1]][[i]] %>% mutate(rep_id = i))
) %>% 
  select(id, rep_id, status, time) %>%
  mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
  group_by(rep_id, time) %>%
  summarise(inUse = mean(inUse)) %>%
  mutate(inUse = cumsum(inUse))

plot_data_6_safe <- do.call(
  bind_rows, 
  lapply(seq_along(data$log[4][[1]]),
         function(i) data$log[4][[1]][[i]] %>% mutate(rep_id = i))
) %>% 
  select(id, rep_id, status, time) %>%
  mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
  group_by(rep_id, time) %>%
  summarise(inUse = mean(inUse)) %>%
  mutate(inUse = cumsum(inUse))

plot_data <- bind_rows(
  plot_data_6_arv %>% mutate(obj = "ARV"),
  plot_data_6_safe %>% mutate(obj = "SAFE"),
)

plot_data %>%
  ggplot() +
  geom_line(aes(x = time, y = inUse/time, group = as.character(rep_id)),
            color = "gray") +
  geom_line(data = plot_data %>%
              group_by(obj, time) %>%
              summarise(inUse = mean(inUse)),
            aes(x = time, y = inUse/time, group = obj)) +
  geom_hline(yintercept = 1, linetype="dashed") +
  facet_wrap(~obj) +
  theme_bw()

# safety distances
ggplot() +
  geom_histogram(data = data$metric[2][[1]][[1]]$distances %>% mutate(obj = "ARV"),
                 aes(x = distance)) +
  geom_histogram(data = data$metric[4][[1]][[1]]$distances %>% mutate(obj = "SAFE"),
                 aes(x = distance)) +
  facet_wrap(~obj, nrow = 2)
  
plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',data$file[2]))
) + ggtitle("obj = ARV")

plot_network(
  instance = NULL,
  solution = readRDS(paste0('./solution_for_simulation/',data$file[4]))
) + ggtitle("obj = SAFE")

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
  mc
}
# We need confidence intervals for the mean instead of this i think
sd_coverage <- function(x) {
  mc <- do.call(
    bind_rows,
    lapply(
      x,
      function(z) z$demandPerformance %>% 
        summarise(sd_coverage = sd(nCovered/nGenerated))
    )
  ) %>% summarise(sd_coverage = mean(sd_coverage))
  return(as.numeric(mc))
}

test <- data %>%
  rowwise() %>%
  mutate(mean_coverage = list(mean_coverage(metric))) %>%
  select(instance, method, obj, no_of_centers, mean_coverage)

sample_m <- mean(test$mean_coverage[2][[1]]$demand_coverage)
t <- (sd(test$mean_coverage[2][[1]]$demand_coverage)/sqrt(19))*pt(c(.975), 19)


mean(test$mean_coverage[4][[1]]$demand_coverage)
sd(test$mean_coverage[4][[1]]$demand_coverage)/sqrt(19)

tibble(
  sample_mean = c(
    mean(test$mean_coverage[1][[1]]$demand_coverage),
    mean(test$mean_coverage[3][[1]]$demand_coverage) 
  ),
  t = c(
    (sd(test$mean_coverage[1][[1]]$demand_coverage)/sqrt(19))*pt(c(.975), 19),
    (sd(test$mean_coverage[3][[1]]$demand_coverage)/sqrt(19))*pt(c(.975), 19)
  ),
  obj = c("ARV", "SAFE")
) %>%
  ggplot(aes(x=obj, y=sample_mean, color=obj)) +
  geom_pointrange(aes(ymin=sample_mean - t, ymax=sample_mean + t))


# Checking the random seeds
data$metric[1][[1]][[1]]$demandPerformance %>% summarise(nGenerated = sum(nGenerated))
data$metric[2][[1]][[1]]$demandPerformance %>% summarise(nGenerated = sum(nGenerated))

data$metric[1][[1]][[2]]$demandPerformance %>% summarise(nGenerated = sum(nGenerated))
data$metric[2][[1]][[2]]$demandPerformance %>% summarise(nGenerated = sum(nGenerated))
