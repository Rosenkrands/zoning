library(parallel)
source('2d-instance.R')
source('simulation-function.R')
source('solution-functions.R')

# Find the best solution
sol_files <- list.files('./solutions')

result <- do.call(
  bind_rows, 
  pbapply::pblapply(sol_files %>% as.list(), calc_obj)
)

# Clean results to reflect correct number of iterations
result_clean <- result %>%
  mutate(miter = replace_na(miter, 100))

best_solutions <- result_clean %>%
  filter(instance == '40988') %>%
  group_by(instance, method) %>%
  slice_min(TOT,n = 1) %>%
  filter(row_number() == 1)

# Load the solutions into a list, with method specified
solutions <- pbapply::pblapply(
  best_solutions$file %>% split(1:nrow(best_solutions)),
  function(file) readRDS(paste0('./solutions/',file))
)

# Perform simulation for the best solutions, in parallel
run_simulation <- function(solution) {
  # Determine solution method
  if (is.null(solution[["ga"]])) method = "KMeans" else method = "GA"
  simulation(solution, method = method)
}

system.time({
# set up of parallel computation
num_cores <- detectCores(logical = F)
cl <- makeCluster(num_cores)

parallel::clusterExport(cl, c('solutions', 'simulation', 'euclid_norm'))
invisible(parallel::clusterEvalQ(cl, library(dplyr)))

simulation_results <- pbapply::pblapply(
  solutions,
  run_simulation,
  cl = cl
)
})
# Join the simulation results on the file version
names(simulation_results) <- best_solutions$file

result_table <- as_tibble(simulation_results) %>% 
  mutate(type = c("metric", "log")) %>%
  pivot_longer(cols = -c(type), names_to = "file") %>%
  pivot_wider(id_cols = c(type, file), names_from = type)

data <- best_solutions %>% inner_join(result_table, by = "file")

#' calculate metrics for the simulation results, this should be done for each
#' repetition of the simulation and for each of the simulations

test <- data$log[1][[1]][[1]]

test %>%
  select(id, status, time) %>%
  mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
  group_by(time) %>%
  summarise(inUse = mean(inUse)) %>%
  mutate(inUse = cumsum(inUse)) %>%
  ggplot(aes(x = time, y = inUse/time)) +
  geom_line() +
  # tidyquant::geom_ma(n=30) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_bw()

mean_coverage <- function(x) {
  mc <- do.call(
    bind_rows,
    lapply(
      x,
      function(z) z$demandPerformance %>% 
        summarise(demand_coverage = sum(nCovered)/sum(nGenerated))
    )
  ) %>% summarise(demand_coverage = mean(demand_coverage))
  return(as.numeric(mc))
}
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

data %>%
  rowwise() %>%
  mutate(mean_coverage = mean_coverage(metric),
         sd_coverage = sd_coverage(metric))

mean(data[1,]$metric[[1]][[1]]$distances$distance)
hist(data[2,]$metric[[1]][[1]]$distances$distance)

