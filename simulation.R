library(parallel)
source('2d-instance.R')
source('simulation-function.R')
source('solution-functions.R')

# Find the best solution
sol_files <- list.files('./dimension_tuning')

result <- do.call(
  bind_rows, 
  pbapply::pblapply(sol_files %>% as.list(), calc_obj)
)

# Clean results to reflect correct number of iterations
result_clean <- result %>%
  mutate(miter = replace_na(miter, 100))

best_solutions <- result_clean %>%
  group_by(instance) %>%
  slice_min(TOT,n = 1) %>%
  filter(row_number() == 1)

# Load the solutions into a list, with method specified
solutions <- pbapply::pblapply(
  best_solutions$file %>% split(1:nrow(best_solutions)),
  function(file) readRDS(paste0('./dimension_tuning/',file))
)

# Perform simulation for the best solutions, in parallel
run_simulation <- function(solution) {
  # Determine solution method
  if (is.null(solution[["ga"]])) method = "KMeans" else method = "GA"
  simulation(solution, method = method)
}

# set up of parallel computation
num_cores <- detectCores()
cl <- makeCluster(num_cores)

parallel::clusterExport(cl, c('solutions', 'simulation', 'euclid_norm'))
invisible(parallel::clusterEvalQ(cl, library(dplyr)))

simulation_results <- pbapply::pblapply(
  solutions,
  run_simulation,
  cl = cl
)

# TODO: find a way to connect the solution (objective, dimension, number of iterations and so on) with the simulation (agent performance, safety distance etc.)