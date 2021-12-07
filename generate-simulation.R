library(parallel)
source('2d-instance.R')
source('simulation-function.R')
source('solution-functions.R')

# Find the best solution
sol_files <- list.files('./solution_for_simulation')

result <- do.call(
  bind_rows, 
  pbapply::pblapply(sol_files %>% as.list(), calc_obj2)
)

result_filtered <- result %>% 
  filter(grid_dimension != 8)

# Load the solutions into a list, with method specified
solutions <- pbapply::pblapply(
  result$file %>% split(1:nrow(result)),
  function(file) readRDS(paste0('./solution_for_simulation/',file))
)
names(solutions) <- str_c("sim_", result$file)

# Perform simulation for solutions, in parallel
run_simulation <- function(i) {
  # Determine solution method
  if (is.null(solutions[[i]][["ga"]])) {
    method = "KMeans"
    flight = "zoned"
  } else {
    method = "GA"
    flight = "zoned"
  }
  file = paste0('./simulations/',names(solutions)[[i]])
  # if (file.exists(file)) {cat("File exists, continuing...\n"); return()}
  rslt <- simulation(solutions[[i]], method = method, flight = flight)
  saveRDS(rslt, file = file)
}

system.time({
# set up of parallel computation
num_cores <- 8 # detectCores(logical = F)
cl <- makeCluster(num_cores)

parallel::clusterExport(cl, c('solutions', 'simulation'))
invisible(parallel::clusterEvalQ(cl, library(dplyr)))

pbapply::pblapply(
  seq_along(solutions),
  run_simulation,
  cl = cl
)
})
