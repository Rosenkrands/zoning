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
  filter(method == 'GA', obj == "TOT", ar_var %in% c(20,50,80), no_of_centers %in% c(5,10,15),
         grid_dimension == 8)

# Load the solutions into a list, with method specified
solutions <- pbapply::pblapply(
  result_filtered$file %>% split(1:nrow(result_filtered)),
  function(file) readRDS(paste0('./solution_for_simulation/',file))
)
names(solutions) <- str_c("sim_", result_filtered$file)

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
  if (file.exists(file)) {cat("File exists, continuing...\n"); return()}
  rslt <- simulation(solutions[[i]], method = method, flight = flight)
  saveRDS(rslt, file = file)
}

system.time({
# set up of parallel computation
num_cores <- detectCores(logical = F)
cl <- makeCluster(num_cores)

parallel::clusterExport(cl, c('solutions', 'simulation'))
invisible(parallel::clusterEvalQ(cl, {library(dplyr); library(tidyr)}))

pbapply::pblapply(
  seq_along(solutions),
  run_simulation,
  cl = cl
)
})
