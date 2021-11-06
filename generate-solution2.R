source("2d-instance.R")

files <- list.files('./instances')
direcs <- as.list(files)
names(direcs) <- vapply(
  files,
  function(file){substring(file,1,nchar(file) - 4)},
  "XXXXX"
)

instances <- lapply(direcs, load_instance)
cat("Pre calculating grid centroids for all instances\n")

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)
parallel::clusterExport(cl, c('grid_centroids','euclid_norm'))
invisible(parallel::clusterEvalQ(cl, library(dplyr)))

centroids <- pbapply::pblapply(
  instances, 
  function(instance) grid_centroids(instance, dimension = 7),
  cl = cl
)

parallel::stopCluster(cl)

# construct all combinations of instances and solution methods

params <- bind_rows(
  expand.grid(names(instances),c("GA"),c("ARV", "SAFE", "TOT")),
  expand.grid(names(instances),c("KM"), c("WCSS"))
) %>%
  rename(instance = Var1, Method = Var2, Obj = Var3) %>%
  tibble() %>%
  mutate(across(everything(), as.character))

params_list <- split(params, 1:nrow(params))

solve_n_save <- function(param) {
  ncent = 5
  instance <- param$instance; method <- param$Method; obj <- param$Obj
  if (method == "KM") {
    solution <- solve_kmeans(instances[[instance]], no_of_centers = ncent)
  } else if (method == "GA") {
    sink("./log.txt")
    solution <- solve_ga(
      instances[[instance]], 
      centroids[[instance]], 
      no_of_centers = ncent, 
      obj = obj
    )
    sink()
  }
  saveRDS(solution, file = paste0(
    './solutions/',instance,'_',
    method,'_',obj,'.rds'
  ))
}

cat("Generating solutions for all combinations\n")
print(Sys.time())
pbapply::pblapply(params_list, solve_n_save)

