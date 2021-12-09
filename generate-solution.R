source('2d-instance.R')

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
  function(instance) grid_centroids(instance, dimension = 8),
  cl = cl
)

parallel::stopCluster(cl)

# construct all combinations of instances and solution methods

params <- bind_rows(
  # expand.grid(names(instances),c("GA"),c("ARV", "SAFE", "TOT"),c(5,10,15)),
  # expand.grid(names(instances),c("KM"), c("WCSS"), c(5,10,15)),
  expand.grid(names(instances),c("WKM"), c("WWCSS"), c(5,10,15))
) %>%
  rename(instance = Var1, Method = Var2, Obj = Var3, no_of_centers = Var4) %>%
  tibble() %>%
  mutate(across(-no_of_centers, as.character))

params_list <- split(params, 1:nrow(params))

solve_n_save <- function(param) {
  instance <- param$instance; method <- param$Method; 
  obj <- param$Obj; ncent <- param$no_of_centers
  
  file = paste0(
    './solution_for_simulation/',
    instance,'_',
    method,'_',
    obj,'_',
    ncent,'.rds'
  )
  
  # if (file.exists(file)) {cat("File exists, continuing...\n"); return()}
  
  if (method == "WKM") {
    solution <- solve_wkmeans(instances[[instance]], no_of_centers = ncent)
  } else if (method == "GA") {
    sink("./log.txt")
    solution <- solve_ga(
      instances[[instance]], 
      centroids[[instance]], 
      no_of_centers = ncent, 
      obj = obj,
      miter = 100000
    )
    sink()
  }
  saveRDS(solution, file = file)
}


# pbapply::pblapply(params_list, solve_n_save)

cat("Generating solutions for all combinations\n")
print(Sys.time())

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)
parallel::clusterExport(cl, c('grid_centroids',
                              'euclid_norm',
                              'solve_ga',
                              # 'centroids',
                              'instances',
                              'solve_kmeans',
                              'solve_wkmeans'))
invisible(parallel::clusterEvalQ(cl, library(dplyr)))
invisible(parallel::clusterEvalQ(cl, library(distanceFunctions)))

# pbapply::pblapply(params_list,
#                   function(x) tryCatch(solve_n_save(x),
#                                        error = function(e) print(e,'\n')),
#                   cl = cl)
pbapply::pblapply(params_list,
                  solve_n_save,
                  cl = cl)

parallel::stopCluster(cl)
