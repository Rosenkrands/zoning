#' The idea is to see how increasing the dimension of the grid will improve
#' the TOT objective for the GA approach as compared to KMeans.
#' We will look at the mean across the 10 instances.

source('2d-instance.R')

# Here we set the dimensions for the GA to use
dimensions <- seq(3,10,1)

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
parallel::clusterExport(cl, c('grid_centroids','euclid_norm','dimensions'))
invisible(parallel::clusterEvalQ(cl, library(dplyr)))

centroids <- pbapply::pblapply(
  instances, 
  function(instance) {
    cent_list <- lapply(as.list(dimensions), function(dimension) {
        grid_centroids(instance, dimension = dimension)
      }
    )
    names(cent_list) <- dimensions
    return(cent_list)
  },
  cl = cl
)

parallel::stopCluster(cl)

# construct all combinations of instances and solution methods

params <- bind_rows(
  expand.grid(names(instances),c("GA"),c("TOT"), dimensions,c("Dynamic2")),
  # expand.grid(names(instances),c("KM"), c("WCSS"), c(NA), c(NA))
) %>%
  rename(instance = Var1, Method = Var2, Obj = Var3, dimension = Var4, miter = Var5) %>%
  tibble() %>%
  mutate(across(everything(), as.character)) %>%
  arrange(desc(dimension))

params_list <- split(params, 1:nrow(params))

dynamic_iter <- function(dimension) {
  if (dimension == 3) {
    return(90)
  } else {
    dynamic_iter(dimension - 1) + 10*(2*(dimension) - 1)
  }
}

solve_n_save <- function(param) {
  ncent = 5
  instance <- param$instance; method <- param$Method; obj <- param$Obj
  dimension <- as.character(param$dimension); 
  
  # dynamic iterations
  miter <- dynamic_iter(as.numeric(dimension))
  # cat('\n','Dimension is', dimension, 'Dynamic iterations is', miter, '\n')
  
  sol_id <- paste0(sample(c(0:9, LETTERS[1:6]), 5, T), collapse = '')
  
  file = paste0(
    './dimension_tuning/',instance,'_',
    method,'_',obj,'_',dimension,'_',"Dynamic2",'_',sol_id,'.rds'
  )
  
  if (file.exists(file)) {cat("File exists, continuing...\n"); return()}
  
  if (method == "KM") {
    solution <- solve_kmeans(instances[[instance]], no_of_centers = ncent)
  } else if (method == "GA") {
    cat(instance, dimension, miter, '\n')
    sink("./log.txt")
    solution <- solve_ga(
      instances[[instance]], 
      centroids[[instance]][[dimension]], 
      no_of_centers = ncent, 
      obj = obj,
      miter = miter*2
    )
    sink()
  }
  saveRDS(solution, file = file)
}

cat("Generating solutions for all combinations\n")
print(Sys.time())
pbapply::pblapply(params_list, solve_n_save)
