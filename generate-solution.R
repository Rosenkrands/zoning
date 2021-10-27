source("2d-instance.R")

files <- list.files('./instances')

for (file in files) {
  instance <- load_instance(file)
  
  # solve_kmeans
  solution <- solve_kmeans(instance, no_of_centers = 5)
  saveRDS(solution, file = paste0(
    './solutions/',substring(file,1,5),'_',
    'KM','_','WCSS','.rds'
  ))
  
  # solve_ga
  centroids <- grid_centroids(instance, dimension = 5)

  for (obj in c("ARV", "TOT", "SAFE")) {
    solution <- solve_ga(instance, centroids, no_of_centers = 5, obj = obj)
    saveRDS(solution, file = paste0(
      './solutions/',substring(file,1,5),'_',
      'GA','_',obj,'.rds'
    ))
  }
}