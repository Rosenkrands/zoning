source("2d-instance.R")

files <- list.files('./instances')

# Modified to troubleshoot issues with specific instance

i = 0
for (file in files) {
# for (file in c("FCB35.rds")) {
  i = i + 1
  print(Sys.time())
  cat(i,"\n")
  
  instance <- load_instance(file)
  
  # solve_kmeans
  # solution <- solve_kmeans(instance, no_of_centers = 5)
  # saveRDS(solution, file = paste0(
  #   './solutions/',substring(file,1,5),'_',
  #   'KM','_','WCSS','.rds'
  # ))
  
  # solve_ga
  centroids <- grid_centroids(instance, dimension = 7)

  # for (obj in c("ARV", "TOT", "SAFE")) {
  for (obj in c("SAFE")) {
    solution <- solve_ga(instance, centroids, no_of_centers = 5, obj = obj)
    saveRDS(solution, file = paste0(
      './solutions/',substring(file,1,5),'_',
      'GA','_',obj,'.rds'
    ))
  }
}
