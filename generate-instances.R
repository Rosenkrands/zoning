source('2d-instance.R')

for (i in 11:20) {
  for (j in c(10,25,40)) {
    instance <- generate_2d_instance(
      seed = i,
      no_of_points = 100,
      arv = c("min" = 40-j, "max" = 40+j)
    )
    saveRDS(instance, file = paste0('./instances/',i,'_',hexadec(size = 5)))
  }
}
