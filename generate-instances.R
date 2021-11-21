source('2d-instance.R')

for (i in c(10,20,30,40)) {
  for (j in 1:10) {
    instance <- generate_2d_instance(
      no_of_points = 100,
      arv = c("min" = 40 - i, "max" = 40 + i)
    )
    save_instance(instance)
  }
}
