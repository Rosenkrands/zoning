source('2d-instance.R')

for (i in 1:3) {
  instance <- generate_2d_instance(no_of_points = 100)
  save_instance(instance)
}

