source("2d-instance.R")
set.seed(1105)
instance <- generate_2d_instance(no_of_points = 100, seed = 1105)

ggplot(
  instance$data
) +
  geom_point(aes(x,y)) +
  theme_bw()

ggsave('plots_for_report/example_instance_big.pdf',width = 4, height = 4)