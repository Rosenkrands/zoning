library(tidyverse)

result <- readRDS('./solution-results.rds')

## Shows that we can lower WCSS with higher dimensional grid,
# and we can even lower the TOT objective. Implying that the
# parameter chosen for grid dimension is not valid for high
# number of UAVs

hd_instance <- result %>% 
  filter(grid_dimension > 8) %>%
  select(instance) %>%
  unique()

result %>%
  filter(instance %in% hd_instance$instance,
         `Solution method` %in% c("GA:TOT", "KM:WCSS"),
         Objective %in% c("TOT", "WCSS"),
         `Number of UAVs` %in% c("high")) %>%
  ggplot(aes(x = `Arrival rate variance`, 
             y = `Objective value`,
             color = factor(paste0(`Solution method`,`Grid dimension`),
                            levels = c("KM:WCSS8", "GA:TOT15", "GA:TOT8")))) +
  geom_boxplot() +
  facet_wrap(~Objective, scales = "free", nrow = 4) +
  theme_bw()
