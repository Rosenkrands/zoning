#MCLP based on https://github.com/njtierney/maxcovr

library(maxcovr)
library(tidyverse)
library(leaflet)

coverage(york_selected, york_crime)

cl <- as_tibble(centroids$locations %>% select(x, y))
loc <- as_tibble(instance$data %>% select(x,y))

cl <- rename(cl, lat = x, long = y)
loc <- rename(loc, lat = x, long = y)
start <- tibble(lat = numeric(), long = numeric())

mc_5 <- max_coverage( existing_facility = start,
                      proposed_facility = cl,
                      user = loc,
                      n_added = 5,
                      distance_cutoff = 200000)

