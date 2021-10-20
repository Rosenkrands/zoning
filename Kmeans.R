library(tidyverse)
library(ggvoronoi)

source('2d-instance.R')

coordinates <- instance$data %>% select(x, y)
centroids <- kmeans(x = coordinates, centers = 5, nstart = 20)
clusters <- as.data.frame(centroids$centers)
#clusters$distance <- sqrt((clusters$x-100)^2 + (clusters$y-100)^2)

clustering_vector <- centroids$cluster

# It is possible to give k-means initial clusters from ga

dat <- rbind(coordinates, clusters)
#dat$distance <- sqrt((dat$x-100)^2 + (dat$y-100)^2)

ggplot() +
    geom_point(data = coordinates, aes(x,y)) +
    geom_point(data = clusters, aes(x, y), color = 'red', shape = 10, size = 5) +
    #geom_voronoi(data = clusters, aes(x, y, fill=distance)) +
    stat_voronoi(data = clusters, aes(x, y), geom="path", outline = data.frame(
        x = 1.1*c(-10,-10,10,10),
        y = 1.1*c(-10,10,10,-10)
    )) +
    theme_void()

# Comparison with GA method
centroids$tot.withinss
#sum(as.matrix(dist(df)^2)) / (2 * nrow(df))

 








