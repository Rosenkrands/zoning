library(tidyverse)
library(ggvoronoi)

generate_2d_instance <- function(
    no_of_points = 50, 
    interval = c("min" = -10, "max" = 10)
) {
    id <- 1:no_of_points
    x <- runif(no_of_points, min = interval["min"], max = interval["max"])
    y <- runif(no_of_points, min = interval["min"], max = interval["max"])
    arrival_rate <- round(runif(no_of_points, min = 1, max = 3))
    data <- tibble(
        "Demand point id" = as.character(id),
        "x" = x, 
        "y" = y, 
        "Arrival rate" = arrival_rate
    )
    results <- list("data" = data, "interval" = interval)
    return(results)
}

instance = generate_2d_instance(
    no_of_points = 100,
    interval = c("min" = -10, "max" = 10)
)

coordinates <- instance$data %>% select(x, y)
centroids <- kmeans(x = coordinates, centers = 5, nstart = 20)
clusters <- as.data.frame(centroids$centers)
#clusters$distance <- sqrt((clusters$x-100)^2 + (clusters$y-100)^2)

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
sum(as.matrix(dist(df)^2)) / (2 * nrow(df))

 








