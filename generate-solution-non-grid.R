# IDEA: should save some instances to compare on so we don't get different results
# everytime

library(maxcovr)
library(tidyverse)
library(ggvoronoi)
library(geosphere)

# euclid_norm <- function(x) sqrt(sum(x^2))

york1 <- select(york, -object_id)
york1 <- unique(york1)
# york1 <- sample_n(york1, 1000)

generate_2d_instance <- function(
  seed = NULL,
  no_of_points = length(york1$long), 
  interval = c("min" = -10, "max" = 10),
  arv = c("min" = .4,
          "max" = .6)
) {
  id <- 1:no_of_points
  set.seed(seed)
  x <- runif(no_of_points, min = interval["min"], max = interval["max"])
  y <- runif(no_of_points, min = interval["min"], max = interval["max"])
  set.seed(NULL)
  arrival_rate <- stats::runif(no_of_points, min = arv["min"]/60, max = arv["max"]/60)
  data <- tibble(
    "Demand point id" = as.character(id),
    "x" = x, 
    "y" = y, 
    "Arrival rate" = arrival_rate
  ) %>%
    mutate(prob = cumsum(`Arrival rate`/sum(`Arrival rate`)))
  results <- list("data" = data, "interval" = interval, 
                  "no_of_points" = no_of_points, "arv" = arv)
  return(results)
}

instance <- generate_2d_instance()

hexadec <- function(size = 64) {
  paste0(
    paste0(sample(c(0:9, LETTERS[1:6]), size, T), collapse = ''),
    '.rds'
  )
}

save_instance <- function(instance) {
  saveRDS(instance, file = paste0('./instances/',hexadec(size = 5)))
}

# hexadec <- 'F13DA776F1'

load_instance <- function(hexadec) {
  readRDS(paste0('./instances/', hexadec))
}

### ADDED
instance$data <- instance$data %>% mutate(x = york1$long, y = york1$lat)
# instance$no_of_points <- length(instance$data$x)

# centroids$distances <- data.frame(`Demand point id` = york1$desig_id[101:110], x = york1$long[101:110], y = york1$lat[101:110])
# centroids$no_of_centroids = 10
# coord <- york1 %>% select(`Demand point id` = desig_id, x = long, y = lat) 
# sample_id <- sample(coord$`Demand point id`, centroids$no_of_centroids)
# centroids$locations$x <- coord %>% unique(filter(coord, `Demand point id` == sample_id))

grid_centroids <- function(
  instance,
  dimension = 3
) {
  # Define the possible x and y possibilities
  x = tail(head(seq(
    instance$interval['min'],instance$interval['max'],
    length.out=dimension+2
  ),-1),-1)
  y = tail(head(seq(
    instance$interval['min'],instance$interval['max'],
    length.out=dimension+2
  ),-1),-1)
  # x = york_selected$long
  # y = york_selected$lat
  
  # Find the cartesian product from the possibilities
  locations <- tibble(expand.grid(x,y)) %>%
    rename(x = Var1, y = Var2) %>%
    mutate(`Centroid id` = as.character(row_number()))
  
  # Function to calculate distance given a point id and centroid id
  cent_dist <- function(id=c("p_id"=-1,"c_id"-1)){
    # It calculates dist to points not centroids
    # distance <- distm(x = c(pull(instance$data[id[1,"p_id"], "x"]),pull(instance$data[id[1,"p_id"], "y"])), y = c(pull(york_selected[id[1,"c_id"], "long"]),pull(york_selected[id[1,"c_id"], "lat"])), fun = distHaversine)/1000
      distance <- euclid_norm(
      c(
        pull(instance$data[id[1,"p_id"], "x"] - locations[id[1,"c_id"], "x"]),
        pull(instance$data[id[1,"p_id"], "y"] - locations[id[1,"c_id"], "y"])
      )
    )
    return(
      tibble(
        `Demand point id` = id[1,"p_id"],
        `Centroid id` = id[1,"c_id"],
        `Distance` = distance
      )
    )
  }
  
  # Find all possible combinations of demand points and centroids
  arg_df <- expand.grid(
    instance$data$`Demand point id`,
    locations$`Centroid id`
  ) %>%
    rename(p_id = Var1, c_id = Var2) %>%
    mutate(p_id = as.character(p_id), c_id = as.character(c_id))
  # Convert dataframe to list
  arg_list <- split(arg_df,1:nrow(arg_df))
  
  # TODO: Parallelize this if it becomes slow
  # numCores <- detectCores()
  result_list <- lapply(arg_list,cent_dist)
  distances <- do.call(rbind,result_list)
  return(list("locations" = locations, "distances" = distances, "no_of_centroids" = length(york_selected$long)))
}

non_grid_centroids <- function(instance){
  york_selected <- york %>% filter(grade == "I")
  york_selected$`Centroid id` <- as.character(seq(1, length(york_selected$long)))
  distance <- matrix(ncol = length(instance$data$`Demand point id`), nrow = length(york_selected$`Centroid id`))
   # vector(length = length(instance$data$`Demand point id`)*length(york_selected$`Centroid id`))
    for (p_id in 1:length(instance$data$`Demand point id`)){
      for (c_id in 1:length(york_selected$`Centroid id`)){
        distance[c_id,p_id] <- distm(x = c(instance$data$x[p_id], instance$data$y[p_id]), y = c(york_selected$long[c_id], york_selected$lat[c_id]), fun = distHaversine)/1000
      }
    }
  distance_tibble <- distance |> as_tibble()
  names(distance_tibble) <- as.character(1:ncol(distance_tibble))
    return(list(
      "distances" = distance_tibble |>
        mutate(`Centroid id` = as.character(row_number())) %>%
        tidyr::pivot_longer(cols = -`Centroid id`,
                            names_to = "Demand point id",
                            values_to = "Distance")
    ,
    "locations" = york_selected %>% select(x = long, y = lat, `Centroid id`),
    "no_of_centroids" = length(york_selected$long))
    )
}

solve_ga <- function(instance, centroids, no_of_centers = 10, obj = c("ARV", "TOT", "SAFE"), miter = 100000) {
  bit_to_cent <- function(bitstring) {
    tibble::tibble(
      `Centroid id` = as.character(1:length(bitstring) * bitstring)
    ) %>% filter(`Centroid id` != 0)
  }
  
  eval_func <- if (obj == "ARV") {
    function(bitstring) {
      if (sum(bitstring) < 2) {return(-Inf)}
      centroids_used <- bit_to_cent(bitstring)
      
      # len <- sum(bitstring) - min(no_of_centers, sum(bitstring))
      len <- abs(no_of_centers - sum(bitstring))  
      result <- centroids$distances %>%
        filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
        group_by(`Demand point id`) %>%
        filter(Distance == min(Distance)) %>%
        ungroup() %>%
        inner_join(
          select(instance$data, `Demand point id`, `Arrival rate`),
          by = "Demand point id"
        ) %>%
        group_by(`Centroid id`) %>%
        summarise(`Arrival rate variance` = sum(`Arrival rate`)) %>%
        summarise(MARV = var(`Arrival rate variance`) + len*1000)
      return(-result$MARV)
    }
  } else if (obj == "TOT") {
    function(bitstring) {
      if (sum(bitstring) < 2) {return(-Inf)}
      centroids_used <- bit_to_cent(bitstring)
      
      # Assume constant speed so time and distance are interchangable
      # Fixed service time per delivery
      tau <- 0
      # Punishment for unwanted centroids
      len <- abs(no_of_centers - sum(bitstring))  
      # Computation of objective value
      result <- centroids$distances %>%
        filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
        group_by(`Demand point id`) %>%
        filter(Distance == min(Distance)) %>%
        ungroup() %>%
        inner_join(
          select(instance$data, `Demand point id`, `Arrival rate`),
          by = "Demand point id"
        ) %>%
        group_by(`Centroid id`) %>%
        summarise(`Operation time` = sum(`Arrival rate` * (Distance + tau))) %>%
        summarise(TOT = sum(`Operation time`) + len*100)
      return(-result$TOT)
    }
  } else if (obj == "SAFE") {
    function(bitstring) {
      if (sum(bitstring) < 2) {return(-Inf)}
      len <- abs(no_of_centers - sum(bitstring))  
      
      centroids_used <- bit_to_cent(bitstring)
      points <- instance$data %>% 
        select(`Demand point id`, x, y)
      # Computation of objective value
      result <- centroids$distances %>%
        filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
        group_by(`Demand point id`) %>%
        filter(Distance == min(Distance)) %>%
        ungroup() %>%
        inner_join(
          select(points, `Demand point id`, x, y),
          by = "Demand point id"
        ) %>%
        group_by(`Centroid id`)
      return(distanceFunctions::distC(result %>% data.matrix) - len*1000)
    }
  }
  ga_model <- GA::ga(
    type="binary", fitness=eval_func, nBits=centroids$no_of_centroids,
    popSize=100, pmutation=0.1, maxiter=miter, parallel = F, run = 500
  )
  
  centroids_used <- bit_to_cent(summary(ga_model)$solution[1,])
  assignment <- centroids$distances %>%
    filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
    group_by(`Demand point id`) %>%
    filter(Distance == min(Distance))
  
  instance <- instance$data %>%
    left_join(assignment %>% select(-Distance), by = "Demand point id") %>%
    left_join(centroids$locations, by = "Centroid id", suffix = c("",".centroid"))
  return(list(
    "ga" = ga_model, 
    "centroids" = centroids_used %>%
      left_join(centroids$locations, by = "Centroid id"),
    "instance" = instance
  ))
}

solve_kmeans <- function(instance, no_of_centers = 5) {
  coordinates <- instance$data %>% select(x, y)
  centroids <- kmeans(x = coordinates, centers = no_of_centers, nstart = 1000)
  clusters <- as.data.frame(centroids$centers)
  clustering_vector <- centroids$cluster
  
  instance <- instance$data %>% 
    mutate("Centroid id" = clustering_vector %>% as.character()) %>%
    left_join(
      tibble(clusters) %>% mutate(`Centroid id` = row_number() %>% as.character()), 
      by =c("Centroid id"), suffix = c("",".centroid")
    )
  
  return(list("clusters" = clusters,
              "clustering_vector" = clustering_vector, 
              "no_of_centers" = no_of_centers,
              "instance" = instance))
}

plot_2d <- function(instance, centroids, solution, type) {
  if ("Centroid id" %in% colnames(instance$data)) {
    instance$data <- instance$data %>% select(-`Centroid id`)
  }
  
  point_plot <- ggplot(instance$data) +
    geom_point(aes(x,y)) +
    # geom_text(aes(x,y,label=`Demand point id`)) +
    theme_void()
  
  if (type == "point") {
    return(
      point_plot
    )
  }
  
  if (type == "centroid") {
    return(
      point_plot +
        geom_point(
          data = centroids$locations, aes(x, y), shape = 10, size = 5
        )
    )
  }
  
  if (type == "chosen") {
    return(
      point_plot +
        geom_point(
          data = solution$centroids, 
          aes(x, y), shape = 10, size = 5
        )
    )
  }
  
  assignment <- centroids$distances %>%
    inner_join(solution$centroids, by = "Centroid id") %>%
    group_by(`Demand point id`) %>%
    filter(Distance == min(Distance)) %>%
    ungroup() %>%
    select(-x, -y)
  
  instance_w_assignment <- instance$data %>%
    inner_join(assignment, by = "Demand point id")
  
  if (type == "group") {
    return(
      ggplot(
        instance_w_assignment
      ) +
        geom_point(aes(x,y,color=`Centroid id`)) +
        geom_point(
          data = solution$centroids, 
          aes(x, y, color=`Centroid id`), shape = 10, size = 5
        ) +
        theme_void()
    )
  }
  
  if (type == "voronoi") {
    return(
      ggplot(
        instance$data %>%
          inner_join(assignment, by = "Demand point id")
      ) +
        geom_point(aes(x,y,color=`Centroid id`)) +
        geom_point(
          data = solution$centroids, 
          aes(x, y, color=`Centroid id`), shape = 10, size = 5
        ) +
        geom_voronoi(
          data = solution$centroids,
          aes(x, y, fill = `Centroid id`),
          alpha = .25,
          # geom="path",
          outline = data.frame(
            x = 1.1*c(-10,-10,10,10),
            y = 1.1*c(-10,10,10,-10)
          )
        ) +
        stat_voronoi(
          data = solution$centroids,
          aes(x, y),
          geom="path",
          outline = data.frame(
            x = 1.1*c(-10,-10,10,10),
            y = 1.1*c(-10,10,10,-10)
          )
        ) +
        theme_void()
    )
  }
  
  if (type == "voronoi_boundary") {
    return(
      ggplot(
        instance$data %>%
          inner_join(assignment, by = "Demand point id")
      ) +
        geom_voronoi(data = instance$data %>% 
                       left_join(assignment, by = "Demand point id"), 
                     aes(x,y,fill=`Centroid id`),
                     alpha = .25,
                     outline = data.frame(
                       x = 1.1*c(-10,-10,10,10),
                       y = 1.1*c(-10,10,10,-10)
                     )) +
        # stat_voronoi(
        #   data = instance$data %>% 
        #     left_join(assignment, by = "Demand point id"), 
        #   aes(x,y),
        #   geom="path",
        #   outline = data.frame(
        #     x = 1.1*c(-10,-10,10,10),
        #     y = 1.1*c(-10,10,10,-10)
        #   )
        # ) +
        geom_point(
          data = solution$centroids, 
          aes(x, y, color=`Centroid id`), shape = 10, size = 5
        ) +
        geom_point(aes(x,y,color=`Centroid id`)) +
        theme_void()
    )
  }
}

plot_network <- function(instance, solution) {
  centroids <- solution$instance %>% 
    select(`Centroid id`, x.centroid, y.centroid) %>%
    distinct()
  
  ggplot(solution$instance) +
    geom_segment(aes(x = x, y = y, xend = x.centroid, yend = y.centroid),
                 color = "gray") +
    geom_point(aes(x,y, color = `Centroid id`)) +
    geom_point(
      data = centroids, 
      aes(x.centroid, y.centroid, color=`Centroid id`), shape = 10, size = 5
    ) +
    theme_void() 
}



# # # TEST
# instance = generate_2d_instance(
#   no_of_points = 100,
#   interval = c("min" = -10, "max" = 10)
# )
# 
# plot_2d(instance, centroids, solution, type = "point")
# 
# centroids = grid_centroids(instance, dimension = 5)
# 
# # plot_2d(instance, centroids, solution, type = "centroid")

#Modify york_selected to look like grid_centroids(instance)
# york_selected$`Centroid id` <- seq(1,length(york_selected$long))
# york_selected$x <- york_selected$long
# york_selected$y <- york_selected$lat

# cents <- list()
# cents$locations <- york_selected %>% select(x, y, `Centroid id`)
# cents$no_of_centroids <- length(york_selected$long)
# cents$distances <- cent_dist(instance)
centroids_data <- non_grid_centroids(instance)

solution_ga <- solve_ga(instance = instance, centroids = centroids_data, obj = "TOT")
# solution_km <- solve_kmeans(instance, no_of_centers = 5)
saveRDS(solution_ga, file = "GA_highUAV_lowARV.rds")

solution_ga_hh <- readRDS("GA_lowUAV_highARV.rds")
solution_wkm_hh <- readRDS("WKM_lowUAV_highARV.rds")

# plot_2d(instance, centroids, solution_ga, type = "chosen")
plot_network(instance, solution_ga_hh)
plot_network(instance, solution_wkm_hh)

ggplot() +
  geom_bar(data = solution_ga_hh$instance, aes(x = length(x), fill = `Centroid id`)) +
  coord_polar("y", start=0) +
  theme_void()

ggplot() +
  geom_bar(data = solution_wkm_hh$instance, aes(x = length(x), fill = `Centroid id`)) +
  coord_polar("y", start=0) +
  theme_void()



points <- instance$data %>% 
  select(`Demand point id`, x, y)
# Select used centroids
possible_centroids <- non_grid_centroids(instance)
used_centriods <- possible_centroids$distances %>% filter(`Centroid id` == solution_ga_hh$centroids$`Centroid id`)
# Computation of objective value
centroids_used <- solution_ga_hh$centroids
result <- used_centriods %>%
  filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
  group_by(`Demand point id`) %>%
  filter(Distance == min(Distance)) %>%
  ungroup() %>%
  inner_join(
    select(points, `Demand point id`, x, y),
    by = "Demand point id"
  ) %>%
  group_by(`Centroid id`)
min(result$Distance)
mean(sort(result$Distance)[1:100])

### (Expected) Operation time for each UAV
tau <- 0
# Punishment for unwanted centroids
len <- 5
# Computation of objective value
result <- used_centriods %>%
  filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
  group_by(`Demand point id`) %>%
  filter(Distance == min(Distance)) %>%
  ungroup() %>%
  inner_join(
    select(instance$data, `Demand point id`, `Arrival rate`),
    by = "Demand point id"
  ) %>%
  group_by(`Centroid id`) %>%
  summarise(`Operation time` = sum(`Arrival rate` * (Distance + tau)))

ggplot(data = result, aes(x = "", y = `Operation time`, fill = `Centroid id`)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# solution_ga$instance %>% 
#   dplyr::filter(!is.na(`Centroid id`)) %>% 
#   ggplot() +
#   geom_point(aes(x,y, color = `Centroid id`))
# 
# unique(solution_ga$instance$`Centroid id`)
# plot_2d(instance, centroids, solution, type = "group")
# plot_2d(instance, centroids, solution, type = "voronoi")


# solution_wkm <- zav::solve_wkmeans(type = "swkm", instance = instance, no_of_centers = 10)
# plot_network(instance, solution_wkm)
# saveRDS(solution_wkm, file = "WKM_highUAV_highARV.rds")

