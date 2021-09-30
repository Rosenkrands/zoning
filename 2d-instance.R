# IDEA: should save some instances to compare on so we dont get different results
# everytime

library(tidyverse)
library(ggvoronoi)

euclid_norm <- function(x) sqrt(sum(x^2))

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
  
  # Find the cartesian product from the possibilities
  locations <- tibble(expand.grid(x,y)) %>% 
    rename(x = Var1, y = Var2) %>%
    mutate(`Centroid id` = as.character(row_number()))
  
  # Function to calculate distance given a point id and centroid id
  cent_dist <- function(id=c("p_id"=-1,"c_id"-1)){
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
  result_list <- lapply(arg_list,cent_dist)
  distances <- do.call(rbind,result_list)
  return(list("locations" = locations, "distances" = distances, "no_of_centroids" = dimension^2))
}

solve_ga <- function(instance, centroids) {
  bit_to_cent <- function(bitstring) {
    tibble::tibble(
      `Centroid id` = as.character(1:length(bitstring) * bitstring)
    ) %>% filter(`Centroid id` != 0)
  }
  
  eval_func <- function(bitstring) {
    centroids_used <- bit_to_cent(bitstring)
    
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
      summarise(`Arrival rate variance` = var(`Arrival rate`)) %>%
      summarise(MARV = mean(`Arrival rate variance`))
    return(-result$MARV)
  }
  ga_model <- GA::ga(
    type="binary", fitness=eval_func, nBits=centroids$no_of_centroids,
    popSize=100, pmutation=0.1, maxiter=100, parallel = TRUE
  )
  return(list(
    "ga" = ga_model, 
    "centroids" = bit_to_cent(summary(ga_model)$solution)
  ))
}

plot_2d <- function(instance, centroids, solution, type) {
  point_plot <- ggplot(instance$data) +
    geom_point(aes(x,y)) +
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
          data = centroids$locations %>% 
            inner_join(solution$centroids, by = "Centroid id"), 
          aes(x, y), shape = 10, size = 5
        )
    )
  }
  
  assignment <- centroids$distances %>%
    inner_join(solution$centroids, by = "Centroid id") %>%
    group_by(`Demand point id`) %>%
    filter(Distance == min(Distance)) %>%
    ungroup()
  
  instance_w_assignment <- instance$data %>%
    inner_join(assignment, by = "Demand point id")
  
  if (type == "group") {
    return(
      ggplot(
        instance_w_assignment
      ) +
        geom_point(aes(x,y,color=`Centroid id`)) +
        geom_point(
          data = centroids$locations %>% 
            inner_join(solution$centroids, by = "Centroid id"), 
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
        geom_voronoi(
          data = centroids$locations %>% 
            inner_join(solution$centroids, by = "Centroid id"),
          aes(x, y, fill = `Centroid id`),
          alpha = .25,
          # geom="path",
          outline = data.frame(
            x = 1.1*c(-10,-10,10,10),
            y = 1.1*c(-10,10,10,-10)
          )
        ) +
        stat_voronoi(
          data = centroids$locations %>% 
            inner_join(solution$centroids, by = "Centroid id"),
          aes(x, y),
          geom="path",
          outline = data.frame(
            x = 1.1*c(-10,-10,10,10),
            y = 1.1*c(-10,10,10,-10)
          )
        ) +
        geom_point(
          data = centroids$locations %>% 
            inner_join(solution$centroids, by = "Centroid id"), 
          aes(x, y, color=`Centroid id`), shape = 10, size = 5
        ) +
        theme_void()
    )
  }
}

# # TEST
# instance = generate_2d_instance(
#   no_of_points = 100,
#   interval = c("min" = -10, "max" = 10)
# )
# 
# plot_2d(instance, centroids, solution, type = "point")
# 
# centroids = grid_centroids(instance, dimension = 5)
# 
# plot_2d(instance, centroids, solution, type = "centroid")
# 
# solution <- solve_ga(instance, centroids)
# 
# plot_2d(instance, centroids, solution, type = "chosen")
# plot_2d(instance, centroids, solution, type = "group")
# plot_2d(instance, centroids, solution, type = "voronoi")
