# IDEA: should save some instances to compare on so we don't get different results
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
  arrival_rate <- round(runif(no_of_points, min = 1, max = 3))/100
  data <- tibble(
    "Demand point id" = as.character(id),
    "x" = x, 
    "y" = y, 
    "Arrival rate" = arrival_rate
  ) %>%
    mutate(prob = cumsum(`Arrival rate`/sum(`Arrival rate`)))
  results <- list("data" = data, "interval" = interval, "no_of_points" = no_of_points)
  return(results)
}

# instance <- generate_2d_instance()

hexadec <- function(size = 64) {
  paste0(
    paste0(sample(c(0:9, LETTERS[1:6]), size, T), collapse = ''),
    '.rds'
  )
}

save_instance <- function(instance) {
  saveRDS(instance, file = paste0('./instances/',hexadec(size = 10)))
}

# hexadec <- 'F13DA776F1'

load_instance <- function(hexadec) {
  readRDS(paste0('./instances/', hexadec))
}

benchmark <- function() {
  files <- list.files('./instances')
  
  results <- tibble(
    "id" = character(),
    "fitness" = numeric(),
    "runtime" = numeric()
  )
  
  for (file in files) {
    instance <- load_instance(file)
    centroids <- grid_centroids(instance, dimension = 3)
    solution <- solve_ga(instance, centroids)
    
    results <- c(file, summary(solution$ga)$fitness, 0)
  }
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
    # if (sum(bitstring) > 5) return(-1)
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
      summarise(`Arrival rate variance` = sum(`Arrival rate`)) %>%
      summarise(MARV = var(`Arrival rate variance`))
    return(-result$MARV)
  }
  ga_model <- GA::ga(
    type="binary", fitness=eval_func, nBits=centroids$no_of_centroids,
    popSize=100, pmutation=0.1, maxiter=10, parallel = TRUE
  )
  return(list(
    "ga" = ga_model, 
    "centroids" = bit_to_cent(summary(ga_model)$solution) %>%
      left_join(centroids$locations, by = "Centroid id")
  ))
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

# TEST
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
