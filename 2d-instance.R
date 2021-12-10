# IDEA: should save some instances to compare on so we don't get different results
# everytime

library(tidyverse)
library(ggvoronoi)

euclid_norm <- function(x) sqrt(sum(x^2))

generate_2d_instance <- function(
  seed = NULL,
  no_of_points = 50, 
  interval = c("min" = -10, "max" = 10),
  arv = c("min" = 0, "max" = 80)
) {
  id <- 1:no_of_points
  set.seed(seed)
  x <- runif(no_of_points, min = interval["min"], max = interval["max"])
  y <- runif(no_of_points, min = interval["min"], max = interval["max"])
  set.seed(NULL)
  arrival_rate <- 1/round(runif(no_of_points, min = arv["min"]*60, max = arv["max"]*60))
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

# instance <- generate_2d_instance()

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

solve_ga <- function(instance, centroids, no_of_centers = 5, obj = c("ARV", "TOT", "SAFE"), miter = 100) {
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

solve_wkmeans <- function(instance, no_of_centers = 5) {
  coordinates <- instance$data %>% select(x, y)
  
  centroids <- flexclust::stepFlexclust(x = coordinates, k = no_of_centers, nrep = 100000,
                           weights = instance$data$`Arrival rate`, FUN = "cclust",
                           dist = "euclidean", verbose = F,
                           method = "hardcl", control = list(iter.max = 100))
  # 
  clusters <- as.data.frame(centroids@centers)
  clustering_vector <- centroids@cluster
  
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

plot_network <- function(instance, solution, size = T) {
  centroids <- solution$instance %>% 
    select(`Centroid id`, x.centroid, y.centroid) %>%
    distinct()
  
  if (size == T) {
    ggplot(solution$instance) +
      geom_segment(aes(x = x, y = y, xend = x.centroid, yend = y.centroid),
                   color = "gray") +
      geom_point(aes(x,y, color = `Centroid id`, size = `Arrival rate`)) +
      geom_point(
        data = centroids, 
        aes(x.centroid, y.centroid, color=`Centroid id`), shape = 10, size = 5
      ) +
      theme_void()  
  } else {
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
}

# # # TEST
# instance = generate_2d_instance(
#   no_of_points = 100,
#   interval = c("min" = -10, "max" = 10)
# )
# 
# # plot_2d(instance, centroids, solution, type = "point")
# 
# centroids = grid_centroids(instance, dimension = 5)
# 
# # plot_2d(instance, centroids, solution, type = "centroid")
# 
# solution_ga <- solve_ga(instance, centroids, no_of_centers = 5, obj = "SAFE")
# solution_km <- solve_wkmeans(instance, no_of_centers = 5)

# plot_network(instance, solution = solution_km)

# 
# plot_2d(instance, centroids, solution, type = "chosen")
# plot_2d(instance, centroids, solution, type = "group")
# plot_2d(instance, centroids, solution, type = "voronoi")
