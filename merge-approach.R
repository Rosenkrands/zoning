source('2d-instance.R')

library(deldir)
library(igraph)

# plotting function to use for showcasing solution
plot_vor <- function(zones) {
  ggplot(instance$data %>% 
           left_join(zones, by = "Demand point id")) +
    geom_voronoi(aes(x,y,fill=`Zone id`),
                 alpha = .25) +
    geom_point(aes(x,y,color = `Zone id`)) +
    theme_void() + 
    theme(legend.position = 'none')
}

voronoi_merge <- function(
  instance, 
  num_zones, 
  method = c("random", "ARV", "distance"),
  animate = F, 
  verbose = T
) {
  # construct the tesselation and triangulation
  vor <- deldir(instance$data$x, instance$data$y)
  
  # TODO: We have to many weights as we find them before filtering the edges!
  # edge weights are calculated using the euclidean norm
  weights <- numeric(nrow(vor$delsgs))
  for (i in 1:nrow(vor$delsgs)) {
    weights[i] <- euclid_norm(
      c(vor$delsgs$x1[i] - vor$delsgs$x2[i],
        vor$delsgs$y1[i] - vor$delsgs$y2[i])
    )
  }
  
  # remove some delaunay edges from the boundary points, so we only have the
  # edges where there are voronoi line segments
  
  # first we find all the allowed edges, here there are more than in the
  # Delaunay triangulation
  original <- data.frame(from = vor$dirsgs$ind1,
                         to = vor$dirsgs$ind2) %>%
    mutate(edge = paste0(from,'-',to))
  inverted <- original %>%
    rename(from = to,
           to = from) %>%
    mutate(edge = paste0(from,'-',to))
  allowed_edges <- bind_rows(original,inverted) %>%
    select(edge)
  
  # Then we inner join the allowed edges with the delaunay edges to get the
  # intersection of the two sets
  edges <- data.frame(from=vor$delsgs$ind1,
                      to=vor$delsgs$ind2) %>%
    mutate(edge = paste0(from,'-',to)) %>%
    inner_join(allowed_edges, by = "edge") %>%
    select(from, to)
  
  # constructing the igraph object from the edges, vertices and weights
  g <- graph_from_data_frame(edges, directed=FALSE, vertices=instance$data)
  # E(g)$weight <- weights
  
  #' TODO: Find groups of points that are to close to not be in the same zone
  #' these should be grouped before hand in a lookup table, that needs to be checked,
  #' everytime a point is added to a zone. If we add a point that are part of a 
  #' group we need to add all members of the group. This could be incorporated in
  #' the calculation of the objective function.
  
  # FIND POINTS THAT ARE CLOSE
  dist_calc = function(points) {
    euclid_norm(
      c(
        instance$data$x[points[1]] - instance$data$x[points[2]],
        instance$data$y[points[1]] - instance$data$y[points[2]]
      )
    )
  }
  
  too_close <- edges %>%
    rowwise() %>%
    mutate(distance = dist_calc(c(from, to))) %>%
    filter(distance < .5) %>%
    select(-distance) %>%
    pivot_longer(cols = c(from, to)) %>%
    select(`Demand point id` = value) %>%
    distinct() %>%
    mutate(`Demand point id` = as.character(`Demand point id`)) %>%
    left_join(instance$data %>% select(`Demand point id`, x, y), by = "Demand point id")
  
  expand.grid(point1 = too_close$`Demand point id`, point2 = too_close$`Demand point id`) %>%
    tibble() %>% mutate(point1 = as.integer(point1), point2 = as.integer(point2)) %>%
    filter(point1 != point2) %>%
    rowwise() %>%
    mutate(distance = dist_calc(c(point1, point2))) %>%
    filter(distance < .5)
  
  ggplot(instance$data) +
    # geom_point(aes(x,y)) +
    geom_text(aes(x,y,label=`Demand point id`)) +
    geom_text(data = too_close, aes(x,y,label=`Demand point id`), color = "red") +
    theme_void()
  
  # randomly assign the first tile to each zone
  initial <- sample(1:instance$no_of_points, num_zones)
  zones <- tibble("Demand point id" = as.character(initial)) %>%
    mutate("Zone id" = as.character(row_number()))
  
  if (animate) print(plot_vor(zones))
  
  find_neighbors <- function(point) {
    tibble('Demand point id' = neighbors(g, point) %>% as.numeric())
  }
  
  # I think this is inherently a sequential process so it cannot be,
  # parallelised as subsequent operations are dependent on each other.
  while (nrow(zones) < nrow(instance$data)) {
    for (i in 1:num_zones) {
      
      if (verbose) {
        cat('Demand points remaining:',
            nrow(instance$data) - nrow(zones), 
            '\r')
      }
      
      p_in_zone <- zones %>%
        filter(`Zone id` == i)
      
      arg_list <- split(p_in_zone$`Demand point id`, 1:nrow(p_in_zone))
      res_list <- lapply(arg_list, find_neighbors)
      
      nbors <- do.call(bind_rows, res_list) %>%
        mutate(`Demand point id` = as.character(`Demand point id`)) %>%
        anti_join(zones, by = ("Demand point id")) %>%
        distinct()
      
      if (nrow(nbors) == 0) next
      
      # select point to add randomly
      if (method == "random") {
        new_point <- nbors[sample(1:nrow(nbors), 1),] %>%
          mutate("Zone id" = as.character(i))
      }
      
      # select point that minimizes the arrival rate variance
      if (method == "ARV") {
        nbors <- nbors %>% left_join(
          instance$data %>% select(`Demand point id`, `Arrival rate`),
          by = "Demand point id"
        )
        
        # calculate the consequence for the ARV for each neighbor
        arv_calc <- lapply(
          split(nbors, 1:nrow(nbors)),
          function (x) {
            result <- zones %>% 
              filter(`Zone id` == i) %>%
              left_join(
                instance$data %>% select(`Demand point id`, `Arrival rate`),
                by = "Demand point id"
              ) %>%
              bind_rows(x) %>%
              summarise(ARV = var(`Arrival rate`))
            return(
              tibble(`Demand point id` = x$`Demand point id`, ARV = result$ARV)
            )
          }
        )
        
        # select the point with the lowest arrival rate variance
        new_point <- do.call(bind_rows, arv_calc) %>%
          filter(ARV == min(ARV)) %>%
          filter(row_number() == 1) %>%
          mutate(`Zone id` = as.character(i)) %>%
          select(-ARV)  
      }
      
      # select point that is closest to a point already in the zone
      if (method == "distance") {
        nbors <- nbors %>% left_join(
          instance$data %>% select(`Demand point id`, x, y),
          by = "Demand point id"
        )
        
        dist_calc <- lapply(
          split(nbors, 1:nrow(nbors)),
          function (arg) {
            result <- zones %>% 
              filter(`Zone id` == i) %>%
              left_join(
                instance$data %>% select(`Demand point id`, x, y),
                by = "Demand point id"
              ) %>%
              rowwise() %>%
              mutate(distance = euclid_norm(c(x - arg$x, y - arg$y))) %>%
              ungroup() %>%
              filter(distance == min(distance)) %>%
              filter(row_number() == 1)
            return(
              tibble(`Demand point id` = arg$`Demand point id`, 
                     distance = result$distance)
            )
          }
        )
        
        new_point <- do.call(bind_rows, dist_calc) %>%
          filter(distance == min(distance)) %>%
          filter(row_number() == 1) %>%
          mutate(`Zone id` = as.character(i)) %>%
          select(-distance)
      }
      
      zones <- bind_rows(zones, new_point)
      if (animate == T) print(plot_vor(zones))
    }
  }
  if (verbose) cat('Demand points remaining: 0\n')
  if (animate) for (i in 1:20) print(plot_vor(zones))
  
  return(list("zones" = zones))
}

instance <- generate_2d_instance(no_of_points = 100)
# rslt <- voronoi_merge(instance, num_zones = 5, method = "distance")
# plot_vor(zones = rslt$zones)

animation::saveGIF(
  voronoi_merge(instance, num_zones = 5, method = "distance", animate = T),
  interval = 0.1,
  outdir = getwd(),
  ani.width = 480,
  ani.height = 480
)
