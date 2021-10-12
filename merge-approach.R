source('2d-instance.R')

library(deldir)
library(igraph)

# plotting function to use for showcasing solution
plot_vor <- function(zones) {
  ggplot(instance$data) +
    geom_voronoi(data = instance$data %>% 
                   left_join(zones, by = "Demand point id"), 
                 aes(x,y,fill=`Zone id`)) +
    geom_text(aes(x,y,label=`Demand point id`)) +
    theme_void() + 
    theme(legend.position = 'none')
}

voronoi_merge <- function(instance, num_zones, animate = F) {
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
  E(g)$weight <- weights
  
  # randomly assign the first tile to each zone
  initial <- sample(1:instance$no_of_points, num_zones)
  zones <- tibble("Demand point id" = as.character(initial)) %>%
    mutate("Zone id" = as.character(row_number()))
  
  if (animate == T) print(plot_vor(zones))
  
  find_neighbors <- function(point) {
    tibble('Demand point id' = neighbors(g, point) %>% as.numeric())
  }
  
  # I think this is inherently a sequential process so it cannot be,
  # parallelised as subsequent operations are dependent on each other
  while (nrow(zones) < nrow(instance$data)) {
    for (i in 1:num_zones) {
      p_in_zone <- zones %>%
        filter(`Zone id` == i)
      
      arg_list <- split(p_in_zone$`Demand point id`, 1:nrow(p_in_zone))
      res_list <- lapply(arg_list, find_neighbors)
      
      nbors <- do.call(bind_rows, res_list) %>%
        mutate(`Demand point id` = as.character(`Demand point id`)) %>%
        anti_join(zones, by = ("Demand point id")) %>%
        distinct()
      
      if (nrow(nbors) == 0) next
      
      # select point to add
      new_point <- nbors[sample(1:nrow(nbors), 1),] %>%
        mutate("Zone id" = as.character(i))
      
      zones <- bind_rows(zones, new_point)
      if (animate == T) print(plot_vor(zones))
    }
  }
  if (animate == T) for (i in 1:20) print(plot_vor(zones))
  
  return(list("zones" = zones))
}

instance <- generate_2d_instance(no_of_points = 100)
rslt <- voronoi_merge(instance, num_zones = 5)
plot_vor(zones = rslt$zones)

# animation::saveGIF(
#   voronoi_merge(instance, num_zones = 5, animate = T),
#   interval = 0.1,
#   outdir = getwd(),
#   ani.width = 480,
#   ani.height = 480
# )
