source('2d-instance.R')

library(deldir)
library(igraph)
# set.seed(2)
no_of_points = 100
instance <- generate_2d_instance(no_of_points = no_of_points)

# ggplot(instance$data) +
#   geom_voronoi(data = instance$data, aes(x,y,fill=`Demand point id`)) +
#   geom_point(aes(x,y,label=`Demand point id`)) +
#   theme_void() + 
#   theme(legend.position = 'none')

rslt <- deldir(instance$data$x, instance$data$y,
               rw=c(-10,10,-10,10))

# edges
weights <- numeric(nrow(rslt$delsgs))
for (i in 1:nrow(rslt$delsgs)) {
  weights[i] <- euclid_norm(
    c(rslt$delsgs$x1[i] - rslt$delsgs$x2[i],
      rslt$delsgs$y1[i] - rslt$delsgs$y2[i])
  )
}

original <- data.frame(from = rslt$dirsgs$ind1,
                            to = rslt$dirsgs$ind2) %>%
  mutate(edge = paste0(from,'-',to))
inverted <- allowed_edges %>%
  rename(from = to,
         to = from) %>%
  mutate(edge = paste0(from,'-',to))
allowed_edges <- bind_rows(original,inverted) %>%
  select(edge)

edges <- data.frame(from=rslt$delsgs$ind1,
                    to=rslt$delsgs$ind2) %>%
  mutate(edge = paste0(from,'-',to)) %>%
  inner_join(allowed_edges, by = "edge") %>%
  select(from, to)

g <- graph_from_data_frame(edges, directed=FALSE, vertices=instance$data)
E(g)$weight <- weights

plot(g)

plot_vor <- function(zones) {
  ggplot(instance$data) +
    geom_voronoi(data = instance$data %>% left_join(zones, by = "Demand point id"), aes(x,y,fill=`Zone id`)) +
    geom_text(aes(x,y,label=`Demand point id`)) +
    theme_void() + 
    theme(legend.position = 'none')
}

merge_approach <- function(animate = T) {
  # select a number of nodes
  num_zones = 6
  
  initial <- sample(1:no_of_points, num_zones)
  
  zones <- tibble("Demand point id" = as.character(initial)) %>%
    mutate("Zone id" = as.character(row_number()))
  
  if (animate == T) print(plot_vor(zones))
  # loop through zones and add a point that is a neighbor to a point in the zone if possible
  
  find_neighbors <- function(point) {
    # cat(point, '\n')
    tibble('Demand point id' = neighbors(g, point) %>% as.numeric())
  }
  
  while (nrow(zones) < nrow(instance$data)) {
    for (i in 1:num_zones) {
      p_in_zone <- zones %>%
        filter(`Zone id` == i)
      
      # print(p_in_zone)
      arg_list <- split(p_in_zone$`Demand point id`, 1:nrow(p_in_zone))
      res_list <- lapply(arg_list, find_neighbors)
      
      nbors <- do.call(bind_rows, res_list) %>%
        mutate(`Demand point id` = as.character(`Demand point id`)) %>%
        # But also anti_join with points in other zones, maybe just zones instead of p_in_zone
        anti_join(zones, by = ("Demand point id")) %>%
        distinct()
      
      if (nrow(nbors) == 0) next
      
      # select point to add
      # new_point <- nbors[sample(1:nrow(nbors), 1),] %>%
      #   mutate("Zone id" = as.character(i))
      
      new_point <- nbors[1,] %>%
        mutate("Zone id" = as.character(i))
      
      zones <- bind_rows(zones, new_point)
      if (animate == T) print(plot_vor(zones))
    }
  }
  if (animate == T) for (i in 1:20) print(plot_vor(zones))
  print(plot_vor(zones))
}

# animation::saveGIF(
#   merge_approach(animate = T),
#   interval = 0.1,
#   outdir = getwd(),
#   ani.width = 480,
#   ani.height = 480
# )

merge_approach(animate = F)
