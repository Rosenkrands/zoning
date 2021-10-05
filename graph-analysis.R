source('2d-instance.R')

library(deldir)
library(igraph)

instance <- generate_2d_instance(no_of_points = 100)
plot_2d(instance, NULL, NULL, type="point")
rslt <- deldir(instance$data$x, instance$data$y)

# edges
weights <- numeric(nrow(rslt$delsgs))
for (i in 1:nrow(rslt$delsgs)) {
  weights[i] <- euclid_norm(
    c(rslt$delsgs$x1[i] - rslt$delsgs$x2[i],
      rslt$delsgs$y1[i] - rslt$delsgs$y2[i])
  )
}
edges <- data.frame(from=rslt$delsgs$ind1,
                    to=rslt$delsgs$ind2)

g <- graph_from_data_frame(edges, directed=FALSE, vertices=instance$data)
E(g)$weight <- weights

# min max feature scaling to range [0,1]
normalize <- function(x) (x - min(x))/(max(x) - min(x))

par(mfrow=c(1,3), mar=c(.1,.1,2,.1))

b <- betweenness(g)
plot(g, vertex.color = normalize(b)*50,
     vertex.size = normalize(b)*30,
     palette = viridis::viridis(50))
title("Betweenness")

c <- closeness(g)
plot(g, vertex.color = normalize(c)*50,
     vertex.size = normalize(c)*30,
     palette = viridis::viridis(50))
title("Closeness")

a <- alpha_centrality(g)
plot(g, vertex.color = normalize(a)*50,
     vertex.size = normalize(a)*40,
     palette = viridis::viridis(50))
title("Alpha")
