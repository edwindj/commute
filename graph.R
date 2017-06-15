dat <- read.csv("data/banen.csv")

library(igraph)
g <- igraph::graph_from_data_frame(dat)
g
plot(g)

V(g)$name
