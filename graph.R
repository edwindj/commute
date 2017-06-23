dat <- read.csv("data/banen.csv")

library(dplyr)
centers <- sf::st_read("data/centers.geojson", stringsAsFactors = FALSE) %>% 
  rename(label = name)
library(igraph)

g_w <- igraph::graph_from_data_frame(dat)


commuters <- 
  dat %>% 
  filter(werk != woon)

g_commuters <- igraph::graph_from_data_frame(commuters)

idx <- match(V(g_commuters)$name, centers$code)

V(g_commuters)$label <- centers$label[idx]

l <- cbind(x=centers$X, y=centers$Y)[idx,]

#plot(g_commuters)

E(g_commuters)$weight <- E(g_commuters)$banen


c_commuters <- cluster_label_prop(g_commuters)

com <- communities(c_commuters)
res <- lapply(names(com), function(n){
  code <- com[[n]]
  group <- rep(n, length(code))
  data.frame(group = group, code=code, stringsAsFactors = F)
}) %>% bind_rows()

V(g_commuters)$cluster <- as.integer(res$group[match(V(g_commuters)$name, res$code)])


pal <- RColorBrewer::brewer.pal(12, "Set3")
pal <- c(pal, pal)
V(g_commuters)$color <- pal[as.integer(V(g_commuters)$cluster)]
V(g_commuters)$color

V(g_commuters)$X <- 1
igraph.options(verbose=TRUE)
igraph.options(vertex.label.cex=0.4)

plot(g_commuters, layout=l)
