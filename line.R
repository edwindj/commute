library(sf)

centers <- st_read("data/centers.geojson")
banen <- read.csv("data/banen.csv")

banen$woon_i <- match(banen$woon, centers$code)
banen$werk_i <- match(banen$werk, centers$code)
coord <- st_coordinates(centers)

# interpoleer tussen de woon en werk afstand (s tussen 0 en 1)
create_lines <- function(banen, coord, s=0){
  woon <- banen$woon_i
  werk <- banen$werk_i
  value <- banen$banen
  sfc <- lapply(1:nrow(banen),function(i){
    st_linestring(coord[c(woon[i], werk[i]),])
  }) %>% st_sfc
  st_sf(banen=value, sfc) 
}

a <- create_lines(banen, coord, s=0.1)

library(tmap)

plot(a, col=adjustcolor("red", alpha.f = 0.2), lwd=log(a$banen)/log(max(a$banen)))
