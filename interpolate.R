library(sf)

centers <- st_read("data/centers.geojson")
banen <- read.csv("data/banen.csv")

banen$woon_i <- match(banen$woon, centers$code)
banen$werk_i <- match(banen$werk, centers$code)
coord <- st_coordinates(centers)

# interpoleer tussen de woon en werk afstand (s tussen 0 en 1)
interpolate <- function(banen, coord, s=0){
  woon <- banen$woon_i
  werk <- banen$werk_i
  value <- banen$banen
  pos <- (1-s)*coord[woon, ] + s * coord[werk,]
  st_sf(banen=value, st_cast(st_sfc(st_multipoint(pos)), "POINT")) 
}

a <- interpolate(banen,coord, s=0.1)

library(tmap)

png("img/p%03d.png")

for (i in seq(0, 1, by=0.05)){
  a <- interpolate(banen, coord, s=i)
  p <- 
    tm_shape(a) + 
    tm_bubbles("banen", col="red")
  print(p)
}

dev.off()

library(magick)
im <- image_read(list.files("img", "*.png", full.names = T))
im <- c(im, tail(rev(im),-1))
an <- image_animate(image_join(im))
image_write(an, "test.gif")
