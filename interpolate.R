library(sf)
library(dplyr)

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

i <- 0.5
a <- interpolate(banen,coord, s=i)

f <- colorRamp(c("#e94c0a", "#0058b8"), space = "Lab", interpolate = "spline")
my_pal <- function(i){
  rgb(f(i), maxColorValue = 255)
}

library(tmap)

unlink("img", recursive = TRUE)
dir.create("img")

png("img/p%03d.png")

my_plot <- function(a, i){
  tm_shape(a) + 
    tm_bubbles("banen", col=my_pal(i), border.lwd=0) + 
    tm_legend(legend.show=F)
}

banen_com <- 
  banen %>% filter(werk_i != woon_i)
for (i in seq(0, 1, by=0.05)){
  a <- interpolate(banen_com, coord, s=i)
  p <- my_plot(a, i)
  print(p)
}

dev.off()

library(magick)
im <- image_read(list.files("img", "*.png", full.names = T))
im <- c(im, tail(rev(im),-1))
an <- image_animate(image_join(im))
image_write(an, "test.gif")
