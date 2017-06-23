library(sf)

centers <- st_read("data/centers.geojson")
banen <- read.csv("data/banen.csv")

library(dplyr)

woon_tot <- 
  banen %>% 
  group_by(code = woon) %>% 
  summarise(inwoners = sum(banen))

werk_tot <- 
  banen %>% 
  group_by(code = werk) %>% 
  summarise(werknemers = sum(banen))

woon_commuters <- 
  banen %>% 
  filter(woon != werk) %>% 
  group_by(code = woon) %>% 
  summarise(commuters_woon = sum(banen))

werk_commuters <- 
  banen %>% 
  filter(woon != werk) %>% 
  group_by(code = werk) %>% 
  summarise(commuters_werk = sum(banen))

centers <- 
  centers %>% 
  left_join(woon_tot) %>% 
  left_join(werk_tot) %>% 
  left_join(woon_commuters) %>% 
  left_join(werk_commuters)


banen$woon_i <- match(banen$woon, centers$code)
banen$werk_i <- match(banen$werk, centers$code)
coord <- st_coordinates(centers)

# interpoleer tussen de woon en werk afstand (s tussen 0 en 1)
interpolate <- function(banen, coord, centers, s=0){
  woon <- banen$woon_i
  werk <- banen$werk_i
  value <- banen$banen
  pos <- (1-s)*coord[woon, ] + s * coord[werk,]
  # animeer ook de bubble in iedere plaats: corrigeert voor het feit dat bubbles over elkaar geplot worden.
  # note, dit werkt niet voor meerkleurige plots....
  in_place <- (woon == werk)
  value[in_place] = 0
  #value[in_place] = ((1-s)*(centers$commuters_woon[woon]) + s*(centers$commuters_werk[werk]))[in_place]
  st_sf(banen=value, st_cast(st_sfc(st_multipoint(pos)), "POINT")) 
}


a <- interpolate(banen,coord, centers, s=0.1)
#View(a)
library(tmap)

tm_shape(a) + 
   tm_bubbles("banen", col="red", border.lwd=0) + tm_legend(legend.show=FALSE)


png("img/p%03d.png")
snapshot <- 
  seq(0, 1, by=0.05) %>% 
  tail(-1) %>% 
  head(-1) %>% 
  c(., rev(.))

for (i in snapshot){
  a <- interpolate(banen, coord, centers, s=i)
  p <- 
    tm_shape(a) + 
    tm_bubbles("banen", col="red", border.lwd=0, alpha=0.6) + tm_legend(legend.show=FALSE)
  print(p)
}

dev.off()

library(magick)
im <- image_read(list.files("img", "*.png", full.names = T))
an <- image_animate(image_join(im))
image_write(an, "nl.gif")


