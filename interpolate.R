library(sf)
library(dplyr)

centers <- st_read("data/centers.geojson")
CRS <- st_crs(centers)
bbox <- tmaptools::bb(centers)

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
interpolate <- function(banen, coord, crs=st_crs(centers), s=0){
  woon <- banen$woon_i
  werk <- banen$werk_i
  value <- banen$banen
  pos <- (1-s)*coord[woon, ] + s * coord[werk,]
  # animeer ook de bubble in iedere plaats: corrigeert voor het feit dat bubbles over elkaar geplot worden.
  # note, dit werkt niet voor meerkleurige plots....
  in_place <- (woon == werk)
  value[in_place] = 0
  #value[in_place] = ((1-s)*(centers$commuters_woon[woon]) + s*(centers$commuters_werk[werk]))[in_place]
  st_sf(banen=value, st_cast(st_sfc(st_multipoint(pos)), "POINT"), crs = crs) 
}

i <- 0.5
a <- interpolate(banen,coord, CRS, s=i)

f <- colorRamp(c("#0058b8","#e94c0a"), space = "Lab", interpolate = "spline")
my_pal <- function(i){
  rgb(f(i), maxColorValue = 255)
}

#st_bbox(a)

color <- my_pal(0.1)
#View(a)
library(tmap)

tm_shape(a, bbox=bbox) + 
   tm_bubbles("banen", col=color, border.lwd=0) +
   tm_legend(legend.show=FALSE) +  
   tm_layout(frame = FALSE) + 
   tm_credits("CBS, 2017", align = "right")

unlink("img", recursive = TRUE)
dir.create("img")

png("img/p%03d.png",height = 600, width=480)

snapshot <- 
  seq(0, 1, by=0.05) %>% 
  tail(-1) %>% 
  head(-1) %>% 
  c(., rev(.))

my_plot <- function(a, i){
  tm_shape(a,bbox = bbox) + 
    tm_bubbles("banen", col=my_pal(i), border.lwd=0, alpha=0.6) + 
    tm_legend(legend.show=FALSE) +  
    tm_layout(frame = FALSE) + 
    tm_credits("CBS, 2017", align = "right")
}

my_plot2 <- function(a, col){
  tm_shape(a, bbox=bbox) + 
    tm_bubbles("banen", col=col, border.lwd=0, alpha=0.8) + 
    tm_legend(legend.show=F)
}

for (i in snapshot){
  a <- interpolate(banen, coord, CRS, s=i)
  p <- my_plot(a, i)
  print(p)
}


# banen_com <- 
#   banen %>% filter(werk_i != woon_i)
# 
# for (i in seq(0, 1, by=0.05)){
#   a <- interpolate(banen_com, coord, s=i)
#   p <- my_plot(a, i)
#   print(p)
# }

dev.off()

library(magick)
im <- image_read(list.files("img", "p\\d{3}\\.png", full.names = T))
im

library(purrr)
athome <- image_read("fig/athome.png")
atwork <- image_read("fig/atwork.png")

embellish <- function(img, s){
  #s <- (s - 0.5) + 0.5
  athome <- image_colorize(athome, 100*(s), "white")
  atwork <- image_colorize(atwork, 100*(1-s), "white")
  image_flatten(c(img, athome, atwork))
}
  
an <-
  map2( seq_along(im)
      , snapshot
      , ~ embellish(im[.x], .y)
      ) %>% 
  image_join() %>% 
  image_animate()

image_write(an, "nl.gif")


# per region
# i = 0.5
# region <- "GM0363"
# 
# ams_from <- 
#   banen_com %>% filter(woon == region)
# 
# ams_to <- 
#   banen_com %>% filter(werk == region)

# 
# png("img/ams%03d.png")
# for (i in seq(0, 1, by=0.05)){
#   from <- interpolate(ams_from, coord, s=i)
#   p <- my_plot2(from, my_pal(0))
# 
#   to  <- interpolate(ams_to, coord, s=i)
#   p <- p + my_plot2(to, my_pal(1))
#   print(p)
# }
# dev.off()
# 
# 
