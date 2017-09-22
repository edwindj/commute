library(dplyr)
library(sf)
library(tmap)

wijk <- st_read("data/wk_2015.shp")  %>% 
  dplyr::select(code = STATCODE, name = WK_NAAM, gm_code = GM_2015)

od_wijk <- read.csv("data/wk_home_work.csv", stringsAsFactors = F) %>% 
  mutate( wk_home = sprintf("WK%06d", wk_home)
        , wk_work = sprintf("WK%06d", wk_work)
        ) %>% 
  filter(wk_home %in% wijk$code, wk_work %in% wijk$code)


wk_banen <- 
  od_wijk %>% 
  group_by(code = wk_work) %>% 
  summarise(n = sum(count)) %>% 
  left_join(wijk, .)

library(raster)
st_extent <- function(x){
  e <- st_bbox(x)
  e <- matrix(e, ncol=2, byrow=TRUE)
  raster::extent(e)
}

st_extent(wijk)

r <- raster(st_extent(wijk), ncol=15, nrow=15)

cell_id <- 
  wijk %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  cellFromXY(r, .) %>% 
  setNames(wijk$code)

w <- 
  od_wijk %>% 
  mutate( home = cell_id[wk_home]
        , work = cell_id[wk_work]
        ) %>% 
  #filter(work != home) %>% 
  group_by(home, work) %>% 
  summarise(n = sum(count))


s <- split(w, w$work)
l <- lapply(names(s), function(cell){
  r_cell <- raster(r)
  names(r_cell) <- paste0("cell_", cell)
  s_cell <- s[[cell]]
  r_cell[s_cell$home] <- s_cell$n
  r_cell
})

b <- do.call(stack, l)
dir.create("od", recursive = TRUE)
writeRaster(b,  filename = "od/od.grd", overwrite=T)

nc <- ncol(b)
nr <- nrow(b)
b2 <- raster(extent(b), ncol = nc*nc, nrow=nr*nr)

cell_id <- 
  names(b) %>% 
  sub("cell_", "", .) %>%
  as.integer()
names(cell_id) <- names(b)

rc <- rowColFromCell(b, cell_id) %>% 
  t()
rc <- t(1 + (rc - 1) * c(nr,nc)) 
rownames(rc) <- names(b)

for (n in names(cell_id)){
  v <- b[[n]]
  ri <- rc[n,1]
  ci <- rc[n,2]
  ri <- seq(ri, length.out = nr)
  ci <- seq(ci, length.out = nc)
  b2[ri, ci] <- v 
}


png("od_wk.png")
par(mai=c(0,0,0,0))
plot(log(b2), legend=FALSE, axes=FALSE, box=F, col=viridis::viridis(10))
dev.off()
