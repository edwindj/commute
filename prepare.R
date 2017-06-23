library(sf)
library(dplyr)

dir.create("data", recursive = TRUE, showWarnings = F)
# gemeentekaart

download.file("https://cartomap.github.io/nl/rd/gemeente_2014.geojson", "data/gemeente_2014.geojson")

gemeente <- st_read("data/gemeente_2014.geojson") %>% 
  select(code = 2, name = 3)

st_write(gemeente, "data/gemeente.geojson",delete_dsn = TRUE)
centers <- st_centroid(gemeente)
centers <-cbind(centers, st_coordinates(centers))
st_write(centers, "data/centers.geojson", delete_dsn=TRUE)


# data 
library(cbsodataR)
library(stringr)

meta <- get_meta("81251ned")
dat <- get_data("81251ned", Perioden = "2014MM12", recode=FALSE)

d <- 
  dat %>% 
  select( woon    = WoonregioS
        , werk    = WerkregioS
        , banen   = BanenVanWerknemers_1
        , afstand = AfstandTussenWoonEnWerkgemeente_2
        ) %>%
  mutate( banen = 1e3 * as.numeric(banen)
        , afstand = as.numeric(afstand)
        ) %>% 
  filter(str_detect(woon, "^GM"),
         str_detect(werk, "^GM"),
         banen > 0
        ) %>% 
  glimpse

write.csv(d, file = "data/banen.csv", row.names=FALSE, na="")
