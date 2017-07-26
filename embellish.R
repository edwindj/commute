library(magick)

athome <- image_read("fig/athome.png")
atwork <- image_read("fig/atwork.png")

image_info(athome)
image_colorize(athome, 50, "white")
frames <- image_morph(c(athome, atwork), frames = 20)

a <- seq(0, 100, length.out = 10)

library(purrr)

imgs <- 
  a %>% map(~ image_colorize(athome, ., "#FFFFFF00")) %>% 
  image_join() %>%
  image_animate(10)

image_write(imgs, "athome.gif")


image_colorize(athome, 50, adjustcolor("white", alpha.f = 0))
adjustcolor("white", alpha.f = 0)
