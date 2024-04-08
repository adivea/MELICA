## Test maps


# libraries
library(sf)
library(mapview)
library(tidyverse)
library(htmlwidgets)

# Load maps (from Downloads)
maps <- list.files("data/Georeferenced/")

m1935 <- raster(paste0("data/Georeferenced/", maps[2]))
mapview(m1935, alpha = 0.5, maxpixels = 5000000)

m1948 <- raster(paste0("data/Georeferenced/", maps[3]))
mapview(m1948, alpha = 0.5, maxpixels = 5000000)

m1952 <- raster(paste0("data/Georeferenced/", maps[4]))
mapview(m1952, alpha = 0.5, maxpixels = 5000000)

for(i in maps){
  map <- raster(paste0("data/Georeferenced/", i))
  m <- mapview(map)
  saveWidget(m, file="m.html")
}

for(i in maps){
  paste0("data/Georeferenced/", i)
}
