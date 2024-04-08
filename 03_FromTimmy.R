### Shelters from Tommy Cassoe

library(tidyverse)
library(sf)
library(mapview)
library(rgdal)

## load from KML

s_i <- read_sf("data/BDfromTimmy.kml", "Ikke verificerede Betondækningsgrave") # this only gets one non'verified table
s_v <- read_sf("data/BDfromTimmy.kml", "Verificerede Betondækningsgrave") # this only gets one non'verified table

# or following https://gis.stackexchange.com/questions/58131/reading-kml-file-into-r

#3 readOGR() # I need rgdal which is a tall order

# Check extent and add info
plot(s_i$geometry)
plot(s_v$geometry)
s_i$verified <- "N"
s_v$verified <- "Y"

# combine two layers into one differentiating their verified status
s <- rbind(s_i, s_v)

# extract coordinates and cast as simple feature
s$long <- st_coordinates(s)[,1]
s$lat <- st_coordinates(s)[,2]
s$z <- st_coordinates(s)[,3]
s <- s %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) 

mapview(s, zcol = "verified")  

# join with attribute data CONTINUE
s$Name
