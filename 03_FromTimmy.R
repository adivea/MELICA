### Shelters from Tommy Cassoe

library(tidyverse)
library(sf)
library(mapview)
#library(rgdal)

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


####################################################### 2023 DATA
# compare with MELICA 2023 data
s23 <- read_sf("output_data/shelters23.geojson")
aar <- readRDS("data/gadm36_DNK_2_sp.rds")
aar <- aar %>% 
  st_as_sf() %>% 
  #slice(31)
  dplyr::filter(NAME_2 == "Århus")

# 2023 covered area
ch <- st_as_sf(st_convex_hull(st_union(s23)))


mapview(ch)+
mapview(s, zcol = "verified")  + mapview(s23) +mapview(aar)


#################################################### WORK FOR 2024
s23buff <- st_buffer(s23, 50)
st_is_valid(s23buff)

# intersecting
s %>% 
  #st_filter(s23buff, .join = intersects) %>% 
  st_filter(s23buff, .predicate = st_intersects) %>%   # 93 overlap with 30m buffer, 101 with 50m buffer
  mapview()

# non intersecting
tovisit24 <- s[st_intersects(s, s23buff) %>% lengths == 0,] # 107 non-overlapping features with 30m buffer, 
# 99 in 50m buffer, so ca 30 inside the city

mapview(tovisit24, zcol = "verified") + mapview(s23)

### remaining area for 2024 outside of the city
outoftowm <- st_difference(s, ch)  # 78

# view
mapview(outoftowm, zcol = "verified")  + mapview(s23) +mapview(aar)

### remaining inside the town for 2024 doublecheck
intowm <- st_intersection(tovisit24, ch)  # 26
# view
mapview(intowm, zcol = "verified") + mapview(aar)

###################################################### ATTRIBUTES
# join with attribute data CONTINUE
s$Name[151] <- 9999
tail(s)

# chat gpt advice on defensive function to get the first number out:
extract_first_number <- function(column_value) {
  number <- as.integer(str_extract(column_value, "\\b\\d+\\b"))
  if (!is.na(number)) {
    return(number)
  } else {
    return(NA)
  }
}

# Create a new column
s <- s %>% 
  mutate(ID = gsub("\\D", ", ", Name)) %>% 
  mutate(first_number = map_dbl(ID, extract_first_number))

# Load attribute data  
library(googlesheets4)
s_data <- read_sheet("https://docs.google.com/spreadsheets/d/1hwzvaYz9HX5-r5ZXklhD7eVcFt1RHXyQknXTAzCpxso/edit#gid=0")
types <- read_sheet("")