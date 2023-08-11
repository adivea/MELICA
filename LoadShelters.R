### The Shelter data processing from FAIMS modules

## This scripts helps import, project and combine data
## from FAIMS2.6 modules online and local instatiations
## It then loads OR streamlined data (line 72) and visualizes it

# libraries
library(sf)
library(mapview)
library(tidyverse)

### CREATE DATA FROM FAIMS SERVER

# To load shp or geojson from FAIMS
sh23 <- read_sf("C:/Users/adela/Documents/Professional/Projects/AarhusShelters/data/AarhusShelters20230515Localserver/Shelter.shp")

# Data lacks projection so project to UTM32N to faciliate analysis
st_crs(sh23) <- 32632

# Load data from online server and process similarly 
sh23_online <- read_sf("data/shelters0507.geojson")

# Merge the two datasets
colnames(sh23)==colnames(sh23_online)
sh23 <- rbind(sh23, sh23_online)

# View projected data in interactive map
mapview(sh23, cex = "Accuracy", name = "identifier" )

# How many total records?
nrow(sh23)

# What's the GPS accuracy? Beware annotations have turned some values into 'characters'
head(sh23$Accuracy)
summary(as.numeric(sh23$Accuracy))

# What's the accessibility?
mapview(sh23, zcol = "Accessibl0")

# the accessibility colum needs to be cleaned up in OpenRefine

# Write data to a shapefile
st_write(sh23, "data/shelters0530.shp") # combined data
st_write(sh23, "data/shelters0530.geojson") # all 143 recs
saveRDS(sh23,"data/shelters.rds")


### LOAD STREAMLINED 2023 DATA FOR SHELTERS

# read it from local data/ folder
shelter <- read_rds("data/shelters.rds")

# convert it to shapefile if you need it in other software, but beware, names get truncated
#?st_write()
# st_write(shelter, "../data/shelters.shp")


# view the data
mapview(shelter, cex = "Accuracy")

mapview(shelter, zcol = "LanduseAr0")
View(shelter)


# Export as csv

shelter %>% 
  st_drop_geometry() %>% 
  write_csv("data/shelters.csv")

###################################################

### Read REFINED csv back after OR processing

shelters <- read_csv("data/sheltersS23.csv")
head(shelters)
colnames(shelters)

sh23 <- shelters %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

mapview(sh23, zcol = "Accessible")
mapview(sh23, zcol = "LanduseOnTop")
mapview(sh23, zcol = "FeatureType")


unique(sh23$FeatureType)
library(tmap)

sh_typesclean <- sh23 %>% 
  filter(FeatureType != "Other") %>% 
  filter(FeatureType != "NA") 
  
  
tmap_options(limits = c(facets.view = 5))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(sh_typesclean)+
  tm_facets(by = "FeatureType",
            ncol = 3)+
  tm_bubbles(col = "LanduseOnTop")

# Rosanna wants SheltersInteractive html 
# (sometime before 10 Aug)

# accessibility
# landuse
# types

sh23 %>% 
  filter(Accessible == "Open") %>% 
  mapview()
