### The Shelter data processing from FAIMS modules

## This scripts helps import, project and combine data
## from FAIMS2.6 modules online and local instatiations


# libraries
library(sf)
library(mapview)

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

shelter <- readRDS("...")

?st_write()
mapview(shelter)
st_write(shelter, "../data/shelters.shp")
