###################################################
## This script helps you load and visualize refined merged field  data on shelters
## around Aarhus
## Input is a csv from OpenRefine and 00_MergeFAIMSdata.rmd
## Outputs are shapefiles and geojson of spatialized shelter csv


library(sf)
library(mapview)
library(tidyverse)


### Read OPEN-REFINED csv back after merging and processing

# processed in OR to separate annotations from data
sh23 <- read_csv("data/SheltersCleanedOR20231117.csv")

dim(sh23)
sort(sh23$FeatureID)
colnames(sh23)

sh23 <- sh23 %>% 
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

#############################################

### SAVE STREAMLINED 2023 SPATIAL DATA FOR SHELTERS
# the data that already exists is a csv from open refine

# Write data to a shapefile
saveRDS(sh23,"output_data/shelters23.rds")
st_write(sh23, "output_data/shelters23.shp", append =F) # combined data
st_write(sh23, "output_data/shelters23.geojson") # all 166 recs

# Reload and check
shelter <- read_rds("output_data/shelters23.rds")
sort(shelter$identifier)
