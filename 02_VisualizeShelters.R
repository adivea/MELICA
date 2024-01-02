###################################################
## This script helps you load and visualize refined merged field data on shelters
## around Aarhus created in November 2023

## Input is a RDS file generated out of open-refined data
## Outputs are various visualisation


# Libraries
library(sf)
library(mapview)
library(tidyverse)
library(tmap)

# Load and check there are 166
shelter <- read_rds("output_data/shelters23.rds")
sort(shelter$identifier)

sh_typesclean <- shelter %>% 
  filter(FeatureType != "Other") %>% 
  filter(FeatureType != "NA") 


# Visualize features by type and landuse
tmap_options(limits = c(facets.view = 5))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(sh_typesclean)+
  tm_facets(by = "FeatureType",
            ncol = 3)+
  tm_bubbles(col = "LanduseOnTop")

# Visualise attributes in space
mapview(shelter, zcol = "Accessible")
mapview(shelter, zcol = "LanduseOnTop")
mapview(shelter, zcol = "FeatureType")

# Make interactive map for Rosanna
