### The Shelter data processing from FAIMS modules

## This scripts helps import, project and combine data
## from FAIMS2.6 modules online and local instantiation
## Upon careful merge (wrong order distorts timestamp)
## we export to OR, streamlined multivalued columns and bring back for visualisation

# Constraints: 0507 dataset lacks 20 points from 10 May !

# libraries
library(sf)
library(mapview)
library(tidyverse)

### CREATE DATA FROM FAIMS SERVER

# To load shp or geojson from FAIMS
#sh23_local <- read_sf("C:/Users/adela/Documents/Professional/Projects/AarhusShelters/data/AarhusShelters20230515Localserver/Shelter.shp")
sh23_local <- read_sf("data/Shelter-local.shp")

# Data lacks projection so project to UTM32N to faciliate analysis
st_crs(sh23_local) <- 32632

# Load data from online server and process similarly 
sh23_online <- read_sf("data/Shelter-online32632.shp") # shelters0507 lacks 23 features from 8 and 10 May
#sh23_online <- read_sf("C:/Users/adela/Documents/Professional/Projects/AarhusShelters/data/AarhusShelters20230510online/Shelter32632.shp")

# Check timeline and authorship
library(lubridate)

sh23_online %>%  # 145 features
  #filter(createdBy == "Lise Søndergaard Jensen") %>% 
  mutate(day = as_date(createdAt0)) %>% 
  group_by(day, createdBy) %>% 
  tally()

sh23_local %>%   # 21 features
  #filter(createdBy == "Lise Søndergaard Jensen") %>% 
  mutate(day = as_date(createdAt0)) %>% 
  group_by(day, createdBy) %>% 
  tally()


##############
# Merge the two datasets (Careful! timestamp gets messed up with reverse order)
colnames(sh23_local)==colnames(sh23_online) 
shelters23 <- rbind(sh23_online, sh23_local)

shelters23 %>% 
  mutate(day = as_date(createdAt0)) %>% 
  group_by(day, createdBy) %>% 
  tally()

# View projected data in interactive map
mapview(shelters23, cex = "Accuracy", name = "identifier" )


# What's the GPS accuracy? Beware annotations have turned some values into 'characters'
head(shelters23$Accuracy)
summary(as.numeric(shelters23$Accuracy))

# What's the accessibility?
mapview(shelters23, zcol = "Accessibl0")

# the accessibility colum needs to be cleaned up in OpenRefine

# view the data
mapview(shelters23, cex = "Accuracy")

mapview(shelters23, zcol = "LanduseAr0")

# Export as csv

shelters23 %>% 
  st_drop_geometry() %>% 
  write_csv("data/shelters23.csv")


###################################################

### Read REFINED csv back after OR processing

# processed in OR to separate annotations from data
sh23 <- read_csv("data/SheltersCleanedOR20231117.csv")
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

### SAVE STREAMLINED 2023 DATA FOR SHELTERS


# Write data to a shapefile (FIRST OPENREFINE)
saveRDS(sh23,"output_data/shelters23.rds")
st_write(sh23, "output_data/shelters23.shp", append =F) # combined data
st_write(sh23, "output_data/shelters23.geojson") # all 166 recs

# Reload and check
shelter <- read_rds("output_data/shelters23.rds")
sort(shelter$identifier)
