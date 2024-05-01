###################################################
## around Aarhus, stored in GD archive https://drive.google.com/drive/folders/1dDFcHf4Mqa2wMQLSgh_sBkLTWGcnjsBW

## Input is a csv from OpenRefine (the archive above with 166 features) and 00_MergeFAIMSdata.rmd
## Outputs are shapefiles and geojson of spatialized shelter csv


library(sf)
library(mapview)
library(tidyverse)


### Read OPEN-REFINED csv back after merging and processing

# Grab OR data where occlusive annotations have been separated from data
sh23 <- read_csv("data/SheltersCleanedOR20231117.csv")  # 166 records

dim(sh23)
sort(sh23$FeatureID)
colnames(sh23)

# Spatialize the shelters
sh23 <- sh23 %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Visualise attributes in space
mapview(sh23, zcol = "Accessible")
mapview(sh23, zcol = "LanduseOnTop")
mapview(sh23, zcol = "FeatureType")

# Select specific values of landuse 
sh23 %>% 
  filter(LanduseOnTop == "Playground") %>% 
  mapview()
unique(sh23$FeatureType)

# Strip problems
library(tmap)
sh_typesclean <- sh23 %>% 
  filter(FeatureType != "Other") %>% 
  filter(FeatureType != "NA") 
  

# Visualize features by type and landuse
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


####################### FIELDWORK SUMMARY
library(lubridate)
date(sh23$createdAt0)
sh23 %>% 
  mutate(day = date(createdAt0)) %>% 
  group_by(day, createdBy) %>% 
  tally() %>% 
 # mutate(day2 = as.factor(day)) %>%
  ggplot(aes(x = day, y = n, fill = createdBy)) +
  geom_bar(stat =  "identity", position = position_dodge2(width = 3, preserve = "single")) + 
  theme_bw()

#############################################

### SAVE STREAMLINED 2023 SPATIAL DATA FOR SHELTERS
# the data that already exists is a csv from open refine

# Write data to a shapefile
dir.create("output_data2")
saveRDS(sh23,"output_data2/shelters23.rds")
st_write(sh23, "output_data2/shelters23.shp", append =F) # combined data
st_write(sh23, "output_data2/shelters23.geojson", append = F) # all 166 records

