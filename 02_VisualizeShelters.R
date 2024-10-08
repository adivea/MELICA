###################################################
## This script helps you load and visualize refined merged field data on shelters
## around Aarhus created in November 2023 and combine it with TOmmy's contributed
## somewhat verified (either via GE or 1987 archive he has) shelters from 2024 

## Input is a RDS file generated out of open-refined data
## Outputs are various visualisation


# Libraries
library(sf)  # generic spatial data library
library(mapview) # interactive maps on the pipeline
library(tidyverse)
library(tmap)  # publication quality or facetted maps

# Load Tommy's kml data 
s <- st_read("output_data/kmlTommy.geojson")
mapview(s, zcol = "verified")


# There should be 166 shelters from FAIMS, verified in 2023
shelter <- read_rds("output_data/shelters23.rds")
shelter <- read_sf("output_data/shelters23.geojson")
names(shelter)
sort(shelter$identifier)

sh_typesclean <- shelter %>% 
  filter(FeatureType != "Other") %>% 
  filter(FeatureType != "NA") 

## Shelter ID 252 currently listed as Type V, is Type 4, as 
# type 5 the swan neck distance is shorter at 40cms
shelter %>% 
  filter(FeatureID == 252) %>% 
  select(FeatureID, FeatureType, `FeatureTy0 2`, DistanceSwanNeck, DistanceSwanNeckNote)


## Tommy: Type 6 is significance as entrances will be expanded
shelter %>% 
  filter(FeatureType == "Shelter Type VI") %>% 
  mapview()


## Andreas: how to look up FeatureIDs, BDnr in s$Name

shelter %>% 
  filter(FeatureID == 158 |FeatureID == 159) %>% mapview()
s %>% 
  filter(Name == 469) %>%  # look up BdNr 469 in TOmmy's data
  mapview(cex = 10) +  # make 469 discernible through size
  mapview(shelter)   # plot all FAIMS data so you can compare IDs


################################################ TMAP FACETS

# Visualize features by type and landuse
tmap_options(limits = c(facets.view = 5))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(sh_typesclean %>% filter(FeatureType %in% c("Shelter Type III","Shelter Type IV", "Shelter Type VI")))+
  tm_facets(by = "FeatureType",
            ncol = 3)+
  tm_bubbles(col = "LanduseOnTop")+
  tm_shape(s)+
  tm_dots()

# See BdNr 496 and 470

s %>% 
  filter(grepl("400", Name)) %>% 
  #filter(Name == 401) %>% 
  #glimpse()
  mapview(cex = 15) + mapview(shelter)

  

mapview(s)
# Visualise attributes in space

mapview(shelter, zcol = "Accessible")
mapview(shelter, zcol = "LanduseOnTop")
mapview(shelter, zcol = "FeatureType")

######################################### Make interactive map for Rosanna

# Packages
library(leaflet)
library(htmltools) 

glimpse(shelter)  

# map backgrounds
l_dk <- leaflet() %>%   # assign the base location to an object
  setView(10.2089, 56.141084,zoom = 12)

esri <- grep("^Esri", providers, value = TRUE)

for (provider in esri) {
  l_dk <- l_dk %>% addProviderTiles(provider, group = provider)
}
l_dk


dkmap <- l_dk %>%
  addLayersControl(baseGroups = names(esri),
                   options = layersControlOptions(collapsed = T)) %>%
  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
             position = "bottomright") %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>% 
  htmlwidgets::onRender("
                        function(el, x) {
                        var myMap = this;
                        myMap.on('baselayerchange',
                        function (e) {
                        myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                        })
                        }") %>% 
  addControl("", position = "topright")

dkmap

dkmap %>% addCircleMarkers(data = shelter,
                           popup = paste0("ID:", shelter$FeatureID,
                                          " ", shelter$FeatureType, 
                                        '<br>', shelter$`FeatureTy0 2`,
                                        '<br>', shelter$LanduseOnTop,
                                        '<br>', shelter$Accessible))


asheltermap <- leaflet() %>%   # assign the base location to an object
  setView(10.2089, 56.151084,zoom = 13) %>% 
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>% 
  addProviderTiles("Esri.WorldStreetMap", group = "OSM") %>% 
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>% 
  addCircleMarkers(data = shelter, group = "Shelters 2023",
                   radius = 5, fillOpacity = 0.75, weight=3, fillColor = "yellow",
                   popup = paste0("ID:", shelter$FeatureID,
                                  " <br>", shelter$FeatureType)) %>% 
  addLayersControl(
    baseGroups = c("Topo","ESRI Aerial", "OSM"),
    overlayGroups = c("Shelters 2023"),
    options = layersControlOptions(collapsed = T))

sheltermap
# Save map as a html document (optional, replacement of pushing the export button)
# only works in root
library(htmlwidgets) # from htmltools

saveWidget(sheltermap, "shelter23map.html", selfcontained = TRUE)
