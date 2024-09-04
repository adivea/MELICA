## Visualizing Accessibility of shelters with matrix MapboxAPI

library(leaflet)
library(mapboxapi)
library(tidyverse)
library(sf)
library(mapview)
library(raster)

# Data

#A_sat <- brick("C:/Users/Adela/Documents/RStudio/1_Teaching/cds-spatial-2024/data/Aarhus_1m.TIF")
A_sat <- brick("C:/Users/au616760/OneDrive - Aarhus universitet/Documents/RStudio/1_Teaching/cds-spatial/data/Aarhus_1m.TIF")

A_sm <- aggregate(A_sat, fact = 10) 
A_sm4326 <- projectRaster(A_sm, crs = 4326,  fun = "ngb")

crs(A_sat)
plotRGB(A_sm)
# Aarhus sat img
# Aarhus bounding boxes
DKmun <- readRDS("C:/Users/Adela/Documents/RStudio/1_Teaching/cds-spatial-2024/data/gadm36_DNK_2_sp.rds")
DKmun <- readRDS("C:/Users/au616760/OneDrive - Aarhus universitet/Documents/RStudio/1_Teaching/cds-spatial/data/gadm36_DNK_2_sp.rds")

Aarhus <- DKmun %>% 
  st_as_sf() %>% 
  st_transform( crs = 4326) %>% 
  filter(NAME_2 == "Århus")

# Union polygons 
extent <- walking_isos %>% 
  st_union() %>% 
  st_as_sf()

extent10 <- isos10m %>% 
  st_union() 
aarhus_ch <- isos10m %>% 
  st_union()%>% 
  st_convex_hull()


mapview(extent)

# Shelters
token <- "pk.eyJ1IjoiYWRpdmVhIiwiYSI6ImNrcWdhNGlybjB4OG0ydnNjcWZtOG9mc3UifQ.EbNmMF9aF8th5cb-h5f1lQ"
# Read in the shelter data
shelter <- read_sf("output_data/kmlTommy.geojson") 

# Unique shelter ID (not yet in Tommy's data, being cleaned in F2024 by Andreas)
shelter %>% 
  pull(first_number) %>% 
  unique() %>% 
  sort()

## private shelters and their geometry
private<- st_read("output_data/bbr_sikringsrum.geojson")
names(private)
sum(st_is_empty(st_as_sf(private$geometry)))

private <- private %>% 
  rename(capacity = places)


#########################################################

######################################################## Vector building footprints

vector_extract15 <- get_vector_tiles(
  tileset_id = "mapbox.mapbox-streets-v8",
  location = c(10.21076, 56.15674),
  zoom = 15
)

names(vector_extract)


library(ggplot2)

ggplot(vector_extract$building$polygons) + 
  geom_sf() + 
  theme_void() +
  geom_sf(vector_extract$building$polygons) + 
  theme_void()
g


#########################################################


# Walking range of exactly 5 mins
walking_isos <- read_sf("output_data/Tommy_walkisos5m_4326.shp")
# walking_isos <- mb_isochrone(shelter, profile = "walking", time = 5, id = "first_number")
# st_write(walking_isos,"output_data/Tommy_walkisos5m_4326.shp")

# Walking range of 1-10 mins
isos10m<- read_sf("output_data/Tommy_walkisos1_10m_4326.shp")

# isos10m <- mb_isochrone(shelter, profile = "walking", time = 1:10, id = "first_number")
# st_write(isos10m,"output_data/Tommy_walkisos1_10m_4326.shp")

# Map the results
mapbox_map <- leaflet() %>%
  addMapboxTiles(style_id = "streets-v11", username = "mapbox")

# 5 min isos
pal <- colorNumeric("viridis", walking_isos$time, na.color = "transparent")

mapbox_map %>% 
  addRasterImage(A_sm4326[[1]]) %>% 
  addCircleMarkers(data = st_transform(private, 4326),
                   radius = sqrt(private$capacity), 
                   color = "black") %>% 
  addPolygons(data = extent, 
              fill = FALSE, 
              stroke = TRUE, color = "black", weight = 1) %>% 
  addPolygons(data = walking_isos, 
              fillColor = ~pal(time),
              stroke = FALSE, 
              fillOpacity = 0.3) %>%
  addLegend(values = walking_isos$time, 
            pal = pal, 
            title = "5-min BTG catchments")

# 1:10min isos
pal2 <- colorNumeric("viridis", isos10m$time, na.color = "transparent")


##################################### Mapbox / Mapview maps

mapbox_map %>% 
  #addRasterImage(A_sat[[1]]) %>% 
  addPolygons(data = aarhus_ch, 
              fill = FALSE,
              stroke = TRUE, color = "black") %>% 
              #label = ~paste0("Total Income: ", dollar(income)),
              
  addPolygons(data = isos10m %>% filter(time < 6), 
              fillColor = ~pal2(time),
              stroke = FALSE, 
              fillOpacity = 0.1,
              highlight = highlightOptions(weight = 10,
                                           color = "blue",
                                           bringToFront = TRUE)) %>%
  addLegend(values = isos10m$time, 
            pal = pal2, 
            title = "Walking time (min)")

################################################## Explore "unprotected"  areas 

isos6_dif  <- isos10m %>% 
  filter(time == 6) %>% 
  st_union() %>% 
  st_as_sf()
aarhus_diff <- aarhus_ch %>% 
  st_difference(isos6_dif) %>% 
  st_as_sf()

# Private shelters construction and capacity over time : facetted maps

library(tmap)
tmap_options(limits = c(facets.view = 8))  # we want to view 5 periods

tmap_mode(mode = "view")

tmap_mode(mode = "plot")


### facetted private shelters and public isochrones

tm_shape(private, bbox = extent)+
  tm_facets(by = "decade",
            ncol = 4)+
  tm_bubbles(size = "capacity",
             col = "hotpink", scale = 2)+
  tm_shape(isos10m %>% filter(time == 5))+
  tm_polygons(col = "darkgreen", border.alpha = 0.1, 
              alpha = 0.1,)+
  tm_layout(legend.outside = "TRUE", 
            legend.outside.position = "bottom")

# un-facetted private and public sheter extent
tm_shape(A_sm)+
  tm_rgb(alpha = 0.7)+
tm_shape(private)+
  # tm_facets(by = "decade",
  #           ncol = 4)+
  tm_bubbles(size = "capacity", 
             col = "hotpink",
             scale = 2)+
tm_shape(isos10m %>% filter(time == 5))+
  tm_polygons(col = "red", border.alpha = 0.1, 
              alpha = 0.1,)+
  tm_layout(legend.outside = "TRUE", 
            legend.outside.position = "bottom")
