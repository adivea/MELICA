#### ------------- MAPPING SR DATA FOR CJ JOURNAL WITH CAPACITY

## Maps in Tmap (for overviews see HistoricalSR end)
### SR data from Ulrik (SR) with sikringsrum, processed in 07_HistoricalSR



# library 
library(tidyverse)
library(sf)
library(mapview)
library(tmap)


# Private shelter data
SR <- st_read("output_data/SR_sikringsrum.geojson")

mapview(SR)

SR_89   <- SR %>%
  mutate(decade = case_when(
    decade == '1930s' ~ '1180-1939',
    TRUE ~ decade  # Keep the original value for other cases
  )) %>% 
  dplyr::filter(decade < "1990s" & decade > "1180-1939") 

table(SR_89$decade)
table(SR_89$year)

SR %>% 
  #SR_89 %>% 
  group_by(decade) %>% 
  summarize(sum = n())


# Public shelter data

############## ----------------  BDG  data from 2024 and 2023 from Andreas wide
# All shelters sheet with long format where temporal changes are represented
BDG <- readRDS("output_data/BDG_long.rds")
glimpse(BDG)

# Plot BDG in space by year of construction (picking the first year)
m <- BDG %>% 
  group_by(BDnr) %>% 
  summarize(Startyear = min(Location_startdate), 
            Capacity = min(Capacity)) %>% 
  mapview(cex = "Capacity", zcol = "Startyear")
m

BDG <- BDG %>% 
  mutate(decade = case_when(
    Location_startdate < 1940 ~ '1930s',
    Location_startdate < 1950 ~ '1940s',
    Location_startdate < 1960 ~ '1950s',
    Location_startdate < 1970 ~ '1960s',
    Location_startdate < 1980 ~ '1970s',
    Location_startdate < 1990 ~ '1980s',
    Location_startdate < 2000 ~ '1990s',
   # Location_startdate < 2010 ~ '2000s'
  )) 

BDG %>% 
  group_by(decade) %>% tally()

BDGw <- readRDS("output_data/BDG_wide.rds")

BDGw %>% 
  mapview(zcol = "FAIMS_verified")

############## ---------------- KOB data from 2024

KOB <- readRDS("../output_data/KOB_sf.rds")
mapview(KOB, cex = "Capacity", zcol = "Year")

############## ------------------------------------- Prep additional map components


install.packages("geodata")
library(geodata)

dk <- gadm(country = "DNK", level = 0, path = "data/")
dk <- dk %>% 
  st_as_sf() 

# Create a bounding box for Aarhus if you don't have a shapefile
aarhus_bbox_sm <- st_as_sfc(st_bbox(c(xmin = 10.1, ymin = 56.1, xmax = 10.25, ymax = 56.2), crs = st_crs(4326)))
aarhus_bbox_m <- st_as_sfc(st_bbox(c(xmin = 10.05, ymin = 56.05, xmax = 10.28, ymax = 56.25), crs = st_crs(4326)))
aarhus_bbox <- st_as_sfc(st_bbox(c(xmin = 9.99, ymin = 56.0, xmax = 10.35, ymax = 56.3), crs = st_crs(4326)))

# Aarhus city border  
aarhus1935 <- read_sf("data/Topo/BordersShapes/Poly1935.shp")%>% 
  st_make_valid()

aarhus1948 <- read_sf("data/Topo/BordersShapes/Poly1948.shp")%>% 
  st_make_valid()
aarhus1952 <- read_sf("data/Topo/BordersShapes/Poly1952.shp")%>% 
  st_make_valid()
plot(aarhus1952$geometry)

st_is_valid(aarhus1952)
# Skip Bornholm
dk_bbox <- st_as_sfc(st_bbox(c(xmin = 8.07, ymin = 54.5, xmax = 13, ymax = 58), crs = st_crs(4326)))
mapview(dk_bbox)


############## ------------------------------------- Create a full map interactive

tmap_mode(mode = "view")

tm_shape(SR_89) +
  #tm_basemap("CartoDB.Positron") +  # CartoDB.Positron as the basemap
  tm_dots(col = "decade", 
          palette = "viridis", 
          title = "Decade",
          size = "places",  # Adjust the size of the points
          alpha = 0.8) +  # Transparency level
  tm_shape(aarhus_bbox_sm) +
  tm_borders(lwd = 2, col = "red") +  # Bounding box around Aarhus
  tm_shape(area) +
  tm_borders(lwd = 2, col = "orange") +  # Circle around DGB
  tm_layout(title = "Private shelter construction in Aarhus",
            legend.position = c("right", "bottom"),
            frame = FALSE) +  # Remove outer frame
  tm_compass(type = "8star", position = c("left", "top"))  # North arrow



################------------------------------ Static map with an inset and public shelters!

tmap_mode(mode = "plot")

# Create the main map
main_map <- tm_shape(dk$geometry, bbox = aarhus_bbox_m) +
#  tm_borders(lwd = 2, col = "grey") +  # Coastline of Aarhus
  tm_polygons(col = "white")+
  tm_shape(aarhus1952$geometry) +
  tm_borders(col = "grey70", 
             lwd = 3)+
  tm_shape(SR_89) +
  tm_dots(col = "decade", 
          palette = "viridis", 
          title = "Decade",  # Label for the color legend
          size = "places",  # Adjust the size of the points
          title.size = "Capacity",  # Label for the size legend
          scale = 1.5,  # Increase the size of all symbols by 1.5 times
          legend.is.portrait = TRUE,
          alpha = 0.8) +  # Transparency level

 
  tm_shape(BDGw %>% rename(Verified = FAIMS_verified)) +
    tm_squares(size = 0.1, col = "Verified", palette = c("white", "grey9"))+
  
  tm_layout(title = "Shelter construction in Aarhus municipality",
            #legend.position = c("right", "bottom"),
            legend.position = c("left", "top"),
            legend.title.size = 1.2,  # Adjust the size of the legend title
            legend.text.size = 0.8,  # Adjust the size of the legend text
            legend.stack = "vertical",  # Stack the legends vertically
            frame = TRUE,  # Remove outer frame
            bg.color = "grey85") + # Set the background color to a light grey  
 
  tm_compass(type = "8star", position = c("right", "bottom")) +  # North arrow
  tm_scale_bar(position = c("right", "bottom"))  # Scale bar

main_map


# Create the inset map of Denmark

inset_map <- tm_shape(st_as_sf(dk), bbox = dk_bbox) +
  tm_polygons(col = "gray90", border.col = "white") +  # Denmark base layer in inset
  tm_shape(aarhus_bbox) +
  tm_borders(lwd = 2, col = "red") +  # Bounding box in inset map
  tm_layout(frame = TRUE)  # Remove frame from inset

library(grid)

# Draw the main map
print(main_map)

# Overlay the inset map
vp_inset <- viewport(width = 0.25, height = 0.25, x = 0.15, y = 0.02, just = c("left", "bottom"))

print(inset_map, vp = vp_inset)



############## -------------------------------------  PRINT IT OUT - Figure 

# Step 3: Export the combined map as a TIFF at 400 DPI
tiff("figures/Figure06.tiff", width = 7, height = 10, units = "in", res = 400)
#png("figures/Figure06.png", width = 7, height = 10, units = "in", res = 400)
# Draw the main map
print(main_map)

# Overlay the inset map
vp_inset <- viewport(width = 0.3, height = 0.3, x = 0.07, y = 0.01, just = c("left", "bottom"))
print(inset_map, vp = vp_inset)

# Close the TIFF device
dev.off()

############## -------------------------------------  TMAP FACETTED PRIVATE SHELTERS OVER DECADES

BDG %>% group_by(decade) %>% tally()


tmap_options(limits = c(facets.view = 6))  # we want to view 5 periods
tmap_mode("plot")

# Create the facetted map
facetted_map <-  tm_shape(st_as_sf(dk), bbox = st_bbox(aarhus1952)) +  # 
  tm_borders(lwd = 2, col = "grey") +  # Coastline of Aarhus
  tm_shape(st_as_sf(dk))+
  tm_polygons(col = "white")+
  
   tm_shape(aarhus1952$geometry) + # City Boundary
  tm_borders(col = "grey70", 
             lwd = 3)+
 
  tm_shape(BDG %>% filter(Location_startdate <1970)) +
  tm_dots(
    col = "grey30", 
    # size = "Capacity",  # Use 'capacity' for size
    palette = "viridis", 
    #  title.size = "Capacity",  # Label for the size legend
    legend.show = FALSE,  # Hide the Decade legend
    #scale = 1.5,  # Increase the size of all symbols by 1.5 times
    #legend.is.portrait = TRUE,
    alpha = 0.8  # Transparency level
  ) +
  
  tm_shape(SR_89) +
  tm_dots(
    col = "decade", 
    size = "places",  # Use 'capacity' for size
    palette = "viridis", 
    title.size = "Capacity",  # Label for the size legend
    legend.show = FALSE,  # Hide the Decade legend
    scale = 1.5,  # Increase the size of all symbols by 1.5 times
    #legend.is.portrait = TRUE,
    alpha = 0.8  # Transparency level
  ) +
  tm_facets(
    by = "decade",  # Create facets by decade
    ncol = 3,
    free.coords = FALSE  # Keep the same coordinates for all facets
  ) +
  
 
  tm_layout(
     bg.color = "grey80",  # Consistent light grey background
     panel.label.size = 1.5,  # Adjust the size of facet labels
    outer.margins = 0.02,  # Reduce outer margins
    inner.margins = 0.05,  # Adjust inner margins
    legend.outside = TRUE,  # Place the legend outside the plotting area
    legend.outside.position = "bottom",  # Ensure the legend is at the bottom
   ) +
  tm_compass(type = "arrow", 
             position = c("left", "top")) +  # North arrow positioned outside the facets
  tm_scale_bar(
    #color.dark = TRUE ,  # Set the color of the line and notches
    breaks = c(0, 5),  # Set scale bar to 5 km only
    text.size = 0.8,  # Adjust text size
    position = c("left", "bottom")  # Position scale bar on the left
  )

# View the facetted map
facetted_map



# Save the facetted map as a high-resolution PNG
tmap_save(facetted_map, "facetted_map.png", width = 10, height = 8, dpi = 300)

# Or save it as a PDF
tmap_save(facetted_map, "figures/Figure10.png", width = 8, height = 8, dpi = 400)

