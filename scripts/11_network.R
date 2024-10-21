# ------  Testing urban road network

# mockup with ChatGPT input

library(tidyverse)
library(sf)
library(igraph)
library(sp)
library(dodgr)
library(tmap)
install.packages("dodgr")
library(mapview)

sikringsrum <- st_read("output_data/bbr_sikringsrum.geojson")
sikringsrum <- sikringsrum %>% 
  st_transform(crs = 4326)
shelters <- st_read("output_data/kmlTommy.geojson")
st_crs(shelters) == st_crs(sikringsrum)

parcels <- st_read("parcels.shp")
streets <- st_read("streets.shp")

plot(shelters$geometry); plot(sikringsrum, color = "blue", add = TRUE)
mapview(sikringsrum, zcol = "decade") + mapview(shelters)
#Create a Network Graph

#Convert the street network into a graph object that can be used for network analysis.

net <- dodgr_streetnet(streets)
graph <- weight_streetnet(net)

#Calculate Shortest Path Distances

# Calculate shortest path distances from each residential parcel to each shelter.

distances <- dodgr_distances(graph,
                             from = st_coordinates(parcels),
                             to = st_coordinates(shelters),
                             shortest = TRUE)

#Incorporate Shelter Capacity

# Use a loop or apply function to allocate population to shelters 
#considering their capacity.

parcels$assigned_shelter <- NA
parcels$distance_to_shelter <- NA

for(i in seq_len(nrow(parcels))){
  available_shelters <- which(shelters$capacity > 0)
  if(length(available_shelters) > 0){
    dists <- distances[i, available_shelters]
    nearest <- available_shelters[which.min(dists)]
    parcels$assigned_shelter[i] <- nearest
    parcels$distance_to_shelter[i] <- min(dists)
    shelters$capacity[nearest] <- shelters$capacity[nearest] - 1
  }
}

# Assess Spatial Equity

# Calculate spatial equity metrics such as the Gini coefficient or Moran's I.

# Calculate Gini coefficient
gini_coefficient <- ineq::Gini(parcels$distance_to_shelter)

# Moran's I for spatial autocorrelation of distances
morans_i <- spdep::moran.test(parcels$distance_to_shelter, 
                              listw = spdep::nb2listw(spdep::poly2nb(parcels)))

#Visualize Service Areas

# Use buffer analysis to create service areas around shelters and 
#visualize them with respect to population density.

service_areas <- st_buffer(shelters, dist = 1000)  # Example 1km buffer

tmap_mode("view")
tm_shape(parcels) +
  tm_polygons("population_density") +
  tm_shape(service_areas) +
  tm_borders(col = "red") +
  tm_shape(shelters) +
  tm_symbols(size = "capacity", col = "blue")
Identify Underserved Areas

# Overlay service areas with population density to identify underserved areas.

r
Copy code
underserved <- st_difference(parcels, service_areas)
underserved_areas <- underserved %>% 
  filter(population_density > threshold)  # Define your threshold

tm_shape(underserved_areas) +
  tm_polygons("population_density", title = "Underserved Areas")
Summary of the Approach
Network Distance Measurement: Calculate shortest path distances between residential parcels and shelters using the street network.

Capacity Constraints: Allocate population demand to shelters based on their capacity, ensuring that once a shelter is full, the next closest shelter is considered.

Spatial Equity Analysis: Assess equity using Gini coefficient, Moran's I, or similar metrics to understand the fairness of the shelter distribution.

Adequacy Analysis: Use service areas and population density mapping to identify underserved areas where shelter accessibility is insufficient.

This approach allows for a comprehensive analysis of both the accessibility (spatial adequacy) and fairness (spatial equity) of the shelter distribution within Aarhus.









