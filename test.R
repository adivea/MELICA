input_sf <- mb_geocode("Dokk1, Aarhus, Denmark", output = "sf") 

all_dist <- mb_matrix(
  origins = input_sf,
  destinations = shelter[140:150,]) %>%
  as.vector() 

within_time <- mb_isochrone(
  location = input_sf,
  profile = "cycling",
  #profile = "walking",
  time = 10,
  access_token = token)

# Transform the nearest shelter dataframe to include travel times to each shelter from the user's input location
hit <- shelter %>% 
  st_filter(within_time, .predicate = st_intersects)
mapview(hit, zcol = "time") + mapview(hit,zcol = "index_order")

# Note: This loop iterates over each route to calculate directions, which can be computationally expensive.
hit$time <- mb_matrix(
    origins = input_sf,
    destinations = hit,
    profile =  "cycling") %>%
   # profile = "walking") %>%
    as.vector()

hit$index_order <- order(hit$time)[1:nrow(hit)]
hit <- hit %>% 
  arrange(time) %>% 
  slice(1:5)

routelist <- list()
for( i in 1:nrow(hit)){
  print(i)
  route <- mb_directions(
  origin = st_coordinates(input_sf),
  destination = st_coordinates(hit[i,]),
  profile = "walking",
  output = "sf",
  steps = TRUE
  #access_token = token
) 
  routelist[[paste0("route", i)]] <- route
}


all_routes <- do.call(rbind, routelist)

# Display the n routes in different colors from green to red by length
color_palette <- rev(heat_hcl(length(routelist)))

# Check if routelist contains more than one route
if (length(routelist) > 1) {
  # Create a leaflet map
  m <- leaflet() %>%
    addMapboxTiles(style_id = "satellite-streets-v11",
                   username = "mapbox",
                   access_token = token) 

  # Add routes to the map in the specified order
  m <- m %>% 
    clearShapes() 
  for (i in rev(index_order)) {
    route <- routelist[[i]]
    color <- color_palette[i]
    m <- m %>% 
    addPolylines(
      data = route,
      color = color,  # Assign a color based on the order specified
      popup = hit$time[i]
      )
  }
  
  # Show the map
  m
} else {
  print("routelist must contain more than one route for plotting.")
}


# Show the map
m

# Troubleshooting
within_time <- mb_isochrone(
  location = input_sf,
  profile = "cycling",
  time = 5,
  access_token = token)
