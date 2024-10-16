
library(shiny)
library(leaflet)
library(mapboxapi)
library(tidyverse)
library(sf)


token <- "pk.eyJ1IjoiYWRpdmVhIiwiYSI6ImNrcWdhNGlybjB4OG0ydnNjcWZtOG9mc3UifQ.EbNmMF9aF8th5cb-h5f1lQ"
# Read in the shelter data
shelter <- read_sf("output_data/kmlTommy.geojson") 

# must be in WGSS84
#shelter <- st_transform(shelter,4326)

# Set up a sidebar panel with a text box for an input address, 
# and a placeholder to print out the driving instructions
ui <- fluidPage(
  sidebarPanel(
    textInput("address_text", label = "Address",
              placeholder = "Type an address or place name"),
    selectInput("transport", label = "On foot or bike?", choices = c("walking", "cycling")),
    selectInput("time", label = "Within 5 or 10 mins?", choices = c(5, 10)),
    actionButton("action", "Find the nearest shelters within 5-10 min distance"),
    p(),
    p("Instructions to the shelter(s):"),
    em("Beware:unverified shelters may not exist"),
    #textInput("instructions_text", label = "Instructions to the nearest shelter /n (beware:location error ~100m)"),
    htmlOutput("instructions"),
    width = 3
  ),
  mainPanel(
    leafletOutput(outputId = "map", width = "100%", height = 1000)
  )
)

# Set up reactive elements to generate routes when the action button is clicked,
# then map the routes and print out the driving directions
server <- function(input, output) {
  
  # Define input_sf as a reactive value
  input_sf <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = "satellite-streets-v11",
                     username = "mapbox",
                     access_token = token)  %>%
      addCircleMarkers(data = shelter)
  })
  
  closest_locations <- eventReactive(input$action, {
    
    input_sf_value <- mb_geocode(input$address_text, 
                                 output = "sf",
                                 access_token = token) 
    
    st_crs(input_sf_value) <- 4326
    
    # Update input_sf with the new value
    input_sf(input_sf_value)
    
    # Calculate isochrones using the Mapbox API based on the user's input address and transport mode
    within_time <- mb_isochrone(
      location = input_sf_value,
      profile = input$transport,
      time = input$time,
      access_token = token)
    
    # Transform the nearest shelter dataframe to include travel times to each shelter from the user's input location
    hit <- shelter %>% 
      st_filter(within_time, .predicate = st_intersects)
    
    # Note: This loop iterates over each route to calculate directions, which can be computationally expensive.
    if(nrow(hit) > 1) {
      hit$time <- mb_matrix(
        origins = input_sf_value,
        destinations = hit,
        profile = input$transport) %>%
        as.vector()
      
      hit$index_order <- order(hit$time)[1:nrow(hit)]
      hit <- hit %>% 
        arrange(time) %>% 
        slice(1:5)
      
    } else if(nrow(hit) == 1){
      return(hit)
    }
    
    return(hit)
    
  })
  
  observeEvent(closest_locations(), {
    if (nrow(closest_locations()) > 0) {
      
      routelist <- list()
      for( i in 1:nrow(closest_locations())){
        print(i)
        route <- mb_directions(
          origin = st_coordinates(input_sf()),
          destination = st_coordinates(closest_locations()[i,]),
          profile = input$transport,
          output = "sf",
          steps = TRUE,
          access_token = token
        ) 
        st_crs(route) <- 4326
        routelist[[paste0("route", i)]] <- route
      }
      
      # Check if routelist contains more than one route
      if (length(routelist) > 2 && length(routelist) < 11) {
        ## SET COLORS FOR DIFFERENT HITS and ROUTES
        library(RColorBrewer)
        
        # Define the number of routes
        num_routes <- length(routelist)
        
        # Define the number of colors you want in the palette
        num_colors <- num_routes
        
        # Display the n routes in different colors from green to red by length
        color_palette <- brewer.pal(num_colors, "RdYlGn")
        
        # extract the order of routes by speed from most distant to the closest
        index_order <- closest_locations()$index_order
        
        leafletProxy(mapId = "map") %>%
          clearShapes() %>%
          addMarkers(data = closest_locations(),
                     popup = ~paste0(input$transport, " travel time: ", round(closest_locations()$time, 2), " minutes"))
        
        # Add routes to the map in the reverse order (most distant first)
       # for (i in rev(index_order)) {
        for (i in rev(seq_along(routelist))) {
          route <- routelist[[i]]
          route <- routelist[[i]]
          color <- color_palette[i]
          
          flyto_coords <- route %>%
            st_union() %>%
            st_centroid() %>%
            st_coordinates() %>%
            as.vector()
          
          ## SHOW OTHER SHELTERS AND ROUTES
          leafletProxy(mapId = "map") %>%
            addPolylines(data = route, color = color,
                         opacity = 1,
                         popup = ~paste0(input$transport, " travel time: ", round(sum(route$duration),2), " minutes")) %>%
            flyTo(lng = flyto_coords[1],
                  lat = flyto_coords[2],
                  zoom = 16)
        }
        
        
        # If the number of routes is 1 or 2, just show one route (the last and therefore shortest)
      } else if (length(routelist) == 2 | length(routelist) == 1) {
        print("Number of routes is 1 or 2")
        num_routes <- length(routelist)
        print(num_routes)
        
        flyto_coords <- routelist[[num_routes]] %>%
          st_union() %>%
          st_centroid() %>%
          st_coordinates() %>%
          as.vector()
        
        leafletProxy(mapId = "map") %>%
          clearShapes() %>%
          addPolylines(data = routelist[[num_routes]], color = "red",
                       opacity = 1,
                       popup = ~paste0(input$transport, " travel time: ", sum(routelist[[num_routes]]$duration) , " minutes")) %>%
          flyTo(lng = flyto_coords[1],
                lat = flyto_coords[2],
                zoom = 16)
      }
      
      output$instructions <- renderUI({
        HTML(paste0(
          paste("&bull;", route$instruction, sep = ""),
          collapse = "<br/>"))
        
      })
    }
  })
} 

shinyApp(ui = ui, server = server)
