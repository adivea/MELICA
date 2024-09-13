#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# This Shiny app creates choropleth overlays on basemaps from OpenStreetMaps, 
# showing the number of new buildings by Aarhus districts in a given year or range of years


library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(leaflegend)
library(htmltools)
library(tidyverse)

# Assuming the required data is already loaded:

# SR : Private shelter locations and buildings in Aarhus
SR <- st_read("../output_data/SK_bbr_oc_addresses.geojson")
SR <- SR %>% 
  rename(ID = id_lokalId, byg026Year = byg026Opførelsesår, places = byg069Sikringsrumpladser)

bbr_aarhus_data_flat <- readRDS("../output_data/bbr_residential_aarhus.rds")

# Polygons for Aarhus district in geoJSON format for mappable results
aarhus_districts <- st_read("https://sciencedata.dk/public/67e2ad2ca642562dacfa6fdf672a1009/aarhus_districts.geojson")

# Polygons with data on new buildings: intersections of the district polygons and the point data from BBR
intersections_aarhus <- st_intersection(aarhus_districts,bbr_aarhus_data_flat) %>% 
  dplyr::select(id = id_lokalId, navn = prog_distrikt_navn, byg021BygningensAnvendelse,byg026Year, decade, byg054AntalEtager,byg406Koordinatsystem,city,geometry)

# Project polygons to EPSG 4326
intersections_aarhus <- st_transform(intersections_aarhus, 4326)  # Polygons


# Load custom functions to select a year or a range of years
get_year_df <- function(df, year){
  temp_df <- df %>% filter(byg026Year %in% year)
  return(temp_df)
}

# Summarize new buildings per year and district
n_new_buildings <- function(year){
  n_new <- rep(0, length(aarhus_districts$prog_distrikt_navn))
  intersections_year <- get_year_df(intersections_aarhus, year)
  for (i in 1:length(aarhus_districts$prog_distrikt_navn)){
    n_new[i] <- nrow(filter(intersections_year, navn == aarhus_districts$prog_distrikt_navn[i]))
  }
  
  temp <- cbind(aarhus_districts, n_new)
  return(temp)
}

# Mapping function
draw_choropleth <- function(year){
  # Ensure that year is a vector of years, not just a single value
  if (length(year) > 1) {
    year_range <- paste0(min(year), ":", max(year))
  } else {
    year_range <- as.character(year)
  }
  
  temp_df_choropleth <- n_new_buildings(year = year)
  temp_df_SR <- get_year_df(SR, year)
  
  pal <- colorNumeric("Greens", domain = NULL)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g New buildings</sup>",
    temp_df_choropleth$prog_distrikt_navn, temp_df_choropleth$n_new
  ) %>% lapply(htmltools::HTML) %>% unlist()
  
  SR_popups <- sprintf(
    "<strong>%s</strong><br/>%g Capacity",
    temp_df_SR$oc_formatted, temp_df_SR$places
  ) %>% lapply(htmltools::HTML)
  
  map_title <- paste0("Showing year(s) ", year_range)
  
  choro <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = st_transform(temp_df_choropleth, 4326),
                fillColor = pal(temp_df_choropleth$n_new),
                weight = 1,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = FALSE
                ),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"
                ),
                group = "Districts") %>%
    addCircleMarkers(data = st_transform(temp_df_SR, 4326),
                     color = "black",
                     radius = sqrt(temp_df_SR$places),
                     opacity = 0.5,
                     popup = SR_popups,
                     group = "Sikringsrum") %>%
    addLayersControl(
      overlayGroups = c("Districts", "Sikringsrum"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend(
      data = temp_df_choropleth,
      position = "bottomright",
      pal = pal,
      values = ~n_new,
      title = "Antal nye bygninger",
      opacity = 0.5
    ) %>%
    addLegendSize(
      values = temp_df_SR$places,
      color = "black",
      position = "bottomleft",
      title = "Capacity",
      shape = "circle",
      fillColor = "black",
      opacity = 1,
      baseSize = 10,
      labelStyle = "font-size: 12px;"
    ) %>%
    addControl(
      html = paste0("<h3 style='text-align:center;'>", map_title, "</h3>"),
      position = "topleft",
      className = "map-title"
    )
  
  return(choro)
}

# Define UI for application
ui <- fluidPage(
  titlePanel("New buildings by urban district and location of sikringsrum in Aarhus"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", 
                  "Select Year or Range of Years:",
                  min = 1940, max = 2020, 
                  value = c(1945,1950), 
                  step = 1, 
                  sep = "")
    ),
    mainPanel(
      leafletOutput("map", height = "500px")
    )
  )
)

# Define server logic required to draw the choropleth
server <- function(input, output) {
  # Draw the initial map
  output$map <- renderLeaflet({
    draw_choropleth(year = input$yearRange[1]:input$yearRange[2])
  })
  
  # Observe changes in year range and update map accordingly
  observe({
    year_range <- input$yearRange
    selected_years <- seq(from = year_range[1], to = year_range[2], by = 1)
    
    output$map <- renderLeaflet({
      draw_choropleth(year = selected_years)
    })
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

