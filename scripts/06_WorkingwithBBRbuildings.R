############### Buildings IN BBR

## This code was first developed by @JakobJM (Jakob Mørup) for 2021 CDS project visualizing urban development 
# of Aarhus and Copenahgen in 1990-1999, openly shared in his  Github repository
# https://github.com/Jakobjm/Project_Cultural_Data_Science/blob/master/data_visualisation.Rmd

# library 
library(jsonlite)
library(tidyverse)
library(sf)

# download aarhus data from sciencedata.dk (only houses)
# bbr_aarhus_data <- fromJSON("https://sciencedata.dk/public/67e2ad2ca642562dacfa6fdf672a1009/clean_BBR_aarhus_data.json")
# saveRDS(bbr_aarhus_data, "data/bbr_aar_list.rds")

bbr_aarhus_data <- readRDS("data/bbr_aar_list.rds")

#flattening it to a data frame and changing the name of a column to get rid of danish letters
flatten <- function(json){
  json <- as.data.frame(json[["BygningList"]])
  names(json)[names(json)=="byg026Opførelsesår"] <- "byg026Year"
  return(json)
}

### all aarhus building data
all_bbr_df <-function(df, year_cutoff){
  df %>% 
    filter(!is.na(byg404Koordinat)) %>% 
    filter(byg404Koordinat != "POINT(0 0)") %>% 
    filter(byg406Koordinatsystem == "5") %>% 
    filter(!is.na(byg026Year)) %>% 
    distinct(id_lokalId,.keep_all = TRUE) %>% 
    filter(byg026Year >= year_cutoff)
}

all_bbr_aarhus <-bbr_aarhus_data %>%
  flatten() %>% 
  all_bbr_df(1900) %>% 
  mutate(city = "Aarhus")%>% 
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)



#filtering for buildings used for recreational, educational, habitation, health, 
# light- and service industry out discarding other buildings, 
# refer to BBRkodelister 20210322 for the complete list of codes

clean_bbr_df <-function(df, year_cutoff){
  df %>% 
    filter(!is.na(byg404Koordinat)) %>% 
    filter(byg404Koordinat != "POINT(0 0)") %>% 
    filter(byg406Koordinatsystem == "5") %>% 
    filter(!is.na(byg026Year)) %>% 
    filter(byg021BygningensAnvendelse %in% 
             c("101","120","121","122","131","132","140","150","160","190","236","321","322","324","331","332","333","334","339","421","422","429","431","432","433","439","441","532","533")) %>% 
    distinct(id_lokalId,.keep_all = TRUE) %>% 
    filter(byg026Year >= year_cutoff)
}

bbr_aarhus_data_flat <-bbr_aarhus_data %>%
  flatten() %>% 
  clean_bbr_df(1900) %>% 
  mutate(city = "Aarhus")%>% 
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)

saveRDS(bbr_aarhus_data_flat,"output_data/bbr_residential_aarhus.rds")
saveRDS(all_bbr_aarhus,"output_data/bbr_all_aarhus.rds")

# # trying the sikringsrum code "236" but to no avail >> no records
# bbr_aarhus_sikring <- bbr_aarhus_data_flat %>% 
#   filter(byg021BygningensAnvendelse == "236")

library(mapview)
mapview(head(bbr_aarhus_data_flat))

# overview of years
all_bbr <- all_bbr_aarhus %>%   # 50,000 entries
  filter(byg026Year >1935 & byg026Year <2005)
res_bbr <- bbr_aarhus_data_flat %>%  # 30,000 entries
  filter(byg026Year >1935 & byg026Year <2005)

hist(all_bbr$byg026Year, main = "Construction in Aarhus (based on BBR)")
hist(res_bbr$byg026Year)

# Combined:
hist1 <- hist(all_bbr$byg026Year, 
                main = "Construction in Aarhus (BBR)", 
                col = rgb(1, 0, 0, 0.5), # Red color with 50% transparency
                xlim = c(1940, 2000),
                # xlim = c(min(c(all_bbr_aarhus$byg026Year, bbr_aarhus_data_flat$byg026Year)),
                #          max(c(all_bbr_aarhus$byg026Year, bbr_aarhus_data_flat$byg026Year))),
                ylim = c(0, max(hist(all_bbr_aarhus$byg026Year, plot = FALSE)$counts)), 
                xlab = "Year of Construction", 
                ylab = "New buildings per five years", 
                las = 1)
  
  # Plot the second histogram without axes but with a secondary y-scale
hist2 <- hist(res_bbr$byg026Year, 
                xlim = c(1940, 2000),
                col = rgb(0, 0, 1, 0.5), # Blue color with 50% transparency
                add = TRUE)
  
  # Add a secondary y-axis on the right-hand side
  #axis(4, at = pretty(range(hist2$counts), 3), las = 2)

  # Add a label for the second y-axis
#  mtext("Residential", side = 4, line = 1)
  
  # Adjust the margins to make space for the secondary y-axis label
  par(mar = c(5, 4, 4, 4) + 0.5)
  
  # Add a legend to differentiate between the datasets
legend("topleft", legend = c("All constructions", "Residential"), bty = "n", 
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))
  

# decades
bbr_aarhus_data_flat <- bbr_aarhus_data_flat %>% 
  mutate(decade = case_when(
    byg026Year < 1910 ~ '1900s',
    byg026Year < 1920 ~ '1910s',
    byg026Year < 1930 ~ '1920s',
    byg026Year < 1940 ~ '1930s',
    byg026Year < 1950 ~ '1940s',
    byg026Year < 1960 ~ '1950s',
    byg026Year < 1970 ~ '1960s',
    byg026Year < 1980 ~ '1970s',
    byg026Year < 1990 ~ '1980s',
    byg026Year < 2000 ~ '1990s',
    byg026Year < 2010 ~ '2000s',
    byg026Year < 2021 ~ '2010s' )) 

# Check spatial integrity
bbr_aarhus_data_flat %>% 
  filter(!st_is_empty(st_as_sf(byg404Koordinat)))


# Explore building sizes
bbr_aarhus_data_flat %>% 
  st_drop_geometry() %>% 
  group_by(byg054AntalEtager) %>% 
  tally()





################################################################################
#Creating choropleth overlays on basemaps from OpenStreetMaps, showing the number of new buildings by districs in a given year or range of years
################################################################################
# Private shelter locations
SR <- st_read("output_data/SK_bbr_oc_addresses.geojson")
SR <- SR %>% 
  rename(ID = id_lokalId, byg026Year = byg026Opførelsesår, places = byg069Sikringsrumpladser)


# Polygons for Aarhus district in geoJSON format for mappable results
aarhus_districts <- st_read("https://sciencedata.dk/public/67e2ad2ca642562dacfa6fdf672a1009/aarhus_districts.geojson")

# Polygons with data on new buildings: intersections of the district polygons and the point data from BBR
intersections_aarhus <- st_intersection(aarhus_districts,bbr_aarhus_data_flat) %>% 
  dplyr::select(id = id_lokalId, navn = prog_distrikt_navn, byg021BygningensAnvendelse,byg026Year, decade, byg054AntalEtager,byg406Koordinatsystem,city,geometry)

#Creating choropleth overlays on basemaps from OpenStreetMaps, showing the number of new buildings by districs in a given year or range of years

#A function that takes a data frame input and a year value (that can be a list/range of values) and returns a subset of the data frame where the year is in the input year(s)
get_year_df <- function(df,year){
  temp_df <- df %>% 
    filter(byg026Year %in% year)
  return(temp_df)
}

#A function that adds a new column to the aarhus districts with counts of new buildings in each districts in that/those year(s) given a range of years, from the intersections data frame and returns the new dataframe
n_new_buildings <- function(year){
  n_new <- rep(0,length(aarhus_districts$prog_distrikt_navn))
  intersections_year <- get_year_df(intersections_aarhus, year)
  for (i in 1:length(aarhus_districts$prog_distrikt_navn)){
    n_new[i] <- nrow(filter(intersections_year, navn == aarhus_districts$prog_distrikt_navn[i]))
  }
  
  temp <- cbind(aarhus_districts, n_new)
  return(temp)
}

#Function that takes a year range and creates a chloropleth map from it, based on BBR data and district geometry
library(leaflegend)

draw_choropleth <- function(year){
  # Ensure that year is a vector of years, not just a single value
  if (length(year) == 1) {
    year <- as.vector(year)
  }
  temp_df_choropleth <- n_new_buildings(year = year)
  temp_df_SR <- get_year_df(SR, year)
  #n_new_buildings(1990)
  
  #creating a color palette
  pal <- colorNumeric("Greens", domain = NULL)
  
  # creating labels for the choropleth map
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Nye bygninger</sup>",
    temp_df_choropleth$prog_distrikt_navn, temp_df_choropleth$n_new) %>% lapply(htmltools::HTML) %>% unlist()
  
  # SR information
  SR_popups <- sprintf(
    "<strong>%s</strong><br/>%g Capacity",
    temp_df_SR$oc_formatted, temp_df_SR$places  # Customize 'navn' with actual field name for district
  ) %>% lapply(htmltools::HTML)
  
  # Checking if 'year' is a range (multiple values)
  if (length(year) > 1) {
    year_range <- paste0(min(year), ":", max(year))  # Format as 'startYear:endYear'
  } else {
    year_range <- as.character(year)  # Single year
  }
  
  # Dynamic map title with the supplied 'year' or year range
  map_title <- paste0("Showing range of years ", year_range)
  
  #
  
  #drawing the choropleth map on top of OpenStreetMaps base map
  choro <- leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(data = st_transform(temp_df_choropleth,4326),
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
                  bringToFront = FALSE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"), 
                group = "Districts") %>% 
    addCircleMarkers(data = temp_df_SR, 
                     color = "black",
                     radius = sqrt(temp_df_SR$places),
                     opacity = .5, 
                     #fillOpacity = 0.7,
                     popup = SR_popups,  # Make points clickable with a popup
                     group = "Sikringsrum") %>%  # Group for points layer
    
    # Add layer control to toggle between polygons and points
    addLayersControl(
      overlayGroups = c("Districts", "Sikringsrum"),  # Allows toggling of both layers
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    addLegend(
      data = temp_df_choropleth,
      position = "bottomright",
      pal = pal,
      values = ~ n_new,
      title = "Antal nye bygninger",
      opacity = 0.5) %>% 
    
     # # Add circle size legend for 'places' column using leaflegend

    addLegendSize(
      values = temp_df_SR$places,       # Values for the size legend
      color = "black",                  # Color for the circles in the legend
      position = "bottomleft",          # Position of the size legend
      title = "Capacity",         # Title of the size legend
      shape = "circle",                 # Shape of the markers in the legend
      fillColor = "black",              # Fill color of the legend circles
      opacity = 1,
      baseSize = 10,
      labelStyle = "font-size: 12px;"   # Customize text size for the legend labels
    ) %>% 
    
    # Adding the title to the top of the map
    addControl(
      html = paste0("<h3 style='text-align:center;'>", map_title, "</h3>"),
      position = "topleft",
      className = "map-title"
    )
  
  
  return(choro)
}

library(leaflet)
library(htmltools)

draw_choropleth(1945:1947)
draw_choropleth(1945:1950)

draw_choropleth(c(1950:1955))
draw_choropleth(c(1950:1960))
draw_choropleth(c(1970:1975))


## making Histograms of average building height measured in floors.

plot_hist <- function(Year, df){
  temp_df_histogram <- 
    get_year_df(df, Year) %>%
    #df %>% 
    dplyr::select(byg026Year, byg054AntalEtager) %>% 
    group_by(byg026Year) %>% 
    summarise(mean_AntalEtager = mean(byg054AntalEtager, na.rm = TRUE))
  
  
  ggplot(temp_df_histogram,aes(x = byg026Year, y= mean_AntalEtager))+
    geom_col(fill = "#76EE00", color = "black")+
    labs(x= "Years", y= "Mean heigth in floors")+
    ggtitle(paste(df$city[1]))+
    theme_minimal()
}

plot_hist(c(1950:1999), intersections_aarhus)
