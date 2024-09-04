
### Shelters from Tommy Cassoe

# This script explores and evaluates the crowdsourced data
# and integrates it with 2023 data? o
# outlines which need to be still surveyed!

## libraries
library(tidyverse)
library(sf)
library(mapview)

## load from KML

s_i <- read_sf("data/BDfromTimmy.kml", "Ikke verificerede Betondækningsgrave") # this only gets one non'verified table
s_v <- read_sf("data/BDfromTimmy.kml", "Verificerede Betondækningsgrave") # this only gets one non'verified table

# or following https://gis.stackexchange.com/questions/58131/reading-kml-file-into-r

#3 readOGR() # I need rgdal which is a tall order

# Check extent and add info
plot(s_i$geometry)
plot(s_v$geometry)
s_i$verified <- "N"
s_v$verified <- "Y"

# combine two layers into one differentiating their verified status
s <- rbind(s_i, s_v)

# extract coordinates and cast as simple feature
s$long <- st_coordinates(s)[,1]
s$lat <- st_coordinates(s)[,2]
s$z <- st_coordinates(s)[,3]
s <- s %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) 

###################################################### ATTRIBUTES
# join with attribute data CONTINUE
s$Name[151] <- 9999
tail(s)

# chat gpt advice on defensive function to get the first number out:
extract_first_number <- function(column_value) {
  number <- as.integer(str_extract(column_value, "\\b\\d+\\b"))
  if (!is.na(number)) {
    return(number)
  } else {
    return(NA)
  }
}

# Create a new column
s <- s %>% 
  mutate(ID = gsub("\\D", ", ", Name)) %>% 
  mutate(first_number = map_dbl(ID, extract_first_number))

#st_write(s, "output_data/kmlTommy.geojson")
s <- read_sf("output_data/kmlTommy.geojson")



################################################  Kommune addresses data SKIP FOR NOW 

# Load attribute data from Tommy "Addresses feedback
library(googlesheets4)
gs4_deauth()

# s_data <- read_sheet("https://docs.google.com/spreadsheets/d/1hwzvaYz9HX5-r5ZXklhD7eVcFt1RHXyQknXTAzCpxso/edit#gid=0")
# class(s_data)
# 
# # streamline coordinates:
# split_and_convert <- function(coord) {
#   parts <- unlist(strsplit(coord, " "))
#   lat <- as.numeric(gsub("[^0-9.]", "", parts[1]))
#   lon <- as.numeric(gsub("[^0-9.]", "", parts[2]))
#   return(list(lat = lat, lon = lon))
# }
# 
# # Apply function to each coordinate
# split_coordinates <- lapply(s_data$Decimalkoordinater, split_and_convert)
# 
# # Convert list to data frame
# coordinates_df <- do.call(rbind.data.frame, split_coordinates)
# 
# # Rename columns
# colnames(coordinates_df) <- c("Latitude", "Longitude")
# 
# # Add to s_data
# s_data <- tibble(cbind(s_data, coordinates_df))
# s_addresses <- s_data %>% 
#   filter(!is.na(Latitude)) %>% 
#   select(-Status1987) %>%
#   select(-`1987kort_nr`) %>% 
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# 
# s_addresses$BDtype <- as.factor(s_addresses$BDtype)
# 
# ############################################################
# # Tommy's points = 200 shelters
# s$first_number
# # Tommy's work over our(?) addresses = 50
# class(s_addresses$BDtype)
# 
# mapview(s_addresses, zcol = "BDtype") # + mapview(s, zcol = "verified")
# 
# # Link betweeen TOmmy's points and BD table from kommune
# sum(s$first_number %in% s_addresses$BDnr)
# 

############################################ RECONCILE THE ADDRESSES ATTRIBUTES

library(googlesheets4)
gs4_deauth()

# addresses from kommune marked by Konstantina as verified or not
address_k_k <- read_sheet("https://docs.google.com/spreadsheets/d/1zKn1vuN_vP_s2G0SosmuuVaOSrf6FvIxp4QQCLuIDJE/edit#gid=1743387780")


# addresses from kommune filtered by Konstantina for those 122 that we need to register
address_k <- read_sheet("https://docs.google.com/spreadsheets/d/15TdABJZveXvHe0L8QT-ujrvkfb1x6TPslGdbrhH2Izs/edit#gid=476511693")

# addresses from kommune ~280  that Tommy annotated "AddressesFeedback"
address_t <- read_sheet("https://docs.google.com/spreadsheets/d/1hwzvaYz9HX5-r5ZXklhD7eVcFt1RHXyQknXTAzCpxso/edit#gid=0")
names(address_k)
names(address_t)


################# COMPARE TYPES!! ADD TOMMY'S TYPES FOR 2023 shelters

# Here I combine Konstantina's marked up sheet with Tommy's attributes on the basis of Bdnr and Feature ID
verified_attr <- address_k_k %>% 
  filter(EXISTS_IN_MELICA != "NO") %>% 
  left_join(address_t, by = c("Bdnr" = "BDnr"), relationship = "many-to-many") %>% # beware that at least two features have moved and thus bdnr is duplicated
  select(Bdnr, FEATURE_ID, BDtype, Status_i_dag, BEMÆRK_L_1, ADDRESS.x, ADDRESS.y, `1987kort_nr`) 
  
# Verified 2023 shelters with Tommy's types
shelter_t_type <- shelter %>% 
  left_join(verified_attr, by = c("FeatureID" = "FEATURE_ID")) %>% 
  select(FeatureID, FeatureType, Bdnr, BDtype, Status_i_dag, LanduseOnTop) 

# Type breakdown
shelter_t_type %>% 
  group_by(BDtype) %>% 
  tally()

# compare the look of Tommy's types with 
shelter_t_type%>% 
  mapview(zcol = "BDtype")
# our type definitions... some differences but not too bad
mapview(shelter, zcol = "FeatureType")

shelter_t_type %>% 
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  write_csv("output_data/sh_types2023.csv")

st_write(shelter_t_type, "output_data/sh_types2023.geojson")

###################### CORRECTED TYPES IN TMAPS
shelter_t_type <- read_sf( "output_data/sh_types2023.geojson") #Tommy's 2024 contribution

shelter_t_type
# Visualize features by type and landuse
tmap_options(limits = c(facets.view = 6))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(shelter_t_type)+
  tm_facets(by = "BDtype",
            ncol = 3)+
  tm_bubbles(col = "LanduseOnTop") +
  tm_shape(s)+
  tm_dots(col = "lightgrey")


############################## FILTER UNVERIFIED SHELTER ATTRIBUTES 

# we wish to join the two address files filtering out Konstantina's 
# and keeping the notes field to inform groundtruthing

toverify <- address_k %>% 
  left_join(address_t, by = c("Bd_nr" = "BDnr"), relationship = "many-to-many") # beware that 1623 and 1501 are duplicated (due to move)


##################  COMPARE TYPES

# Many differences are in Tommy list subtypes III A where we list just III
shelter_t_type %>% 
  mutate(Type = gsub("Shelter Type ", "" , FeatureType)) %>% 
  mutate(BDType = gsub(" A", "" , BDtype)) %>%   
  dplyr::select(FeatureID, Type, BDType) %>% 
  dplyr::filter(Type != BDType)  %>% 
  mapview()

# get data out
shelter_t_type %>% 
    mutate(Type = gsub("Shelter Type ", "" , FeatureType)) %>% 
    mutate(BDType = gsub(" A", "" , BDtype)) %>%   
    dplyr::select(FeatureID, Type, BDType) %>% 
    dplyr::filter(Type != BDType) %>% 
      # st_write("output_data/type_mismatch.geojson")
    mutate(longitude = st_coordinates(.)[,1],
           latitude = st_coordinates(.)[,2]) %>% 
    st_drop_geometry() %>% 
    write_csv("output_data/type_mismatch.csv")
  

##################  VERIFICATION WITH TOMMY

shelter_t_type %>% 
  filter(FeatureID %in% c(128, 170, 173, 175, 183, 200, 251, 252, 270, 269)) %>% 
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  write_csv("output_data/visit11June.csv")


