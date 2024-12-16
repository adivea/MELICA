library(tidyverse)
library(googlesheets4)
library(sf)
library(mapview)


# Load Andreas complete data from Google and create a 1987 version (Wide dataset)
BDG <- read_sheet("https://docs.google.com/spreadsheets/d/1H8EhFgwhDGKCsM95BTjNwifg5K-oXTv2Q4Qbx84k7ZU/edit?gid=0#gid=0",
                  range = "Shelters", 
                  col_types = "dddccddcddcccccdddcccccdddccddcccdc")
BDG <- BDG %>% 
  filter(!is.na(Final_Longitude_1987) | !is.na(Final_Latitude_1987) ) %>% 
  st_as_sf(coords = c("Final_Longitude_1987", "Final_Latitude_1987"), crs = 4326) %>% 
  dplyr::select(BDnr_1987,Year_of_Construction,  Final_type, Final_Pub_Size, Needs_Revisit, Final_Longitude_2024, Final_Latitude_2024)

# 10 closely undefined shelters - NEED REVISIT
BDG %>% 
  filter(is.na(Final_type))

# Summaries of capacities
BDG %>% 
  group_by(Final_type) %>% 
  summarize(total_capacity = sum(Final_Pub_Size))

saveRDS(BDG, "output_data/BDG_andreas.rds")
glimpse(BDG)
mapview(BDG, zcol = "Year_of_Construction")
########################################################################

# Load Andreas historical "long" data and show through time
BDG <- read_sheet("https://docs.google.com/spreadsheets/d/1C4GEgq4UmEwx_Xi84FiNBrRowmmJHi3S191byjF-hWc/edit?gid=0#gid=0",
                  range = "Ark1", 
                  col_types = "ddcdddcdccdcc")

BDG  <- BDG %>% 
  filter(!is.na(Long) | !is.na(Lat) ) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) 

glimpse(BDG)

# Plot BDG in space by year of construction
m <- BDG %>% 
  group_by(BDnr) %>% 
  summarize(Startyear = min(Location_startdate), 
            Capacity = min(Capacity)) %>% 
  mapview(cex = "Capacity", zcol = "Startyear")

#######################################################################

# Plot BDGs in 1944
BDG %>% 
  filter(Location_startdate==1944) %>% 
  mapview(cex = "Capacity")


BDG_noBrabrand_sf <- BDG %>% 
  filter(!is.na(Long) | !is.na(Lat) ) %>% 
  filter(Lat != 56.150046 & Long !=10.107009) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) 


# Some shelters changed location, in which case they are mentioned multiple times.  
# How many shelter moves do we have on file? (1-3 moves)
movedBDG <- BDG %>% 
  group_by(BDnr) %>% 
  tally() %>% 
  filter(n>1)

# 8 shelters moved twice times, three thrice, 134 were moved at least once. 

######################################################################

## Let's map the movement of shelters

BDG_lines <- movedBDG %>%
  filter(st_geometry_type(geometry) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

library(mapview)
mapview(BDG_lines, zcol = "BDnr") + mapview(BDG, zcol = "Location_startdate")

## majority of shelters are disposed of to fortify the banks of Brabrand!

######################################################################

## Filter away the Brabrand location
movedBDG_noB <- BDG_noBrabrand_sf %>% 
  group_by(BDnr) %>% 
  tally() %>% 
  filter(n>1)

## Let's map the movements besides Brabrand

BDG_noB_lines <- movedBDG_noB %>%
  filter(st_geometry_type(geometry) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

library(mapview)
mapview(BDG_noB_lines, zcol = "BDnr") + mapview(BDG_noBrabrand_sf, zcol = "Location_startdate")

#####################################################

#GET WFS data stednavne
install.packages("ows4R")
library(ows4R)
library(httr) # generic webservice package

wfs <- "https://api.dataforsyningen.dk/digdag_gml2?service=WFS&request=GetCapabilities&token=036d0986e0412d0a474b85d0384ec2e1"
#review in browser

wfs <- "http://schemas.kms.dk/wfs"

wfs <- "https://api.dataforsyningen.dk/digdag_gml2"

url <- parse_url(wfs)
url$query <- list(service = "wfs",
                  version = "2.0.0", # optional
                  request = "GetFeature",
                  typename = "kms:Stednavn",
                  srsName = "EPSG:4326",
                  token ="036d0986e0412d0a474b85d0384ec2e1"
)

request <- build_url(url)
stednavn <- read_sf(request) # trying to get city placenames
head(stednavn)


st_bbox(st_transform(BDG, 25832))
provinces2 <- read_sf(paste0("WFS:", wfs),
                      layer = "Stednavn",
                       bbox = "566847.7,6212708.6,578856.0,6230253.3"
                     # FEATUREID = ""
)

head(provinces2)

prov2 <- read_sf(paste0("WFS:", wfs),
                 query = "SELECT * FROM Stednavn WHERE NavnID=751"
)

