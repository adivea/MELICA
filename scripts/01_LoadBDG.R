library(tidyverse)
library(googlesheets4)
library(sf)
library(mapview)


# Load Andreas complete data from Google and create a 1987 version (Wide dataset)
BDGw <- read_sheet("https://docs.google.com/spreadsheets/d/1H8EhFgwhDGKCsM95BTjNwifg5K-oXTv2Q4Qbx84k7ZU/edit?gid=0#gid=0",
                  range = "Shelters", 
                  col_types = "dddccddcddcccccdddcccccdddccddccccc")

# Load saved rds based on wide data above
BDGw <- readRDS("output_data/BDG_andreas.rds")
glimpse(BDGw)

BDGw <- BDGw %>% 
  filter(!is.na(Final_Longitude_1987) | !is.na(Final_Latitude_1987) ) %>% 
  st_as_sf(coords = c("Final_Longitude_1987", "Final_Latitude_1987"), crs = 4326) %>% 
  dplyr::select(BDnr_1987,Year_of_Construction,  Final_type, Final_Pub_Size, 
                Needs_Revisit, Final_Longitude_2024, Final_Latitude_2024)

# 10 closely undefined shelters - NEED REVISIT
BDGw %>% 
  filter(is.na(Final_type))

# Summaries of capacities
BDGw %>% 
  group_by(Final_type) %>% 
  summarize(number = n(),
    total_capacity = sum(Final_Pub_Size))

#saveRDS(BDGw, "output_data/BDG_wide.rds")

glimpse(BDGw)
mapview(BDGw, zcol = "Year_of_Construction")
        
######################################################################

# Load Andreas historical "long" data and show through time
BDG <- read_sheet("https://docs.google.com/spreadsheets/d/1C4GEgq4UmEwx_Xi84FiNBrRowmmJHi3S191byjF-hWc/edit?gid=0#gid=0",
                  range = "Ark1", 
                  col_types = "ddcdddcdccdc")

BDG  <- BDG %>% 
  filter(!is.na(Long) | !is.na(Lat) ) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) 

# saveRDS(BDG, "output_data/BDG_long.rds")

glimpse(BDG)

BDG %>% 
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>% 
  rename(id = ID) %>% 
  st_drop_geometry() %>% 
  write_csv("output_data/BDGlong.csv")

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


# Some shelters changed location, in which case they are mentioned multiple times.  
# How many shelter moves do we have on file? (1-3 moves)
movedBDG <- BDG %>% 
  group_by(BDnr) %>% 
  summarize(n = n(),
    year = last(Location_startdate)) %>% 
  filter(n==2)

# 8 shelters moved twice times, three thrice, 134 were moved at least once. 

######################################################################

## Let's map the movement of shelters

BDG_lines <- movedBDG %>%
  filter(st_geometry_type(geometry) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

library(mapview)
mapview(BDG_lines, zcol = "year") + mapview(BDG, zcol = "Location_startdate")

## majority of shelters are disposed of to fortify the banks of Brabrand!

######################################################################

## Filter away the Brabrand location from the csv dataset
BDG_noBrabrand_sf <- read_csv("output_data/BDG_long.csv")%>% 
  filter(!is.na(longitude) | !is.na(latitude) ) %>% 
  filter(latitude != 56.150046 & longitude !=10.107009) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

movedBDG_noB <- BDG_noBrabrand_sf %>% 
  group_by(BDnr) %>% 
  summarize(n = n(),
            year = last(Location_startdate)) %>% 
  filter(n==2)

## Let's map the movements besides Brabrand

BDG_noB_lines <- movedBDG_noB %>%
  filter(st_geometry_type(geometry) == "MULTIPOINT") %>%
  st_cast("LINESTRING")

library(mapview)
mapview(BDG_noB_lines, zcol = "BDnr") + mapview(BDG_noBrabrand_sf, zcol = "Location_startdate")

##################################################### ANIMATE BDG IN TIME

library(gganimate)

?transition_time()
anim_bdg <- ggplot(data = BDG_noBrabrand_sf )+
    geom_sf(aes(color = Location_startdate))+
    theme_bw() +
    transition_time(Location_startdate)+
    labs(subtitle = "Year {round(frame_time,0)}")
anim_bdg 
# Keep the shelters plotted if they were not moved
# Fade out moved shelters' original locations
BDG %>% 
  select(ID, BDnr, Capacity, Location_startdate) %>% 
  arrange(ID)

A <- ggplot(data = BDG )+
  geom_sf(aes(color = Location_startdate))+
  theme_bw() +
  transition_time(Location_startdate)+
  labs(subtitle = "Year {round(frame_time,0)}")

# (In theory) Animate with custom settings to slow down the animation
anim <- animate(A, 
                fps = 10,                # Lower frames per second
                duration = 20,           # Total duration of the animation in seconds
                end_pause = 5)           # Adds a pause at the end of the animation

B <- BDG_noBrabrand_sf %>% 
  rename(year = Location_startdate) %>% 
  ggplot()+
  geom_sf(aes(size = 2, color = year, group = seq_along(year)),show.legend = F)

G2 <- B  +
  transition_states(states = year, state_length = 10, wrap = FALSE)+
   enter_recolor(fill = "#f0f5f9") +
  shadow_mark(past = TRUE, alpha = 1, fill = "#3a6589")

G1 <- B+
  transition_time(time = year, )

BDG_noB_lines

# Animate without rewinding
animate(G2, rewind = FALSE)




    #geom_sf(data = BDG_noB_lines, aes(color = n))
mapview(BDG_noB_lines, zcol = "BDnr") + transition_time("Location_startdate")
#################################################### GET AARHUS BOUNDARY STEDNAVN API

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

#################### Modern cities

## https://dawadocs.dataforsyningen.dk/dok/stednavne

#aarhus <- read_sf("https://api.dataforsyningen.dk/steder?hovedtype=Bebyggelse&undertype=by&primærtnavn=Aarhus&format=geojson")
#st_write(aarhus, "data/DanishCities2024.geojson")
towns <- read_sf("data/DanishCities2024.geojson")
head(towns)
mapview(towns)

aarhus <- towns %>% 
  filter(bebyggelseskode == 11045) 
aarhus %>% 
  mapview()

