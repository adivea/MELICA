### Geocoding_KON Cmbined public shelters

#install.packages("opencage")  
library(opencage)
help("oc_config")
base::interactive() # to interactively put in the API key from https://opencagedata.com/api
oc_config()

######################## Test the Workflow

#KOB data
library(googlesheets4)
library(tidyverse)
library(mapview)
library(leaflet)
library(sf)

gs4_deauth()

KOB <- read_sheet("https://docs.google.com/spreadsheets/d/1kBkz2PdR_jMYIx5Y014oWMiVepXb4sQVx9fc30Vwn4A/edit?usp=sharing", 
                  range = "KOB",
                  col_types = "dccccdcddcccc" )


################# PARSE THE MANUAL COORDINATES

glimpse(KOB)
KOB$Coords

KOB <- KOB %>%
  mutate(
    Latitude = as.numeric(sub(",.*", "", Coords)),  # Extract longitude before the comma
    Longitude = as.numeric(sub(".*, ", "", Coords))   # Extract latitude after the comma
  ) %>% 
  filter(!is.na(Longitude) | !is.na(Latitude)) %>%  # Filter out missing values
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

saveRDS(KOB, "./output_data/KOB_sf.rds")

################# GEOCODE FROM ADDRESSES (for comparison)

# Clean address column for geocoding
KOB <- KOB %>% 
  mutate(address = str_split(Address, "/") %>% map_chr(., 1)) %>%  # Keeps only the first address part

  mutate(address = if_else(row_number() == 17, 
                           paste(address, ", Denmark"), 
                           paste(address, ", Aarhus, Denmark")))%>%
  mutate(address = str_replace_all(address, " ,", ","))

# alternative breaking of corner addresses
#KOB$address[2] <- "Tordenskjoldsgade 30, Aarhus, Denmark"

# review
KOB$address
KOB$Name

# geocode
KOB_ll <- oc_forward_df(data = KOB, placename = address, 
                        bind_cols = TRUE, 
                        countrycode = "DK")


leaflet() %>% 
  addTiles()%>% 
  addCircleMarkers(lng = KOB_ll$oc_lng, lat = KOB_ll$oc_lat, radius = sqrt(KOB_ll$Capacity),
                   color = KOB_ll$Year)

KOB_ll %>% 
  #distinct(oc_formatted) %>% 
  st_as_sf(coords = c("oc_lng", "oc_lat"), crs = 4326) %>% 
  mapview(cex = "Capacity")

KOB_ll %>% 
  select(Address, address,oc_formatted) %>% 
  print(n = 20)
  
KOB_2 <- oc_forward(placename = "Tordenskjoldsgade 30, Aarhus, Denmark")
KOB_2 %>% 
  slice()
