### Geocoding_BBR

install.packages("opencage")  
library(opencage)
help("oc_config")
base::interactive() # to interactively put in the API key
oc_config()

######################## Test the Workflow


# test data
bbr <- read_csv("data/BBR_SKtest.csv")

bbr %>% 
  group_by(byg404Koordinat) %>% 
  tally()

bbr <- bbr %>% 
  st_transform(crs = 4326) %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1])


# lets take a small piece as the free API is limited 
mapview(bbr[1:10,])

# Reverse geocode : get addressses from GPS points
bbr_address_only <- oc_reverse(bbr$latitude[1:10], bbr$longitude[1:10] , abbrv = TRUE,  address_only = TRUE)
bbr_address_only

# Recommended test workflow at https://docs.ropensci.org/opencage/articles/output_options.html
library(dplyr, warn.conflicts = FALSE)

bbr_oc_add <- bbr %>% 
  slice(1:10) %>% 
  select(-kommunekode) %>% 
  mutate(oc_result = oc_reverse(latitude, longitude, abbrv = TRUE,  address_only = TRUE))

bbr_oc_add %>% unnest(oc_result)




##################################### Real run

# real data
bbr <- read_csv("data/BBR_AarhusAll.csv")

# spatialize and get decades
bbr <- bbr %>% 
  filter(!grepl("GEOMETRY", byg404Koordinat)) %>%  # only valid coordinates
  mutate(decade = case_when(
    byg026Opførelsesår < 1940 ~ '1930s',
    byg026Opførelsesår < 1950 ~ '1940s',
    byg026Opførelsesår < 1960 ~ '1950s',
    byg026Opførelsesår < 1970 ~ '1960s',
    byg026Opførelsesår < 1980 ~ '1970s',
    byg026Opførelsesår < 1990 ~ '1980s',
    byg026Opførelsesår < 2000 ~ '1990s',
    byg026Opførelsesår < 2010 ~ '2000s'
  )) %>% 

  st_as_sf(wkt = "byg404Koordinat", crs = 25832) %>% 

# convert to 4326
 
  st_transform(crs = 4326) %>% 
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1])

bbr


# The workflow at https://docs.ropensci.org/opencage/articles/output_options.html
library(dplyr, warn.conflicts = FALSE)

bbr_oc_add <- bbr %>% 
  select(-kommunekode) %>% 
  mutate(oc_result = oc_reverse(latitude, longitude, abbrv = TRUE,  address_only = TRUE))

bbr_oc_addresses <- bbr_oc_add %>% unnest(oc_result)

# save full output
saveRDS(bbr_oc_addresses, "output_data/SK_bbr_oc_addresses.rds") # won't save as geojson

# save spatial output
bbr_oc_addresses %>% 
  select(id_lokalId:oc_formatted, oc_category, oc_type) %>% 
  st_write("output_data/SK_bbr_oc_addresses.geojson")

# save addresses
bbr_oc_addresses %>% 
  select(id_lokalId:oc_formatted, oc_category, oc_type) %>% 
  st_drop_geometry() %>% 
  write_csv("output_data/SK_bbr_oc_addresses.csv")

# TASK COMPLETED
test <- readRDS("output_data/SK_bbr_oc_addresses.rds")
