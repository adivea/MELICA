### BBR data from Ulrik (BBR) with sikringsrum

# This script creates historical sikringsrum spatial data from BBR data provided by Ulrik
# It removes empty geometries and renames columns to English, saves data as bbr_sikringsrum.geojson
# and creates bbr_89 data for historical overview and visualisation
# offers summary mapview and facetted tmaps by decade in undifferentiated dataset

# library 
library(tidyverse)
library(sf)
library(mapview)

# data sample
bbr <- read_csv("data/BBR_SKtest.csv")

# real data
bbr <- read_csv("data/BBR_AarhusAll.csv")

# kommunekode
bbr %>% 
  group_by(kommunekode) %>% 
  tally() # What is 751 - modern sikringsrum code?


# spatialize and get decades
bbr <- bbr %>% 
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
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)

#################################################  VERIFY DATA QUALITY

### Attributes
# any NAs in the 'decade'?  27 built in 2000-2004
bbr %>% 
 # filter(is.na(decade))  
  filter(decade == '2000s') %>% pull(byg026Opførelsesår)

bbr %>% 
  group_by(decade) %>% 
  tally()

#################################################  REMOVE EMPTY GEOMETRIES

### Spatial
sum(st_is_valid(bbr$byg404Koordinat))
sum(st_is_empty(bbr$byg404Koordinat))


## 2 ways of eliminating empty values: 1) st_is_empty() or 2) group and eliminate

# First
bbr <- bbr %>% 
  # filter away valid but empty geometries 
  filter(!st_is_empty(st_as_sf(byg404Koordinat))) %>%
  # rename columns
  rename(ID = id_lokalId, year = byg026Opførelsesår, places = byg069Sikringsrumpladser, 
         code = kommunekode, geometry = byg404Koordinat) %>% 
  # reinitialize renamed geometry column
  st_as_sf(wkt = "geometry", crs = 25832)


# Second
# bbr %>% 
#   #group_by(byg404Koordinat) %>% tally()  #13 empty collections
#  filter(grepl("GEOMETRY", byg404Koordinat)) %>% pull(id_lokalId) -> missingGeo
# 
# #now spatialized bbr
# bbr[bbr$id_lokalId%in%missingGeo,]
# bbr

# save spatial data
st_write(bbr, "output_data/bbr_sikringsrum.geojson", append = FALSE)

#################################################  TEST DATA QUALITY - 

## Testing private shelters and their extent 
private<- st_read("output_data/bbr_sikringsrum.geojson")
private <- private %>% 
  rename(ID = id_lokalId, year = byg026Opførelsesår, places = byg069Sikringsrumpladser, 
         code = kommunekode)

# find and remove empty geometries
sum(st_is_empty(st_as_sf(private$geometry)))

private <- private %>% 
  filter(!st_is_empty(st_as_sf(geometry)))

# save spatial data
st_write(private, "output_data/bbr_sikringsrum.geojson", append = FALSE)


###################################################  - BBR89 - SUMMARY MAPs with MAPVIEW
# quick map
bbr <- st_read("output_data/bbr_sikringsrum.geojson")
bbr <- bbr %>%
  mutate(decade = case_when(
    decade == '1930s' ~ '1180-1939',
    TRUE ~ decade  # Keep the original value for other cases
  )) 
mapview(bbr, zcol = "decade")


bbr_89 <- bbr %>% 
  dplyr::filter(decade < "1990s") 
bbr_89 %>% 
  mapview(zcol = "decade")

###################################################  FACETTED MAPs with TMAP
# Visualize sikringsrum construction and capacity over time
library(tmap)
tmap_options(limits = c(facets.view = 8))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(bbr)+
  tm_facets(by = "decade",
            ncol = 4)+
  tm_bubbles(size = "places")


# Up to 1989
library(tmap)
tmap_options(limits = c(facets.view = 6))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(bbr_89)+
  tm_facets(by = "decade",
            ncol = 3)+
  tm_bubbles(size = "places")

tmap_mode("plot")


##########################################      What next? 
# - reverse geocode
# - intersect with building footprints, crosscheck with archival records, 
# - calculate total capacity per decade and compare with population trends
# - check for duplicates: are all private shelters unique or are some double-entered,
# - because a building got upgraded, etc.?
