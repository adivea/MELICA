### BBR data from Ulrik with sikringsrum

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

### Spatial
st_is_valid(bbr$byg404Koordinat)

bbr %>% 
  #group_by(byg404Koordinat) %>% tally()  #13 empty collections
 filter(grepl("GEOMETRY", byg404Koordinat)) %>% pull(id_lokalId) -> missingGeo

#now spatialized bbr
bbr[bbr$id_lokalId%in%missingGeo,]

bbr
bbr
# save spatial data
st_write(bbr, "output_data/bbr_sikringsrum.geojson")

###################################################  MAPs
# quick map
bbr %>% 
  mapview()

# how many valid spatial points

# Visualize sikringsrum construction and capacity over time
library(tmap)
tmap_options(limits = c(facets.view = 8))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(bbr)+
  tm_facets(by = "decade",
            ncol = 4)+
  tm_bubbles(size = "byg069Sikringsrumpladser")


# What next? 
# - intersect with building footprints, crosscheck with archival records, 
# - calculate total capacity per decade and compare with population trends
  