### BBR data from Ulrik with sikringsrum

# library 
library(jsonlite)
library(tidyverse)
library(sf)

# data sample
bbr <- read_csv("data/BBR_SKtest.csv")

# kommunekode
bbr %>% 
  group_by(kommunekode) %>% 
  tally() # What is 751 - modern sikringsrum code?


# spatialize and get decades
bbr <- bbr %>% 
  mutate(decade = case_when(
    
    byg026Opførelsesår < 1950 ~ '1940s',
    byg026Opførelsesår < 1960 ~ '1950s',
    byg026Opførelsesår < 1970 ~ '1960s',
    byg026Opførelsesår < 1980 ~ '1970s',
    byg026Opførelsesår < 1990 ~ '1980s',
  )) %>% 
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)


# quick map
bbr %>% 
  mapview()


# Visualize sikringsrum construction and capacity over time
tmap_options(limits = c(facets.view = 5))  # we want to view 5 periods

tmap_mode(mode = "view")

tm_shape(bbr)+
  tm_facets(by = "decade",
            ncol = 3)+
  tm_bubbles(size = "byg069Sikringsrumpladser")


# What next? 
# - intersect with building footprints, crosscheck with archival records, 
# - calculate total capacity per decade and compare with population trends
  