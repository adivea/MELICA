## Field-visited BDG - statistical overview of insights for IJHA paper
# I use Andreas table wide, which includes dta on 2023 and 2024 fieldwork

library(tidyverse)
library(googlesheets4)
library(sf)
library(mapview)

# 
s23 <- read_sf("output_data/sh_types2023.geojson")
s23 <- readRDS("output_data/shelters23.rds")
s24 <- readRDS("data/SheltersVerified24.rds")
mapview(s23) + mapview(s24)
s23 %>% 
  filter(FeatureID == 265|FeatureID == 266) %>% mapview()
# Load Andreas complete data from Google and create a 1987 version (Wide dataset)
BDGw <- read_sheet("https://docs.google.com/spreadsheets/d/1H8EhFgwhDGKCsM95BTjNwifg5K-oXTv2Q4Qbx84k7ZU/edit?gid=0#gid=0",
                   range = "Shelters", 
                   col_types = "dddccddcddccccccdddcccccdddccddcccc")

# Load saved rds based on wide data above
glimpse(BDGw)

BDGw <- BDGw %>% 
  filter(!is.na(Final_Longitude_1987) | !is.na(Final_Latitude_1987) ) %>% 
  st_as_sf(coords = c("Final_Longitude_1987", "Final_Latitude_1987"), crs = 4326) %>% 
  dplyr::select(BDnr_1987, Final_type, Final_Pub_Size, FAIMS_verified,
                Needs_Revisit, Final_Longitude_2024, Final_Latitude_2024, Status_1987, Status_2024)

# Summaries of capacities
BDGw %>% 
  group_by(Final_type) %>% 
  summarize(number = n(),
            total_capacity = sum(Final_Pub_Size))

# Summary of visited/unvisited 
BDGw %>% 
  group_by(FAIMS_verified) %>% 
  tally()


BDGw %>% 
 filter(grepl("^Need", Needs_Revisit)) 
#7

# according to Andreas's data , howe many total locations we visited? (202 between the extant and destroyed ones)
BDGw %>% 
  group_by(Status_2024) %>% 
  tally()


BDGw %>% 
  filter(FAIMS_verified == "Yes") %>% 
  group_by(Status_2024) %>% 
  tally()

BDGw %>% 
  filter(FAIMS_verified == "Yes") %>% 
  filter(Status_2024 == "Moved") 

# Typology of extant shelters
BDGw %>% 
  filter(FAIMS_verified == "Yes") %>% 
  filter(Status_2024 == "Exists") %>% 
  group_by(Final_type) %>% 
  tally()

# Which need to be seen and why? (ca. 20, mostly removed or destroyed)
BDGw %>% 
  st_drop_geometry() %>% 
  group_by(Needs_Revisit) %>% tally() %>% print(n=40)

# openness
s23 %>% 
  filter(Accessible == "Open") # 5 in 2023

s24 %>% 
  filter(`accessiblity-of-shelter-during-visit` == "Open")  # 3 in 2024 +1 reported by Sahel

s23 %>% 
  group_by(LanduseAround) %>% 
  tally()
s24 %>% 
  group_by(`landuse-around`) %>% 
  tally()

# Which of the unvisited, do NOT NEED a visit (ie. they have not been moved acc to Google Earth)
BDGw %>% 
  filter(FAIMS_verified == "No") %>% # 45
  filter(grepl("^Need", Needs_Revisit)) %>%  #42
  mapview() + mapview(s23)
