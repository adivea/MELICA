
# Load Andreas complete data from Google and create a 1987 version (Wide dataset)
BDGw <- read_sheet("https://docs.google.com/spreadsheets/d/1H8EhFgwhDGKCsM95BTjNwifg5K-oXTv2Q4Qbx84k7ZU/edit?gid=0#gid=0",
                   range = "Shelters", 
                   col_types = "dddccddcddcccccdddcccccdddccddccccc")

# Load saved rds based on wide data above
BDGw <- readRDS("output_data/BDG_andreas.rds")
glimpse(BDGw)
BDGw$Needs_Revisit
BDGw <- BDGw %>% 
  filter(!is.na(Final_Longitude_1987) | !is.na(Final_Latitude_1987) ) %>% 
  st_as_sf(coords = c("Final_Longitude_1987", "Final_Latitude_1987"), crs = 4326) %>% 
  dplyr::select(BDnr_1987,Year_of_Construction,  Final_type, Final_Pub_Size, 
                Needs_Revisit, Final_Longitude_2024, Final_Latitude_2024)



verified24 <- read_csv("data/SheltersALL2024.csv")
data_sf <- verified24 %>% 
  select(- `take-point-from-gps`) %>% 
  st_as_sf(coords = c("take-point-from-gps_longitude", 
                      "take-point-from-gps_latitude"), crs = 4326)
mapview(data_sf)
verified24_1 <- read_sf("../MELICA-Field/data/Fieldmark/fieldmark_20241012.geojson")%>% select(- `take-point-from-gps`)
verified24_2 <- read_sf("../MELICA-Field/data/Fieldmark/fieldmark_20240922.geojson") %>% select(- `take-point-from-gps`)
mapview(data_sf, cex = 4) + mapview(verified24_2, cex = 4) +mapview(verified24_1)

verified24_1$id
verified24_2$id %>% 
  filter(id > 198 & id < 287) %>% pull(id)
cols <- names(verified24_2)
ver <- rbind(verified24_2[,cols], verified24_1[,cols])

names(data_sf)
names(ver)

duplicated <- ver$id %in% data_sf$id
`%nin%` <-  Negate(`%in%`)

ver24 <- ver %>% 
  filter(id %nin% data_sf$id) %>% 
  rbind(data_sf[,cols])
names(ver24)  
ver24 <- ver24[, c(1,6:8, 12:17, 19:21, 23:26,28:31)]
glimpse(ver24)

saveRDS(ver24, "data/SheltersVerified24.rds")

mapview(ver24, cex = 4, zcol = "shelter-type") +mapview(verified, cex = 4)

BDGw %>% 
  # filter(Needs_Revisit != "Needs FAIMS" &
  #          Needs_Revisit !=  "Needs FAIMS, unsure if still exist") %>%
   mapview(zcol = "Final_type", cex = 5) 
 

m1 <- mapview(ver, cex = 4, zcol = "shelter-type") 
m2 <- mapview(data_sf, cex = 4,zcol = "shelter-type")
library(leafsync)
sync(m1,m2)

static_BDG %>% distinct(BDnr) # 191
moving_BDG %>% distinct(BDnr) # 147
