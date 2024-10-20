####################################################### 2023 DATA and 2024 TOMMY'S DATA - SPATIAL COMPARISON

# compare Tommy's GE verification with MELICA 2023 data
s23 <- read_sf("output_data/shelters23.geojson")
aar <- readRDS("data/gadm36_DNK_2_sp.rds")
aar <- aar %>% 
  st_as_sf() %>% 
  #slice(31)
  dplyr::filter(NAME_2 == "Århus")

# 2023 covered area
ch <- st_as_sf(st_convex_hull(st_union(s23)))

mapview(s, zcol = "verified") + mapview(s23) + mapview(aar)  

#################################################### FIELDWORK FOR 2024 on basis of TOMMY's and 2023 data

s23buff <- st_buffer(s23, 50)
st_is_valid(s23buff)

# WHAT WE HAVE
# intersecting Tommy's KML points with MELICA 2023 collected points by buffer of 50m
s %>% 
  #st_filter(s23buff, .join = intersects) %>% 
  st_filter(s23buff, .predicate = st_intersects) %>%   # 93 overlap with 30m buffer, 101 with 50m buffer
  mapview()

# WHAT WE NEED TO VISIT
# non intersecting
tovisit24 <- s[st_intersects(s, s23buff) %>% lengths == 0,] # 107 non-overlapping features with 30m buffer, 
# 99 in 50m buffer, so ca 30 inside the city

mapview(tovisit24, zcol = "verified") + mapview(s23)



######### IN AND OUT OF TOWN

### remaining area for 2024 outside of the city
outoftown <- st_difference(s, ch)  # 78

# view
mapview(outoftown, zcol = "verified")  + mapview(s23) +mapview(aar)

### remaining inside the town for 2024 doublecheck
intown <- st_intersection(tovisit24, ch)  # 26
# view
mapview(intown, zcol = "verified") + mapview(aar)


################# EXPORT STUFFS

st_write(outoftown, "output_data/2024_out.geojson")
st_write(intown, "output_data/2024_in.geojson")
st_write(tovisit24, "output_data/2024_needlgv.geojson")

tovisit24 %>% 
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  write_csv("output_data/visit2024.csv")


##########################################  SPATIAL aggregation of verified and unverified

# there are no shared attributes because Tommy often has one point for multiple features
# we need to do a spatial join
names(shelter_t_type)
names(s)

s <- s %>% 
  mutate(Source = "Tommy") 

# I am overlapping our verified points over Tommy's points to get true points
# where he clusters multiple shelters under 1 point
# result in 182 points

intersecting_points <- sh_typesclean %>% 
  mutate(Source = "FAIMS23") %>% 
  st_buffer(50) %>% 
  st_join(s, join = st_intersects)  %>%  # figure out which intersect and with what
  st_centroid() %>% #glimpse()  # convert back from polyogns (buffers) to points 
  rename(Source = Source.x) %>% 
  group_by(FeatureID) %>% 
    tally() %>% arrange(desc(n))
  # group_by(Source.x) %>% 
# tally()
# mapview(zcol = "verified")

# Difference between Tommy and FAIMS data

# First, create a union of all buffer geometries
buffer_union <- sh_typesclean %>% 
  mutate(Source = "FAIMS23") %>% 
  st_buffer(50) %>% 
  st_union() %>% 
  st_make_valid()

all.equal(st_crs(s), st_crs(buffer_union))

# Use st_difference to find points that are not within the buffer_union
non_intersecting_Tpoints <- s[st_difference(s, buffer_union), ]

# View the result
mapview(non_intersecting_Tpoints, zcol = "verified") + mapview(intersecting_points)

# Save results
st_write(intersecting_points, "output_data/TF_verified23.geojson", append = FALSE)  # Tommy's points we verified with FAIMS in 2023
st_write(non_intersecting_Tpoints, "output_data/TF_unverified.geojson") # TOmmy's points we need to visit

library(sf)
tovisit24 <- read_sf("output_data/2024_needlgv.geojson")
st_write(tovisit24,"output_data/2024_needlgv.shp")
shelter$identifier


########################################################
########################  ANDREAS' SUGGESTIONS ON BASIS OF STREAMLINING

andreas <- read_sheet("https://docs.google.com/spreadsheets/d/1H8EhFgwhDGKCsM95BTjNwifg5K-oXTv2Q4Qbx84k7ZU/edit?gid=0#gid=0",
                      range = "Shelters", 
                      col_types = "ddcccddcddccccccdddcccccdddcdddcccc")
#filter those needing review in 2024
andreas <- andreas %>% 
  #filter(Needs_Revisit == "No") # 144 surveyed
  filter(Needs_Revisit != "No") %>% # 135 need something verified
  filter(!is.na(Final_Longitude_1987)) %>%  # 134 valid coordinates
  st_as_sf(coords = c("Final_Longitude_1987","Final_Latitude_1987" ), crs = 4326)

andreas %>% glimpse()

andreas %>% 
  mapview(zcol  = "Coordinats_changed_by_Andreas")

st_write(andreas, "output_data/andreas_shelters.geojson")

andreas <- st_read("output_data/andreas_shelters.geojson")
andreas %>% 
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  write_csv("output_data/andreas20240901.csv")
