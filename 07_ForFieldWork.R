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

#################################################### FIELDWORK FOR 2024

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
