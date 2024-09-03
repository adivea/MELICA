# -----------------  TEMPORAL WORK WITH BBR PRIVATE SHELTERS

# library 
library(tidyverse)
library(sf)
library(mapview)
library(tsibble)
library(fable)

###  BBR temporal overviews

################################################### Temporal overview

bbr <- st_read("output_data/bbr_sikringsrum.geojson")
names(bbr)

### Oldest buildings that contain private shelters

early_bldg_SK <-  bbr %>% 
  #st_drop_geometry() %>%
  dplyr::select(year, places) %>% 
  distinct() %>% 
  arrange(year) %>% 
  slice(1:10)

early_bldg_SK %>% 
  mapview(cex = "places")
# Year represents the construction year of the building containing the shelter, and not shelter construction, 
# so we get as early years as 1180, 1782, etc!

################### MAP of Shelters in buildings that predate 1950
bbr %>% 
  #st_drop_geometry() %>%
  dplyr::select(year, places) %>% 
  distinct() %>% 
  arrange(year) %>% 
  # filter(year < 1945) %>%  # 64
  #filter(year < 1940 & year > 1200) %>% 
# filter(year > 1939 & year < 1950) %>% 
  filter(year < 1950) %>%   # 72
  summarize(sum = sum(places)) #%>% 
  #tally(places)
  mapview(cex = "places", zcol = "year")

bbr %>% 
  st_drop_geometry() %>% 
  group_by(decade) %>% 
  summarize(buildings = n(), 
            capacity = sum(places))

####################### TIMESERIES SK BLDG CAPACITY per DECADE

bbr_summarized <- bbr %>% 
  st_drop_geometry() %>% 
  group_by(decade) %>% 
  summarize(buildings = n(), 
            capacity = sum(places), 
            avg_places = capacity/buildings) %>%
  mutate(decade = factor(decade, levels = unique(decade), ordered = TRUE))

bbr_summarized %>% 
  filter(decade < "1990s") %>% 
  tally(capacity)

library(ggplot2)

# Define a scaling factor to align the capacity with the number of buildings
scaling_factor <- max(bbr_summarized$buildings) / max(bbr_summarized$capacity)

# Create the combined plot
SK_places_buildings <- ggplot(data = bbr_summarized, aes(x = decade)) +
  # First plot (buildings count)
  geom_line(aes(y = buildings, group = 1), color = "black") +
  # Second plot (capacity) with scaled y values
  geom_line(aes(y = capacity * scaling_factor, group = 1), color = "blue") +
  scale_y_continuous(
    name = "Number of Buildings",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scaling_factor, name = "Capacity (Places)")  # Secondary y-axis label with inverse scaling
  ) +
  labs(
    title = "Buildings and Capacity Over Time by Decade",
    x = "Decade"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "blue")  # Color the secondary y-axis label to match the capacity line
  )

# View the plot
print(SK_places_buildings)  # boring to look at


#################### TIMESERIES BLDG and CAPACITY BY YEAR

bbr_summarized_annual <- bbr %>% 
  st_drop_geometry() %>% 
  filter(year > 1939) %>% 
  group_by(year) %>% 
  summarize(buildings = n(), 
            capacity = sum(places)) 

# Define a scaling factor to align the capacity with the number of buildings (if necessary)
scaling_factor <- max(bbr_summarized_annual$buildings) / max(bbr_summarized_annual$capacity)

# Create the combined plot
SK_bldcapacity_time <- ggplot() +
  # First plot (buildings count)
  geom_line(data = bbr_summarized_annual, aes(x = year, y = buildings), color = "black") +
  # Second plot (capacity) with scaled y values
  geom_line(data = bbr_summarized_annual, aes(x = year, y = capacity * scaling_factor), color = "blue") +
  scale_y_continuous(
    name = "Number of new constructions",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scaling_factor, name = "Shelter capacity (places)")  # Secondary y-axis label with inverse scaling
  ) +
  labs(
    title = "Buildings with private shelters: capacity over time",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "blue")  # Color the secondary y-axis label to match the capacity line
  )

# View the plot
SK_bldcapacity_time

ggsave("figures/SK_bldgcapacity_time.png", width = 7, height = 3 )
ggsave("figures/SK_bldgcapacity_time.tiff", width = 7, height = 3)

###############################################
bbr %>%
  st_drop_geometry() %>%
  filter(year >1900) %>% 
  group_by(year) %>%
  summarize(sum = n())%>%
  ggplot(aes(x = year, y = sum)) +
  geom_line()+
  theme_bw()+
  labs(x = "Building year", 
       y = "Number of buildings containing  private shelters")


### - -- TIMESERIES Shelters in buildings that postdate 1950

SK_year<- bbr %>%
  st_drop_geometry() %>%
  filter(year > 1950) %>% 
  group_by(year) %>%
  summarize(sum = n())%>%
  ggplot(aes(x = year, y = sum)) +
  geom_line()+
  theme_bw()+
  labs(x = "Year of construction", 
       y = "Private shelter containing buildings")
SK_year
ggsave("figures/SK_post1950.png", width = 7, height = 3)
