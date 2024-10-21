# -----------------  TEMPORAL WORK WITH BBR PRIVATE SHELTERS

# library 
library(tidyverse)
library(sf)
library(mapview)
library(tsibble)
library(fable)

### SR and BBR temporal overviews

################################################### Temporal overview

SR <- st_read("output_data/bbr_sikringsrum.geojson")
names(SR)

### Oldest buildings that contain private shelters

early_bldg_SK <-  SR %>% 
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
SR %>% 
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

SR %>% 
  st_drop_geometry() %>% 
  group_by(decade) %>% 
  summarize(buildings = n(), 
            capacity = sum(places))


#################### TIMESERIES BLDG and CAPACITY BY YEAR

SR_summarized_annual <- SR %>% 
  st_drop_geometry() %>% 
  filter(year > 1939) %>% 
  group_by(year) %>% 
  summarize(buildings = n(), 
            capacity = sum(places)) 

# Define a scaling factor to align the capacity with the number of buildings (if necessary)
scaling_factor <- max(SR_summarized_annual$buildings) / max(SR_summarized_annual$capacity)

# Create the combined plot
SK_bldcapacity_time <- ggplot() +
  # First plot (buildings count)
  geom_line(data = SR_summarized_annual, aes(x = year, y = buildings), color = "black") +
  # Second plot (capacity) with scaled y values
  geom_line(data = SR_summarized_annual, aes(x = year, y = capacity * scaling_factor), color = "blue") +
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
SR %>%
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

SK_year<- SR %>%
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


######################################### TIMESERIES: SIKRINGSRUM BLDG and CAPACITY BY YEAR and BBR CONSTRUCTIONS

bbr_aarhus_data_flat <- readRDS("output_data/bbr_residential_aarhus.rds")
all_bbr_aarhus <- readRDS("output_data/bbr_all_aarhus.rds")
all_bbr <- all_bbr_aarhus %>%   # 50,000 entries
  filter(byg026Year >1935 & byg026Year <2005) %>% 
  mutate(decade = case_when(
    byg026Year < 1940 ~ '1930s',
     byg026Year < 1950 ~ '1940s',
     byg026Year < 1960 ~ '1950s',
     byg026Year < 1970 ~ '1960s',
     byg026Year < 1980 ~ '1970s',
     byg026Year < 1990 ~ '1980s',
     byg026Year < 2000 ~ '1990s',
     byg026Year < 2010 ~ '2000s'
  )) %>% 
  group_by(byg026Year) %>%
  summarise(count = n())
res_bbr <- bbr_aarhus_data_flat %>%  # 30,000 entries
  filter(byg026Year >1935 & byg026Year <2005) %>% 
  group_by(byg026Year, byg054AntalEtager) %>%
  summarise(count = n()) %>% 
  mutate(decade = case_when(
      byg026Year < 1940 ~ '1930s',
      byg026Year < 1950 ~ '1940s',
      byg026Year < 1960 ~ '1950s',
      byg026Year < 1970 ~ '1960s',
      byg026Year < 1980 ~ '1970s',
      byg026Year < 1990 ~ '1980s',
      byg026Year < 2000 ~ '1990s',
      byg026Year < 2010 ~ '2000s'
    ))


# statistics
res_bbr %>% 
  st_drop_geometry() %>% 
  filter(byg054AntalEtager>2) %>% 
  group_by(decade) %>% 
  summarize(sum = sum(count))
  

range(SR$places)
sort(unique(SR$places))
# proof of concept 
SR %>%
  st_drop_geometry() %>%
  filter(year > 1935 & year <2005) %>% 
  group_by(year) %>%
  summarize(count = n())%>%
  ggplot(aes(x = year, y = count, color = "With private shelters")) +
  geom_line()+
  theme_bw()+
  geom_line(data = all_bbr, aes(x = byg026Year, y = count, color = "All BBR")) +
  geom_line(data = res_bbr, aes(x = byg026Year, y = count, color = "Residential")) +
  labs(x = "Year", y = "Number of new constructions", title = "New constructions per year in Aarhus") +
  scale_color_manual(name = "Datasets", values = c("With private shelters" = "darkgrey",  "All BBR" = "blue", "Residential" = "red")) +
  theme_minimal()

# Full figure
# Create a scaling factor for SR dataset (adjust as necessary)
scale_factor <- 10

# Plot
SR %>%
  st_drop_geometry() %>%
  filter(year > 1935 & year < 2005) %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = year, y = count * scale_factor, color = "With private shelters")) + # Apply scale factor
  geom_line(size = 1.5) +
  theme_bw() +
  
  # Add the BBR datasets as additional geom_line
  geom_line(data = all_bbr, aes(x = byg026Year, y = count, color = "All buildings"), size = 1.5) +
  geom_line(data = res_bbr, aes(x = byg026Year, y = count, color = "Residential"), size = 1.5) +
  
  # Set the labels for primary y-axis and title
  labs(x = "Year", y = "Number of new constructions", 
       title = "New constructions per year in Aarhus (based on BBR)") +
  
  # Custom colors for the datasets
  scale_color_manual(name = "Datasets", 
                     values = c("With private shelters" = "red", 
                                "All buildings" = "grey", 
                                "Residential" = "grey5")) +
  
  # Add a secondary y-axis for the SR dataset
  scale_y_continuous(
    sec.axis = sec_axis(~./scale_factor, name = "Buildings with private shelters")  # Undo the scaling for the secondary axis
  ) +
  
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),  # Color for the secondary y-axis title
    axis.text.y.right = element_text(color = "red"),   # Color for the secondary y-axis tick labels
    axis.line.y.right = element_line(color = "red"),   # Color for the secondary y-axis line
    
    # Move legend inside the plot to the top-left corner
    legend.position = c(0.02, 0.98),  # Top-left inside the plot (x, y coordinates from 0 to 1)
    legend.justification = c("left", "top"),  # Justify the legend box to align with the top-left
    legend.background = element_rect(fill = "white", color = NA),  # White background for the legend
    legend.title = element_blank() # Bold legend title    # Color for the secondary y-axis line
  )

ggsave(filename = "figures/all_construction_trend1935_2005.png", width = 7, height = 4)
ggsave(filename = "figures/all_construction_trend1935_2005.tiff", width = 7, height = 4, dpi = 300)
