# Animate BDG

# Lengthen BDG dataset
expanded_BDG <- BDG %>%
  select(BDnr, Type, Capacity, Location_startdate) %>% 
  group_by(BDnr) %>%
  arrange(Location_startdate) %>%
  mutate(end_year = lead(Location_startdate, default = 2024) - 1) %>%
  ungroup() %>%
  mutate(geometry = if_else(is.na(geometry), lag(geometry), geometry)) %>%
  rowwise() %>%
  mutate(year = list(seq(Location_startdate, end_year, by = 5))) %>%
  unnest(year) %>%
  select(-end_year)

glimpse(expanded_BDG)


## Animation one (too fast and choppy)
anim_bdg <- ggplot(data = expanded_BDG )+
  geom_sf(aes(color = Type))+
  theme_bw() +
  transition_time(year)+
  labs(subtitle = "Year {round(frame_time,0)}")
anim_bdg 

## 
p <- ggplot(BDG, aes(group = BDnr)) +
geom_sf(aes(color = Type), size = 2, show.legend = FALSE) +
  labs(title = 'Year: {round(frame_time,0)}') +
  theme_minimal()

# Animate the plot
anim <- p +
  transition_time(Location_startdate) + 
  ease_aes('linear')  # Ensures smooth transition

# Render the animation (takes too long)
# animate(anim, duration = 20, fps = 10, width = 500, height = 700)


################ SPLIT THE DATA

static_shelters <- BDG %>% 
  group_by(BDnr) %>% 
  filter(n() == 1) %>%  # Assumes there's only one record for static shelters
  ungroup()

static_BDG <-static_shelters %>%
  select(BDnr, Type, Capacity, Location_startdate) %>% 
  group_by(BDnr) %>%
  arrange(Location_startdate) %>%
  mutate(end_year = lead(Location_startdate, default = 2024) - 1) %>%
  ungroup() %>%
  mutate(geometry = if_else(is.na(geometry), lag(geometry), geometry)) %>%
  rowwise() %>%
  mutate(year = list(seq(Location_startdate, end_year, by = 1))) %>%
  unnest(year) %>%
  select(-end_year)


moving_shelters <- BDG %>% 
  group_by(BDnr) %>% 
  filter(n() > 1) %>%  # More than one record indicates movement
  ungroup()

moving_BDG <- moving_shelters %>%
  select(BDnr, Type, Capacity, Location_startdate) %>% 
  group_by(BDnr) %>%
  arrange(Location_startdate) %>%
  mutate(end_year = lead(Location_startdate, default = 2024) - 1) %>%
  ungroup() %>%
  mutate(geometry = if_else(is.na(geometry), lag(geometry), geometry)) %>%
  rowwise() %>%
  mutate(year = list(seq(Location_startdate, end_year, by = 5))) %>%
  unnest(year) %>%
  select(-end_year)


# Create the base plot with static shelters
p <- ggplot() +
  geom_sf(data = static_BDG, color = "darkgrey", size = 2) + 
  labs(title = 'Year: {round(frame_time,0)}') +
  theme_minimal()

# Add moving shelters with animation
p2 <- p + geom_sf(data = moving_BDG, aes(color = Type), size = 2, show.legend = FALSE) +
  transition_time(year) +
  ease_aes('cubic-in-out') +  # Change easing function for smoother transition
  shadow_mark(past = TRUE, future = FALSE, size = 1, alpha = 0.3)  # Show past positions as shadow
p2

# Save the animation if needed
anim_save("figures/shelters_over_time.gif", animation = p)

# Render the animation
anim <- animate(p, duration = 20, fps = 10, width = 500, height = 500)
