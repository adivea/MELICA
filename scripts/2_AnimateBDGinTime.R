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
  mutate(year = list(seq(Location_startdate, end_year, by = 1))) %>%
  unnest(year) %>%
  select(-end_year)

expanded_BDG %>% 
  filter(BDnr == 111) %>% 
  print(n =50)


anim_bdg <- ggplot(data = expanded_BDG )+
  geom_sf(aes(color = Type))+
  theme_bw() +
  transition_time(year)+
  labs(subtitle = "Year {round(frame_time,0)}")
anim_bdg 
