############### Buildings IN BBR

## This code was first developed by @JakobJM (Jakob Mørup) for 2021 CDS project visualizing urban development 
# of Aarhus and Copenahgen in 1990-1999, openly shared in his  Github repository
# https://github.com/Jakobjm/Project_Cultural_Data_Science/blob/master/data_visualisation.Rmd

# library 
library(jsonlite)
library(tidyverse)
library(sf)

# download aarhus data from sciencedata.dk (only houses)
# bbr_aarhus_data <- fromJSON("https://sciencedata.dk/public/67e2ad2ca642562dacfa6fdf672a1009/clean_BBR_aarhus_data.json")
# saveRDS(bbr_aarhus_data, "data/bbr_aar_list.rds")

bbr_aarhus_data <- readRDS("data/bbr_aar_list.rds")

#flattening it to a data frame and changing the name of a column to get rid of danish letters
flatten <- function(json){
  json <- as.data.frame(json[["BygningList"]])
  names(json)[names(json)=="byg026Opførelsesår"] <- "byg026Year"
  return(json)
}

#filtering for buildings used for recreational,educational,habitation,health, 
# light- and service industry out discarding other buildings, 
# refer to BBRkodelister20210322 for the complete list of codes
clean_bbr_df <-function(df, year_cutoff){
  df %>% 
    filter(!is.na(byg404Koordinat)) %>% 
    filter(byg404Koordinat != "POINT(0 0)") %>% 
    filter(byg406Koordinatsystem == "5") %>% 
    filter(!is.na(byg026Year)) %>% 
    filter(byg021BygningensAnvendelse %in% 
             c("101","120","121","122","131","132","140","150","160","190","236","321","322","324","331","332","333","334","339","421","422","429","431","432","433","439","441","532","533")) %>% 
    distinct(id_lokalId,.keep_all = TRUE) %>% 
    filter(byg026Year >= year_cutoff)
}

bbr_aarhus_data_flat <-bbr_aarhus_data %>%
  flatten() %>% 
  clean_bbr_df(1900) %>% 
  mutate(city = "Aarhus")%>% 
  st_as_sf(wkt = "byg404Koordinat", crs = 25832)

# trying the sikringsrum code "236" but to no avail >> no records
bbr_aarhus_sikring <- bbr_aarhus_data_flat %>% 
  filter(byg021BygningensAnvendelse == "236")

library(mapview)
mapview(head(bbr_aarhus_data_flat))

# overview of years
table(bbr_aarhus_data_flat$byg026Year)

bbr_aarhus_data_flat %>% 
  filter(byg026Year>1960) %>% 
  group_by(id_lokalId) %>% 
  #pull(byg026Year) %>% 
  tally()
  table()
#loading distric geoJSONs to get mappable district polygons
#aarhus_districts <- st_read("https://sciencedata.dk/public/67e2ad2ca642562dacfa6fdf672a1009/aarhus_districts.geojson")


# Creating dataframes for intersections of the district polygons and the point data from BBR
intersections_aarhus <- st_intersection(aarhus_districts,bbr_aarhus_data_flat) %>% 
  dplyr::select(id = id_lokalId, navn = prog_distrikt_navn,byg021BygningensAnvendelse,byg026Year,byg054AntalEtager,byg406Koordinatsystem,city,geometry)

## making Histograms of average building height measured in floors.

plot_hist <- function(Year, df){
  temp_df_histogram <- 
    get_year_df(df, Year) %>%
    dplyr::select(byg026Year, byg054AntalEtager) %>% 
    group_by(byg026Year) %>% 
    summarise_at(vars(byg054AntalEtager),funs(mean(., na.rm =TRUE)))
  
  
  ggplot(temp_df_histogram,aes(x = byg026Year, y= byg054AntalEtager))+
    geom_col(fill = "#76EE00", color = "black")+
    labs(x= "Years", y= "Mean heigth in floors")+
    ggtitle(paste(df$city[1]))+
    theme_minimal()
}

plot_hist(c(1990:1999), intersections_aarhus)