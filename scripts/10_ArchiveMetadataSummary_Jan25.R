# --- Checking Aarhus CIty Archive Digitisation progress


# Libraries
library(tidyverse)
library(googlesheets4)
# install.packages("fable")
library(tsibbledata)
library(fable)
library(lubridate)

# Load metadata of scanned documents 2024-08-21
gs4_deauth()
metadata <- read_sheet("https://docs.google.com/spreadsheets/d/1iEA_Hr0Yr0rdHP60w-TaBnn6wocpLQ-xT8xggTlHd2Q/edit?gid=0#gid=0",
                       range = "CDdata", 
                       col_types = "ccDDccccccccccccc") 
glimpse(metadata)

# Clean column names

metadata <- metadata %>% 
  rename(ID = ScanningsID, Startdate = `Dato fra (åååå-mm-dd)`, Enddate = `Dato til (åååå-mm-dd)`, Contenttype2 = `Indholdstype 2`,
         Contenttype3 =  `Indholdstype 3`, Contenttype4 = `Indholdstype 4`, Handwriting = `Indeholder håndskrift`, Visuals = `Mulige problemer med OCR`) %>% 
  dplyr::select(ID, Startdate, Enddate, Beskrivelsesnoter, Contenttype2, Contenttype3, Contenttype4, Kommentar, Handwriting, Visuals, EnhedsID )

table(metadata$Startdate)
names(metadata)
unique(metadata$EnhedsID)


# ---Classification of documents by content (Map, Visuals, Text)

metadata <- metadata %>%
  mutate(
    Type = case_when(
      str_detect(Contenttype2, regex("kort", ignore_case = TRUE)) |
        str_detect(Contenttype3, regex("kort", ignore_case = TRUE)) |
        str_detect(Contenttype4, regex("kort", ignore_case = TRUE)) ~ "map",
      str_detect(Contenttype2, regex("billed", ignore_case = TRUE)) |
        str_detect(Contenttype3, regex("billed", ignore_case = TRUE)) |
        str_detect(Contenttype4, regex("billed", ignore_case = TRUE)) ~ "plan",
      Visuals == "x" ~ "graphic",
      TRUE ~ "text"
    )
  )


metadata %>% 
  group_by(Type) %>% 
  summarize(sum = n(),
            percent = round(sum / nrow(metadata) *100, 1))

# ------- Start with time

# Convert the tibble into a temporal tsibble (lose 20 records without date)
m_ts <- as_tsibble(metadata %>% filter(!is.na(Startdate)), key = ID, index = Startdate)

#saveRDS(m_ts, "output_data/archivedocs20240820_tsbl.rds")

# Aggregate publication dates over monthly periods
df_monthly <- m_ts %>%
  filter(Startdate > "1920-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type
publ_monthly <- m_ts %>% 
  filter(Startdate > "1920-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Type) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type, and 'date' type for ggplot
docs_monthly <- publ_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# ---- Initial visualisation test - all documents, typed docs, and a grid

autoplot(df_monthly, count) +
  labs(
    title = "Civil Defence Commission document output per Month",
    x = "Year-Month",
    y = "Number of archived documents"
  ) +
  theme_minimal()
ggsave("figures/archiveddocs_20250123.png")


# Aggregate by group publication dates over monthly periods

autoplot(publ_monthly, count) +
  labs(
    title = "Civil Defence Commission document output per Month",
    x = "Year-Month",
    y = "Number of archived documents"
  ) +
  guides(colour = guide_legend(title = "Document type")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),  # Optional: Add a background to the legend for better visibility
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8)  # Adjust legend text size
  )# Adjust these values to move the legend
  
ggsave("figures/archiveddocsType_20250123.png")


# Types in a grid per month

ggplot(docs_monthly, aes(x = year_month, y = count, color = Type)) +
  geom_line() +
  facet_grid( Type ~ .) +
  theme_minimal()


#  ----   Combine visual overview into two
install.packages("patchwork")
library(patchwork)

plain <- autoplot(df_monthly, count) +
  labs(
    title = "Civil Defence Commission document output per month",
    x = NULL,
    y = "Archived documents",
  ) +
  theme_minimal()

typed <- autoplot(publ_monthly, count) +
  labs(
    x = NULL,
    y = "Documents by type"
  ) +
  guides(colour = guide_legend(title = "Document type")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.6),
    legend.background = element_rect(fill = "white", color = "black"),  # Optional: Add a background to the legend for better visibility
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8)  # Adjust legend text size
  )# Adjust these values to move the legend

combined_plot <- plain / typed 
combined_plot

ggsave("figures/Figure05.png", width = 8, height = 4 )
ggsave("figures/Figure05.tiff", width = 8, height = 4 )


# grab SR_year or SR from bbr subset 11_TemporalOverview

SR <- st_read("output_data/SR_sikringsrum.geojson")
SR_year<- SR %>%
  st_drop_geometry() %>%
  filter(year > 1920) %>% 
  group_by(year) %>%
  summarize(sum = n())%>%
  ggplot(aes(x = year, y = sum)) +
  geom_line()+
  theme_minimal()+
  labs(x = "Year of construction", 
       y = "Private shelter containing buildings")

SR_year

combined_plot <- plain / SR_year
combined_plot




###################### PLOT ARCHIVE VS SK CONSTRUCTION WITH TWO AXES:

library(lubridate)
library(tsibble)

# Aggregate publication dates over yearly periods
df_annual <- m_ts %>% 
  tsibble::index_by(year = ~ year(.)) %>% # annual aggregates
  summarise(count = n()) 

# First plot (primary axis)
plot1 <- autoplot(df_annual, count) +
  labs(
    title = "Civil Defence Commission document output per Month",
    x = NULL,
    y = "Archived documents"
  ) +
  theme_minimal()

# Second plot (secondary axis)

# Aggregate SR into tsibble
SR_ts <- SR %>%
  st_drop_geometry() %>%
  filter(year > 1920) %>%
  mutate(year_dec = ymd(paste(year, "12", "31", sep = "-")))  %>%   # Create year_month with December
  as_tsibble(index = year_dec, key = ID) %>% 
  tsibble::index_by(year_month = ~ yearmonth(.))

# Group by the year_month column and count the number of occurrences

SR_ts_annual <- SR_ts %>%
  tsibble::index_by(year = ~ year(.)) %>% # annual aggregates
  summarise(count = n()) 

glimpse(SR_ts_annual)

# Overlay the plots
# Define a scaling factor to align the second plot's y-axis with the first plot's y-axis
scaling_factor <- 2  # You can adjust this factor to get the desired scaling

# Combined plot
plot_combined <- ggplot() +
  # First plot (primary axis)
  geom_line(data = df_monthly, aes(x = year_month, y = count), color = "black") +
  # Second plot (secondary axis), with scaled y values
  geom_line(data = SR_ts_annual, aes(x = year_month, y = count * scaling_factor), color = "red", size = 1.2) +
  scale_y_continuous(
    name = "CDC documents",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scaling_factor, name = "New buildings with private shelter")  # Secondary y-axis label with inverse scaling
  ) +
  labs(
    title = "CDC activity vis-a-vis private shelter construction",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red")  # Color the secondary y-axis label to match the line
  )

plot_combined
ggsave("figures/archiveddocsvsSR_20250123.png", width = 7, height = 3)
ggsave("figures/archiveddocsvsSR_20250123.tiff", width = 7, height = 3)
ggsave("figures/Figure08.png", width = 7, height = 3)

######  ------------------- Plotting with zoom facet

library(ggforce)


# Plot the data

# 1950s
p50 <- ggplot(docs_monthly, aes(x = year_month, y = count, color = Type)) +
  geom_line() +
  facet_zoom(x = year_month >as.Date("1949-01-30") & year_month < as.Date("1960-01-31"))+
  
  labs(
    title = "Aarhus CDC output per month",
    x = "Year-Month",
    y = "Number of shelter-related documents"
  ) +
  theme_light() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") 


# 1960s
p60 <- ggplot(docs_monthly, aes(x = year_month, y = count, color = Type)) +
  geom_line() +
  facet_zoom(x = year_month >as.Date("1959-01-30") & year_month < as.Date("1970-01-31"))+
  
  labs(
    title = "Aarhus CDC output per month",
    x = "Year-Month",
    y = "Number of shelter-related documents"
  ) +
  theme_light() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") 

# 1970s
p70 <- ggplot(docs_monthly, aes(x = year_month, y = count, color = Type)) +
  geom_line() +
  facet_zoom(x = year_month >as.Date("1969-01-30") & year_month < as.Date("1980-01-31"))+
  
  labs(
    title = "Aarhus CDC output per month",
    x = "Year-Month",
    y = "Number of shelter-related documents"
  ) +
  theme_light() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") 

# 1980s
p80 <- ggplot(docs_monthly, aes(x = year_month, y = count, color = Type)) +
  geom_line() +
  facet_zoom(x = year_month >as.Date("1979-01-30") & year_month < as.Date("1990-01-31"))+
  
  labs(
    title = "Aarhus CDC output per month",
    x = "Year-Month",
    y = "Number of shelter-related documents"
  ) +
  theme_light() +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") 

p50
ggsave("figures/archiveddocs1950_20250123.png")
p60
ggsave("figures/archiveddocs1960_20250123.png")
p70
ggsave("figures/archiveddocs1970_20250123.png")
p80
ggsave("figures/archiveddocs1980_20250123.png")
