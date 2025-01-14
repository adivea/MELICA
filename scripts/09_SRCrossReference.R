## Working with SR_overview csv and BBR csv from Ulrik

# - - - -  Loading Packages, Data, and Cleaning Data

# install.packages("pacman")
library(pacman) # for easier managing of packages
pacman::p_load(tidyverse, stringr, sf)

# loading in CSVs with SR addresses, currently only for testing
df_addressSR <- read_csv("PGit/SR_overview_test.csv") # manually aggregated SR addresses (excerpt of WORKING_SR_overview_filtered)
df_addressBBR <- read_csv("MELICA/output_data/SK_bbr_oc_addresses.csv") # data from BBR

# removal of duplicate + empty columns
df_addressSR <- subset(df_addressSR, select = -c(longitude, latitude))

# removing of whitespaces and all lowercase addresses
df_addressSR$address <- str_trim(tolower(df_addressSR$address))
df_addressBBR$oc_formatted <- str_trim(tolower(df_addressBBR$oc_formatted))

# replacing strings to avoid any inconsistencies in spelling + spacing across sources
## preliminary cleaning of dataframes, to be synthesised later
df_addressSR$address <- gsub("å", "aa", df_addressSR$address)
df_addressBBR$oc_formatted <- gsub("å", "aa", df_addressBBR$oc_formatted)

df_addressBBR$oc_formatted <- gsub("é", "e", df_addressBBR$oc_formatted)
df_addressBBR$oc_formatted <- gsub("ü", "u", df_addressBBR$oc_formatted)

df_addressSR$address <- gsub("i. huitfeldt", "ivar huitfeldt", df_addressSR$address)
df_addressSR$address <- gsub("nordre", "ndr", df_addressSR$address)
df_addressSR$address <- gsub("dr. holstvej", "doktor holstsvej", df_addressSR$address)
df_addressSR$address <- gsub("chr.", "christian", df_addressSR$address)
df_addressSR$address <- gsub("rudolf", "rudolph", df_addressSR$address)
df_addressSR$address <- gsub("rud.", "rudolph", df_addressSR$address)
df_addressSR$address <- gsub("p. muller", "paludan muller", df_addressSR$address)
df_addressSR$address <- gsub("j. knudsen", "jacob knudsen", df_addressSR$address)
df_addressSR$address <- gsub("dr. margrethe", "dronning margrethe", df_addressSR$address)
df_addressSR$address <- gsub("kongsvangsalle", "kongsvangalle", df_addressSR$address)

df_addressSR$address <- gsub("(?<=[a-zA-Z])-(?=[a-zA-Z])", "", df_addressSR$address, perl = TRUE)
df_addressBBR$oc_formatted <- gsub("(?<=[a-zA-Z])-(?=[a-zA-Z])", "", df_addressBBR$oc_formatted, perl = TRUE)

df_addressSR$address <- str_replace_all(df_addressSR$address, "\\s+", "")
df_addressBBR$oc_formatted = str_replace_all(df_addressBBR$oc_formatted, "\\s+", "")

# getting decades, taken from Adela's (adivea) code in 07_HistoricalBBR.R
df_addressBBR <- df_addressBBR %>%
  mutate(
    decade = case_when(
      byg026Opførelsesår < 1950 ~ '1940s',
      byg026Opførelsesår < 1960 ~ '1950s',
      byg026Opførelsesår < 1970 ~ '1960s',
      byg026Opførelsesår < 1980 ~ '1970s',
      byg026Opførelsesår < 1990 ~ '1980s',
    )
  )

# ignoring all bbr address info. after the first comma
## thus ignoring postal codes and instances of "denmark" as well as special chars.
df_addressBBR$oc_formatted <- str_extract(df_addressBBR$oc_formatted, "^[^,]*")
df_addressSR$address <- str_extract(df_addressSR$address, "^[^,]*")

# - - - - Cross-Checking Addresses

# checking what address info DOES match at the moment (16.07.2024)
checkAddressMatch <- inner_join(df_addressSR, df_addressBBR, by = c("address" = "oc_formatted"),
                                relationship = "many-to-many", unmatched = "drop")

# keeping relevant columns + removing empty columns
checkAddressMatch <- subset(
  checkAddressMatch,
  select =
    c(
      id_lokalId,
      address,
      longitude,
      latitude,
      byg026Opførelsesår,
      decade,
      capacity,
      byg069Sikringsrumpladser,
      oc_category,
      oc_type,
      box,
      document
    )
)

# - - - - looking at the leftover addresses from both sources

# Rows in addressSR that don't match bbr and vice versa
unmatched_addressSR <- anti_join(df_addressSR, df_addressBBR, by = c("address" = "oc_formatted"))
unmatched_addressBBR <- anti_join(df_addressBBR, df_addressSR, by = c("oc_formatted" = "address"))

# Calculate the row difference
row_diff <- nrow(unmatched_addressSR) - nrow(unmatched_addressBBR)

# Adding NA rows to the shorter df
if (row_diff > 0) {
  # unmatched_addressSR is longer
  extra_rows <- data.frame(oc_formatted = rep(NA, row_diff))
  unmatched_addressBBR <- bind_rows(unmatched_addressBBR, extra_rows)
} else {
  # unmatched_addressBBR is longer
  extra_rows <- data.frame(address = rep(NA, abs(row_diff)))
  unmatched_addressSR <- bind_rows(unmatched_addressSR, extra_rows)
}

# Combine the two data frames into one with two columns
checkAddressUnmatched <- data.frame(
  addressSR = unmatched_addressSR$address,
  addressBBR = unmatched_addressBBR$oc_formatted
)

# - - - - Inspecting Aarhus Kommune GeoJSON + Comparing Street Names with those in BBR register 

### INSPECTION

geojson_streets <- "https://webkort.aarhuskommune.dk/spatialmap?page=get_geojson_opendata&datasource=vjmdt_tot"

data_geo <- sf::st_read(geojson_streets, quiet = FALSE)
summary(data_geo)

# number of multilinestring geometries
multilinestring_count <- sum(st_geometry_type(data_geo) == "MULTILINESTRING")
## calculated number of multilinestrings / addresses: 10448L

# bounding box of the geojson geometries
bounding_box <- st_bbox(data_geo)
bounding_box

# visualising the extent
bbox_sf <- st_as_sfc(bounding_box)

ggplot() +
  geom_sf(data = data_geo, color = 'blue', size = 0.5) +
  geom_sf(data = bbox_sf, color = 'red', fill = NA, size = 1) +
  ggtitle("Extent of MULTILINESTRING Geometries")

# Add the bounding box
box_coords <- st_as_sfc(bounding_box)
plot(box_coords, add = TRUE, border = 'red', lwd = 2)

### COMPARISON

# reading in BBR data
df_streetBBR <- read_csv("MELICA/output_data/SK_bbr_oc_addresses.csv")

# removing everything after the first comma and all numbers that may come before
df_streetBBR$oc_formatted <- str_extract(df_streetBBR$oc_formatted, "^[^,]*")
df_streetBBR$all_extracted_numbers <- str_extract_all(df_streetBBR$oc_formatted, "[0-9]+", simplify = TRUE)

# checking matching street names according to how many times they appear in the BBR
checkStreetMatch <- inner_join(df_streetBBR, data_geo, by = c("oc_formatted" = "vejnavne"),
                                relationship = "many-to-many", unmatched = "drop")

# keeping relevant columns + removing empty columns
checkStreetMatch <- subset(
  checkStreetMatch,
  select =
    c(
      id_lokalId,
      oc_formatted,
      byg069Sikringsrumpladser,
      latitude,
      longitude,
      geometry
    )
)

