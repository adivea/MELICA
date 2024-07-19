## Working with SR_overview csv and bbr csv from Ulrik

# - - - -  Loading Packages, Data, and Cleaning Data

# install.packages("pacman")
library(pacman) # for easier managing of packages
pacman::p_load(tidyverse, stringr, sf)

# loading in CSVs with SR addresses, currently only for testing
addressSR <- read_csv("Own Git Stuff/SR_overview_test.csv") # manually aggregated SR addresses
bbr <- read_csv("MELICA/output_data/SK_bbr_oc_addresses.csv") # data from BBR

# removal of duplicate + empty columns
addressSR <- subset(addressSR, select = -c(longitude, latitude))

# removing of whitespaces and all lowercase addresses
addressSR$address <- str_trim(tolower(addressSR$address))
bbr$oc_formatted <- str_trim(tolower(bbr$oc_formatted))

# replacing strings to avoid any inconsistencies in spelling + spacing across sources
## preliminary cleaning of dataframes, to be synthesised later
addressSR$address <- gsub("å", "aa", addressSR$address)
bbr$oc_formatted <- gsub("å", "aa", bbr$oc_formatted)

bbr$oc_formatted <- gsub("é", "e", bbr$oc_formatted)
bbr$oc_formatted <- gsub("ü", "u", bbr$oc_formatted)

addressSR$address <- gsub("i. huitfeldt", "ivar huitfeldt", addressSR$address)
addressSR$address <- gsub("nordre", "ndr", addressSR$address)
addressSR$address <- gsub("dr. holstvej", "doktor holstsvej", addressSR$address)
addressSR$address <- gsub("chr.", "christian", addressSR$address)
addressSR$address <- gsub("rudolf", "rudolph", addressSR$address)
addressSR$address <- gsub("rud.", "rudolph", addressSR$address)
addressSR$address <- gsub("p. muller", "paludan muller", addressSR$address)
addressSR$address <- gsub("j. knudsen", "jacob knudsen", addressSR$address)
addressSR$address <- gsub("dr. margrethe", "dronning margrethe", addressSR$address)

addressSR$address <- gsub("(?<=[a-zA-Z])-(?=[a-zA-Z])", "", addressSR$address, perl = TRUE)
bbr$oc_formatted <- gsub("(?<=[a-zA-Z])-(?=[a-zA-Z])", "", bbr$oc_formatted, perl = TRUE)

addressSR$address <- str_replace_all(addressSR$address, "\\s+", "")
bbr$oc_formatted = str_replace_all(bbr$oc_formatted, "\\s+", "")

addressSR$address <- gsub("kongsvangsalle", "kongsvangalle", addressSR$address)

# getting decades, taken from Adela's code in 07_HistoricalBBR.R
bbr <- bbr %>%
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
bbr$oc_formatted <- str_extract(bbr$oc_formatted, "^[^,]*")
addressSR$address <- str_extract(addressSR$address, "^[^,]*")

# - - - - Cross-Checking Addresses

# checking what address info DOES match at the moment (16.07.2024)
checkAddressMatch <- inner_join(addressSR, bbr, by = c("address" = "oc_formatted"),
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
unmatched_addressSR <- anti_join(addressSR, bbr, by = c("address" = "oc_formatted"))
unmatched_bbr <- anti_join(bbr, addressSR, by = c("oc_formatted" = "address"))

# Calculate the row difference
row_diff <- nrow(unmatched_addressSR) - nrow(unmatched_bbr)

# Adding NA rows to the shorter df
if (row_diff > 0) {
  # unmatched_addressSR is longer
  extra_rows <- data.frame(oc_formatted = rep(NA, row_diff))
  unmatched_bbr <- bind_rows(unmatched_bbr, extra_rows)
} else {
  # unmatched_bbr is longer
  extra_rows <- data.frame(address = rep(NA, abs(row_diff)))
  unmatched_addressSR <- bind_rows(unmatched_addressSR, extra_rows)
}

# Combine the two data frames into one with two columns
checkAddressUnmatched <- data.frame(
  addressSR = unmatched_addressSR$address,
  addressBBR = unmatched_bbr$oc_formatted
)

# - - - - Spatially join identified BBR points with BBR building footprints (?)
## geojson open with sf package + create brief report on usability + relevance

geojson_streets <- "https://webkort.aarhuskommune.dk/spatialmap?page=get_geojson_opendata&datasource=vjmdt_tot"

data_geo <- sf::st_read(geojson_streets, quiet = TRUE)

spatial_data$name <- tolower(gsub("[[:punct:][:space:]]", "", spatial_data$name))
csv_data$name <- tolower(gsub("[[:punct:][:space:]]", "", csv_data$name))
