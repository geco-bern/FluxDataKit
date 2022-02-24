# load libraries
library(tidyverse)

# read site data RDS files
# append site types (ICOS, PLUMBER etc)
# sort through the sites lists depending on the
# order or preference of the data

# 1. ICOS X
icos <- readRDS("data/icos_meta-data.rds") %>%
  rename(
    'sitename' = 'id',
    'elev' = 'elevation'
  ) %>%
  select(
    sitename, lat, lon, elev
  )
nrow(icos)

# 2. OneFlux
oneflux <- readRDS("data/oneflux_meta-data.rds") %>%
  filter(
    DATA_POLICY != "LEGACY"
  ) %>%
  rename(
    'sitename' = 'SITE_ID',
    'lat' = 'LOCATION_LAT',
    'lon' = 'LOCATION_LONG',
    'elev' = 'LOCATION_ELEV',
    'date_start' = 'DATA_START',
    'date_end' = 'DATA_END',
    'koeppen_code' = 'CLIMATE_KOEPPEN'
  ) %>%
  select(
    sitename, lat, lon, elev, koeppen_code, date_start, date_end
  )

nrow(oneflux)

# 3. ameriflux
ameriflux <- readRDS("data/amf_meta-data.rds") %>%
  filter(
    DATA_POLICY != "LEGACY"
  ) %>%
  rename(
    'sitename' = 'SITE_ID',
    'lat' = 'LOCATION_LAT',
    'lon' = 'LOCATION_LONG',
    'elev' = 'LOCATION_ELEV',
    'date_start' = 'DATA_START',
    'date_end' = 'DATA_END',
    'koeppen_code' = 'CLIMATE_KOEPPEN'
  ) %>%
  select(
    sitename, lat, lon, elev, koeppen_code, date_start, date_end
  )

nrow(ameriflux)

# 4. plumber
plumber <- readRDS("data/plumber_meta-data.rds")
nrow(plumber)

# Merging routine
df <- icos


