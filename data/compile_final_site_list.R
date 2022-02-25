# load libraries
library(tidyverse)
library(raster)
library(MODISTools)

# read site data RDS files
# append site types (ICOS, PLUMBER etc)
# sort through the sites lists depending on the
# order or preference of the data

# 1. ICOS X
icos <- readRDS("data/icos_meta-data.rds") %>%
  rename(
    'elev' = 'elevation',
    'date_start' = 'year_start',
    'date_end' = 'year_end'
  ) %>%
  select(
    sitename,
    lat,
    lon,
    elev,
    date_start,
    date_end
  ) %>%
  mutate(
    product = "icos"
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
    sitename,
    lat,
    lon,
    elev,
    koeppen_code,
    date_start,
    date_end
  ) %>%
  mutate(
    product = "oneflux"
  )

nrow(oneflux)

# 3. ameriflux
ameriflux <- readRDS("data/amf_meta-data.rds") %>%
  filter(
    DATA_POLICY != "LEGACY"
  ) %>%
  mutate(
    DATA_START = ifelse(is.na(DATA_START),as.numeric(TOWER_BEGAN), DATA_START)
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
    sitename,
    lat,
    lon,
    elev,
    koeppen_code,
    date_start,
    date_end
  )%>%
  mutate(
    product = "ameriflux"
  )

nrow(ameriflux)

# 4. plumber
plumber <- readRDS("data/plumber_meta-data.rds") %>%
  rename(
    'lat' = 'latitude',
    'lon' =  'longitude',
    'elev' = 'elevation',
    'date_start' = 'year_start',
    'date_end' = 'year_end'
  ) %>%
  select(
    sitename,
    lat,
    lon,
    elev,
    date_start,
    date_end
  )%>%
  mutate(
    product = "plumber"
  ) %>%
  mutate(
    date_start = as.numeric(date_start),
    date_end = as.numeric(date_end)
  )

nrow(plumber)

# Merging routine
df <- icos

oneflux <- oneflux %>%
  filter(
    !(sitename %in% df$sitename)
  )

df <- full_join(df, oneflux)

ameriflux <- ameriflux %>%
  filter(
    !(sitename %in% df$sitename)
  )

df <- full_join(df, ameriflux)


plumber <- plumber %>%
  filter(
    !(sitename %in% df$sitename)
  )

df <- full_join(df, plumber)

# fill in end years
df <- df %>%
  mutate(
    date_end = ifelse(is.na(date_end), 2022, date_end)
  ) %>%
  filter(
    !is.na(date_start)
  )

# nr site years
df %>%
  mutate(years = date_end - date_start) %>%
  summarize(
    sum(years, na.rm = TRUE)
    ) %>%
  print()

# get the koeppen geiger values

# get water holding capacity

# download IGBP class (MODISTools)


