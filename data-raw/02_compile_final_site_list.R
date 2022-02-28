# load libraries
library(tidyverse)
library(raster)
library(MODISTools)

# read site data RDS files
# append site types (ICOS, PLUMBER etc)
# sort through the sites lists depending on the
# order or preference of the data

# 1. ICOS X
icos <- readRDS("data-raw/icos_meta-data.rds") %>%
  rename(
    'elev' = 'elevation',
    'date_start' = 'year_start',
    'date_end' = 'year_end',
    'elv' = 'elevation'
  ) %>%
  dplyr::select(
    sitename,
    lat,
    lon,
    elv,
    date_start,
    date_end
  ) %>%
  mutate(
    product = "icos"
  )

# 2. OneFlux
oneflux <- readRDS("data-raw/oneflux_meta-data.rds") %>%
  filter(
    DATA_POLICY != "LEGACY"
  ) %>%
  rename(
    'sitename' = 'SITE_ID',
    'lat' = 'LOCATION_LAT',
    'lon' = 'LOCATION_LONG',
    'elv' = 'LOCATION_ELEV',
    'date_start' = 'DATA_START',
    'date_end' = 'DATA_END',
    'koeppen_code' = 'CLIMATE_KOEPPEN'
  ) %>%
  dplyr::select(
    sitename,
    lat,
    lon,
    elv,
    koeppen_code,
    date_start,
    date_end
  ) %>%
  mutate(
    product = "oneflux"
  )

# 3. ameriflux
ameriflux <- readRDS("data-raw/amf_meta-data.rds") %>%
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
    'elv' = 'LOCATION_ELEV',
    'date_start' = 'DATA_START',
    'date_end' = 'DATA_END',
    'koeppen_code' = 'CLIMATE_KOEPPEN'
  ) %>%
  dplyr::select(
    sitename,
    lat,
    lon,
    elv,
    koeppen_code,
    date_start,
    date_end
  )%>%
  mutate(
    product = "ameriflux"
  )

# 4. plumber
plumber <- readRDS("data-raw/plumber_meta-data.rds") %>%
  rename(
    'lat' = 'latitude',
    'lon' =  'longitude',
    'elv' = 'elevation',
    'date_start' = 'year_start',
    'date_end' = 'year_end'
  ) %>%
  dplyr::select(
    sitename,
    lat,
    lon,
    elv,
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
    date_end = ifelse(is.na(date_end), "2021", date_end)
  ) %>%
  filter(
    !is.na(date_start)
  )

# convert year_end
df <- df %>%
  mutate(
    date_start = sprintf("%s-01-01", date_start),
    date_end = sprintf("%s-12-31", date_end),
    year_end = format(as.Date(date_end), "%Y"),
    year_start = format(as.Date(date_start), "%Y")
  )

# set locations for ancillary data extractions
loc <- data.frame(lon = df$lon, lat = df$lat)

# get the koeppen geiger values
kg <- raster("data-raw/KG/Beck_KG_V1_present_0p0083.tif")
kg_v <- raster::extract(kg, loc)
kg_key <- read.table(
  "data-raw/KG/key.csv",
  header = TRUE,
  sep = ","
)

# append to original data frame
df$koeppen_code_beck <- kg_v

# read in igbp labels
x <- kg_key$class
names(x) <- kg_key$id

# rename land cover classes from numeric to
# factor (character description)
df <- df %>%
  mutate(
    koeppen_code_beck = recode(koeppen_code_beck, !!!x)
  )

# backfill koeppen code with Beck et al. data
df <- df %>%
  mutate(
    koeppen_code = ifelse(
      is.na(koeppen_code),
      koeppen_code_beck,
      koeppen_code)
  )

# get water holding capacity
whc <- raster("data-raw/field_capacity/field_capacity.tif")
whc_v <- raster::extract(whc, loc)

# append to original data frame
df$whc <- whc_v

# download IGBP class values (MODISTools)
igbp <- apply(df, 1, function(x){
  mt_subset(
    lat = x['lat'],
    lon = x['lon'],
    product = "MCD12Q1",
    band = "LC_Type1",
    start = "2019-01-01",
    end = "2019-01-01",
    internal = TRUE,
    progress = FALSE
  )$value
})

df$igbp_land_use <- igbp

igbp_key <- read.table(
  "data-raw/igbp_key.csv",
  header = TRUE,
  sep = ","
)

# read in igbp labels
x <- igbp_key$class
names(x) <- igbp_key$id

# rename land cover classes from numeric to
# factor (character description)
df <- df %>%
  mutate(
    igbp_land_use = recode(igbp_land_use, !!!x)
  )

# save the data
saveRDS(df, file = "data/flux_data_kit_site-info.rds", compress = "xz")
