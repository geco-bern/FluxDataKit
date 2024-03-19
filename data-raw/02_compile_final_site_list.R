# load libraries
library(dplyr)
library(raster)
# library(MODISTools)

# read site data RDS files
# append site types (ICOS, PLUMBER etc)
# sort through the sites lists depending on the
# order or preference of the data

# Read collected meta data------------------------------------------------------
## Plumber ----------------------------------------------------------------------
plumber <- readRDS(here::here("data-raw/meta_data/plumber_meta-data.rds")) |>
  dplyr::select(
    sitename,
    lon = longitude,
    lat = latitude,
    elv = elevation,
    year_start,
    year_end,
    classid = IGBP_veg_short,
    canopy_height,
    reference_height
  ) |>
  mutate(
    year_start = as.numeric(year_start),
    year_end = as.numeric(year_end)) |>
  mutate(
    koeppen_code = NA,
    nyears = year_end - year_start + 1,
    product = "plumber"
  )

## Ameriflux---------------------------------------------------------------------
ameriflux <- readRDS(here::here("data-raw/meta_data/amf_meta_data.rds")) |>
  filter(
    DATA_POLICY != "LEGACY"
  ) |>
  dplyr::select(
    sitename = site,
    lon = LOCATION_LONG,
    lat = LOCATION_LAT,
    elv = LOCATION_ELEV,
    year_start = DATA_START,
    year_end = DATA_END,
    classid = IGBP,
    koeppen_code = CLIMATE_KOEPPEN
  ) |>
  mutate(
    canopy_height = NA,
    reference_height = NA,
    product = "ameriflux",
    nyears = year_end - year_start + 1,
  )

## ICOS Drought 2018 release ---------------------------------------------------
icos_drought2018 <- readRDS(here::here("data-raw/meta_data/icos_drought2018_meta_data.rds")) |>
  dplyr::select(
    sitename = site,
    lon,
    lat,
    elv = elevation,
    year_start,
    year_end
  ) |>
  mutate(
    classid = NA,
    koeppen_code = NA,
    canopy_height = NA,
    reference_height = NA,
    product = "icos_drought2018",
    nyears = year_end - year_start + 1,
  )

## ICOS Warm Winter 2020 release ---------------------------------------------------
icos_warmwinter2020 <- readRDS(here::here("data-raw/meta_data/icos_warmwinter2020_meta_data.rds")) |>
  dplyr::select(
    sitename = site,
    lon,
    lat,
    elv = elevation,
    year_start,
    year_end
  ) |>
  mutate(
    classid = NA,
    koeppen_code = NA,
    canopy_height = NA,
    reference_height = NA,
    product = "icos_warmwinter2020",
    nyears = year_end - year_start + 1,
  )

# Determine longest source per site --------------------------------------------
df <- plumber |>
  bind_rows(
    ameriflux
  ) |>
  bind_rows(
    icos_drought2018
  ) |>
  bind_rows(
    icos_warmwinter2020
  ) |>
  group_by(sitename) |>

  # fill missing values within product, ideally with PLUMBER data
  tidyr::fill(
    c(lon, lat, elv, koeppen_code, classid),
    .direction = c("downup")) |>

  # retain only product providing longest running data
  filter(nyears == max(nyears)) |>

  # there are (two) duplicates because of identical nyears
  # keep only the one with the most recent 'year_end'
  filter(year_end == max(year_end)) |>

  # there may still be duplicates (FR-LGt), drop duplicates
  distinct(sitename, .keep_all = TRUE)


# Complement missing meta information-------------------------------------------
## Get info from Falge et al https://doi.org/10.3334/ORNLDAAC/1530 -------------
siteinfo_falge <- readr::read_csv(here::here("data-raw/meta_data/fluxnet_site_info_all_falge.csv")) |>
  dplyr::select(-sitename) |>
  dplyr::rename(
    sitename = fluxnetid
    ) |>

  mutate(gtopo30_elevation = as.numeric(gtopo30_elevation)) |>

  # split koppen geiger code into separate column
  dplyr::mutate(koeppen_climate = str_split( koeppen_climate, " - " ) ) |>
  dplyr::mutate(koeppen_code_falge = purrr::map( koeppen_climate, 1 ) ) |>
  dplyr::mutate(koeppen_word = purrr::map( koeppen_climate, 2 ) ) |>
  tidyr::unnest(c(koeppen_code_falge, koeppen_word)) |>

  # treat IGBP codes
  mutate(
    igbp_land_use = dplyr::case_match(
      igbp_land_use,
      "Woody Savannas" ~ "WSA",
      "Savannas" ~ "SAV",
      "Permanent Wetlands" ~ "WET",
      "Open Shrublands" ~ "OSH",
      "Closed Shrublands" ~ "CSH",
      "Grasslands" ~ "GRA",
      "Evergreen Needleleaf Forest" ~ "ENF",
      "Evergreen Broadleaf Forest" ~ "EBF",
      "Deciduous Broadleaf Forest" ~ "DBF",
      "Mixed Forests" ~ "MF",
      "Croplands" ~ "CRO",
      "Cropland/Natural Vegetation Mosaic" ~ "MF",
      "Urban and Built-Up" ~ NA,
      .default = igbp_land_use
    )
  ) |>
  dplyr::select(-koeppen_climate)

df <- df |>
  arrange(sitename) |>
  left_join(
    siteinfo_falge,
    by = "sitename"
  ) |>
  mutate(
    lon = ifelse(is.na(lon), longitude, lon),
    lat = ifelse(is.na(lat), latitude, lat),
    elv = ifelse(is.na(elv), gtopo30_elevation, elv),
    classid = ifelse(is.na(classid), igbp_land_use, classid),
    koeppen_code = ifelse(is.na(koeppen_code), koeppen_code_falge, koeppen_code)
  )

## Get missing data from ICOS site list file -----------------------------------
siteinfo_icos <- readr::read_csv(
  here::here(
    "data-raw/meta_data/sites_list_icos.csv"
  )
)

df <- df |>
  left_join(
    siteinfo_icos |>
      rename(sitename = site),
    by = "sitename"
  ) |>
  mutate(
    lon = ifelse(is.na(lon), lon_icos, lon),
    lat = ifelse(is.na(lat), lat_icos, lat),
    classid = ifelse(is.na(classid), classid_icos, classid)
  )

## Get still missing koeppen-geiger info from global map------------------------
loc <- data.frame(lon = df$lon, lat = df$lat)

# get the koeppen geiger values
kg <- raster("data-raw/ancillary_data/koeppen_geiger/Beck_KG_V1_present_0p0083.tif")
kg_v <- raster::extract(kg, loc)
kg_key <- read.table(
  "data-raw/ancillary_data/koeppen_geiger/key.csv",
  header = TRUE,
  sep = ","
)

# append to original data frame
df$koeppen_code_beck <- kg_v

# read in koeppen code labels
x <- kg_key$class
names(x) <- kg_key$id

# rename land cover classes from numeric to
# factor (character description)
df <- df |>
  mutate(
    koeppen_code_beck = recode(koeppen_code_beck, !!!x)
  )

# backfill koeppen code with Beck et al. data
df <- df |>
  mutate(
    koeppen_code = ifelse(
      is.na(koeppen_code),
      koeppen_code_beck,
      koeppen_code)
  )

## root zone water storage capacity---------------------------------------------
# using the map from Stocker et al., 2023, obtainable from Zenodo at https://doi.org/10.5281/zenodo.5515246
whc <- raster("data-raw/ancillary_data/root_zone_water_capacity/cwdx80.nc")
whc_v <- raster::extract(whc, loc)

# append to original data frame
df$whc <- whc_v

# for missing values, extract again with buffer (in m)
whc_v <- raster::extract(whc, loc, buffer = 10000)
df$whc_buffer <- whc_v

df <- df |>
  mutate(whc_buffer = purrr::map_dbl(
    whc_buffer,
    ~mean(., na.rm = TRUE)
  )) |>
  mutate(whc = ifelse(is.na(whc), whc_buffer, whc))


## Get still missing elevation data from ETOPO1---------------------------------
# file is too large to add it to this repo.
etopo <- raster("~/data/archive/etopo_NA_NA/data/ETOPO1_Bed_g_geotiff.tif")
etopo_v <- raster::extract(etopo, loc)

# add etopo1 column
df$elv_etopo1 = etopo_v

# backfill elevation data
df <- df |>
  mutate(
    elv = ifelse(
      is.na(elv),
      elv_etopo1,
      elv)
  )

# ## get IGBP from MODIS data-----------------------------------------------------
# # download IGBP class values (MODISTools)
# igbp <- apply(df, 1, function(x){
#   mt_subset(
#     lat = x['lat'],
#     lon = x['lon'],
#     product = "MCD12Q1",
#     band = "LC_Type1",
#     start = "2019-01-01",
#     end = "2019-01-01",
#     internal = TRUE,
#     progress = FALSE
#   )$value
# })
#
# df$igbp_land_use <- igbp
#
# igbp_key <- read.table(
#   "data-raw/ancillary_data/igbp_key.csv",
#   header = TRUE,
#   sep = ","
# )
#
# # read in igbp labels
# x <- igbp_key$class
# names(x) <- igbp_key$id
#
# # rename land cover classes from numeric to
# # factor (character description)
# df <- df |>
#   mutate(
#     igbp_land_use = recode(igbp_land_use, !!!x)
#   )


# save the data, retaining only key columns (note: valuable information also in
# other columns!)
fdk_site_info <- df |>
  dplyr::select(
    sitename,
    lon,
    lat,
    elv,
    year_start,
    year_end,
    koeppen_code,
    igbp_land_use = classid,
    whc,
    product
  ) |>
  ungroup()

# # quick check for missing data
# visdat::vis_miss(df)

# saveRDS(df, file = here::here("data/fdk_site_info.rds"), compress = "xz")
save(fdk_site_info,
     file = here::here("data/fdk_site_info.rda"), compress = "xz"
     )
# readr::write_csv(df, file = here::here("data/fdk_site_info.csv"))


