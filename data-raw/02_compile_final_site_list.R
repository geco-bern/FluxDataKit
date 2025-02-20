# load libraries
library(dplyr)
library(raster)
library(stringr)
library(readr)
# library(MODISTools)

output_path <- "/data_2/FluxDataKit/v3.4"

# read site data RDS files
# append site types (ICOS, PLUMBER etc)
# sort through the sites lists depending on the
# order or preference of the data

# function to extract values from BADM file
get_values_badm <- function(df_badm, varnam){

  groups <- df_badm |>
    filter(
      VARIABLE_GROUP == paste0("GRP_", varnam),
      VARIABLE == paste0(varnam, "_STATISTIC"),
      DATAVALUE == "Mean"
    ) |>
    pull(GROUP_ID)

  df_badm |>
    filter(
      GROUP_ID %in% groups,
      VARIABLE == varnam
    ) |>
    mutate(DATAVALUE = as.numeric(DATAVALUE)) |>
    group_by(SITE_ID) |>
    summarise(
      var = mean(DATAVALUE)
    ) |>
    rename(!!varnam := var)
}

get_values_badm_HEIGHTSENSOR <- function(df_badm){

  # get group referring to variable information: height
  groups <- df_badm |>
    filter(VARIABLE_GROUP == "GRP_VAR_INFO", VARIABLE == "VAR_INFO_HEIGHT") |>
    pull(GROUP_ID)

  # within that, get group referring to height of latent heat measurement
  groups2 <- df_badm |>
    filter(
      GROUP_ID %in% groups,
      VARIABLE_GROUP == "GRP_VAR_INFO",
      VARIABLE == "VAR_INFO_VARNAME",
      DATAVALUE == "LE_F_MDS"
    ) |>
    pull(GROUP_ID)

  # extract value and average if multiple values are given per site
  df_badm |>
    filter(
      GROUP_ID %in% groups2,
      VARIABLE == "VAR_INFO_HEIGHT"
    ) |>
    mutate(DATAVALUE = as.numeric(DATAVALUE)) |>
    group_by(SITE_ID) |>
    summarise(
      HEIGHTSENSOR = mean(DATAVALUE)
    )
}


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

## Ameriflux--------------------------------------------------------------------

# Additional site meta info from BADM file
df_amf_badm <- read_csv(here::here("data-raw/meta_data/AMF_AA-Flx_BIF_CCBY4_20240910.csv"))

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
    product = "ameriflux",
    nyears = year_end - year_start + 1,
  ) |>
  left_join(
    get_values_badm(df_amf_badm, "HEIGHTC") |>
      rename(
        canopy_height = HEIGHTC,
        sitename = SITE_ID
        ),
    join_by("sitename")
  ) |>
  left_join(
    get_values_badm_HEIGHTSENSOR(df_amf_badm) |>
      rename(
        reference_height = HEIGHTSENSOR,
        sitename = SITE_ID
      ),
    join_by("sitename")
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
## Get data from Falge et al -------------
# obtained from https://doi.org/10.3334/ORNLDAAC/1530
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

## Get data from ICOS site list file -----------------------------------
siteinfo_icos <- readr::read_csv(
  here::here(
    "data-raw/meta_data/sites_list_icos.csv"
  )
)

## Get data from Fluxnet2015 and their BADM -----------------------------------
siteinfo_fluxnet2015 <- readr::read_csv(
  here::here(
    "data-raw/meta_data/fluxnet2015_site_list.csv"
  )
)

# Additional site meta info from FLUXNET BADM file
df_flx_badm <- read_csv(here::here("data-raw/meta_data/FLX_AA-Flx_BIF_DD_20200501.csv"))

siteinfo_fluxnet2015 <- siteinfo_fluxnet2015 |>
  left_join(
    get_values_badm(df_flx_badm, "HEIGHTC") |>
      rename(
        canopy_height = HEIGHTC,
        id = SITE_ID
      ),
    join_by("id")
  ) |>
  left_join(
    get_values_badm_HEIGHTSENSOR(df_flx_badm) |>
      rename(
        reference_height = HEIGHTSENSOR,
        id = SITE_ID
      ),
    join_by("id")
  )

## Complement various variables with Falge et al. -----------------
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


## Correct IGBP types  -----------------
# Priority: ICOS, Fluxnet2015, plumber
#   i.e, Fluxnet2015 overwrites plumber, then ICOS overwrites the reuslt
df <- df |>
  left_join(
    siteinfo_fluxnet2015 |>
      dplyr::select(id, IGBP) |>
      rename(sitename = id,
             classid_flx2015 = IGBP),
    by = "sitename"
  ) |>
  mutate(
    classid = ifelse(!is.na(classid_flx2015), classid_flx2015, classid)
  ) |>
  left_join(
    siteinfo_icos |>
      dplyr::select(site, lat_icos, lon_icos, classid_icos) |>
      rename(sitename = site),
    by = "sitename"
  ) |>
  mutate(
    lon = ifelse(is.na(lon), lon_icos, lon),
    lat = ifelse(is.na(lat), lat_icos, lat),
    classid = ifelse(!is.na(classid_icos), classid_icos, classid)
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

## root zone water storage capacity (mm) ---------------------------------------
# using the map from Stocker et al., 2023, obtainable from Zenodo at https://doi.org/10.5281/zenodo.5515246
whc <- raster("/data/archive/whc_stocker_2023/data/cwdx80_forcing.nc")  # on geco server
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
etopo <- raster("/data/archive/etopo_NA_NA/data/ETOPO1_Bed_g_geotiff.tif")  # on geco server
# etopo <- raster("~/data/etopo/ETOPO1_Bed_g_geotiff.tif")
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

## Get C3/C4 classification ----------------------------------------------------
# ... and C4-% from data shared by Yanghui Kang (Trevor's group)
site_summary_YK = readr::read_csv("data-raw/meta_data/site_summary_yanghui_kang.csv")

df <- df |>
  left_join(
    site_summary_YK |>
      dplyr::select(SITE_ID, c3c4 = `C3/C4`) |>
      rename(sitename = SITE_ID),
    by = join_by(sitename)
  ) |>
  mutate(c3c4 = dplyr::case_match(
    c3c4,
    "unknown" ~ NA,
    .default = c3c4
  )
  )

## Get vegetation and measurement height ---------------------------------------
df <- df |>

  # complement with AmeriFlux meta info
  left_join(
    ameriflux |>
      dplyr::select(
        sitename,
        canopy_height_amf = canopy_height,
        reference_height_amf = reference_height
        ),
    join_by(sitename)
  ) |>
  mutate(
    canopy_height = ifelse(is.na(canopy_height), canopy_height_amf, canopy_height),
    reference_height = ifelse(is.na(reference_height), reference_height_amf, reference_height)
  ) |>

  # complement with FLUXNET2015 meta info
  left_join(
    siteinfo_fluxnet2015 |>
      dplyr::select(
        sitename = id,
        canopy_height_flx = canopy_height,
        reference_height_flx = reference_height
      ),
    join_by(sitename)
  ) |>
  mutate(
    canopy_height= ifelse(is.na(canopy_height), canopy_height_flx, canopy_height),
    reference_height = ifelse(is.na(reference_height), reference_height_flx, reference_height)
  ) |>

  # complement with Plumber data
  left_join(
    plumber |>
      dplyr::select(
        sitename,
        canopy_height_plb = canopy_height,
        reference_height_plb = reference_height
      ),
    join_by(sitename)
  ) |>
  mutate(
    canopy_height= ifelse(is.na(canopy_height), canopy_height_plb, canopy_height),
    reference_height = ifelse(is.na(reference_height), reference_height_plb, reference_height)
  )

# get mean ratio of measurement height over vegetation height (for later)
vec_ratio <- df |>
  mutate(ratio = reference_height/canopy_height) |>
  ungroup() |>
  pull(ratio)

ratio_q10 <- quantile(vec_ratio, probs = 0.25, na.rm = TRUE)
ratio_q50 <- quantile(vec_ratio, probs = 0.5, na.rm = TRUE)
ratio_q90 <- quantile(vec_ratio, probs = 0.75, na.rm = TRUE)

# fill by mean by vegetation type and major climate zone (biome)
df_fill <- df |>
  mutate(biome = stringr::str_sub(koeppen_code, start = 1, end = 1)) |>
  group_by(biome, classid) |>
  summarise(
    canopy_height = mean(canopy_height, na.rm = TRUE),
    reference_height = mean(reference_height, na.rm = TRUE)
  )

# fill remaining only by veg type
df_fill2 <- df |>
  group_by(classid) |>
  summarise(
    canopy_height2 = mean(canopy_height, na.rm = TRUE),
    reference_height2 = mean(reference_height, na.rm = TRUE)
  )

# fill remaining only by climate zone
df_fill3 <- df |>
  group_by(koeppen_code) |>
  summarise(
    canopy_height3 = mean(canopy_height, na.rm = TRUE),
    reference_height3 = mean(reference_height, na.rm = TRUE)
  )

# df_fill <- df_fill |>
#   left_join(
#     df_fill2,
#     join_by(classid)
#   ) |>
#   mutate(
#     canopy_height = ifelse(is.na(canopy_height), canopy_height2, canopy_height),
#     reference_height = ifelse(is.na(reference_height), reference_height2, reference_height)
#   ) |>
#   dplyr::select(
#     -reference_height2,
#     -canopy_height2
#   )

# some are still missing: take ENF for DNF
df_fill4 <- df_fill |>
  filter(
    classid == "ENF"
  ) |>
  rename(
    canopy_height4 = canopy_height,
    reference_height4 = reference_height
  )

# fill missing
df <- df |>

  # fill with average by biome and vegetation type
  mutate(biome = stringr::str_sub(koeppen_code, start = 1, end = 1)) |>
  left_join(
    df_fill |>
      rename(
        canopy_height_fill = canopy_height,
        reference_height_fill = reference_height
      ),
    join_by(biome, classid)
  ) |>
  mutate(
    canopy_height = ifelse(is.na(canopy_height), canopy_height_fill, canopy_height),
    reference_height = ifelse(is.na(reference_height), reference_height_fill, reference_height)
  ) |>
  dplyr::select(
    -reference_height_fill,
    -canopy_height_fill
  ) |>

  # fill with average by vegetation type
  left_join(
    df_fill2 |>
      rename(
        canopy_height_fill = canopy_height2,
        reference_height_fill = reference_height2
      ),
    join_by(classid)
  ) |>
  mutate(
    canopy_height = ifelse(is.na(canopy_height), canopy_height_fill, canopy_height),
    reference_height = ifelse(is.na(reference_height), reference_height_fill, reference_height)
  ) |>
  dplyr::select(
    -reference_height_fill,
    -canopy_height_fill
  ) |>

  # fill remaining with ENF for DNF
  left_join(
    df_fill4 |>
      mutate(classid = "DNF") |>
      rename(
        canopy_height_fill = canopy_height4,
        reference_height_fill = reference_height4
      ),
    join_by(biome, classid)
  ) |>
  mutate(
    canopy_height = ifelse(is.na(canopy_height), canopy_height_fill, canopy_height),
    reference_height = ifelse(is.na(reference_height), reference_height_fill, reference_height)
  ) |>
  dplyr::select(
    -reference_height_fill,
    -canopy_height_fill
  ) |>

  # fill with average by koeppen climate
  left_join(
    df_fill3 |>
      rename(
        canopy_height_fill = canopy_height3,
        reference_height_fill = reference_height3
      ),
    join_by(koeppen_code)
  ) |>
  mutate(
    canopy_height = ifelse(is.na(canopy_height), canopy_height_fill, canopy_height),
    reference_height = ifelse(is.na(reference_height), reference_height_fill, reference_height)
  ) |>
  dplyr::select(
    -reference_height_fill,
    -canopy_height_fill
  )

# correct those where ratio of canopy and vegetation height is in 10% quantiles
df <- df |>
  mutate(ratio = reference_height/canopy_height) |>
  mutate(reference_height = ifelse(ratio > ratio_q90, canopy_height * ratio_q50, reference_height)) |>
  mutate(reference_height = ifelse(ratio < ratio_q10, canopy_height * ratio_q50, reference_height))


## Add climatic meta info-------------------------------------------------------
# file created in data-raw/01_collect_meta-data.R
df_climatic <- readr::read_rds(here::here("data-raw/meta_data/climatic_meta_info.rds"))

df <- df |>
  left_join(
    df_climatic,
    by = join_by(sitename)
  )

# some climatic meta info is missing
visdat::vis_miss(df_climatic)
visdat::vis_miss(df)


# df |>
#   mutate(ratio = reference_height/canopy_height) |>
#   ggplot(aes(ratio, ..count..)) +
#   geom_histogram()

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


## --------- IGBP types: check for inconsistencies -----------
## Jaideep note:
# 22 sites in plumber have conflicting IGBP types as compared with ICOS and Fluxnet2015
# This code just checks how many still have inconsistent types are reassigning the codes by priority
# siteinfo_eu_fluxnet = read.csv("data-raw/meta_data/siteinfo_europe-fluxdata.eu.csv")
# siteinfo_fluxnet2015 = read.csv("data-raw/meta_data/fluxnet2015_site_list.csv")

igbp_by_source = df |>
  dplyr::select(sitename, classid) |>
  rename(IGBP_fdk = classid) |>
  left_join(siteinfo_icos |>
              dplyr::select(site, classid_icos) |>
              rename(sitename = site,
                     IGBP_eu = classid_icos),
            by = join_by(sitename)
  ) |>
  left_join(siteinfo_fluxnet2015 |>
              dplyr::select(id, IGBP) |>
              rename(sitename = id,
                     IGBP_flx2015 = IGBP),
            by = join_by(sitename)
  ) |>
  mutate(IGBP_synth = ifelse(!is.na(IGBP_eu),
                             yes = IGBP_eu,
                             no = ifelse(!is.na(IGBP_flx2015),
                                         yes = IGBP_flx2015,
                                         no = IGBP_fdk
                             )
  )
  )

message("IGBP: found ",
  igbp_by_source |>
    dplyr::filter(IGBP_fdk != IGBP_eu | IGBP_fdk != IGBP_flx2015) |>
    nrow(),
  " conficted entries:")
print(igbp_by_source |>
          dplyr::filter(IGBP_fdk != IGBP_eu | IGBP_fdk != IGBP_flx2015))

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
    canopy_height,
    reference_height,
    koeppen_code,
    igbp_land_use = classid,
    whc,
    product,
    c3c4,
    mat,
    p_over_pet
  ) |>
  ungroup()

# quick check for missing data
visdat::vis_miss(fdk_site_info)

# write binary file to be included to package (.rds not possible)
save(
  fdk_site_info,
  file = here::here("data/fdk_site_info.rda"),
  compress = "xz"
  )

readr::write_csv(
  fdk_site_info,
  file = here::here("data/fdk_site_info.csv")
)

# write CSV file for upload to Zenodo
readr::write_csv(
  fdk_site_info,
  file = paste0(output_path, "/fdk_site_info.csv")
)
