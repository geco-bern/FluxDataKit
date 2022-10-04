# Batch conversion of FLUXNET data to LSM formatting
# in line with the PLUMBER2 release
library(tidyverse)
library(FluxDataKit)

# Check for FluxnetLSM library
# should be the bug fixed one
if(!require(devtools)){install.packages("devtools")}
if(!require(FluxnetLSM)){
  devtools::install_github("computationales/FluxnetLSM")
  }

# read in the fluxnetlsm data
fls_meta_data <-  csv_data <- read.csv(
  system.file("extdata", "Site_metadata.csv", package = "FluxnetLSM"),
  header = TRUE,
  stringsAsFactors = FALSE
)

# read in the meta data as listed
# within FluxDataKit and
# rename the columns
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
  select(
    sitename,
    lat,
    lon,
    elv,
    igbp_land_use
  ) |>
  rename(
    'SiteCode' = 'sitename',
    'SiteLatitude' = 'lat',
    'SiteLongitude' = 'lon',
    'SiteElevation' = 'elv',
    'IGBP_vegetation_short' = 'igbp_land_use'
  ) |>
  filter(
    !(SiteCode %in% !!fls_meta_data$SiteCode)
  )

# merge the two datasets
# using bind rows
final_data <- bind_rows(fls_meta_data, sites)

# write data to file (in ./data/)
write.csv(final_data, file = file.path(tempdir(), "meta_data.csv"))

# read in all site meta-data, only test on
# SE-Nor to debug FluxnetLSM for now
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
  filter(
    sitename == "SE-Nor"
  )

# process all sites
fdk_process_lsm(
  sites,
  out_path = "data/test/",
  modis_path = "data-raw/modis",
  format = "lsm",
  site_csv_file = file.path(tempdir(), "meta_data.csv")
)
