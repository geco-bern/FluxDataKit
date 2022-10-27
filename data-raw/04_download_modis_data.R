#!/usr/bin/Rscript

# This script is best run as a job and or background process as downloading
# all site data takes a while

# load libraries
library(FluxDataKit)

# load site meta-data
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
  dplyr::filter(
    product == "oneflux"
  )

# download the modis data
fdk_download_modis(
  df = sites,
  path = "data-raw/modis/"
)
