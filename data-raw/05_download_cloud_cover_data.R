#!/usr/bin/Rscript

# This script downloads all required cloud cover data from ERA5
# using ecmwfr.

# This script is best run as a job and or background process as downloading
# all site data takes a while.

# load libraries
library(FluxDataKit)
library(ecmwfr)
source("R/fdk_download_cloud_cover.R")

# load site meta-data
sites <- FluxDataKit::fdk_site_info |>
  filter(sitename == "FI-Qvd")

# download the ERA4 cloud cover data from ecmwf from CDS
# Make sure to register for CDS before running this, following
# https://github.com/bluegreen-labs/ecmwfr?tab=readme-ov-file#use-copernicus-climate-data-store-cds
fdk_download_cloud_cover(
  df = sites,
  user = "295612",
  path = "~/data/FluxDataKit/FDK_inputs/cloud_cover/" # "data-raw/cloud_cover/"
)


# Alternatively, get it from CRU
path_cru <- "~/data/cru/ts4.07/"

# use ingestr to get it from CRU TS (local file)
# ingestr: https://geco-bern.github.io/ingestr/index.html
df_cru <- ingestr::ingest(
  siteinfo = sites,
  source = "cru",
  getvars = c("ccov"),
  dir = path_cru,
  settings = list(
    correct_bias = NULL       # 0.5 deg resolution
  )
)

write_rds(
  df_cru,
  file = "~/data/FluxDataKit/FDK_inputs/cloud_cover/df_cru.rds"
)
