#!/usr/bin/env Rscript

# Updates MODIS subsets on Euler
#
# This is run for Ameriflux data
# but can be appended with other datasets
#
# Assumes that your local (home dir)
# data directory links to the CES Euler
# data repository as mentioned in the notions.co
# setup page for Euler
#

# load script arguments
args <- commandArgs(trailingOnly=TRUE)

# load required libraries
library(ingestr)

# set sites
sites <- readRDS("data/flux_data_kit_site-info.rds")
sites$year_start <- 2000
sites$year_end <- as.numeric(format(Sys.Date(),"%Y"))
bundle <- args[1]

# feedback
message("processing MODIS data product: ")

# feedback
message(paste("  - ", bundle))

# grab settings
settings_modis <- get_settings_modis(
  bundle            = bundle,
  data_path         = "~/data/modis_subsets/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = TRUE,  # not too costly when using networks
  overwrite_interpol= TRUE,
  n_focal           = 0,
  network           = "FLUXNET"
)

# run the ingest routine
df_modis <- ingest(
  sites,
  source = "modis",
  settings = settings_modis,
  parallel = FALSE
)

