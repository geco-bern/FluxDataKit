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
message("processing GEE data product: ")

# feedback
message(paste("  - ", bundle))

settings_gee <- get_settings_gee(
  bundle            = bundle,
  python_path       = "/usr/bin/python3",
  gee_path          = "./src/gee_subset/src/gee_subset",
  data_path         = "data-raw/modis/",
  method_interpol   = "none",
  keep              = TRUE,
  overwrite_raw     = TRUE,
  overwrite_interpol= TRUE
)

# run the ingest routine
df_modis <- ingest(
  sites,
  source = "gee",
  settings = settings_gee,
  parallel = FALSE
)

