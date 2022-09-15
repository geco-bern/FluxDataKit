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

# read in all site meta-data
sites <- readRDS("data/flux_data_kit_site-info.rds")

# process all sites
fdk_process_lsm(
  sites,
  out_path = "/data/scratch/PLUMBER_X",
  modis_path = "data-raw/modis",
  format = "lsm"
)

