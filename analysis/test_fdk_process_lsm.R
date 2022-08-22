library(dplyr)
#detach("package:FluxnetLSM", unload = TRUE)
library(FluxnetLSM)
source("R/fdk_process_lsm.R")
source("R/fdk_correct_era.R")
source("R/fdk_smooth_ts.R")
source("R/fdk_download_modis.R")
source("R/site_exceptions.R")
source("R/fdk_correct_fluxes.R")
source("R/fdk_balance_energy.R")
source("R/helper_functions.R")

# select datasets / sites to process
datasets <- c("fluxnet2015","icos","oneflux")
sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  filter(
    product != "plumber",
    sitename == "AT-Neu"
  )

fdk_process_lsm(
  sites,
  out_path = "data/tmp/",
  format = "lsm",
  overwrite = TRUE
  )
