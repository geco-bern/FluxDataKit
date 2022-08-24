# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
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
source("R/fdk_convert_lsm.R")
source("R/fdk_format_drivers.R")

sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  filter(
    sitename == "AT-Neu"
  )

# output FLUXNET formatted data
fdk_process_lsm(
  sites,
  out_path = "data/tmp/",
  modis_path = "data-raw/modis/",
  format = "lsm",
  overwrite = TRUE
)

# read in demo data
# for AT-Neu site (as LSM data)
# convert to the HH fluxnet format
fluxnet <- fdk_convert_lsm(
  site = "AT-Neu",
  path = "data/tmp",
  fluxnet_format = TRUE,
  meta_data = FALSE,
  out_path = "data/tmp"
)

# Use a uniform FLUXNET HH input
# file to generate p-model (rsofun)
# compatible driver data
test <- fdk_format_drivers(
  site_info = sites,
  freq = "hh",
  path = "data/tmp/",
  verbose = TRUE
)

print(test$data[[1]])
plot(test$data[[1]]$fpar)
