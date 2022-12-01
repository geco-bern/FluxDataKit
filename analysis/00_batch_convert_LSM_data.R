# Batch conversion of FLUXNET data to LSM formatting
# in line with the PLUMBER2 release
#library(FluxDataKit)

lapply(list.files("R/","*.R", full.names = TRUE), function(file){
  source(file)
})

library(dplyr)

# Check for FluxnetLSM library
# should be installed but just in case
if(!require(devtools)){install.packages("devtools")}
if(!require(FluxnetLSM)){
  devtools::install_github("computationales/FluxnetLSM")
  }

# Renew install (for debugging purposes)
detach("package:FluxnetLSM", unload = TRUE)
library(FluxnetLSM)

# read in all site meta-data, only test on
# SE-Nor to debug FluxnetLSM for now
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
  mutate(
    data_path = "/data/scratch/FDK_inputs/flux_data/"
  ) |>
  filter(
    sitename == "FR-Fon"
  )

#---- FluxnetLSM reprocessing routine ----

# process all sites, by calling the processing routine
# all data is returned to the specified output path (out_path)
fdk_process_lsm(
  sites,
  out_path = "/data/scratch/PLUMBER_X/lsm/",
  modis_path = "/data/scratch/FDK_inputs/modis/",
  format = "lsm",
  overwrite = TRUE,
  save_tmp_files = FALSE
)

# quick check
orig <- fdk_convert_lsm(
  site = "FR-Fon",
  path = "/data/scratch/FDK_inputs/flux_data/plumber/"
)

df <- fdk_convert_lsm(
  site = "FR-Fon",
  path = "/data/scratch/PLUMBER_X/lsm/",
  out_path = "/data/scratch/PLUMBER_X/",
  fluxnet_format = TRUE
)

plot(orig$time,orig$LAI)
points(df$time,df$LAI, col = "red")
