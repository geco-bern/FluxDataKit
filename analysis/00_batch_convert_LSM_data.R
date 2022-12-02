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

#---- create a new release ----

fdk_release(
  df = sites,
  input_path = "/data/scratch/FDK_inputs/",
  output_path = "/data/scratch/test"
)
