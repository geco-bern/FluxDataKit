# Batch convert LSM netcdf files to
# the FLUXNET based CSV format
# for easy input output for novices

# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(FluxDataKit)

sites <- readRDS("data/flux_data_kit_site-info.rds")

driver_data <- lapply(sites$sitename, function(site){

  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = "/data/scratch/PLUMBER_X/",
    out_path = "/data/scratch/fluxnet_plumber_x/"
    )
  ))

  if(inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(NULL)
  } else {
    return(invisible())
  }
})
