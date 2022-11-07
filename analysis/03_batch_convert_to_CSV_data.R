# Batch convert LSM netcdf files to
# the FLUXNET based CSV format
# for easy input output for novices

# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(FluxDataKit)
lapply(list.files("R/","*", full.names = TRUE), source)

sites <- readRDS("data/flux_data_kit_site-info.rds")

failed_sites <- lapply(sites$sitename[1], function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = "/data/scratch/PLUMBER_X/"
  )
  ))

  if(inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(site)
  }

  message("- downsampling FLUXNET format")
  filename <- suppressMessages(
    suppressWarnings(
      try(fdk_downsample_fluxnet(
        df,
        site = site,
        out_path = "/data/scratch/fluxnet_plumber_x/"
      )
      )
    )
  )

  if(inherits(filename, "try-error")){
    message("!!! downsampling failed !!!")
    return(site)
  }

  return(NULL)
})

failed_sites <- do.call("rbind", failed_sites)
