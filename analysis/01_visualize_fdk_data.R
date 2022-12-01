# This routines visualizes the output
# of the Land Surface Model (netcdf) conversion
# and allows you to quickly check data conversions
# do note that some variables might be missing
# depending on the input available (which varies
# on a site by site basis)
#
# Failed sites are trapped and reported at
# the end of the routine.

# load libraries
library(tidyverse)
library(FluxDataKit)

# load sites
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
    filter(
      sitename %in% c("FR-Fon")
    )

# loop over all sites and plot all time series
failed_sites <- lapply(sites$sitename, function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = "/data/scratch/PLUMBER_X/lsm/"
  )
  ))

  if(inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(site)
  }

  message("- plotting FLUXNET data")
  filename <- suppressMessages(
    suppressWarnings(
      try(fdk_plot(
        df,
        site = site,
        out_path = "/data/scratch/PLUMBER_X/plots/",
        overwrite = TRUE
      )
      )
    )
  )

  if(inherits(filename, "try-error")){
    message("!!! plotting failed !!!")
    return(site)
  }

  return(NULL)
})

# list failed sites
failed_sites <- do.call("rbind", failed_sites)
print(failed_sites)
