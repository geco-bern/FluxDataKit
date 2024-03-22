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
sites <- FluxDataKit::fdk_site_info |>
    filter(
      sitename %in% c("FR-Fon")
    )

# loop over all sites and plot all time series
failed_sites <- lapply(sites$sitename, function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = FALSE,
    path = "~/data/FluxDataKit/v3"
  )
  ))

  print(head(df))

})

