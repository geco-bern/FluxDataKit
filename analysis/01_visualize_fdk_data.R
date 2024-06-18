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

failed_sites <- readRDS(here::here("data/failed_sites.rds"))

# load sites
sites <- FluxDataKit::fdk_site_info |>
  filter(!sitename %in% failed_sites)

# site subset------------------
# # xxx debug
# # chose representative sites for LES book
# use_sites <- c(
#   # "FI-Hyy", # Boreal Forests/Taiga
#   # "US-SRM", # Deserts & Xeric Shrublands
#   # "FR-Pue", # Mediterranean Forests, Woodlands & Scrub
#   # "DE-Hai", # Temperate Broadleaf & Mixed Forests
#   "DE-Gri",
#   "DE-Tha"
#   # "US-Tw1", # Temperate Grasslands, Savannas & Shrublands
#   # "AU-How", # Tropical & Subtropical Grasslands, Savannas & Shrubland
#   # "BR-Sa3", # Tropical
#   # "ZM-Mon", # Tropical deciduous forest (xeric woodland)
#   # "US-ICh"  # Tundra
# )
# sites <- sites |>
#   filter(sitename %in% use_sites)
#----------------------------


# loop over all sites and plot all time series
failed_sites <- lapply(sites$sitename, function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(
    try(
      fdk_convert_lsm(
        site = site,
        fluxnet_format = FALSE,
        path = "~/data/FluxDataKit/v3.2"
      )
      )
    )

  print(head(df))

})

