# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(dplyr)
library(tidyr)

lapply(list.files("R/","*.R", full.names = TRUE), source)

# read in sites to process
site <- "FR-Pue"

# get half-hourly data  --------------------------------------------------------
message("- convert to FLUXNET standard CSV file")
hhdf <- suppressWarnings(
  try(
    fdk_convert_lsm(
      site = site,
      fluxnet_format = TRUE,
      path = "~/data/flux_data_kit_beta/fluxes/"
      )
    )
  )

if(inherits(hhdf, "try-error")){
  message("!!! conversion to FLUXNET failed  !!!")
  return(NULL)
}

# Aggregate to daily  ----------------------------------------------------------
message("- downsampling FLUXNET format")
ddf <-
  suppressWarnings(
    try(
      fdk_downsample_fluxnet(
        hhdf,
        site = site,
        overwrite = TRUE,
        out_path = tempdir()
      )
    )
  )


# Creating driver object  ------------------------------------------------------
message("- compiling drivers")
# Use a uniform FLUXNET HH input
# file to generate p-model (rsofun)
# compatible driver data
df_drivers <-
  try(
    suppressWarnings(
      fdk_format_drivers(
        site_info = FluxDataKit::fdk_site_info |>
          filter(sitename == !!site),
        path = paste0(tempdir(),"/"),
        verbose = TRUE
      )
    )
  )

# write all drivers to file
# apply compression to minimize space
filn <- "data/p_model_drivers.rda"
message(paste0("- writing to file", filn))
saveRDS(
  df_drivers,
  filn
  )
