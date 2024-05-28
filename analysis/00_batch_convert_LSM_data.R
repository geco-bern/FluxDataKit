# Batch conversion of FLUXNET data to LSM formatting
# in line with the PLUMBER2 release
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
lapply(list.files("R/","*.R", full.names = TRUE), source)
library(FluxDataKit)
library(FluxnetLSM)
library(dplyr)
library(ingestr)
library(rsofun)

input_path <- "~/data/FluxDataKit/FDK_inputs"
output_path <- "~/data/FluxDataKit/v3.1"

sites <- FluxDataKit::fdk_site_info |>
  mutate(
    data_path = file.path(input_path, "flux_data/")
  )

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

#---- create a new release ----
fdk_release(
  df = sites,
  input_path = input_path,
  output_path = output_path,
  overwrite_lsm = FALSE,
  overwrite_fluxnet = FALSE
)

#---- create matching plots ----
# loop over all sites and plot all time series
failed_sites <- lapply(sites$sitename, function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(
    try(
      fdk_convert_lsm(
        site = site,
        fluxnet_format = TRUE,
        path = file.path(output_path, "lsm"),
        overwrite = FALSE
        )
      )
    )

  if(inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(site)
  }

  message("- plotting HH FLUXNET data")
  filename <- suppressMessages(
    suppressWarnings(
      try(
        fdk_plot(
          df,
          site = site,
          out_path = file.path(output_path, "plots"),
          overwrite = FALSE
        )
      )
    )
  )

  message("- plotting DD FLUXNET data")
  # get file name path
  filn <- list.files(
    file.path(output_path, "fluxnet"),
    pattern = paste0("FLX_", site, ".*_FULLSET_DD.*.csv"),
    recursive = TRUE
    )

  df <- read.csv(file.path(file.path(output_path, "fluxnet"), filn))

  filename <- suppressMessages(
    suppressWarnings(
      try(
        fdk_plot(
          df,
          site = site,
          out_path = file.path(output_path, "plots"),
          overwrite = FALSE,
          daily = TRUE
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

saveRDS(unlist(failed_sites), here::here("data/failed_sites.rds"))

