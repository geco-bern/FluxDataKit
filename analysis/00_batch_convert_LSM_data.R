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

input_path <- "/data_2/FluxDataKit/FDK_inputs/"
output_path <- "/data_2/FluxDataKit/v3.4/"

sites <- FluxDataKit::fdk_site_info |>
  mutate(
    data_path = file.path(input_path, "flux_data/")
  )

# site subset------------------
# xxx debug
# # missing patm
# use_sites <- c(
#   "BE-Maa", "CH-Aws", "CH-Cha", "CH-Dav", "CH-Fru", "CH-Oe2", "CZ-Lnz", "CZ-wet",
#   "DE-Akm", "DE-Geb", "DE-Gri", "DE-Hzd", "DE-Kli", "DE-Obe", "DE-Tha", "FI-Hyy",
#   "FI-Ken", "FI-Sii", "FR-FBn", "FR-Lam", "GF-Guy", "GL-Dsk", "IT-Lav", "IT-MBo",
#   "IT-Tor", "RU-Fyo"
# )
use_sites <- c(
  "AR-TF1"
  # "FI-Hyy", # Boreal Forests/Taiga
  # "US-SRM", # Deserts & Xeric Shrublands
  # "FR-Pue", # Mediterranean Forests, Woodlands & Scrub
  # "DE-Hai", # Temperate Broadleaf & Mixed Forests
  # "DE-Gri",
  # "DE-Tha"
  # "US-Tw1", # Temperate Grasslands, Savannas & Shrublands
  # "AU-How", # Tropical & Subtropical Grasslands, Savannas & Shrubland
  # "BR-Sa3", # Tropical
  # "ZM-Mon", # Tropical deciduous forest (xeric woodland)
  # "US-ICh"  # Tundra
)

# use_sites <- readRDS(here::here("data/failed_sites.rds"))
sites <- sites |>
  filter(sitename %in% use_sites)
# ----------------------------

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

  # This is done already in fdk_release()
  message("- converting to FLUXNET format")
  df <- suppressWarnings(
    try(
      fdk_convert_lsm(
        site = site,
        fluxnet_format = TRUE,
        path = file.path(output_path, "lsm"),
        out_path = file.path(output_path, "fluxnet"),
        overwrite = TRUE
        )
      )
    )

  if (inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(site)
  }

  # not plotting these anymore - too slow and too large and too obscure
  # message("- plotting HH FLUXNET data")
  # filename <- suppressMessages(
  #   suppressWarnings(
  #     try(
  #       fdk_plot(
  #         df,
  #         site = site,
  #         out_path = file.path(output_path, "plots"),
  #         overwrite = FALSE
  #       )
  #     )
  #   )
  # )

  message("- plotting DD FLUXNET data")
  # get file name path
  filn <- list.files(
    file.path(output_path, "fluxnet"),
    pattern = paste0("FLX_", site, ".*_FULLSET_DD.*.csv"),
    recursive = TRUE
    )

  if (length(filn) > 0){
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

  } else {
    message("Warning: No daily plot created for this site.")
  }

  return(NULL)
})

saveRDS(unlist(failed_sites), here::here("data/failed_sites.rds"))

