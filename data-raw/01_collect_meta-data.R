# This script collects all meta-data
# for all available data products
# (note set the correct paths if these do
# not correspond to the defaults as listed
# below)

library(ingestr)
library(icoscp)
library(RCurl)
library(XML)
library(dplyr)
library(amerifluxr)

# Set local paths of data files -------------------------------------------------
# Get PLUMBER2 data from https://dx.doi.org/10.25914/5fdb0902607e1
# plumber_path <- "/scratch/FDK_inputs/flux_data/plumber_fluxnet/"
plumber_path <- "~/data/FluxDataKit/FDK_inputs/flux_data/plumber/"

# Get Ameriflux data from Downloaded data on 14 Oct 2023 from https://ameriflux.lbl.gov/.
path_ameriflux <- "~/data/FluxDataKit/FDK_inputs/flux_data/ameriflux/"

# Get ICOS Drought2018 data from https://doi.org/10.18160/YVR0-4898.
path_icos_drought2018 <- "~/data/FluxDataKit/FDK_inputs/flux_data/icos_drought_2018/"

# Get ICOS WarmWinter2020. data from https://doi.org/10.18160/2G60-ZHAK
path_icos_warm_winter_2020 <- "~/data/FluxDataKit/FDK_inputs/flux_data/icos_warm_winter_2020/"

# No other sources to be used!
# oneflux_path <- "/scratch/FDK_inputs/flux_data/oneflux/"
# icos_path <- "/scratch/FDK_inputs/flux_data/icos/"
# fluxnet_path <- "/scratch/FDK_inputs/flux_data/fluxnet2015/"
# fluxnet_path <- "~/data/FLUXNET-2015_Tier1/"

# Plumber ----------------------------------------------------------------------
filnam <- here::here("data-raw/meta_data/plumber_meta-data.rds")
if (!file.exists(filnam)){

  # list files
  files <- list.files(
    plumber_path,
    utils::glob2rx(paste0("*Flux.nc")),
    full.names = TRUE,
    recursive = TRUE
  )

  # collect meta data
  df <- do.call("rbind",
          lapply(files, function(file){
            fdk_convert_lsm(file, meta_data = TRUE)
          }
      )
    )

  # save output to file
  saveRDS(df, file = filnam, compress = "xz")
}

# Ameriflux  -------------------------------------------------------------------
filnam <- here::here("data-raw/meta_data/amf_meta_data.rds")
if (!file.exists(filnam)){

  # Downloaded data on 14 Oct 2023 from https://ameriflux.lbl.gov/.
  dirs <- list.files(path = path_ameriflux, pattern = "_FLUXNET_FULLSET_")

  # take only sites for which there is data
  amf <- tibble(site = str_sub(dirs, start = 5, end = 10)) |>

    # complement with meta info
    left_join(
      amerifluxr::amf_site_info() |>
        rename(site = SITE_ID),
      by = "site"
    ) |>

    # determine number of years data
    mutate(nyears = DATA_END - DATA_START + 1)

  saveRDS(amf, file = filnam, compress = "xz")
}

# ICOS Drought 2018 release ---------------------------------------------------
## Data downloaded from https://doi.org/10.18160/YVR0-4898.
filnam <- here::here("data-raw/meta_data/icos_drought2018_meta_data.rds")
if (!file.exists(filnam)){

  dirs <- list.files(path = path_icos_drought2018, pattern = "_FLUXNET2015_FULLSET_")

  sites_icos_drought2018 <- tibble(
    site = str_sub(dirs, start = 5, end = 10),
    year_start = as.integer(str_sub(dirs, start = 32, end = 35)),
    year_end = as.integer(str_sub(dirs, start = 37, end = 40))) |>
    mutate(nyears = year_end - year_start + 1) |>
    left_join(
      icoscp::icos_stations() |>
        filter(
          theme == "ES"
        ) |>
        rename(site = id),
      by = "site"
    )

  saveRDS(sites_icos_drought2018, file = filnam, compress = "xz")
}

# ICOS Warm Winter 2020 release-------------------------------------------------
## Data downloaded from https://doi.org/10.18160/2G60-ZHAK
filnam <- here::here("data-raw/meta_data/icos_warmwinter2020_meta_data.rds")
if (!file.exists(filnam)){

  dirs <- list.files(path = path_icos_warm_winter_2020, pattern = "_FLUXNET2015_FULLSET_")

  # interpret directory names
  sites_icos_warm_winter_2020 <- tibble(
    site = str_sub(dirs, start = 5, end = 10),
    year_start = as.integer(str_sub(dirs, start = 32, end = 35)),
    year_end = as.integer(str_sub(dirs, start = 37, end = 40))) |>
    mutate(nyears = year_end - year_start + 1) |>
    left_join(
      icoscp::icos_stations() |>
        filter(
          theme == "ES"
        ) |>
        rename(site = id),
      by = "site"
    )

  saveRDS(sites_icos_warm_winter_2020, file = filnam, compress = "xz")
}

# # FLUXNET2015-------------------------------------------------------------------
# filnam <- here::here("data-raw/meta_data/fluxnet_meta_data.rds")
# if (!file.exists(filnam)){
#
#   fluxnet_list <- read_csv("data-raw/meta_data/fluxnet2015_site_list.csv") |>
#     filter(
#       license == "CC-BY-4.0"
#     )
#
#   fluxnet_sites <- unique(substring(list.files(fluxnet_path,"*"),5,10))
#
#   fluxnet_list <- fluxnet_list |>
#     filter(id %in% fluxnet_sites)
#
#   fluxnet_files <- list.files(
#     fluxnet_path,
#     glob2rx("*FULLSET_DD*"),
#     recursive = TRUE,
#     full.names = TRUE
#   )
#
#   years <- lapply(fluxnet_sites, function(site){
#
#     df <- read.table(
#       fluxnet_files[grep(site, fluxnet_files)],
#       header = TRUE,
#       sep = ",")
#
#     year_end <- max(as.numeric(substr(df$TIMESTAMP,1,4)))
#     year_start <- min(as.numeric(substr(df$TIMESTAMP,1,4)))
#
#     return(
#       data.frame(
#         sitename = site,
#         year_start,
#         year_end
#       ))
#   })
#
#   years <- bind_rows(years)
#
#   fluxnet_list <- fluxnet_list |>
#     rename(
#       'sitename' = 'id'
#     ) |>
#     left_join(years)
#
#   saveRDS(fluxnet_list, file = filnam, compress = "xz")
#
# }
