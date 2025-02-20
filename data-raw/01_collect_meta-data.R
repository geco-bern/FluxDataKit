# This script collects all meta-data
# for all available data products
# (note set the correct paths if these do
# not correspond to the defaults as listed
# below)

library(tidyverse)
library(ingestr)
# remotes::install_github("bluegreen-labs/icoscp")
library(icoscp)
library(RCurl)
library(XML)
library(amerifluxr)
# remotes::install_github("geco-bern/cwd")
library(cwd)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)

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

  # get list of sites
  files <- list.files(
    plumber_path,
    utils::glob2rx(paste0("*Flux.nc")),
    full.names = TRUE,
    recursive = TRUE
  )
  sites <- str_sub(basename(sites), start = 1, end = 6)

  # collect meta data
  # df <- do.call("rbind",
  #         lapply(sites, function(site){
  #           # fdk_convert_lsm(file, meta_data = TRUE)
  #           fdk_convert_lsm(
  #             site,
  #             path = plumber_path,
  #             meta_data = TRUE
  #             )
  #         }
  #     )
  #   )
  df <- purrr::map_dfr(
    sites,
    ~fdk_convert_lsm(
      .,
      path = plumber_path,
      meta_data = TRUE
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

# Climatic meta information ----------------------------------------------------
filnam <- here::here("data-raw/meta_data/climatic_meta_info.rds")
if (!file.exists(filnam)){

  # load rsofun driver data (created by analysis/02_batch_format_rsofun_driver.R)
  driver <- readr::read_rds("/data_2/FluxDataKit/v3.4/zenodo_upload/rsofun_driver_data_v3.4.rds")

  # make flat
  df <- driver %>%
    dplyr::select(sitename, forcing) %>%
    unnest(forcing) %>%
    dplyr::select(sitename, date, temp, rain, snow, ppfd, netrad, patm)

  # impute netrad globally (across all sites) by learning from a reduced dataset
  df_missing <- df |>
    dplyr::filter(is.na(netrad))

  # make data smaller to make KNN handle it
  df_present <- df |>
    dplyr::filter(!is.na(netrad)) |>
    dplyr::sample_n(1.0e4)

  # run imputation
  df_filled <- bind_rows(
    df_missing,
    df_present
  ) |>
    fill_netrad(limit_missing = 0.99) |>
    rename(netrad_filled = netrad)

  # fill missing data in full dataframe
  df <- df |>
    left_join(
      df_filled |>
        dplyr::select(sitename, date, netrad_filled),
      by = join_by(sitename, date)
    ) |>
    mutate(
      netrad = ifelse(is.na(netrad), netrad_filled, netrad)
    ) |>
    dplyr::select(-netrad_filled)

  # check visually for missing data
  visdat::vis_miss(df, warn_large_data = FALSE)

  df <- df %>%

    # get PET and daily total water variables in mm
    mutate(
      year = lubridate::year(date),
      pet = 60 * 60 * 24 * cwd::pet(netrad, temp, patm),
      prec = (snow + rain) *  60 * 60 * 24
    ) %>%

    # get annual sum for prec and pet
    group_by(sitename, year) %>%
    summarise(
      pet = sum(pet),
      prec = sum(prec),
      temp = mean(temp)
    ) %>%

    # get mean annual temperature and aridity index
    ungroup() %>%
    group_by(sitename) %>%
    summarise(
      pet = mean(pet, na.rm = TRUE),
      prec = mean(prec, na.rm = TRUE),
      mat = mean(temp, na.rm = TRUE)
    ) %>%
    mutate(
      p_over_pet = prec / pet
    ) %>%
    dplyr::select(
      sitename, mat, p_over_pet
    )

  # check visually for missing data
  visdat::vis_miss(df, warn_large_data = FALSE)

  saveRDS(df, file = filnam, compress = "xz")
}


# df |>
#   ggplot(aes(mat, ..count..)) +
#   geom_histogram()
#
# df |>
#   ggplot(aes(p_over_pet, ..count..)) +
#   geom_histogram()


# FLUXNET2015-------------------------------------------------------------------
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

# SAPFLUXNET meta information --------------------------------------------------
# Following quick guide:
# https://cran.r-project.org/web/packages/sapfluxnetr/vignettes/sapfluxnetr-quick-guide.html
filnam <- here::here("data-raw/meta_data/sapfluxnet_meta_info.rds")
if (!file.exists(filnam)){

  install.packages('sapfluxnetr')
  library(sapfluxnetr)

  dir <- "~/data_2/sapfluxnet/"

  # # Data is publicly available on Zenodo to download.
  # download.file(
  #   url = "https://zenodo.org/record/3971689/files/0.1.5.zip?download=1",
  #   destfile = "~/data_2/sapfluxnet/0.1.5.zip"
  # )
  #
  # # unzip the data
  # # BE SURE YOU HAVE AT LEAST 24GB OF DISK SPACE
  # unzip(paste0(dir, "0.1.5.zip"))
  #
  # # check if files are present
  # list.files(file.path(paste0(dir, "0.1.5"), 'RData', 'plant'))
  # list.files(file.path(paste0(dir, "0.1.5"), 'csv', 'plant'))

  # collect meta data from all sites
  sfn_metadata <- read_sfn_metadata(
    folder = paste0(dir, "0.1.5/RData/plant/"),
    .write_cache = TRUE
    )

  # for scaleup table: temperate and boreal forest sites
  sites_sfn <- sfn_metadata$site_md |>
    dplyr::filter(si_biome %in% c("Temperate forest", "Boreal forest"), si_flux_network) |>
    dplyr::select(si_code, si_long, si_lat, si_igbp, si_biome)

  # join with FLUXNET site info table, identifying site matches by whether
  # longitude and latitude values are within 0.1 degrees.
  sites_sfn <- fuzzyjoin::fuzzy_inner_join(
    sites_sfn,
    FluxDataKit::fdk_site_info,
    by = c("si_long" = "lon", "si_lat" = "lat"),
    match_fun = list(
      function(x, y) abs(x - y) <= 0.1,  # Condition for value1a and value2a
      function(x, y) abs(x - y) <= 0.1  # Condition for value1b and value2b
      )
    )

  # Complement with species information
  sites_sfn <- sites_sfn |>
    dplyr::left_join(
      sfn_metadata$species_md |>
        dplyr::group_by(si_code) |>
        dplyr::summarise(
          species = paste(sp_name, collapse = ", "),
          .groups = "drop"  # Ensures the result is not grouped
        )
      ) |>
    dplyr::select(id_fluxnet = sitename, id_sapfluxnet = si_code, si_biome, si_igbp, species)


}
