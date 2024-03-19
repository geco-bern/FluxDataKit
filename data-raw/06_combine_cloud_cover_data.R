#!/usr/bin/Rscript

# This script is best run as a job and or background process as downloading
# all site data takes a while.

# load libraries
library(FluxDataKit)
library(dplyr)
library(ggplot2)

# load site meta-data
sites <- readRDS("data/flux_data_kit_site-info.rds")

cloud_cover <- lapply(sites$sitename, function(site){

  message(paste0("--- processing site: ", site, "\n"))

  # list all files for a given site
  files <- list.files(
    "~/data/FluxDataKit/FDK_inputs/cloud_cover/",
    glob2rx(paste0(site,"*.nc")),
    full.names = TRUE
    )

  # if no files are there move on
  # return NULL will ignored by lapply()
  if (length(files) == 0){
    message("Nothing found")
    return(NULL)
  }

  df <- lapply(files, function(file){

    # read netcdf file
    r <- terra::rast(file)

    # split out data
    dates <- as.POSIXct(terra::time(r), "%Y-%m-%d %H:%M:%S", tz = "GMT")
    cloud_cover <- unlist(as.data.frame(r))

    # combine in data frame
    df <- data.frame(
      date_time = dates,
      cloud_cover = cloud_cover
    )

    # take daily mean
    df <- df |>
      mutate(
        date = as.Date(date_time)
      ) |>
      group_by(date) |>
      summarize(
        cloud_cover = mean(cloud_cover, na.rm = TRUE)
      ) |>
      ungroup()

    # return data
    return(df)
  })

  # combine list elements
  df <- bind_rows(df)

  # add site name
  df$site <- site

  # return data
  return(df)
})

# bind all data together
cloud_cover <- bind_rows(cloud_cover)

# missing data for some sites - take them from the cloud cover data extracted
# from CRU
missing_sites <- sites$sitename[which(!(sites$sitename %in% cloud_cover$site))]

cloud_cover_cru <- read_rds("~/data/FluxDataKit/FDK_inputs/cloud_cover/df_cru.rds")

cloud_cover_cru <- cloud_cover_cru |>
  filter(sitename %in% missing_sites) |>
  unnest(data) |>
  rename(cloud_cover = ccov,
         site = sitename) |>
  mutate(cloud_cover = cloud_cover/100)

cloud_cover <- cloud_cover |>
  bind_rows(
    cloud_cover_cru
  ) |>
  arrange(site) |>
  dplyr::select(-ccov_int)

# save the data
saveRDS(cloud_cover, "~/data/FluxDataKit/FDK_inputs/cloud_cover/cloud_cover.rds", compress = "xz")
