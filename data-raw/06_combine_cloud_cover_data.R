#!/usr/bin/Rscript

# This script downloads all required MODIS remote sensing data
# using MODISTools. Both FPAR and LAI are downloaded, all other
# remote sensing products in context of FluxDataKit are provided
# through the FluxnetEO dataset and package.

# This script is best run as a job and or background process as downloading
# all site data takes a while.

# load libraries
library(FluxDataKit)
library(dplyr)
library(ggplot2)

# load site meta-data
sites <- readRDS("data/flux_data_kit_site-info.rds")$sitename[1:2]

cloud_cover <- lapply(sites, function(site){

  message(paste0("--- processing site: ", site, "\n"))

  # list all files for a given site
  files <- list.files(
    "data-raw/cloud_cover/",
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

  print("main file")
  print(head(df))

  # add site name
  df$site <- site

  # return data
  return(df)
})

# bind all data together
cloud_cover <- bind_rows(cloud_cover)

# p <- ggplot(cloud_cover) +
#   geom_line(
#     aes(
#       date,
#       cloud_cover
#     )
#   ) +
#   facet_wrap(~site, scales = "free")
#
# print(p)
