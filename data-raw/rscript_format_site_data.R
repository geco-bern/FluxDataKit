#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
freq <- args[1]

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
library(rsofun)

source("R/format_site_drivers.R")

# read sites data frame
df_sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  dplyr::select(sitename, lat, lon, year_start, year_end, elv, product)

data <- df_sites %>%
  rowwise() %>%
  do({
    ss <- as.data.frame(.)

    # process data
    df <- try(
      format_drivers_site(
        ss,
        verbose = FALSE,
        product = .$product[1],
        freq = freq
      )
    )

    if(inherits(df, "try-error")){
      df <- data.frame(
        forcing = NA
        )
    } else {
      df
    }
  })

if (freq == "hh"){

  saveRDS(
    data,
    "data/site_based_drivers_HH.rds",
    compress = "xz")

  } else {
  saveRDS(
    data,
    "data/p_model_drivers/site_based_drivers.rds",
    compress = "xz")
}

