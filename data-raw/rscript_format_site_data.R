#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
freq <- args[1]
freq <- "daily"

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
source("R/collect_drivers_rsofun.R")
source("R/format_site_drivers.R")
source("R/prepare_setup_sofun.R")

# read sites data frame
df_sites <- readRDS("data/flux_data_kit_site-info.rds")

# %>%
#   filter(product == "oneflux")

data <- df_sites %>%
  rowwise() %>%
  do({

    ss <- as.data.frame(.)

    print(ss)

    # process data
    df <- try(
      #suppressWarnings(
      format_drivers_site(
        ss,
        verbose = FALSE,
        product = .$product[1],
        freq = freq
      )
      #)
    )

    if(inherits(df, "try-error")){
      df <- data.frame(
          sitename = ss$sitename,
          forcing = NA,
          params_siml = NA,
          site_info = NA,
          params_soil = NA
        )
    } else {
      df
    }
  })

# remove empty locations
# some Ameriflux sites are not
# available
data <- data %>%
  filter(!is.null(forcing))

if (freq == "hh"){

  data <- data %>%
    unnest()

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
