#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
library(rsofun)
#library(rbeni)

source("R/format_site_drivers.R")

# read sites data frame
df_sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  dplyr::select(sitename, lat, lon, year_start, year_end, elv, product) %>%
  mutate(
    year_end = 2018 # force 2018 as end year
  ) %>%
  filter(
    !(year_start > year_end)
  )

data <- df_sites %>%
  rowwise() %>%
  do({

    if (.$product[1] == "oneflux"){
      path = "~/data/flux_data_kit/oneflux/"
    }

    if (.$product[1] == "icos"){
      path = "~/data/flux_data_kit/icos_releaseX/"
    }

    if (.$product[1] == "plumber"){
      path = "~/data/flux_data_kit/plumber_fluxnet/"
    }

    if (.$product[1] == "ameriflux"){
      path = "~/data/flux_data_kit/fluxnet2015/"
    }

    ss <- as.data.frame(.)

    # process data
    df_pmodel <- format_drivers_site(
      ss,
      verbose = TRUE,
      path = path
    )
  })

#filename <- file.path("data/p_model_drivers/", paste0("output_",args[1],".rds"))
#saveRDS(df_pmodel, filename)
