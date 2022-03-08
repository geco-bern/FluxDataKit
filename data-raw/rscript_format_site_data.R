#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
library(rsofun)
library(rbeni)

source("R/format_drivers_site.R")

# read sites data frame
df_sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  dplyr::select(sitename, lat, lon, year_start, year_end, elv) %>%
  mutate(
    year_end = 2018 # force 2018 as end year
  ) %>%
  filter(
    !(year_start > year_end)
  )

df_sites %>%
  group_by(product) %>%
  do({

    if (.$product == "icos"){
      path = "~/data/flux_data_kit"
    }

    # process data
    df_pmodel <- format_drivers_site(
      df_sites_sub,
      bias_correction = TRUE,
      verbose = TRUE,
      path = path
    )
  })


filename <- file.path("data/p_model_drivers/", paste0("output_",args[1],".rds"))
saveRDS(df_pmodel, filename)
