#!/usr/bin/env Rscript

# load libraries and
# scripts
library(tidyverse)
library(ingestr)

df <- readRDS("data/flux_data_kit_site-info.rds") %>%
  filter(
    product == "plumber"
  )

# convert all sites from NETCDF to CSV
df %>%
  rowwise() %>%
  do({
    fdk_read_plumber(
      site = .$sitename,
      path = "data-raw/flux_data/plumber/",
      fluxnet_format = TRUE,
      meta_data = FALSE,
      out_path = "data-raw/flux_data/plumber_fluxnet/"
    )
  })
