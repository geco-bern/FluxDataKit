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

data <-
  apply(
  df_sites,
  1,
  function(site){
    message(site['sitename'])

    ss <- as.data.frame(site)
    print(ss)

    # process data
    df <- try(
      format_drivers_site(
        ss,
        verbose = FALSE,
        product = site['product']
      )
    )

    if(inherits(df, "try-error")){
      return(NULL)
    } else {
      return(df)
    }

  })

saveRDS(
  data,
  "data/p_model_drivers/site_based_drivers.rds",
  compress = "xz")
