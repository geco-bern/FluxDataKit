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

data <- df_sites[1:2,] %>%
  rowwise() %>%
  do({

    message(.$sitename[1])

    ss <- as.data.frame(.)

    # process data
    df_pmodel <- format_drivers_site(
      ss,
      verbose = TRUE,
      product = .$product[1]
    )
  })

print(data)

#filename <- file.path("data/p_model_drivers/", paste0("output_",args[1],".rds"))
#saveRDS(df_pmodel, filename)
