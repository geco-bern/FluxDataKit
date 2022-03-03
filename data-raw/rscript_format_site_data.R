#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
path <- "data/p_model_drivers/"

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
library(rsofun)
library(rbeni)

source("R/format_drivers.R")
#source("R/process_pmodel_data.R")
#source("R/calc_climate_index.R")

# read sites data frame
df_sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  dplyr::select(sitename, lat, lon, year_start, year_end, elv) %>%
  mutate(
    year_end = 2018 # force 2018 as end year
  ) %>%
  filter(
    !(year_start > year_end)
  ) %>%
  mutate(idx = 1:n()) %>%
  mutate(
    chunk = rep(1:as.integer(args[2]),
      each = (nrow(.)/as.integer(args[2])), len = nrow(.)))

# split sites data frame into (almost) equal chunks
list_df_split <- df_sites %>%
  group_by(chunk) %>%
  group_split()

df_sites_sub <- list_df_split[[as.integer(args[1])]] %>%
  dplyr::select(-c(chunk, idx))


print(df_sites_sub)

message("Processing cells:")
message(nrow(df_sites_sub))

# process data
df_pmodel <- format_drivers(
    df_sites_sub,
    bias_correction = TRUE,
    verbose = TRUE,
    run_model = FALSE
    )

filename <- file.path(path, paste0("output_",args[1],".rds"))
saveRDS(df_pmodel, filename)
