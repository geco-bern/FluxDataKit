# helping draft a script to convert the plumber data
# to a fluxnet compatible format in order to process
# data using the current ingestr routines rather
# than writing another set of code for plumber data
library(tidyverse)
library(ingestr)
source("R/fdk_convert_lsm.R")
source("R/fdk_downsample.R")

# read in demo data
# test <- fdk_convert_lsm(
#   site = "AT-Neu",
#   path = "data/tmp",
#   fluxnet_format = TRUE,
#   meta_data = FALSE
# )

fdk_downsample_fluxnet(
  test
)

# settings_fluxnet <- list(
#   getswc       = FALSE,
#   filter_ntdt  = TRUE,
#   threshold_GPP= 0.8,
#   remove_neg   = FALSE,
#   dir_hh = "data/tmp/"
# )
#
# siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
#   filter(sitename == "AT-Neu")
#
# df <- ingest(
#   siteinfo,
#   source    = "fluxnet",
#   getvars   = list("gpp" = "GPP_VUT_REF"),
#   dir       = "data/tmp/",
#   settings  = settings_fluxnet,
#   timescale = "hh",
#   verbose = TRUE
# )
#
# print(df$data)
