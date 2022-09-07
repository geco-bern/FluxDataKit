# helping draft a script to convert the plumber data
# to a fluxnet compatible format in order to process
# data using the current ingestr routines rather
# than writing another set of code for plumber data
library(tidyverse)
library(ingestr)
source("R/fdk_convert_lsm.R")
source("R/fdk_downsample_fluxnet.R")
source("R/fdk_plot.R")

# read in demo data
# test <- fdk_convert_lsm(
#   site = "AT-Neu",
#   path = "data/tmp",
#   fluxnet_format = TRUE,
#   meta_data = FALSE
# )

# fdk_downsample_fluxnet(
#   test,
#   site = "AT-Neu",
#   out_path = "data/tmp/"
# )

fdk_plot(
  site = "AT-Neu",
  path = "data/tmp/"
)

