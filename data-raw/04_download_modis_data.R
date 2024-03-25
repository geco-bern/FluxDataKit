#!/usr/bin/Rscript

# This script downloads all required MODIS remote sensing data
# using MODISTools. Both FPAR and LAI are downloaded, all other
# remote sensing products in context of FluxDataKit are provided
# through the FluxnetEO dataset and package.

# This script is best run as a job and or background process as downloading
# all site data takes a while.

# load libraries
library(FluxDataKit)

 # download the modis data
fdk_download_modis(
  df = fdk_site_info,
  path = "~/data/FluxDataKit/FDK_inputs/modis/"
)
