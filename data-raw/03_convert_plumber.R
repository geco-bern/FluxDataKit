# Converts all plumber netcdf files to
# a CSV format

# load libraries
library(FluxDataKit)

# List all files to process
df <- readRDS("data/flux_data_kit_site-info.rds") |>
  filter(
    product == "plumber"
  )

# convert all sites from NETCDF to CSV
df |>
  rowwise() |>
  do({
    fdk_convert_lsm(
      site = .$sitename,
      path = "~/data/FluxDataKit/FDK_inputs/plumber/", #  "/scratch/FDK_inputs/flux_data/plumber/",
      fluxnet_format = TRUE,
      meta_data = FALSE,
      out_path ="~/data/FluxDataKit/FDK_inputs/plumber_fluxnet/" # "/scratch/FDK_inputs/flux_data/plumber_fluxnet/"
    )
  })
