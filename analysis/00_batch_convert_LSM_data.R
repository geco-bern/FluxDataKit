# Batch conversion of FLUXNET data to LSM formatting
# in line with the PLUMBER2 release
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(FluxDataKit)
library(FluxnetLSM)
library(dplyr)
library(ingestr)
library(rsofun)

input_path <- "/data/scratch/FDK_inputs"
output_path <- "/data/scratch/beta-v3"

# read in all site meta-data, only test on
# SE-Nor to debug FluxnetLSM for now
sites <- FluxDataKit::fdk_site_info |>
  mutate(
    data_path = file.path(input_path, "flux_data/")
  )

#---- create a new release ----

fdk_release(
  df = sites,
  input_path = input_path,
  output_path = output_path,
  overwrite = TRUE
)

#---- create matching plots ----

# loop over all sites and plot all time series
failed_sites <- lapply(sites$sitename, function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = file.path(output_path,"lsm")
  )
  ))

  if(inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(site)
  }

  message("- plotting FLUXNET data")
  filename <- suppressMessages(
    suppressWarnings(
      try(fdk_plot(
        df,
        site = site,
        out_path = file.path(output_path, "plots"),
        overwrite = TRUE
      )
      )
    )
  )

  if(inherits(filename, "try-error")){
    message("!!! plotting failed !!!")
    return(site)
  }

  return(NULL)
})
