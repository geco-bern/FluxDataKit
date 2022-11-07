
# load libraries
library(tidyverse)
library(FluxDataKit)
lapply(list.files("R/","*", full.names = TRUE), source)

# load sites
sites <- readRDS("data/flux_data_kit_site-info.rds")

# loop over all sites and plot all time series
failed_sites <- lapply(sites$sitename, function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = "/data/scratch/PLUMBER_X/"
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
        out_path = "/data/scratch/fluxnet_plots/",
        overwrite = TRUE
      )
      )
    )
  )

  if(inherits(filename, "try-error")){
    message("!!! downsampling failed !!!")
    return(site)
  }

  return(NULL)
})

failed_sites <- do.call("rbind", failed_sites)
print(failed_sites)
