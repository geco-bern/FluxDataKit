# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
library(ingestr)
library(rsofun)
#library(FluxDataKit)

lapply(list.files("R/","*", full.names = TRUE), source)

sites <- readRDS("data/flux_data_kit_site-info.rds")

driver_data <- lapply(sites$sitename[1], function(site){

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
    return(NULL)
  }

  message("- downsampling FLUXNET format")
  filename <- suppressMessages(
    suppressWarnings(
      try(fdk_downsample_fluxnet(
        df,
        site = site,
        out_path = tempdir()
      )
      )
    )
  )

  if(inherits(filename, "try-error")){
    message("!!! downsampling failed !!!")
    return(NULL)
  }

  message("- compiling drivers")
  # Use a uniform FLUXNET HH input
  # file to generate p-model (rsofun)
  # compatible driver data
  output <-
    #suppressMessages(
    #suppressWarnings(
      fdk_format_drivers(
        site_info = sites |> filter(sitename == !!site),
        freq = "d",
        path = paste0(tempdir(),"/"), # f-ing trailing /
        verbose = TRUE
      )
  #  )
  #)

  if(inherits(output, "try-error")){
    message("!!! formatting drivers failed  !!!")
    return(NULL)
  } else {
    return(output)
  }
})

# bind all tibbles
driver_data <- dplyr::bind_rows(driver_data)

# write all drivers to file
#saveRDS(driver_data, "data/rsofun_driver_data.rds", compress = "xz")
