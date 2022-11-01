# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(tidyverse)
#detach("package:FluxnetLSM", unload = TRUE)
#detach("package:ingestr", unload = TRUE)
library(ingestr)

source("R/fdk_process_lsm.R")
source("R/fdk_correct_era.R")
source("R/fdk_smooth_ts.R")
source("R/fdk_download_modis.R")
source("R/site_exceptions.R")
source("R/fdk_correct_fluxes.R")
source("R/fdk_balance_energy.R")
source("R/helper_functions.R")
source("R/fdk_convert_lsm.R")
source("R/fdk_format_drivers.R")
source("R/fdk_downsample_fluxnet.R")

sites <- readRDS("data/flux_data_kit_site-info.rds")

lapply(sites$sitename, function(site){

  message(site)

  df <- try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = "/data/scratch/PLUMBER_X/"
  ))

  if(inherits(df, "try-error")){
    message("conversion to FLUXNET failed")
    return(NULL)
  }

  filename <- fdk_downsample_fluxnet(
    df,
    site = site,
    out_path = tempdir()
  )

  if(inherits(filename, "try-error")){
    message("downsampling failed")
    return(NULL)
  }

  # Use a uniform FLUXNET HH input
  # file to generate p-model (rsofun)
  # compatible driver data
  test <- fdk_format_drivers(
    site_info = sites,
    freq = "d",
    path = paste0(tempdir(),"/"), # f-ing trailing /
    verbose = TRUE
  )

  if(inherits(test, "try-error")){
    message("formatting drivers failed")
    return(NULL)
  }

  print(test$data[[1]])
  plot(test$data[[1]]$fpar)

})
