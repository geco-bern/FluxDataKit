# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(ingestr)
library(rsofun)
# lapply(list.files("R/","*.R", full.names = TRUE), source)

input_path <- "~/data/FluxDataKit/v3/fluxnet/"

# read in sites to process
sites <- FluxDataKit::fdk_site_info

# loop over all sites and process them to format
# them into the correct rsofun format
driver_data <- lapply(sites$sitename, function(site){

  message(sprintf("Processing %s ----", site))

  message("- compiling drivers")
  # Use a uniform FLUXNET HH input
  # file to generate p-model (rsofun)
  # compatible driver data

 output <-
    try(
    suppressWarnings(
      fdk_format_drivers(
        site_info = sites |> filter(sitename == !!site),
        path = input_path,
        verbose = TRUE
      )
    )
  )

  if(inherits(output, "try-error")){
    message(paste0("!!! formatting drivers failed for",site,"!!!"))
    return(NULL)
  } else {
    return(output)
  }
})

# bind all tibbles into one big tibble
driver_data <- dplyr::bind_rows(driver_data)

# write all drivers to file
# apply compression to minimize space
saveRDS(
  driver_data,
  "~/data/FluxDataKit/v3/rsofun_driver_data_v3.rds",
  compress = "xz"
  )

