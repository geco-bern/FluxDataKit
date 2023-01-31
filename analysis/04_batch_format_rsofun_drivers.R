# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(dplyr)
library(ggplot2)
library(ingestr)
library(rsofun)
library(FluxDataKit)

input_path <- "/data/scratch/FDK_inputs"

# read in sites to process
sites <- FluxDataKit::fdk_site_info |>
  mutate(
    data_path = file.path(input_path, "flux_data/")
  ) |>
  filter(
    sitename == "SE-Nor"
  )

# loop over all sites and process them to format
# them into the correct rsofun format
driver_data <- lapply(sites$sitename, function(site){

  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = "/data/scratch/beta-v3/"
    )
  ))

  if(inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(NULL)
  }

  message("- downsampling FLUXNET format")
  filename <-
    suppressWarnings(
      try(fdk_downsample_fluxnet(
        df,
        site = site,
        out_path = tempdir(),
        overwrite = TRUE
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
    try(
    suppressWarnings(
      fdk_format_drivers(
        site_info = sites |> filter(sitename == !!site),
        path = "/data/scratch/beta-v3/fluxnet/",  #paste0(tempdir(),"/"),
        verbose = TRUE
      )
    )
  )

  if(inherits(output, "try-error")){
    message("!!! formatting drivers failed  !!!")
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
  "data/rsofun_driver_data.rds",
  compress = "xz"
  )

# #--- visualize some data for cursory checks ---
#
# # optimized parameters from previous
# # work
# params_modl <- list(
#   kphio           = 0.09423773,
#   soilm_par_a     = 0.33349283,
#   soilm_par_b     = 1.45602286,
#   tau_acclim_tempstress = 10,
#   par_shape_tempstress  = 0.0
# )
#
# # run the model for these parameters
# output <- rsofun::runread_pmodel_f(
#   driver_data,
#   par = params_modl,
#   makecheck = TRUE
# )
#
# # we only have one site so we'll unnest
# # the main model output
# model_data <- output |>
#   filter(sitename == "FR-Pue") |>
#   tidyr::unnest(data)
#
# validation_data <- driver_data |>
#   filter(sitename == "FR-Pue") |>
#   tidyr::unnest(forcing)
#
# p <- ggplot() +
#   geom_line(
#     data = model_data,
#     aes(
#       date,
#       gpp
#     ),
#     colour = "red"
#   ) +
#   geom_line(
#     data = validation_data,
#     aes(
#       date,
#       gpp
#     )
#   ) +
#   labs(
#     x = "Date",
#     y = "GPP"
#   )
#
# print(p)
