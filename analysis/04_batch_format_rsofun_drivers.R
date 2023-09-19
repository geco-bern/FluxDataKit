# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(rsofun)
lapply(list.files("R/","*.R", full.names = TRUE), source)

input_path <- "/data/scratch/beta-v3/fluxnet/"

# read in sites to process
sites <- FluxDataKit::fdk_site_info

site <- c("FR-Pue", "CH-Lae", "CH-Dav")

# subset sites
sites <- sites |>
  dplyr::filter(
    sitename %in% site
  )

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
  "data/rsofun_driver_data.rds",
  compress = "xz"
  )

#--- visualize some data for cursory checks ---

# optimized parameters from previous
# work
params_modl <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41
)

# run the model for these parameters
output <- rsofun::runread_pmodel_f(
  driver_data,
  par = params_modl,
  makecheck = TRUE
)

# we only have one site so we'll unnest
# the main model output
model_data <- output |>
  filter(sitename %in% site) |>
  tidyr::unnest("data")

validation_data <- driver_data |>
  filter(sitename %in% site) |>
  tidyr::unnest(forcing)

p <- ggplot() +
  geom_line(
    data = model_data,
    aes(
      date,
      gpp
    ),
    colour = "red"
  ) +
  geom_line(
    data = validation_data,
    aes(
      date,
      gpp
    )
  ) +
  labs(
    x = "Date",
    y = "GPP"
  ) +
  facet_wrap(~sitename)

print(p)
