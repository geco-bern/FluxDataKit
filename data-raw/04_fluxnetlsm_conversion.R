try(detach("package:FluxnetLSM", unload = TRUE))
library(tidyverse)
library(FluxnetLSM)

# select datasets / sites to process
datasets <- c("fluxnet2015","icos","oneflux")
sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  filter(
    product != "plumber"
  )

fdk_process_lsm <- function(df){

  # list all datasets
  datasets <- unique(sites$product)

  # loop over all datasets and sites
  lapply(datasets, function(dataset){

    message(sprintf("---- processing product: %s", dataset))

    site_selection <- sites %>%
      filter(
        product == !!dataset
      )

    lapply(site_selection$sitename, function(site_code){

      message(sprintf("-- processing site: %s", site_code))

      # This directory should contain appropriate data from
      # http://fluxnet.fluxdata.org/data/fluxnet2015-dataset/
      in_path <- file.path("data-raw/flux_data", dataset)
      ERA_path <- file.path("data-raw/flux_data", dataset)

      #Outputs will be saved to this directory
      out_path <- file.path("data/fluxnetlsm", dataset)

      #--- Automatically retrieve all Fluxnet files in input directory ---#

      # Input Fluxnet data file (using FULLSET in this example,
      # see R/Helpers.R for details)

      if (dataset != "fluxnet2015"){
        infile <- get_fluxnet_files(
          in_path,
          site_code,
          resolution = "HH",
          datasetversion = "[A-Z]{4}-[0-9]{1}"
        )

        # Retrieve dataset version
        datasetversion <- get_fluxnet_version_no(
          infile
        )

        # Retrieve ERAinterim file
        era_file <- get_fluxnet_erai_files(
          ERA_path,
          site_code,
          resolution = "HH",
          datasetversion = "[A-Z]{4}-[0-9]{1}"
        )

      } else {
        infile <- get_fluxnet_files(
          in_path,
          site_code,
          resolution = "HH"
        )

        # Retrieve dataset version
        datasetversion <- get_fluxnet_version_no(
          infile
        )

        # Retrieve ERAinterim file
        era_file <- get_fluxnet_erai_files(
          ERA_path,
          site_code,
          resolution = "HH"
        )
      }

      #---- Settings ----

      #Retrieve default processing options
      conv_opts <- get_default_conversion_options()

      # Set gapfilling options to ERAinterim
      conv_opts$met_gapfill  <- "ERAinterim"

      # Thresholds for missing and gap-filled time steps
      missing_met <- 100   #max. percent missing (must be set)
      missing_flux <- 100
      gapfill_met_tier1 <- 100  #max. gapfilled percentage
      gapfill_met_tier2 <- 100
      gapfill_flux <- 100
      min_yrs <- 1   #min. number of consecutive years

      #---- Run analysis ----

      try(
        convert_fluxnet_to_netcdf(
          infile = infile,
          site_code = site_code,
          out_path = out_path,
          met_gapfill = "ERAinterim",
          flux_gapfill = "statistical",
          era_file = era_file,
          missing_met = missing_met,
          missing_flux = missing_flux,
          gapfill_met_tier1 = gapfill_met_tier1,
          gapfill_met_tier2 = gapfill_met_tier2,
          gapfill_flux=gapfill_flux, min_yrs=min_yrs,
          check_range_action = "warn",
          include_all_eval=TRUE
        )
      )

    })

  })
}
