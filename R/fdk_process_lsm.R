#library(tidyverse)
library(dplyr)
library(FluxnetLSM)

# select datasets / sites to process
datasets <- c("fluxnet2015","icos","oneflux")
sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  filter(
    product != "plumber",
    sitename == "AT-Neu"
  )

fdk_process_lsm <- function(
    df,
    out_path,
    format = "netcdf"
    ){

  # create full path
  df$data_path <- file.path(df$data_path, df$product)

  # process sites row by row
  apply(df, 1, function(x){

      message(sprintf("-- processing site: %s", x['sitename']))

      # Outputs will be saved to this directory
      tmp_path <- file.path(tempdir(), "fluxnetlsm")

      if ( x['product'] != "fluxnet2015"){
        infile <- FluxnetLSM::get_fluxnet_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH",
          datasetversion = "[A-Z]{4}-[0-9]{1}"
        )

        # Retrieve dataset version
        datasetversion <- FluxnetLSM::get_fluxnet_version_no(
          infile
        )

        # Retrieve ERAinterim file
        era_file <- FluxnetLSM::get_fluxnet_erai_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH",
          datasetversion = "[A-Z]{4}-[0-9]{1}"
        )

      } else {
        infile <- FluxnetLSM::get_fluxnet_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH"
        )

        # Retrieve dataset version
        datasetversion <- FluxnetLSM::get_fluxnet_version_no(
          infile
        )

        # Retrieve ERAinterim file
        era_file <- FluxnetLSM::get_fluxnet_erai_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH"
        )
      }

      #---- Settings ----

      # Thresholds for missing and gap-filled time steps
      missing_met <- 100   #max. percent missing (must be set)
      missing_flux <- 100
      gapfill_met_tier1 <- 100  #max. gapfilled percentage
      gapfill_met_tier2 <- 100
      gapfill_flux <- 100
      min_yrs <- 1   #min. number of consecutive years

      #---- Run analysis ----

      status <- try(
        suppressWarnings(
          suppressMessages(
            convert_fluxnet_to_netcdf(
              infile = infile,
              site_code = x['sitename'],
              out_path = tmp_path,
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
        )
      )

      #----- Corrections ----

      if(!inherits(status, "try-error")){

        message("applying corrections")

      }


      #----- Cleanup ----

      if(!inherits(status, "try-error")){

        message("cleanup temporary files...")
        unlink(tmp_path, recursive = T)
      }


    })
}

fdk_process_lsm(sites)

