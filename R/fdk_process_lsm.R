
#' Generate LSM data
#'
#' Convert FLUXNET data to land surface model compatible netcdf files, using
#' the FluxnetLSM package and PLUMBER2 based workflows.
#'
#' @param df dataframe with sites to process
#' @param out_path output directory for processed data
#' @param modis_path where to store downloaded MODIS data
#' @param format the format of the output (fluxnet = FLUXNET formatting)
#' @param save_tmp_files retain temporary files (TRUE or FALSE)
#'
#' @return LSM compatible netcdf files in the output directory, with
#'  intermediary files saved upon request
#' @export

fdk_process_lsm <- function(
    df,
    out_path,
    modis_path,
    format = "fluxnet",
    save_tmp_files = TRUE,
    overwrite = TRUE
    ) {

  # check if files are already processed
  if(!overwrite){
    if(any(grepl(df$sitename, list.files(out_path, "*.nc")))) {
      message(paste0(df$sitename, " files exist, skipping"))
      return(invisible())
    }
  }

  # create full data path
  df$data_path <- file.path(df$data_path, df$product)

  # process sites row by row
  apply(df, 1, function(x){

      message(sprintf("-- processing site: %s", x['sitename']))

      # Outputs will be saved to this directory
      tmp_path <- file.path(tempdir(), "fluxnetlsm", x['sitename'])

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

      # This is based upon
      # https://github.com/aukkola/PLUMBER2/
      # Step1_Process_all_available_flux_sites_for_PLUMBER2.R

      nc_files <- try(
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

      if(inherits(nc_files, "try-error")){
        warning("conversion failed --- skipping this site")
        return(invisible())
      }

      #----- Downloading and adding MODIS data ----

      # This is based upon
      # https://github.com/aukkola/PLUMBER2/
      # Step2_Process_MODIS_LAI_for_all_sites.R
      message("Merging in MODIS LAI/FPAR data")

      fdk_download_modis(
          df = x,
          path = modis_path,
          nc_file = nc_files$met
        )

      #----- Corrections ----

      message("applying ERA corrections")

      # meteorological corrections
      # written to ncdf file
      fdk_correct_era(
        infile_met = nc_files$met,
        new_qc = 101
        )

      message("applying FLUX corrections")

      # correct energy balance
      fdk_flux_corrections(
        infile = nc_files$flux
      )

      #----- Convert to FLUXNET formatting ----

      if(format == "fluxnet") {
        message("converting to fluxnet")
        message("saving data in your output directory")
      }
    })

  #---- cleanup of files ----

  # copy "raw" netcdf files to output path
  # if requested (format != fluxnet)
  if(format != "fluxnet") {
    nc_files <- list.files(
      path = file.path(tempdir(), "fluxnetlsm"),
      pattern = "*.nc",
      recursive = TRUE,
      full.names = TRUE
    )

    file.copy(
      from = nc_files,
      to = out_path,
      overwrite = TRUE
    )
  }

  # delete tmp files if requested
  if (!save_tmp_files) {
    message("cleanup all temporary files...")
    unlink(file.path(tempdir(), "fluxnetlsm"), recursive = T)
  }

}

