
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
#' @param site_csv_file CSV file with site meta-data which is parsed to populate
#'        ancillary data provided with the flux data. By default the hard coded
#'        packaged data are used, but alternatively an external file can be
#'        provided. This limits the need to recompile the package when processing
#'        files which were not originally selected.
#'
#' @return LSM compatible netcdf files in the output directory, with
#'  intermediary files saved upon request
#' @export

fdk_process_lsm <- function(
    df,
    out_path,
    modis_path,
    format = "lsm",
    save_tmp_files = TRUE,
    overwrite = TRUE,
    site_csv_file = system.file(
      "extdata",
      "Site_metadata.csv",
      package = "FluxnetLSM"
    )
    ) {

  # check if files are already processed
  if(!overwrite){
    if(any(grepl(df$sitename, list.files(out_path, "*.nc")))) {
      message(paste0(df$sitename, " files exist, skipping"))
      return(invisible())
    }
  }

  # Set path to NA if missing
  # can't forward missing elements
  # to other functions within
  # a lapply() which also
  # calls its own function
  # and is sand-boxed in ways
  if(missing(modis_path)){
    modis_path <- NA
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

      # Retrieve default processing options
      conv_opts <- get_default_conversion_options()

      # Set gapfilling options to ERAinterim
      conv_opts$metadata_source <- "csv"

      #---- Run analysis ----

      # This is based upon
      # https://github.com/aukkola/PLUMBER2/
      # Step1_Process_all_available_flux_sites_for_PLUMBER2.R

      message("Converting fluxnet data to netcdf")

      nc_files <- try(
        suppressWarnings(
          suppressMessages(
            FluxnetLSM::convert_fluxnet_to_netcdf(
              infile = infile,
              site_code = x['sitename'],
              out_path = tmp_path,
              # manual setting of the site meta-data
              site_csv_file = site_csv_file,
              conv_opts = conv_opts,
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

      # function to download and or
      # process MODIS data (LAI/FPAR)
      fdk_match_modis(
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

      #----- Export and/or convert to FLUXNET formatting ----

      # copy "raw" netcdf files to output path
      if(format == "lsm") {

        # list all flux files
        files <- list.files(
          path = file.path(tempdir(), "fluxnetlsm"),
          pattern = "*.nc",
          recursive = TRUE,
          full.names = TRUE
        )

        # copy files
        file.copy(
          from = files,
          to = out_path,
          overwrite = TRUE
        )

        file.remove(files)

      } else {
          message("converting to fluxnet")
          message("saving data in your output directory")
      }
    })

  # delete tmp files if requested
  if (!save_tmp_files) {
    message("cleanup all temporary files...")
    unlink(file.path(tempdir(), "fluxnetlsm"), recursive = T)
  }
}

