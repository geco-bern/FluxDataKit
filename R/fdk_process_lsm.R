
#' Generate LSM data
#'
#' Convert FLUXNET data to land surface model compatible netcdf files, using
#' the FluxnetLSM package and PLUMBER2 based workflows.
#'
#' @param df dataframe with sites to process
#' @param out_path output directory for processed data
#' @param modis_path where to store downloaded MODIS data
#' @param save_tmp_files retain temporary files (TRUE or FALSE)
#'
#' @return LSM compatible netcdf files in the output directory, with
#'  intermediary files saved upon request
#' @export

fdk_process_lsm <- function(
    df,
    out_path,
    modis_path,
    save_tmp_files = FALSE,
    overwrite = TRUE
) {

  #---- set meta-data required by FluxnetLSM ----

  # read in the fluxnetlsm meta data
  # this is required by some FluxnetLSM functions
  # as we are reprocessing data no holds bar we can
  # set the Exclude parameter to FALSE

  # The desired site meta-data is then either spoofed
  # if missing from the original or extracted from
  # original file

  fls_meta_data <- read.csv(
    system.file("extdata", "Site_metadata.csv", package = "FluxnetLSM"),
    header = TRUE,
    stringsAsFactors = FALSE
  ) |>
    mutate(
      Exclude = FALSE # allow all sites to be processed
    )

  # read in the meta data as listed
  # within FluxDataKit and
  # rename the columns
  sites_meta_data <- sites |>
    dplyr::select(
      sitename,
      lat,
      lon,
      elv,
      igbp_land_use
    ) |>
    rename(
      'SiteCode' = 'sitename',
      'SiteLatitude' = 'lat',
      'SiteLongitude' = 'lon',
      'SiteElevation' = 'elv',
      'IGBP_vegetation_short' = 'igbp_land_use'
    ) |>
    filter(
      !(SiteCode %in% !!fls_meta_data$SiteCode)
    )

  # merge the two datasets
  # using bind rows
  fls_meta_data <- bind_rows(fls_meta_data, sites_meta_data)

  # write data to file, this is the amended
  # meta-data required for successful processing of the
  # flux data (this includes some columns which aren't
  # provided in the original meta-data or have the wrong name)
  write.csv(fls_meta_data, file = file.path(tempdir(), "meta_data.csv"))

  #---- formal processing workflow ----

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

    # check if files are already processed
    if(!overwrite){
      if(any(grepl(x['sitename'], list.files(out_path, "*.nc")))) {
        message(paste0(x['sitename'], " files exist, skipping"))
        return(invisible(NULL))
      }
    }

    # Outputs will be saved to this directory
    tmp_path <- file.path(tempdir(), "fluxnetlsm", x['sitename'])

    # create output path if it doesn't exist
    if(!dir.exists(tmp_path)){
      dir.create(tmp_path, recursive = TRUE)
    }

    if ( x['product'] == "plumber"){

      # list all plumber files
      files_to_copy <- list.files(
        df$data_path,
        glob2rx(sprintf("*%s*.nc",x['sitename'])),
        full.names = TRUE
      )

      # copy to temporary directory
      file.copy(
        files_to_copy,
        tmp_path,
        overwrite = TRUE,
        recursive = TRUE
      )

    } else {

      if ( x['product'] == "icos_warmwinter2020" || x['product'] == "icos_drought2018" ){
        infile <- FluxnetLSM::get_fluxnet_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH",
          datasetversion = "[A-Z]{4}-[0-9]{1}"
        )

        # Retrieve ERAinterim file
        era_file <- FluxnetLSM::get_fluxnet_erai_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH",
          datasetversion = "[A-Z]{4}-[0-9]{1}"
        )
      }

      if ( x['product'] == "ameriflux" ){
        # first, try half-hourly file
        infile <- FluxnetLSM::get_fluxnet_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH",
          datasetname = "FLUXNET",
          datasetversion = "[0-9]{1}-[0-9]{1}"
        )

        # if HH file not available, try getting HR file
        if (length(infile) == 0){
          infile <- FluxnetLSM::get_fluxnet_files(
            x['data_path'],
            x['sitename'],
            resolution = "HR",
            datasetname = "FLUXNET",
            datasetversion = "[0-9]{1}-[0-9]{1}"
          )
        }

        # Retrieve ERAinterim file
        # first, try half-hourly file
        era_file <- FluxnetLSM::get_fluxnet_erai_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH",
          datasetname = "FLUXNET",
          datasetversion = "[0-9]{1}-[0-9]{1}"
        )

        # if HH file not available, try getting HR file
        if (length(era_file) == 0){
          era_file <- FluxnetLSM::get_fluxnet_erai_files(
            x['data_path'],
            x['sitename'],
            resolution = "HR",
            datasetname = "FLUXNET",
            datasetversion = "[0-9]{1}-[0-9]{1}"
          )
        }

      }

      if (x['product'] == "fluxnet2015" ) {

        infile <- FluxnetLSM::get_fluxnet_files(
          x['data_path'],
          x['sitename'],
          resolution = "HH"
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

      # Set gap filling options to ERAinterim
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
              # generated on the fly top of function
              site_csv_file = file.path(tempdir(), "meta_data.csv"),
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
              include_all_eval = TRUE,
              plot = NA
            )
          )
        )
      )

      if(inherits(nc_files, "try-error")){
        warning("conversion failed --- skipping this site")

        # list all flux files
        files <- list.files(
          path = file.path(tempdir(), "fluxnetlsm"),
          pattern = "*.nc",
          recursive = TRUE,
          full.names = TRUE
        )

        # remove all files
        file.remove(files)

        return(invisible())
      }
    }

    #----- Downloading and adding MODIS data ----

    # This is based upon
    # https://github.com/aukkola/PLUMBER2/
    # Step2_Process_MODIS_LAI_for_all_sites.R

    if ( x['product'] != "plumber"){

      # function to download and or
      # process MODIS data (LAI/FPAR)
      check <- try(fdk_match_modis(
        df = x,
        path = modis_path,
        nc_file = nc_files$met
      ))

      if(inherits(check, "try-error")){
        warning("MODIS data injection failed --- skipping this site")

        # list all flux files
        files <- list.files(
          path = file.path(tempdir(), "fluxnetlsm"),
          pattern = "*.nc",
          recursive = TRUE,
          full.names = TRUE
        )

        # remove all files
        file.remove(files)

        return(invisible())
      }

    } else {

      # list all plumber files
      plumber_met_file <- list.files(
        tempdir(),
        glob2rx(sprintf("*%s*Met.nc",x['sitename'])),
        recursive = TRUE,
        full.names = TRUE
      )

      # function to download and or
      # process MODIS data (LAI/FPAR)
      check_modis <- try(fdk_match_modis(
        df = x,
        path = modis_path,
        nc_file = plumber_met_file
      ))

      # correct LAI naming
      check_renaming <- try(
        fdk_correct_rs(plumber_met_file)
      )

      if(inherits(check_modis, "try-error") ||
         inherits(check_renaming, "try-error")){
        warning("MODIS data injection failed --- skipping this site")

        # list all flux files
        files <- list.files(
          path = file.path(tempdir(), "fluxnetlsm"),
          pattern = "*.nc",
          recursive = TRUE,
          full.names = TRUE
        )

        # remove all files
        file.remove(files)

        return(invisible())
      }
    }

    #----- Corrections ----
    message("applying ERA/FLUX corrections")

    if ( x['product'] != "plumber"){

      # meteorological corrections
      # written to ncdf file
      fdk_correct_era(
        infile_met = nc_files$met,
        new_qc = 101
      )

      # correct energy balance
      fdk_flux_corrections(
        infile = nc_files$flux
      )
    }
    #----- Export and/or convert to FLUXNET formatting ----

    message("converting as LSM netcdf")

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
  })

  # delete tmp files if requested
  if (!save_tmp_files) {
    message("cleanup all temporary files...")
    unlink(file.path(tempdir(), "fluxnetlsm"), recursive = T)
  }
}
