#' Creates a new FluxDataKit product release
#'
#' Creates a new product release when data is organized
#' in the fixed data structure as specified. This function
#' calls individual functions which are more flexible but
#' for consistency this wrapper restricts things further.
#'
#' @param df data frame with site meta-data information
#' @param input_path path with input data
#' @param output_path path where to store the output data
#' @param overwrite_lsm overwrite the LSM-formatted NetCDF files if the output
#' path exists
#' @param overwrite_fluxnet overwrite the FLUXNET-formatted CSV files if the
#' output path exists
#'
#' @return the FluxDataKit data release as a set of netCDF, CSV data,
#'  compressed RDS rsofun drivers and meta-data
#' @export

fdk_release <- function(
  df,
  input_path,
  output_path,
  overwrite_lsm = TRUE,
  overwrite_fluxnet = TRUE
) {

  # create output directories

  # create main output data
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }

  # create remaining output directories
  dir.create(
    file.path(output_path, "lsm"),
    recursive = TRUE,
    showWarnings = FALSE
    )

  dir.create(
    file.path(output_path, "fluxnet"),
    recursive = TRUE,
    showWarnings = FALSE
    )

  dir.create(
    file.path(output_path, "pmodel"),
    recursive = TRUE,
    showWarnings = FALSE
    )

  dir.create(
    file.path(output_path, "plots"),
    recursive = TRUE,
    showWarnings = FALSE
    )

  # amend path to the set input path
  sites <- df |>
    mutate(
      data_path = file.path(input_path, "flux_data/")
    )

  #---- FluxnetLSM reprocessing routine ----

  # process all sites, by calling the processing routine
  # all data is returned to the specified output path (out_path)
  suppressMessages(
    fdk_process_lsm(
      sites,
      out_path = file.path(output_path, "lsm"),
      modis_path = file.path(input_path, "modis"),
      overwrite = overwrite_lsm,
      save_tmp_files = FALSE
    )
  )

  #---- Convert files to CSV files ----

  # loop over all sites and process the
  # LSM data into FLUXNET compatible daily (DD)
  # data formats combining fluxes and ERA gap
  # filled data
  failed_sites <- lapply(df$sitename, function(site){

    # check if site is a plumber site, if so
    # switch input directories
    message(sprintf("- converting to FLUXNET format %s", site))

    status <- suppressWarnings(
      try(
        fdk_convert_lsm(
          site = site,
          fluxnet_format = TRUE,
          path = file.path(output_path, "lsm"),
          out_path = file.path(output_path, "fluxnet"),
          overwrite = overwrite_fluxnet
        )
      )
    )

    if(inherits(status, "try-error")){
      message("!!! FLUXNET conversion failed !!!")
      return(site)
    }

    return(invisible(NULL))
  })

  failed_sites <- do.call("rbind", failed_sites)

  # return meta-data for all processed sites
  # to be processed into a final meta-data file
  # and distributed together with the data
  return(failed_sites)
}
