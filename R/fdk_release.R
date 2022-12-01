
#' Create a new product release
#'
#' Creates a new product release when data is organized
#' in the fixed data structure as specified. This function
#' calls individual functions which are more flexible but
#' for consistency this wrapper restricts things further.
#'
#' @param input_path path with input data
#' @param df data frame with site meta-data information
#' @param output_path path where to store the output data
#' @param overwrite overwrite the results if the output path exists
#'
#' @return the FluxDataKit data release as a set of netCDF and CSV data
#' @export

fdk_release <- function(
  df,
  input_path,
  output_path,
  overwrite = TRUE
  ) {

  # create output directories

  # create main output data
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }

  # create remaining output directories
  dir.create(file.path(output_path,"lsm"), recursive = TRUE)
  dir.create(file.path(output_path,"fluxnet"), recursive = TRUE)
  dir.create(file.path(output_path,"pmodel"), recursive = TRUE)
  dir.create(file.path(output_path,"plots"), recursive = TRUE)

  # amend path to the set input path
  sites <- sites |>
    mutate(
      data_path = file.path(input_path,"flux_data/")
    )

  #---- FluxnetLSM reprocessing routine ----

  # process all sites, by calling the processing routine
  # all data is returned to the specified output path (out_path)
  suppressMessages(
    fdk_process_lsm(
      sites,
      out_path = file.path(output_path, "lsm"),
      modis_path = file.path(input_path,"modis"),
      format = "lsm",
      overwrite = TRUE,
      save_tmp_files = FALSE
    )
  )

  #---- List all converted files (sites) ----

  nc_files <- list.files(
    file.path(output_path,"lsm"),
    glob2rx("*Flux.nc"),
    full.names = TRUE
  )

  # extract site names from all files

  # loop over all sites and process the
  # LSM data into FLUXNET compatible daily (DD)
  # data formats combining fluxes and ERA gap
  # filled data
  failed_sites <- lapply(sites, function(site){

    # check if site is a plumber site, if so
    # switch input directories

    message("- converting to FLUXNET format")
    df <- suppressWarnings(try(fdk_convert_lsm(
      site = site,
      fluxnet_format = TRUE,
      path = file.path(output_path,"lsm")
    )
    ))

    if(inherits(df, "try-error")){
      message("!!! conversion to FLUXNET failed  !!!")
      return(site)
    }

    message("- downsampling FLUXNET format")
    filename <- suppressWarnings(
        try(fdk_downsample_fluxnet(
          df,
          site = site,
          out_path = file.path(output_path,"fluxnet"),
        )
        )
      )

    if(inherits(filename, "try-error")){
      message("!!! downsampling failed !!!")
      return(site)
    }

    return(NULL)
  })

  failed_sites <- do.call("rbind", failed_sites)

}
