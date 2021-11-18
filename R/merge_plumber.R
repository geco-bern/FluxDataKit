#' Merges plumber Flux and Met data
#'
#' Reads data in a given directory
#' by (fluxnet) site name and merges
#' both flux and meteo data in one file
#'
#' @param site fluxnet site name
#' @param path path with plumber2 data (both flux and meteo data files)
#'
#' @return
#' @export

merge_plumber <- function(
  site = "AT-Neu",
  path = "~/Desktop"
  ){

  # list all files
  files <- list.files(
    path,
    utils::glob2rx(paste0(site,"*.nc")),
    full.names = TRUE,
    recursive = TRUE
  )

  # check if both files are there
  # requirement for processing the fluxes
  if (length(files) != 2){
    stop("Missing either flux or meteo data for the requested site")
  }

  df <- lapply(files, function(file){

    # read netcdf file of either
    # flux or meteo data
    df <- read_nc(file)

    # return data frame
    return(df)
  })

  # combine met and flux data
  all <- dplyr::left_join(df[[1]], df[[2]])

  # return the merged file
  return(all)
}

