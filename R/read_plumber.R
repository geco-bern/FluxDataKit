#' Reads plumber2 netcdf data
#'
#' Reads data in a given directory
#' by (fluxnet) site name
#'
#' @param site fluxnet site name
#' @param path path with plumber2 data (both flux and meteo data files)
#' @param meta_data only return meta-data for the site
#'
#' @return
#' @export

read_plumber <- function(
  site = "AT-Neu",
  path = "~/Desktop",
  meta_data = FALSE
  ){

  # list all files
  files <- list.files(
      path,
      utils::glob2rx(paste0(site,"*.nc")),
      full.names = TRUE,
      recursive = TRUE
    )

  # only retain one source file
  # to grab meta-data from
  if (meta_data) {
    files <- files[1]
  } else {
    # check if both files are there
    # requirement for processing the fluxes
    if (length(files) != 2){
      stop("Missing either flux or meteo data for the requested site")
    }
  }

  df <- lapply(files, function(file){
    # convert time (needs attribute read)
    nc <- ncdf4::nc_open(file)

    # Get time vector
    time <- ncdf4::ncvar_get(nc, "time")

    # Get time units
    time_units <- strsplit(
      ncdf4::ncatt_get(nc, "time")$units,
      "seconds since ")[[1]][2]

    # Convert to Y-M-D h-m-s (hack around attribute issues in dates
    # when using bind_cols() or joins)
    time_date <- as.character(as.POSIXct(time, origin = time_units, tz="GMT"))
    time_date <- as.POSIXct(time_date, tz = "GMT")

    # Get variable names
    vars <- names(nc$var)

    # Load variable data
    df <- as.data.frame(
      lapply(vars, function(x) ncdf4::ncvar_get(nc, x))
    )

    # close file
    ncdf4::nc_close(nc)

    # Set names
    colnames(df) <- vars

    # add time column
    df$time <- time_date

    # remove trailing spaces
    # in IGBP classes
    df$IGBP_veg_short <- gsub(" ","", df$IGBP_veg_short)
    df$IGBP_veg_long <- gsub(" ","", df$IGBP_veg_long)

    # subset and constrain data
    if (meta_data) {

      df$start_date <- min(df$time)
      df$end_date <- max(df$time)

      df <- df[1,c("latitude", "longitude", "reference_height",
                  "canopy_height", "elevation", "IGBP_veg_short",
                  "IGBP_veg_long","start_date","end_date")]
      df$sitename <- site
    }

    # return data frame
    return(df)
  })

  if (meta_data) {
    all <- df[[1]]
  } else {
    # combine met and flux data
    all <- left_join(df[[1]], df[[2]])
  }

  # return the merged file
  return(all)
}

