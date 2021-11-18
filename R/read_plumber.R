#' Reads Plumber netcdf files
#'
#' Returns either the raw flux or meteo
#' data or the site meta-data based upon
#' a provided path to a plumber Met or Flux
#' file.
#'
#' @param file plumber netcdf file
#'
#' @return dataframe with read in nc values
#' @export

read_plumber <- function(
  file,
  meta_data = FALSE
  ){

  # grab sitename (fluxnet encoding)
  site <- site <- strsplit(basename(file), "_")[[1]][[1]]

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

  # remove trailing / leading white spaces
  # in IGBP classes
  df$IGBP_veg_short <- trimws(df$IGBP_veg_short)
  df$IGBP_veg_long <- trimws(df$IGBP_veg_long)

  # subset and constrain data
  if (meta_data) {

    df$year_start <- format(min(df$time),"%Y")
    df$year_end <- format(max(df$time),"%Y")

    df <- df[1,c("latitude", "longitude", "reference_height",
                 "canopy_height", "elevation", "IGBP_veg_short",
                 "IGBP_veg_long","year_start","year_end")]
    df$sitename <- site
  }

  return(df)
}
