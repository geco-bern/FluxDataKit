#' Smooth MODIS time series
#'
#' Smooth and gapfill modis time series, and insert
#' results into FluxnetLSM netcdf files for data fusion.
#'
#' @param df vector of dates
#' @param variable what MODIS variable is processed (LAI or FPAR)
#' @param nc_file FluxnetLSM netcdf ERA Met file to amend, scaling
#'  the data to the respective time step required
#'
#' @return a data frame with smoothed gapfilled time series
#' @export

fdk_smooth_ts <- function(
  df,
  variable,
  nc_file
  ) {

  if(missing(nc_file)){
    message("returning climate anomalies")
  } else {

    # create a new data frame based on the variable selected
    df <- df |>
      rename(
        "values" = tolower(variable)
      )

    # detect outliers
    df <- suppressWarnings(
      fdk_detect_outliers(
      df = df,
      sigma = 1,
      plot = FALSE
      )
    )

    # get mean annual cycle
    # and linearly interpolate and
    # carry forward or backward any
    # remaining gaps
    mean_df <- df |>
      mutate(
        values = ifelse(outlierflag == 1, NA, values)
        ) |>
      group_by(doy) |>
      summarize(
        mean_values = median(values, na.rm = TRUE)
      ) |>
      mutate(
        mean_values = zoo::na.approx(mean_values, na.rm = FALSE),
        mean_values = zoo::na.locf(mean_values, na.rm = FALSE),
        mean_values = zoo::na.locf(mean_values, fromLast = TRUE, na.rm = FALSE)
      )

    # join with original data, and create
    # combined results (filling NA values in the
    # original)
    df <- left_join(df, mean_df, by = "doy") |>
      mutate(
        combined_values = ifelse(is.na(values) | outlierflag == 1, mean_values, values),
        weights = ifelse(is.na(values), 0.1, 1)
      )

    # calculate the optimal span based on
    # a BIC metric (phenocam approach)
    span <- fdk_optimal_span(
      y = df$combined_values,
      x = as.numeric(df$date),
      weights = df$weights
    )

    # fit the model with the optimal
    # span and export the smooth
    # time series (see modis_tseries below)
    fit <- stats::loess(
      combined_values ~ as.numeric(date),
      span = span,
      weights = weights,
      data = df
    )

    #---- write data to netcdf file ----
    # open netcdf file for writing
    site_nc <- ncdf4::nc_open(nc_file, write = TRUE)

    # Get timing info for site
    site_start_time <- ncdf4::ncatt_get(site_nc, "time")$units
    site_time       <- ncdf4::ncvar_get(site_nc, "time")
    site_tstep_size <- 86400 / (site_time[2] - site_time[1])

    # Extract year
    start_year    <- as.numeric(substr(site_start_time, start=15, stop=18))
    site_time <- as.Date(as.POSIXct(site_time, origin = sprintf("%s-01-01", start_year)))

    # predict values using the fit
    # model
    modis_tseries <- suppressWarnings(
      stats::predict(
        fit,
        as.numeric(site_time),
        se = FALSE)
    )

    plot(site_time, modis_tseries)
    print(site_time[which(is.na(modis_tseries))])

    # Check that the number of time steps match
    if (length(modis_tseries) != length(site_time)) {
      stop("MODIS and site time steps don't match")
    }

    # Also check that no missing values
    if (any(is.na(modis_tseries))) {
      stop("Missing values in final MODIS time series")
    }

    message("writing MODIS data to netcdf file")
    variable_short <- paste0(variable, "_MODIS")

    # Define variable:
    var <- ncdf4::ncvar_def(
      variable_short,
      '-',
      list(site_nc$dim[[1]],
           site_nc$dim[[2]],
           site_nc$dim[[3]]),
      missval = -9999,
      longname = paste0('MODIS 8-daily ', variable)
      )

    # Add variable and then variable data:
    site_nc <- ncdf4::ncvar_add(site_nc, var)
    ncdf4::ncvar_put(
      site_nc,
      variable_short,
      modis_tseries
      )

    #Close file handle
    ncdf4::nc_close(site_nc)

    return(modis_tseries)
  }
}
