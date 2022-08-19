#' Smooth MODIS time series
#'
#' Smooth and gapfill modis time series, and insert
#' results into FluxnetLSM netcdf files for data fusion.
#'
#' @param dates vector of dates
#' @param values vector of values
#' @param variable what MODIS variable is processed (LAI or FPAR)
#' @param start_year a start year of the desired series
#' @param end_year an end year of the desired series
#' @param nc_file FluxnetLSM netcdf ERA Met file to amend, scaling
#'  the data to the respective time step required
#'
#' @return a data frame with smoothed gapfilled time series
#' @export

fdk_smooth_ts <- function(
  dates,
  values,
  variable,
  start_year,
  end_year,
  nc_file
  ) {

  # Set x values
  x <- 1:length(dates)

  # Define spline function
  func <- splinefun(
    x = x,
    y = values,
    method = "fmm",
    ties = mean
    )

  # Gapfill with spline (and cap negative values)
  spline_values <- func(seq(min(x), max(x), by=1))
  spline_values[spline_values < 0] <- 0

  # Smooth with spline (and cap negative values)
  smooth_values = smooth.spline(x, spline_values)$y
  smooth_values[smooth_values < 0] <- 0

  # Some MODIS time series are missing time steps, not sure why.
  # Add these missing time steps as NAs and then gapfill with climatology
  # Also add missing time steps to first year (since MODIS starts in July)

  # Get modis timing information
  modis_startyr <- as.numeric(format(dates[1], "%Y"))
  modis_endyr   <- as.numeric(format(dates[length(dates)], "%Y"))

  # Create time vector for complete years

  # Loop through years
  all_tsteps <- vector()
  for (y in modis_startyr:modis_endyr){
    all_tsteps <- append(
      all_tsteps,
      seq.Date(as.Date(paste0(y, "-01-01")),
      by = dates[2] - dates[1],
      length.out=46
      )
    )
  }

  # Remove extra tstesp for final year
  all_tsteps <- all_tsteps[-which(all_tsteps > dates[length(dates)])]

  #Find missing tsteps
  missing <- which(!(all_tsteps %in% dates))

  if (length(missing) > 0) {

    # Create new time series, where missing tsteps set to NA
    new_ts <- smooth_values

    #Add new value
    for (n in missing) {
      new_ts <- append(new_ts, NA, after=n-1)
    }

    # Replace time vector
    dates <- all_tsteps
    smooth_values <- new_ts

  }

  # Each year has 46 time steps, but first and last year are incomplete
  # Create climatological average for each time step

  # Check that time series starts on 1 Jan
  if (!(format(dates[1], "%m-%d") == "01-01")) {
    stop("Time series does not start 1 Jan")
    }

  # Find time steps for last (incomplete) year
  last_year <- which(grepl(modis_endyr, dates))

  # MODIS has 46 time steps per year
  no_tsteps <- length(which(grepl(modis_startyr , dates)))

  # Initialise
  modis_clim <- vector(length = no_tsteps)

  for (c in 1:no_tsteps) {

    # Indices for whole years
    inds <- seq(c, by=no_tsteps, length.out=floor(length(dates)/no_tsteps))

    # Add last year if applicable
    if( c <= length(last_year)) { inds <- append(inds, last_year[c]) }

    # Calculate average for time step
    modis_clim[c] <- mean(smooth_values[inds], na.rm=TRUE)

  }

  #Check that no NA values
  if (any(is.na(modis_clim))) {

    missing <- which(is.na(modis_clim))

    #Use the mean of next and previous non-NA value to gapfill climatology
    for(m in missing) {
      previous_val    <- modis_clim[tail(which(!is.na(modis_clim[1:max(c(1, m-1))])), 1)]
      next_val        <- modis_clim[m + which(!is.na(modis_clim[(m+1):length(modis_clim)]))[1]]
      modis_clim[m]   <- mean(c(previous_val, next_val), na.rm=TRUE)
    }
  }

  # Initialise
  clim_anomalies <- rep(NA, length(dates))

  # Repeat climatology for whole time series
  modis_clim_all <- rep_len(modis_clim, length(dates))

  # Calculate running mean anomaly (+/- 6 months either side of each time step)
  anomaly <- zoo::rollmean(
    smooth_values - modis_clim_all,
    k = 12,
    fill = NA
    )

  # Add rolling mean anomaly to climatology
  clim_anomalies <- modis_clim_all + anomaly

  if(missing(nc_file)){

    message("returning climate anomalies")
    return(clim_anomalies)

  } else {

    # open netcdf file for writing
    site_nc <- ncdf4::nc_open(nc_file, write = TRUE)

    # Get timing info for site
    site_start_time <- ncdf4::ncatt_get(site_nc, "time")$units
    site_time       <- ncdf4::ncvar_get(site_nc, "time")
    site_tstep_size <- 86400 / (site_time[2] - site_time[1])

    #Extract year
    startyr    <- as.numeric(substr(site_start_time, start=15, stop=18))
    obs_length <- length(site_time)
    nyr        <- round(obs_length/(site_tstep_size*365))
    endyr      <- startyr + nyr - 1

    # Add climatological values to smoothed time series

    if ((startyr < modis_startyr)) {

      # Overwrite original time series with new extended data
      clim_anomalies <- append(
        rep(modis_clim, modis_startyr - startyr),
        clim_anomalies
      )

      # Add new time stamps
      extended_time <- vector()
      for (y in startyr:(modis_startyr-1)) {

        extended_time <- append(
          extended_time,
          seq.Date(as.Date(paste0(y, "-01-01")),
                   by = dates[2] - dates[1],
                   length.out = no_tsteps
          )
        )
      }

      # Overwrite lai_time with new time series
      dates <- append(extended_time, dates)

    }

    # Check if remaining NA values from missing time steps, gapfill if found
    if (any(is.na(clim_anomalies))) {

      # Find missing values
      missing <- which(is.na(clim_anomalies))

      # Repeat climatology for all years and gapfill time series
      clim_all_yrs <- rep(modis_clim, floor(length(dates)/no_tsteps))
      clim_anomalies[missing] <- clim_all_yrs[missing]

    }

    # Find modis time step corresponding to site start time
    start_ind <- which(dates == paste0(startyr, "-01-01"))
    end_ind   <- tail(which(grepl(endyr, dates)), 1) #Last index of end year

    # Extract MODIS time steps matching site
    modis_ts_for_site   <- clim_anomalies[start_ind:end_ind]
    modis_time_for_site <- dates[start_ind:end_ind]

    # Repeat modis time series to create a time series matching site time step
    modis_tseries <- vector()

    # Loop through time steps
    for (t in 1:length(modis_time_for_site)) {

      #Last time step
      if (t == length(modis_time_for_site)) {

        #Use the number of time steps that ensures final time series matches the length of site data
        modis_tseries <- append(
          modis_tseries,
          rep(modis_ts_for_site[t],
              length(site_time) - length(modis_tseries)
          )
        )

        # All other time steps
      } else {

        time_diff <- modis_time_for_site[t+1] - modis_time_for_site[t]

        # Repeat each days estimate by the number of days and time steps per day
        modis_tseries <- append(
          modis_tseries,
          rep(modis_ts_for_site[t],
              time_diff * site_tstep_size
          )
        )
      }

    }

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
