#' Corrects ERA data
#'
#' Corrects ERA data of a FluxnetLSM file (changes the file)
#' locally, if a path is provided a new file is created.
#'
#' @param infile_met input netcdf filename
#' @param new_qc quality control parameters
#'
#' @return corrects an existing ERA-Interim FluxnetLSM file in place
#' @export

fdk_correct_era <- function(
    infile_met,
    new_qc
    ) {

  # Open nc file handle
  met_nc <- ncdf4::nc_open(infile_met, write = TRUE)

  # extract site code from file (global attributes)
  site_code <- ncdf4::ncatt_get(met_nc, 0)$site_code

  # Get time vector
  time <- ncdf4::ncvar_get(met_nc, "time")

  # Get time units
  time_units <- strsplit(ncdf4::ncatt_get(met_nc, "time")$units, "seconds since ")[[1]][2]

  # Convert to Y-M-D h-m-s
  time_date <- as.POSIXct(time, origin=time_units, tz="GMT")

  # Get time interval (in fraction of day)
  tsteps_per_day <-  60*60*24 / (time[2] - time[1])

  # Find adjusted start and end time
  years <- as.numeric(format(time_date, "%Y"))

  #---- Get variables -----

  # Get all data variables with a time dimension ###

  # Get variable names
  vars <- names(met_nc$var)

  # Load variable data
  var_data <- lapply(vars, function(x) ncdf4::ncvar_get(met_nc, x))

  # Set names
  names(var_data) <- vars

  # Get variable attributes
  att_data <- lapply(vars, function(x) ncdf4::ncatt_get(met_nc, x))

  #Set names
  names(att_data) <- vars

  #---- Correct CO2 using global records ----

  # If replacing CO2 with global CO2

  # download/load annual mean atmospheric CO2
  # read data dynamically
  global_co2 <- try(
    read.table(
    "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv",
    header = TRUE,
    skip = 55,
    sep = ",")
    )

  # if data isn't available stop processing
  if(inherits(global_co2,"try-error")){
    stop("Can't download NOAA CO2 data, check internet connection!")
  }

  # loop through years
  co2_ts <- sapply(years, function(x) global_co2[which(global_co2 == x), 2])

  # Replace values in CO2 variable
  var_data$CO2air <- co2_ts

  # Set QC values to missing
  var_data$CO2air_qc <- rep(new_qc, length(time))

  # Replace gapfill percentage (now 100%)
  att_data$CO2air["Gap-filled_%"] <- 100

  #Add information to metadata
  att_data$CO2air$CO2_correction <- "Global CO2 (annual Mauna Loa time series)"

  #---- Check for additional site corrections -----

  # Check if any apply to this site
  site_fixes <- site_exceptions(
    site_code,
    var_data,
    att_data,
    qc_val = new_qc
    )

  # Replace with new fixed data
  var_data <- site_fixes$var_data
  att_data <- site_fixes$att_data

  #---- Adjust time period ----

  # Get years to process
  # NOT SURE WHAT THIS EVEN DOES??
  start_yr <- 0
  end_yr   <- 0

  #---- Adjust length of time-varying variables ----

  # Get dimensions for each variable
  dims <- lapply(vars, function(x) {
    sapply(met_nc[["var"]][[x]][["dim"]], function(dim) dim[["name"]])
    })

  # Find which variables are time-varying (used later so leave outside if-loop)
  var_inds <- which(sapply(dims, function(x) any(x == "time")))

  # If need to adjust
  if (start_yr > 1 | end_yr < 0) {

    # New start and end year
    new_start_year <- years[1] + start_yr -1
    new_end_year   <- years[length(years)] + end_yr #end_yr negative so need to sum

    # Start and end indices
    start_ind <- which(years == new_start_year)[1]
    end_ind   <- tail(which(years == new_end_year), 1)

    # Create new time stamp
    new_time_unit <- paste0("seconds since ", new_start_year, "-01-01 00:00:00")

    # New time vector
    time_var <- seq(
      0,
      by=60*60*24 / tsteps_per_day,
      length.out=length(c(start_ind:end_ind)))

    # Change dimensions and values for time-varying data
    for (v in vars[var_inds]) {

      # Change time dimension
      met_nc$var[[v]]$varsize[3] <- length(time_var)

      # Change time values
      met_nc$var[[v]]$dim[[3]]$vals <- time_var

      # Change time size
      met_nc$var[[v]]$dim[[3]]$len <- length(time_var)

      # Change length
      met_nc$var[[v]]$size[3] <- length(time_var)

      # Change values in var_data
      var_data[[v]] <- var_data[[v]][start_ind:end_ind]

      # Change chunk size (no idea what this is but produces an error otherwise
      # during nc_create)
      # met_nc[[s]]$var[[v]]$chunksizes <- NA

      # Replace time unit
      time_ind <- which(sapply(met_nc$var[[v]]$dim, function(x) x$name) == "time")
      met_nc$var[[v]]$dim[[time_ind]]$units <- new_time_unit

    }

    # Also adjust time dimension and units
    # Change time dimensions
    # Change values, length and unit
    met_nc$dim$time$vals <- time_var
    met_nc$dim$time$len <- length(time_var)
    met_nc$dim$time$units <- new_time_unit

    # Also adjust years in output file name
    # New years
    new_yr_label <- paste0(new_start_year, "-", new_end_year)

  }

  #---- Check for missing vals in met data ----

  # Do a final check to make sure there are no missing values in
  # any met variables

  # Find met vars, ignoring any qc variables (don't care about gaps in those)
  met_vars <- vars[var_inds][which(!grepl("_qc", vars[var_inds]))]

  # Loop through variables
  for (v in met_vars) {

    # Check if any missing values
    if (any (is.na(var_data[[v]]))) {
      stop(paste0("Missing values in ", v, ", site: ", site_code))
    }
  }

  # First check that LAI data is available
  modis_vars <- vars[which(grepl("_MODIS", vars))]

  for (v in names(att_data)) {

    # Missing percentage
    if (any(names(att_data[[v]]) == "Missing_%")) {
      att_data[[v]]["Missing_%"] <-  round(
        length(which(is.na(var_data[[v]])))/length(var_data[[v]]) * 100,
        digits = 1
      )
    }

    # Gap-filled percentage
    if (any(names(att_data[[v]]) == "Gap-filled_%")) {
      att_data[[v]]["Gap-filled_%"] <- round(
        length(which(var_data[[paste0(v, "_qc")]] > 0)) / length(var_data[[paste0(v, "_qc")]]) * 100,
        digits = 1
      )
    }
  }

  #---- write files ----

  # Get dimensions from input file
  new_dims <- met_nc$dim

  # Get variables from input file
  new_vars <- met_nc$var

  # New file handle
  out_nc <- ncdf4::nc_create(infile_met, vars = new_vars)

  # Get global attributes
  global_atts <- ncdf4::ncatt_get(met_nc, varid=0)

  # Add new QC flag value to metadata
  global_atts$QC_flag_descriptions <- paste0(
    global_atts$QC_flag_descriptions,", Post-processed: ", new_qc)

  # Add to file
  # For some reason this crashes if using lapply, loop works ok-ish
  for(a in 1:length(global_atts)){
    ncdf4::ncatt_put(out_nc, varid=0, attname=names(global_atts)[a],
              attval=unlist(global_atts[a]))
  }

  # Write variables to output file
  for (v in names(new_vars)) {
    ncdf4::ncvar_put(nc=out_nc, varid=new_vars[[v]],
              vals=var_data[[v]])
  }

  # Write attributes to output file
  for (v in names(att_data)) {
    for (a in names(att_data[[v]]))
      ncdf4::ncatt_put(nc=out_nc, varid=new_vars[[v]],
                attname=a, attval=att_data[[v]][[a]])
  }

  # # Close output file
  ncdf4::nc_close(out_nc)

  # Close original file handle
  ncdf4::nc_close(met_nc)

  #----- default LAI check ----

  # Open file handle
  nc_out <- ncdf4::nc_open(infile_met, write=TRUE)

  # Should have two available, check that they are there
  # Koen: not applicable anymore, only including LAI and FPAR
  if (length(modis_vars) != 2) {
    warning(paste0("LAI variables not available, check site: ", site_code))
  } else {

    # MODIS
    default_lai    <- "LAI_MODIS"
    default_fpar <- "FPAR_MODIS"
    default_source <- "MODIS"

    alt_lai    <- "LAI_Copernicus"
    alt_source <- "Copernicus"

    # Rename LAI
    nc_out <- ncdf4::ncvar_rename(nc_out, default_lai, "LAI")
    nc_out <- ncdf4::ncvar_rename(nc_out, default_fpar, "FPAR")
    #nc_out <- ncdf4::ncvar_rename(nc_out, alt_lai, "LAI_alternative")

    # Add source in attribute data
    ncdf4::ncatt_put(
      nc = nc_out,
      varid = "LAI",
      attname = "source",
      attval = default_source
    )

    ncdf4::ncatt_put(
      nc = nc_out,
      varid = "FPAR",
      attname = "source",
      attval = default_source
    )

    # Close file handle
    ncdf4::nc_close(nc_out)
  }

  # rename the orignal file if time
  # varying components are changed
  # see above

  # If need to adjust
  if (start_yr > 1 | end_yr < 0) {

    # File name without path
    filename <- gsub("[0-9]{4}-[0-9]{4}", new_yr_label, basename(infile_met))
    outdir <- dirname(infile_met)

    # Replace file name with new years
    file.rename(
      infile_met,
      file.path(outdir, filename)
    )

    # reset infile_met
    infile_met <- file.path(outdir, filename)
  }
}
