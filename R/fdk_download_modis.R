# library(ncdf4)
# library(zoo)
#
# #clear R environment
# rm(list=ls(all=TRUE))
#
# #Set path
# path <- "/srv/ccrc/data04/z3509830/Fluxnet_data/All_flux_sites_processed_PLUMBER2/"
#
# #Which fluxnet data to use?
# flx_path <- path
#
# #Create output folder for met files with processed LAI time series
# outdir <- paste0(flx_path, "/all_sites_no_duplicates/Nc_files/Met_with_LAI/")
#
# dir.create(outdir, recursive=TRUE)
#
# #Copy Met files to the new folder
# file.copy(from = list.files(paste0(flx_path, "/all_sites_no_duplicates/Nc_files/Met/"), full.names=TRUE),
#           to = outdir, overwrite=TRUE)
#
# #Get sites
# site_files <- list.files(outdir, full.names=TRUE)
#
# #Open file handles
# site_nc <- lapply(site_files, nc_open, write=TRUE)
#
# #Get site codes
# site_codes <- sapply(site_nc, function(x) ncatt_get(x, varid=0, "site_code")$value)

fdk_download_modis <- function(
  df,
  path
  ) {

  # Download MODIS data
  product <- "MCD15A2H"
  bands <- c(
    "Lai_500m",
    "LaiStdDev_500m",
    "FparStdDev_500m",
    "Fpar_500m",
    "FparLai_QC"
  )

  #Get LAI
  df_modis <- try(MODISTools::mt_subset(
      site_name = df['sitename'],
      lat = df['lat'],
      lon = df['lon'],
      product = product,
      band = bands,
      start = "2021-01-01",
      end = format(Sys.time(), "%Y-%m-%d"),
      km_lr = 0,
      km_ab = 0,
      internal = TRUE
    )
  )

  if(inherits(df_modis, "try-error") ) {
    stop("MODIS data download failed")
  }

  # Extracting pixels in the centre and immediately around it (*)
  # These correspond to a radius of 500m around site coordinates
  pixel_no <- c(7, 8, 9, 12, 13, 14, 17, 18, 19)

  # Use only good quality data
  # Random bit integer format, ask Martin if need to work these out again...
  qc_flags <- c(0, 2, 24 ,26, 32, 34, 56, 58)

  #Exception for US-ORv, wetland site with no MODIS LAI available
  if (df['sitename'] == "US-ORv") return(NULL)


    #Number of time steps

    #If data files have been acquired on different days from MODIS
    #server, they might be different lengths so take minimum. adjust further down

    no_tsteps <- min(nrow(lai), nrow(sd), nrow(qc)) / max(lai$pixel)


    #Extract 3 x 3 pixels
    lai_pixel <- matrix(nrow=no_tsteps, ncol=length(pixel_no))
    sd_pixel  <- matrix(nrow=no_tsteps, ncol=length(pixel_no))
    qc_pixel  <- matrix(nrow=no_tsteps, ncol=length(pixel_no))

    #Save time stamps
    lai_time <- as.Date(lai$calendar_date[which(lai$pixel == pixel_no[1])])

    #Loop through pixels
    for (p in 1:length(pixel_no)) {

      #Get time series for pixel and scale using scale factor (and adjust for different lengths with min_dim)
      lai_pixel[,p] <- lai$value[which(lai$pixel == pixel_no[p])][1:no_tsteps] * lai$scale[1]
      sd_pixel[,p]  <- sd$value[which(sd$pixel == pixel_no[p])][1:no_tsteps] * sd$scale[1]
      qc_pixel[,p]  <- qc$value[which(sd$pixel == pixel_no[p])][1:no_tsteps]

    }



    ##################################
    ### Mask out poor quality data ###
    ##################################

    #Mask out where QC flag
    lai_pixel <- replace(lai_pixel, !(qc_pixel %in% qc_flags), NA)
    sd_pixel  <- replace(sd_pixel, !(qc_pixel %in% qc_flags), NA)

    #Also mask out where sd really low (likely cloud effects)
    sd_pixel  <- replace(sd_pixel, sd_pixel < 0.1, NA)
    lai_pixel <- replace(lai_pixel, is.na(sd_pixel), NA)

    #Set fill values to missing
    lai_pixel <- replace(lai_pixel, lai_pixel > 10, NA)


    ### Average each time step ###

    #Initialise lai time series
    lai_ts <- vector(length=no_tsteps)
    #lai_ts_mean <- vector(length=no_tsteps)

    #Loop through time steps
    for (t in 1:no_tsteps) {

      #If no values available
      if (all(is.na(lai_pixel[t,]))) {

        lai_ts[t] <- NA

        #If values available
      } else {

        #Weight grid cell estimates by their standard deviation
        #Following Martin's method (https://github.com/mdekauwe/get_MODIS_LAI_australia/blob/master/build_modis_climatology.py),
        #but normalising by sum of standard deviations

        sd_vals <- sd_pixel[t,]

        weights = (1/sd_vals**2) / sum(1/sd_vals**2, na.rm=TRUE)

        #Check that weights sum up to 1 (because of a precision issue presumably,
        #rounding to 5 decimals, otherwise might not equal 1 even when correct)
        if (round(sum(weights, na.rm=TRUE), 5) != 1) stop("Weighting not correct")

        #Calculate weighted average
        lai_ts[t] <- weighted.mean(lai_pixel[t,], w=weights, na.rm=TRUE)

        #lai_ts_mean[t] <- mean(lai_pixel[t,], na.rm=T)
      }
    }


    ######################################
    ### Gapfill and smooth with spline ###
    ######################################

    #Set x values
    x <- 1:length(lai_ts)

    #Define spline function
    func = splinefun(x=x, y=lai_ts, method="fmm",  ties = mean)

    #Gapfill with spline (and cap negative values)
    lai_spline <- func(seq(min(x), max(x), by=1))
    lai_spline[lai_spline < 0] <- 0

    #Smooth with spline (and cap negative values)
    smooth_lai_ts = smooth.spline(x, lai_spline)$y
    smooth_lai_ts[smooth_lai_ts < 0] <- 0


    # #Test
    # plot(lai_spline, type='l')
    # lines(lai_ts, col='red')
    # lines(smooth_lai_ts, col='blue')


    #######################################
    ### Add missing time steps in MODIS ###
    #######################################

    #Some MODIS time series are missing time steps, not sure why.
    #Add these missing time steps as NAs and then gapfill with climatology
    #Also add missing time steps to first year (since MODIS starts in July)

    #Get modis timing information
    modis_startyr <- as.numeric(format(lai_time[1], "%Y"))
    modis_endyr   <- as.numeric(format(lai_time[length(lai_time)], "%Y"))


    #Create time vector for complete years

    #Loop through years
    all_tsteps <- vector()
    for (y in modis_startyr:modis_endyr){

      all_tsteps <- append(all_tsteps, seq.Date(as.Date(paste0(y, "-01-01")),
                                                by=lai_time[2]-lai_time[1], length.out=46))

    }

    #Remove extra tstesp for final year
    all_tsteps <- all_tsteps[-which(all_tsteps > lai_time[length(lai_time)])]

    #Find missing tsteps
    missing <- which(!(all_tsteps %in% lai_time))

    if (length(missing) > 0) {

      #Create new time series, where missing tsteps set to NA
      new_ts <- smooth_lai_ts

      #Add new value
      for (n in missing) {  new_ts <- append(new_ts, NA, after=n-1)  }

      #Replace time vector
      lai_time <- all_tsteps
      smooth_lai_ts <- new_ts

    }


    ##########################
    ### Create climatology ###
    ##########################


    #Each year has 46 time steps, but first and last year are incomplete

    #Create climatological average for each time step

    #Check that time series starts on 1 Jan
    if (!(format(lai_time[1], "%m-%d") == "01-01")) { stop("Time series does not start 1 Jan") }


    #Find time steps for last (incomplete) year
    last_year <- which(grepl(modis_endyr, lai_time))

    #MODIS has 46 time steps per year
    no_tsteps <- length(which(grepl(modis_startyr , lai_time)))

    #Initialise
    modis_clim <- vector(length=no_tsteps)

    for (c in 1:no_tsteps) {

      #Indices for whole years
      inds <- seq(c, by=no_tsteps, length.out=floor(length(lai_time)/no_tsteps))

      #Add last year if applicable
      if( c <= length(last_year)) { inds <- append(inds, last_year[c]) }

      #Calculate average for time step
      modis_clim[c] <- mean(smooth_lai_ts[inds], na.rm=TRUE)

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



    ###################################
    ### Calculate running anomalies ###
    ###################################

    #Initialise
    lai_clim_anomalies <- rep(NA, length(lai_time))

    #Repeat climatology for whole time series
    modis_clim_all <- rep_len(modis_clim, length(lai_time))


    #Calculate running mean anomaly (+/- 6 months either side of each time step)
    anomaly <- rollmean(smooth_lai_ts - modis_clim_all, k=12, fill=NA)

    #Add rolling mean anomaly to climatology
    lai_clim_anomalies <- modis_clim_all + anomaly



    ###################################
    ### Match with site time series ###
    ###################################


    #Get timing info for site
    site_start_time <- ncatt_get(site_nc[[s]], "time")$units
    site_time       <- ncvar_get(site_nc[[s]], "time")
    site_tstep_size <- 86400 / (site_time[2] - site_time[1])

    #Extract year
    startyr    <- as.numeric(substr(site_start_time, start=15, stop=18))
    obs_length <- length(site_time)
    nyr        <- round(obs_length/(site_tstep_size*365))
    endyr      <- startyr + nyr - 1


    #Add climatological values to smoothed lai time series

    if ((startyr < modis_startyr)) {

      #Overwrite original time series with new extended data
      lai_clim_anomalies <- append(rep(modis_clim, modis_startyr - startyr),
                                   lai_clim_anomalies)

      #Add new time stamps
      extended_lai_time <- vector()
      for (y in startyr:(modis_startyr-1)) {

        extended_lai_time <- append(extended_lai_time, seq.Date(as.Date(paste0(y, "-01-01")),
                                                                by=lai_time[2]-lai_time[1], length.out=no_tsteps))
      }

      #Overwrite lai_time with new time series
      lai_time <- append(extended_lai_time, lai_time)

    }


    #Check if remaining NA values from missing time steps, gapfill if found
    if (any(is.na(lai_clim_anomalies))) {

      #Find missing values
      missing <- which(is.na(lai_clim_anomalies))

      #Repeat climatology for all years and gapfill time series
      clim_all_yrs <- rep(modis_clim, floor(length(lai_time)/no_tsteps))
      lai_clim_anomalies[missing] <- clim_all_yrs[missing]

    }

    #Find modis time step corresponding to site start time
    start_ind <- which(lai_time == paste0(startyr, "-01-01"))
    end_ind   <- tail(which(grepl(endyr, lai_time)), 1) #Last index of end year

    #Extract MODIS time steps matching site
    modis_ts_for_site   <- lai_clim_anomalies[start_ind:end_ind]
    modis_time_for_site <- lai_time[start_ind:end_ind]


    #Repeat modis time series to create a time series matching site time step
    modis_tseries <- vector()


    #Loop through time steps
    for (t in 1:length(modis_time_for_site)) {

      #Last time step
      if (t == length(modis_time_for_site)) {

        #Use the number of time steps that ensures final time series matches the length of site data
        modis_tseries <- append(modis_tseries, rep(modis_ts_for_site[t], length(site_time) - length(modis_tseries)))

        #All other time steps
      } else {

        time_diff <- modis_time_for_site[t+1] - modis_time_for_site[t]

        #Repeat each days estimate by the number of days and time steps per day
        modis_tseries <- append(modis_tseries, rep(modis_ts_for_site[t], time_diff * site_tstep_size))

      }

    }


    #Check that the number of time steps match
    if (length(modis_tseries) != length(site_time)) stop("MODIS and site time steps don't match")

    #Also check that no missing values
    if (any(is.na(modis_tseries))) { stop("Missing values in final MODIS time series")}


    ######################################
    ### Add LAI time series to NC file ###
    ######################################

    #Save to file

    # Define variable:
    laivar = ncvar_def('LAI_MODIS', '-', list(site_nc[[s]]$dim[[1]], site_nc[[s]]$dim[[2]], site_nc[[s]]$dim[[3]]),
                       missval=-9999,longname='MODIS 8-daily LAI')
    # Add variable and then variable data:
    site_nc[[s]] = ncvar_add(site_nc[[s]], laivar)
    ncvar_put(site_nc[[s]], 'LAI_MODIS', modis_tseries)

    #Close file handle
    nc_close(site_nc[[s]])

}

fdk_download_modis(df = sites)

