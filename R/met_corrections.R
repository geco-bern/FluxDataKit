met_corrections <- function(infile_met, outfile_met, outdir, qc_info, new_qc, global_co2)
{


  ###############################################
  #########------ Met corrections ------#########
  ###############################################
  
  #if (file.exists(outfile_met)) return()
  
  #check that qc_info matches site
  if (!grepl(qc_info$Site_code, outfile_met)) stop("QC info does not match site")
  
  
  # - gets desired years
  # - fixes CO2 if applicable
  # - applies other fixes if applicable
  # - checks that no missing values in met data
  
  site_code <- qc_info$Site_code
  
  #Open file handle
  met_nc <- nc_open(infile_met)
  

  #######################
  ### Get timing info ###
  #######################
  
  
  #Get time vector
  time <- ncvar_get(met_nc, "time")
  
  #Get time units
  time_units <- strsplit(ncatt_get(met_nc, "time")$units, "seconds since ")[[1]][2]
  
  #Convert to Y-M-D h-m-s
  time_date <- as.POSIXct(time, origin=time_units, tz="GMT")
  
  #Get time interval (in fraction of day)
  tsteps_per_day <-  60*60*24 / (time[2] - time[1]) 
  
  
  #Find adjusted start and end time
  years <- as.numeric(format(time_date, "%Y"))
  
  
  
  #####################
  ### Get variables ###
  #####################
  
  
  ### Get all data variables with a time dimension ###
  
  #Get variable names
  vars <- names(met_nc$var)
  
  #Load variable data
  var_data <- lapply(vars, function(x) ncvar_get(met_nc, x))
  
  #Set names
  names(var_data) <- vars 
  
  
  #Get variable attributes
  att_data <- lapply(vars, function(x) ncatt_get(met_nc, x))
  
  #Set names
  names(att_data) <- vars 
  
  
  
  ########################################
  ### Correct CO2 using global records ###
  ########################################
  
  
  #If replacing CO2 with global CO2
  
  if (qc_info$Global_CO2) {
    
    #loop through years
    co2_ts <- sapply(years, function(x) global_co2[which(global_co2 == x), 2])
    
    #Replace values in CO2 variable
    var_data$CO2air <- co2_ts 
    
    #Set QC values to missing
    var_data$CO2air_qc <- rep(new_qc, length(time))  
    
    #Replace gapfill percentage (now 100%)
    att_data$CO2air["Gap-filled_%"] <- 100
    
    #Add information to metadata
    att_data$CO2air$CO2_correction <- "Global CO2 (annual Mauna Loa time series)"
    
  }
  
  
  
  #############################################
  ### Check for additional site corrections ###
  #############################################
  
  #We have determined additional corrections to individual sites
  #mostly to CO2 and LWdown records
  
  #Check if any apply to this site
  
  site_fixes <- site_exceptions(site_code, var_data, att_data, qc_val=new_qc)
  
  
  #Replace with new fixed data
  var_data <- site_fixes$var_data
  att_data <- site_fixes$att_data
  
  
  
  
  ##########################
  ### Adjust time period ###
  ##########################
  
  
  #Get years to process
  start_yr <- qc_info$Start_year
  
  end_yr   <- qc_info$End_year
  
  
  ### Adjust length of time-varying variables ##
  
  #Get dimensions for each variable
  dims <- lapply(vars, function(x) sapply(met_nc[["var"]][[x]][["dim"]], function(dim) dim[["name"]]))
  
  # #Find which variables are time-varying (used later so leave outside if-loop)
  var_inds <- which(sapply(dims, function(x) any(x == "time")))
  
  
  #If need to adjust
  if (start_yr > 1 | end_yr < 0) {
    
    #New start and end year
    new_start_year <- years[1] + start_yr -1 
    new_end_year   <- years[length(years)] + end_yr #end_yr negative so need to sum
    
    
    #Start and end indices
    start_ind <- which(years == new_start_year)[1]
    end_ind   <- tail(which(years == new_end_year), 1)
    
    #Create new time stamp
    new_time_unit <- paste0("seconds since ", new_start_year, "-01-01 00:00:00")
    
    #New time vector
    time_var <- seq(0, by=60*60*24 / tsteps_per_day, length.out=length(c(start_ind:end_ind)))
  
    
    #Change dimensions and values for time-varying data
    for (v in vars[var_inds]) {
      
      #Change time dimension
      met_nc$var[[v]]$varsize[3] <- length(time_var)
      
      #Change time values
      met_nc$var[[v]]$dim[[3]]$vals <- time_var
      
      #Change time size
      met_nc$var[[v]]$dim[[3]]$len <- length(time_var)
      
      #Change length
      met_nc$var[[v]]$size[3] <- length(time_var)
      
      #Change values in var_data
      var_data[[v]] <- var_data[[v]][start_ind:end_ind]
      
      # #Change chunk size (no idea what this is but produces an error otherwise
      # #during nc_create)
      # met_nc[[s]]$var[[v]]$chunksizes <- NA
      
      #Replace time unit
      time_ind <- which(sapply(met_nc$var[[v]]$dim, function(x) x$name) == "time")
      met_nc$var[[v]]$dim[[time_ind]]$units <- new_time_unit
      
    }
    
    
    #Also adjust time dimension and units
    
    #Change time dimensions
    #Change values, length and unit
    met_nc$dim$time$vals  <- time_var
    met_nc$dim$time$len   <- length(time_var)
    
    met_nc$dim$time$units          <- new_time_unit
    
    
    
    #Also adjust years in output file name
    
    #New years
    new_yr_label <- paste0(new_start_year, "-", new_end_year)
    
    #File name without path
    filename <- basename(outfile_met)
    
    #Replace file name with new years
    outfile_met <- paste0(outdir, gsub("[0-9]{4}-[0-9]{4}", new_yr_label, filename))
    
  }
  
  
  
  ##########################################
  ### Check for missing vals in met data ###
  ##########################################
  

  #Do a final check to make sure there are no missing values in
  #any met variables
  
  #Find met vars, ignoring any qc variables (don't care about gaps in those)
  met_vars <- vars[var_inds][which(!grepl("_qc", vars[var_inds]))]
  
  #Loop through variables
  for (v in met_vars) {
    
    #Check if any missing values
    if (any (is.na(var_data[[v]]))) {
      
      stop(paste0("Missing values in ", v, ", site: ", site_code))
    } 
  }

  
  
  #First check that LAI data is available
  lai_vars <- vars[which(grepl("LAI_", vars))]
  
  #Should have two available, check that they are there
  
  if (length(lai_vars) != 2) {
    stop(paste0("LAI variables not available, check site: ", site_code))
  }
  
  
  
  
  #########################
  ### Update attributes ###
  #########################
  
  #Need to update missing and gap-filled percentages
  
  for (v in names(att_data)) {
    
    #Missing percentage
    if (any(names(att_data[[v]]) == "Missing_%")) {
      
      att_data[[v]]["Missing_%"] <-  round(length(which(is.na(var_data[[v]])))/
                                           length(var_data[[v]]) * 100, digits=1)
        
    }
  
    #Gap-filled percentage  
    if (any(names(att_data[[v]]) == "Gap-filled_%")) {
      
      att_data[[v]]["Gap-filled_%"] <-  round(length(which(var_data[[paste0(v, "_qc")]] > 0)) /
                                              length(var_data[[paste0(v, "_qc")]]) * 100, digits=1)
      
    }

  }
  
  
  ##################### 
  ### Re-write file ###
  #####################
  

  ###--- Set dimensions ---###
  
  #Get dimensions from input file
  new_dims <- met_nc$dim
  
  
  ###--- Define variables ---###
  
  #Get variables from input file
  new_vars <- met_nc$var
  
  
  ###--- Set up new file ---###
  
  #New file handle
  out_nc <- nc_create(outfile_met, vars=new_vars)
  
  
  ###--- Global attributes ---###
  
  #Get global attributes
  global_atts <- ncatt_get(met_nc, varid=0)
  
  #Add new QC flag value to metadata    
  global_atts$QC_flag_descriptions <- paste0(global_atts$QC_flag_descriptions, 
                                             ", Post-processed: ", new_qc)
  
  #Add to file
  #For some reason this crashes if using lapply, loop works ok-ish    
  for(a in 1:length(global_atts)){
    ncatt_put(out_nc, varid=0, attname=names(global_atts)[a], 
              attval=unlist(global_atts[a]))
  }
  
  
  ###--- Variable data ---###
  
  #Write variables to output file
  for (v in names(new_vars)) {
    
    ncvar_put(nc=out_nc, varid=new_vars[[v]],
              vals=var_data[[v]])
  }

  
  ###--- Variable attributes ---###
  
  #Write attributes to output file
  for (v in names(att_data)) {
    
    for (a in names(att_data[[v]]))
      
      ncatt_put(nc=out_nc, varid=new_vars[[v]],
                attname=a, attval=att_data[[v]][[a]])
    
  }
  
  
  
  #Close output file
  nc_close(out_nc)
  
  #Close original file handle
  nc_close(met_nc)
  
  
  
  ##########################
  ### Select default LAI ###
  ##########################
  
  
  #Open file handle
  nc_out <- nc_open(outfile_met, write=TRUE)
  
  
  #MODIS
  if (qc_info$LAI == "MODIS") {
    
    default_lai    <- "LAI_MODIS"
    default_source <- "MODIS"
    
    alt_lai    <- "LAI_Copernicus"
    alt_source <- "Copernicus"
  
    
  #Copernicus  
  } else if (qc_info$LAI == "Copernicus") {
    
    
    default_lai    <- "LAI_Copernicus"
    default_source <- "Copernicus"
    
    alt_lai    <- "LAI_MODIS"
    alt_source <- "MODIS"
    
    
  #Else stop  
  } else if (!is.na(qc_info$LAI)) {
    stop("Incorrect LAI specified in qc_info")
  }
  
  
  #Rename LAI
  nc_out <- ncvar_rename(nc_out, default_lai, "LAI")
  nc_out <- ncvar_rename(nc_out, alt_lai, "LAI_alternative")
  
  
  #Add source in attribute data
  ncatt_put(nc=nc_out, varid="LAI",
            attname="source", attval=default_source)
  
  ncatt_put(nc=nc_out, varid="LAI_alternative",
            attname="source", attval=alt_source)
  
  
  #Close file handle
  nc_close(nc_out)
  
   
} #function 
  