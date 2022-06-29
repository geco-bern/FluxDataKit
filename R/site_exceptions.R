site_exceptions <- function(site_code, var_data, att_data, qc_val) {
  
    #NB. all indices to fix were found manually
  
    #Variables available
    vars <- names(var_data)
  
    
  if (site_code == "AU-Cow") {
      
      #Bad RH values, replace with values converted from Qair
      #First part of time series looks similar, but Qair conversion removes
      #some of the biggest outliers (>100) and the bad data for the last third of
      #the time series is replaced with more realistic values
      
      var_data$RH <- SpecHumidity2Rel(var_data$Qair, var_data$Tair, 
                                      tair_units=att_data$Tair$units, 
                                      pressure=var_data$Psurf, psurf_units=att_data$Psurf$units)
      
      #Use QC flags from Qair
      var_data$RH_qc <- var_data$Qair_qc
      
      #Add info that converted from Qair
      att_data$RH$correction <- "Converted from specific humidity, qc flags as per Qair"
      
      
      
  } else if (site_code == "AU-Tum") {
    
    #Three outlier Psurf values, replace these with the previous time step
    #(outliers are back to back so use the value before first outlier)
    
    #Using 84000 Pa as the limit as start to get more values if use
    #higher values
    
    ind <- which(var_data$Psurf < 84000)
  
    #Add if-clause in case data changes so doesn't cause bugs
    if (length(ind) > 0) {
      
      #Replace with previous value
      var_data$Psurf[ind] <- var_data$Psurf[ind[1]-1]
      
      #Update QC
      var_data$Psurf_qc[ind] <- qc_val
      
    }  
      
    
       
  } else if (site_code == "BE-Lon") {
    #Longwave data needs fixing    
  
    ind_start <- 74000
    ind_end   <- 92500
    
    #Check that Tair units are in Kelvin and that RH is available
    if (att_data$Tair$units != "K") stop("Wrong LWdown units")
    if (length(which(vars == "RH")) == 0) stop("RH not available")
    
    lwdown <- SynthesizeLWdown(TairK = var_data$Tair[ind_start:ind_end], 
                               RH = var_data$RH[ind_start:ind_end])
    
    #Add corrected flux to variable data and QC flag
    var_data$LWdown[ind_start:ind_end]    <- lwdown  
    var_data$LWdown_qc[ind_start:ind_end] <- qc_val 
    
    #Add new gapfilled % to attribute data 
    att_data$LWdown$`Gap-filled_%` <- round(length(which(var_data$LWdown_qc > 0)) /
                                      length(var_data$LWdown_qc) * 100, digits=1)
    
    
    
    
  } else if (site_code == "BE-Vie") {
    #CO2 drift correction, interpolate back from time step 60100
    
    #Interpolate back from this point
    ind <- 60100
    
    #Linear prediction
    co2_pred <- linear_pred_co2(co2=var_data$CO2air, start_ind=ind+1, 
                                end_ind=length(var_data$CO2air))
    
 
    #Replace predicted and QC with post-processing value
    var_data$CO2air[1:ind]    <- co2_pred[1:ind]
    var_data$CO2air_qc[1:ind] <- qc_val
    
    #Add information to metadata
    att_data$CO2air$CO2_correction <- "Linear fit"
    
    #Add new gapfilled % to attribute data 
    att_data$CO2air$`Gap-filled_%` <- round(length(which(var_data$CO2air_qc > 0)) /
                                              length(var_data$CO2air_qc) * 100, digits=1)
    
    
    
    
  } else if (site_code == "ES-ES2") {
    
    #One day missing in met data, gapfill with previous day
    vars_to_fix <- c("Tair", "VPD", "Qair", "Precip", "SWdown", "Wind", "Psurf")
    
    for (v in vars_to_fix) {
      
      #Missing time steps
      na_ind <- which(is.na(var_data[[v]]))
      
      var_data[[v]][na_ind]    <- var_data[[v]][na_ind - (length(na_ind))]
      var_data[[paste0(v, "_qc")]][na_ind] <- qc_val
      
      #Add new gapfilled % to attribute data 
      att_data[[v]]$`Gap-filled_%` <- round(length(which(var_data[[paste0(v, "_qc")]] > 0)) /
                                              length(var_data[[paste0(v, "_qc")]]) * 100, digits=1)
      
    }
  
    
    
    
  } else if (site_code == "DK-Sor") {
    #Correct CO2 by fitting a linear slope to data, and using that to predict CO2
    
    #Linear prediction
    co2_pred <- linear_pred_co2(co2=var_data$CO2air, start_ind=1, 
                                end_ind=length(var_data$CO2air))
    
    #Replace all values
    var_data$CO2air    <- co2_pred
    var_data$CO2air_qc <- qc_val
    
    #Add information to metadata
    att_data$CO2air$CO2_correction <- "Linear fit, correcting all time steps"
    
    #Add new gapfilled % to attribute data 
    att_data$CO2air$`Gap-filled_%` <- round(length(which(var_data$CO2air_qc > 0)) /
                                              length(var_data$CO2air_qc) * 100, digits=1)
    
    
    
    
  } else if (site_code == "PT-Mi2") {
    
    
    ### Anomalous LWdown values ###
  
    #some unrealistic fluctuations in lwdown,
    #smooth by linearly interpolating between two non-anomalous values
    #Found values to fix manually. Tried automating by detecting large changes between
    #time steps but does not work well

    inds_to_fix <- c(49808:49851, 50039:50042, 50192:50195, 52065:52066,
                     52244:52249, 52300:52301, 52348, 52350)
    
    #Set values to NA and then use linear interpolation
    lwdown <- var_data$LWdown
    
    lwdown[inds_to_fix] <- NA
    
    #Gapfill with linear interpolation
    lwdown_new <- na.approx(lwdown)
      
    #Replace with new data
    var_data$LWdown[inds_to_fix] <- lwdown_new[inds_to_fix]
    
    #Change QC flag
    var_data$LWdown_qc[inds_to_fix] <- qc_val
    
    
    
  } else if (site_code == "RU-Fyo") {
    #Gapfill LWdown from XX onwards using data from previous years
    
    ind_start <- 144000
    ind_end   <- 277000
    
    #Check that Tair units are in Kelvin and that RH is available
    if (att_data$Tair$units != "K") stop("Wrong LWdown units")
    if (length(which(vars == "RH")) == 0) stop("RH not available")
    
    lwdown <- SynthesizeLWdown(TairK = var_data$Tair[ind_start:ind_end], 
                               RH = var_data$RH[ind_start:ind_end])
    
    #Add corrected flux to variable data and QC flag
    var_data$LWdown[ind_start:ind_end]    <- lwdown  
    var_data$LWdown_qc[ind_start:ind_end] <- qc_val 
    
    #Add new gapfilled % to attribute data 
    att_data$LWdown$`Gap-filled_%` <- round(length(which(var_data$LWdown_qc > 0)) /
                                              length(var_data$LWdown_qc) * 100, digits=1)
    
    
    
  } else if (site_code == "RU-Zot") {
    #Gaps in LWdown and RH, need to gapfill
    
    #-- Gapfill RH by converting VPD
    
    rh <- VPD2RelHum(VPD=var_data$VPD, airtemp=var_data$Tair, 
                     vpd_units=att_data$VPD$units, tair_units=att_data$Tair$units)
      
    #Replace missing values
    na_ind <- which(is.na(var_data$RH))
    
    var_data$RH[na_ind]    <- rh[na_ind]
    var_data$RH_qc[na_ind] <- qc_val
    
    #Add new gapfilled % to attribute data 
    att_data$RH$`Gap-filled_%` <- round(length(which(var_data$RH_qc > 0)) /
                                              length(var_data$RH_qc) * 100, digits=1)
    
    
    
    #-- Synthesise LWdown
    
    #Check that Tair units are in Kelvin and that RH is available
    if (att_data$Tair$units != "K") stop("Wrong LWdown units")
    if (length(which(vars == "RH")) == 0) stop("RH not available")
    
    lwdown <- SynthesizeLWdown(TairK = var_data$Tair, 
                               RH = var_data$RH)
    
    #Add corrected flux to variable data and QC flag
    na_ind <- which(is.na(var_data$LWdown))
    var_data$LWdown[na_ind]    <- lwdown[na_ind]  
    var_data$LWdown_qc[na_ind] <- qc_val
    
    #Add new gapfilled % to attribute data 
    att_data$LWdown$`Gap-filled_%` <- round(length(which(var_data$LWdown_qc > 0)) /
                                              length(var_data$LWdown_qc) * 100, digits=1)
    
  
    
  } else if (site_code == "US-ARM") {
    #Need to re-gapfill parts of LWdown
    
    inds <- c(20500:71000, 90500:93500)

    #Check that Tair units are in Kelvin and that RH is available
    if (att_data$Tair$units != "K") stop("Wrong LWdown units")
    if (length(which(vars == "RH")) == 0) stop("RH not available")
    
    lwdown <- SynthesizeLWdown(TairK = var_data$Tair[inds], 
                               RH = var_data$RH[inds])
    
    #Add corrected flux to variable data and QC flag
    var_data$LWdown[inds]    <- lwdown[inds]  
    var_data$LWdown_qc[inds] <- qc_val 
    
    #Add new gapfilled % to attribute data 
    att_data$LWdown$`Gap-filled_%` <- round(length(which(var_data$LWdown_qc > 0)) /
                                              length(var_data$LWdown_qc) * 100, digits=1)
    
    
    
  } else if (site_code == "US-NR1") {
    
    #Re-gapfill parts ofLWdown
    
    #-- Re-gapfill LWdown
    ind <- c(107500:137000)
    
    #Check that Tair units are in Kelvin and that RH is available
    if (att_data$Tair$units != "K") stop("Wrong LWdown units")
    if (length(which(vars == "RH")) == 0) stop("RH not available")
    
    lwdown <- SynthesizeLWdown(TairK = var_data$Tair, 
                               RH = var_data$RH)
    
    #Add corrected flux to variable data and QC flag
    var_data$LWdown[ind]    <- lwdown[ind]  
    var_data$LWdown_qc[ind] <- qc_val
    
    #Add new gapfilled % to attribute data 
    att_data$LWdown$`Gap-filled_%` <- round(length(which(var_data$LWdown_qc > 0)) /
                                              length(var_data$LWdown_qc) * 100, digits=1)
    
  } 
    
    
    
  #########################################
  ### Any sites with missing CO2 values ###
  #########################################
    
    
  #Missing CO2 values
  if (any(is.na(var_data$CO2air))) {
    
    co2 <- var_data$CO2air
    
    #Linear prediction
    co2_pred <- linear_pred_co2(co2=co2, start_ind=1, end_ind=length(co2))
    
    #Replace missing with predicted and QC with post-processing value
    na_ind <- which(is.na(co2))
    
    var_data$CO2air[na_ind]    <- co2_pred[na_ind]
    var_data$CO2air_qc[na_ind] <- qc_val
    
    #Add information to metadata
    att_data$CO2air$CO2_correction <- "Linear fit, gapfilling missing time steps"
    
    #Add new gapfilled % to attribute data 
    att_data$CO2air$`Gap-filled_%` <- round(length(which(var_data$CO2air_qc > 0)) /
                                              length(var_data$CO2air_qc) * 100, digits=1)
    
  }
  

      
  #########################  
  ### Missing RH values ###
  #########################  

     
  #(some sites have missing RH but all VPD so convert between the two)
  if (any(is.na(var_data$RH))) {
    
    rh <- VPD2RelHum(VPD=var_data$VPD, airtemp=var_data$Tair, 
                     vpd_units=att_data$VPD$units, tair_units=att_data$Tair$units)
    
    #Replace missing values
    na_ind <- which(is.na(var_data$RH))
    
    var_data$RH[na_ind]    <- rh[na_ind]
    var_data$RH_qc[na_ind] <- qc_val
    
    #Add new gapfilled % to attribute data 
    att_data$RH$`Gap-filled_%` <- round(length(which(var_data$RH_qc > 0)) /
                                          length(var_data$RH_qc) * 100, digits=1)
    
  }
  
    
  #############################  
  ### Missing LWdown values ###
  #############################  
    
  ### Missing values ###
    
  if (any(is.na(var_data$LWdown))) {
    
    #Check that Tair units are in Kelvin and that RH is available
    if (att_data$Tair$units != "K") stop("Wrong LWdown units")
    if (length(which(vars == "RH")) == 0) stop("RH not available")
    
    lwdown <- SynthesizeLWdown(TairK = var_data$Tair, 
                               RH = var_data$RH)
    
    #Replace missing with predicted and QC with post-processing value
    na_ind <- which(is.na(var_data$LWdown))
    
    var_data$LWdown[na_ind]    <- lwdown[na_ind]
    var_data$LWdown_qc[na_ind] <- qc_val
    
    #Add information to metadata
    att_data$LWdown$LWdown_correction <- "Synthesised from RH and Tair (Abramowitz method)"
    
    #Add new gapfilled % to attribute data 
    att_data$LWdown$`Gap-filled_%` <- round(length(which(var_data$LWdown_qc > 0)) /
                                              length(var_data$LWdown_qc) * 100, digits=1)
    
  }

    
    
  #######################
  ### Negative values ###
  #######################
    
  ### LAI ###
    
  lai_vars <- vars[which(grepl("LAI_", vars))]
  
  #Loop through products
  for (lai in lai_vars) {
    
    #Replace negative values with zero
    var_data[[lai]] <- replace(var_data[[lai]], var_data[[lai]] < 0, 0)
  }  
    
  
  ### Met data ###
  
  #Check for negative rainfall, wind speed, SWdown and VPD
  #(only a problem in OzFlux)
  #as well as negative lwdown
  
  vars_to_check <- c("Precip", "Wind", "VPD", "RH", "Qair", "SWdown", "LWdown")
  
  for (v in vars_to_check)
  {
    
    if (any(var_data[[v]] < 0, na.rm=TRUE)) { #need to use na.rm, otherwise returns NA if any missing
      
      #Find negative values
      neg_ind <- which(var_data[[v]] < 0)
      
      #Set at zero
      var_data[[v]][neg_ind]    <- 0
      var_data[[paste0(v, "_qc")]][neg_ind] <- qc_val
      
      att_data[[v]]$neg_value_correction <- "Negative values existed, set to 0"
      
    }
  }
  

    
  ####################################
  ### Relative humidity above 100% ### 
  ####################################
  
  #(mainly in OzFlux but also a couple of La Thuile sites)
  
  if (any(var_data$RH > 100, na.rm=TRUE)) {
    
    #Find negative values
    high_ind <- which(var_data$RH > 100)
    
    #Cap to 100%
    var_data$RH[high_ind]    <- 100
    var_data$RH_qc[high_ind] <- qc_val
    
    #Add info on correction to metadata
    att_data$RH$range_correction <- "RH values >100% existed, capped to 100%"
    
  }
  
  

  #If not fixes, returns original data
  
  return(list(var_data=var_data, att_data=att_data))
  
  
} #function






