# 
# 
# #clear R environment
# rm(list=ls(all=TRUE))
# 
# 
# ### Temporary for writing function ###
# data <- read.csv("~/Documents/FLUXNET2016_processing/FLUXNET2015/FLX_AR-SLu_FLUXNET2015_FULLSET_HH_2009-2011_1-3.csv", header=TRUE)
# 
# qle    <- data$LE_F_MDS
# qle_qc <- data$LE_F_MDS_QC
# 
# qh    <- data$H_F_MDS
# qh_qc <- data$H_F_MDS_QC
# 
# rnet  <- data$NETRAD
# qg    <- data$G_F_MDS
# qg_qc <- data$G_F_MDS_QC
# 
# time <- strptime(data$TIMESTAMP_START,"%Y%m%d%H%M", tz="GMT")
# tstepsize <- 1800
# # 
# # #---------------------------


  
energy_balance_correction <- function(qle, qle_qc, qh, qh_qc, rnet, qg, qg_qc, time, tstepsize)
{


  #Method is from: https://github.com/AmeriFlux/ONEFlux
  
  library(chron)
  
  #Save original values for later use
  qle_all    <- qle
  qle_all_qc <- qle_qc
   
  qh_all    <- qh
  qh_all_qc <- qh_qc
  
  
  
  ### Hour of day ###
  hod_vec <- format(time, format="%H:%M:%S")
  ch      <- times(hod_vec)
  
  #Vector of hours from midnight
  hod <- 60 * hours(ch) + minutes(ch)
  
  
  
  ##############################
  ### Find good quality data ###
  ##############################
  
  
  #[AmeriFlux Github]: The corrected fluxes are obtained multiplying the original, gapfilled LE and H data 
  #by a correction factor (EBCcf: Energy Balance Closure correction factor) calculated 
  #starting from the halfhours where all the components needed to calculated the energy 
  #balance closure were available (Latent Heat, Sensible Heat, Net Radiation, Soil Heat 
  #Flux, with LE and H original or gapfilled with high quality). The EBCcf is calculated 
  #for each half hour as (NETRAD-G)/(H+LE). The EBCcf timeserie is filtered removing the 
  #values that are outside 1.5 times the interquartile range and used as basis to calculate 
  #the corrected H and LE fluxes.
  
  
  #Find time steps where Rnet and Qg are measured, and Qh and Qle are observed or high quality gap-filled
  
  poor_quality_ind <- unique(c(which(qle_all_qc > 1 | is.na(qle_all_qc)), 
                               which(qh_all_qc > 1 | is.na(qh_all_qc)),
                               which(qg_qc != 0 | is.na(qg_qc)),
                               which(is.na(rnet))))#,
                               #which(qg == -9999),
                               #which(qle_all == -9999),
                               #which(qh_all == -9999)))#,
                               #))
  
  
  
  #Mask out data that is not sufficient quality
  
  #Then mask
  qle[poor_quality_ind]  <- NA
  qh[poor_quality_ind]   <- NA
  rnet[poor_quality_ind] <- NA
  qg[poor_quality_ind]   <- NA
  
  
  ### Calculate energy balance correction ###
  
  #Calculate for each half-hourly period from the good quality data
  #(no need to screen for NA, if any component is missing, resulting correction factor
  #will automatically become NA)
  
  ebcf <- (rnet - qg) / (qh + qle)
  
  
  ### Then remove values outside 1.5 * interquartile range ###
  
  #Interquartile range
  iq <- quantile(ebcf, probs=c(0.25, 0.75), na.rm=TRUE)
  
  
  #Determine 1.5*IQ range
  iqr <- iq[2] - iq[1]
  
  median <- median(ebcf, na.rm=TRUE)
  
  higher <- median + 1.5*iqr
  lower  <- median - 1.5*iqr
  
  
  #Mask value outside range  
  ebcf[ebcf < lower]  <- NA
  ebcf[ebcf > higher] <- NA
  
  
  
  ### Mask out sunrise and sunset times ###
    
  #Only want to use times between 22:00-2:30 and 10:00-14:30
  #150 minutes past midnight equals 2:30, 600 equals 10:00,
  #870 equals 14:30 and 1320 equals 22:00
  
  #Mask out other time periods (noting timestamp indicates start time)
  time_of_day <- hod
  time_of_day[time_of_day >= 150 & time_of_day < 600]  <- NA #between 2:30 and 10:00
  time_of_day[time_of_day >= 870 & time_of_day < 1320] <- NA #between 14:30 and 22:00
  
  
  #Equivalent of:
  # time_of_day[time_of_day >= 2:30 & time_of_day < 10:00]  <- NA #between 2:30 and 10:00
  # time_of_day[time_of_day >= 14:30 & time_of_day < 22:00] <- NA #between 14:30 and 22:00
  
  
  
  #Mask EBCF for hours outside above, and time when EBCF not available
  ebcf[is.na(time_of_day)] <- NA
  time_of_day[is.na(ebcf)] <- NA
  
  
  
  
  
  ###############################
  ### Correct LE and H fluxes ###
  ###############################
  
  
  #The corrected fluxes calculation are obtained using three hierarchical methods:
    
    
  ################
  ### Method 1 ###
  ################
  
  #[AmeriFlux Github]: Method 1: For each half hour a moving window of +/- 15 days 
  #is applied to select EBCcf of halfhours in the periods 22.00-2.30 and 10.00-14.30. 
  #The selection if these time windows is to avoid sunshine and sunset periods where 
  #changes in the heat storage in the ecosystem is large and for this reason the 
  #energy balance not closed using the available measurements. The selected EBCcf 
  #are then used to calculate corrected versions of the LE and H fluxes (one 
  #corrected fluxes version for each EBCcf factor included in the moving window) 
  #and 25, 50 and 75 percentiles of the corrected fluxes are extracted (LEcorr25, 
  #LEcorr, LEcorr75, Hcorr25, Hcorr, Hcorr75).
  
  
  # st <- Sys.time()
  # 
  
  #Initialise
  qle_corrected <- rep(NA, length(qle_all))
  qh_corrected  <- rep(NA, length(qh_all))
  
  
  #Method (save for debugging)
  method <- rep(NA, length(qle_all))
  
  
  #Remove after debugging
  #n_for_ebcf <- rep(NA, length(qle_all))
  
  
  #Loop through time steps
  for (t in 1:length(qle_corrected)) {  
  
  
    #Start and end index for moving window around time step
    #Use a moving window of +/- 15 days
    #(need to remove one time step at each end to match Fluxnet method)
    start <- which(time == (time[t] - hrs(24*15))) + 1
    end   <- which(time == (time[t] + hrs(24*15))) - 1
    
    
    #Add exceptions for first and last parts of tiem series
    if (length(start) == 0) {
      start <- 1
    }
    if (length(end) == 0) {
      end <- length(time)
    }
    
    
    #Extract EBCF for this time step moving window
    ebcf_tstep <- ebcf[start:end]
    
    #Indices to use for calculating EB correction
    ind_avail <- which(!is.na(ebcf_tstep))
    
    
    #Check that at least 5 values available, if not skip time step
    if (length(ind_avail) < 5) {
      next
    }
    
    
    #Calculate corrected values for each available ebcf
    qle_corr <- sapply(1:length(ind_avail), function(e) qle_all[t] * ebcf_tstep[ind_avail[e]])
    qh_corr  <- sapply(1:length(ind_avail), function(e) qh_all[t] * ebcf_tstep[ind_avail[e]])
    
    
    #Calculate median and save in method 1 data vectors
    qle_corrected[t] <- quantile(qle_corr, probs=0.5, na.rm=TRUE)
    qh_corrected[t]  <- quantile(qh_corr, probs=0.5, na.rm=TRUE)
  
    #Remove after debugging
    method[t]     <- 1
    #n_for_ebcf[t] <- length(which(!is.na(ebcf_tstep)))
    
  }
  
  
  
  
  
  
  #   
  # et <- Sys.time()
  # et-st
  # 
  
  
  
  # #REMOVE ONCE DEBUGGED ------------------
  # t1 <-t #which(!is.na(qle_corrected))[1]  +1
  # 
  # 
  # print(t1)
  # which(data$EBC_CF_METHOD == 1)[1]
  # 
  # print("#--------")
  # qle_corrected[t1]
  # data$LE_CORR[t1]
  # 
  # print("#--------")
  # n_for_ebcf[t1]
  # data$EBC_CF_N[t1]
  # 
  # 
  # #---------------------------------
  
  
  
  # ################
  # ### Method 2 ###
  # ################
  # 
  # #[AmeriFlux Github] Method 2: applied in the halfhours where the moving window of method 1 
  # #includes less than 5 EBCcf values. The corrected fluxes are calculated 
  # #using the average of the EBCcf used to calculate LEcorr and Hcorr in 
  # #method 1 in the halfhours included in a moving window of +/- 5 days 
  # #and +/- one hour. With method 2 LEcorr25, Hcorr25, LEcorr75 and Hcorr75 
  # #are not calculated.
  # 
  # 
  # #Find incides of corrected Qle and Qh that are missing after applying Method 1
  # #These should match for Qle and Qh
  # 
  # # 
  # # st <- Sys.time()
  # 
  # 
  # 
  # ind_missing_m2 <- which(is.na(qle_corrected))
  # 
  # 
  # #If values to process
  # if (length(ind_missing_m2) > 0) {
  #   
  #   
  #   #Loop through incides
  #   for (t in ind_missing_m2) {
  #     
  #     
  #     #Start and end index for moving window around time step
  #     #Use a moving window of +/- 5 days and one hour
  #     #(need to remove one time step at each end to match Fluxnet method)
  #     start <- which(time == (time[t] - hrs(24*5 +1)))
  #     end   <- which(time == (time[t] + hrs(24*5 +1)))
  #     
  #     
  #     #Add exceptions for first and last parts of tiem series
  #     if (length(start) == 0) {
  #       start <- 1
  #     }
  #     if (length(end) == 0) {
  #       end <- length(time)
  #     }
  #     
  #     
  #     #Extract EBCF for this time step moving window
  #     #and calculate average
  #     ebcf_tstep <- mean(ebcf[start:end], na.rm=TRUE)
  #     
  #     
  #     #Calculate median and save in method 1 data vectors
  #     qle_corrected[t] <- qle_all[t] * ebcf_tstep
  #     qh_corrected[t]  <- qh_all[t] * ebcf_tstep
  #     
  #     #Debugging
  #     if (!is.na(ebcf_tstep)) method[t] <- 2
  #  
  #   }
  #   
  # }
  # 
  # # et <- Sys.time()
  # # et-st
  # 
  # 
  # 
  # 
  # 
  ################
  ### Method 3 ###
  ################
  
  
  #[AmeriFlux Github] Method 3: If it is still not possible to calculate an EBCcf (e.g. in case of 
  #long gaps) the same moving window is also applied to the same halfhour of the 
  #years before and after when available. With method 3 LEcorr25, Hcorr25, LEcorr75 
  #and Hcorr75 are not calculated.
  
  
  #Find incides of corrected Qle and Qh that are missing after applying Method 1 and 2
  #These should match for Qle and Qh
  
  ind_missing_m3 <- which(is.na(qle_corrected))
  
  
  #If values to process
  if (length(ind_missing_m3) > 0) {
    
  
    #Loop through incides
    for (t in ind_missing_m3) {
      
      
      #Start and end index for moving window around time step
      #Use a moving window of +/- 5 days and one hour
      #(need to remove one time step at each end to match Fluxnet method)
      start_time <- time[t] - hrs(24*15) +1 
      end_time   <- time[t] + hrs(24*15) -1
      
      
      #Current year (do min and max to account for time periods outside data range)
  #    start_time_cur <- max(1, which(time == start_time))
   #   end_time_cur   <- min(length(time), which(time == end_time))
      
      time_indices <- vector()
      
      
      #Next year
      start_time_next <- which(time == add_months(start_time, 12))
      end_time_next   <- min(length(time), which(time == add_months(end_time, 12)))
      
      if (length(start_time_next) > 0) {
        time_indices <- append(time_indices, start_time_next:end_time_next)
      }

    
      #Add previous if available
      
      #Previous year 
      start_time_prev <- which(time == add_months(start_time, -12))
      end_time_prev   <- which(time == add_months(end_time, -12))
  
      if (length(end_time_prev) > 0) {
        
        time_indices <- append(time_indices, c(max(1, start_time_prev) : end_time_prev))
   
      }
      
      #Extract EBCF for this time step moving window
      #and calculate average
      ebcf_tstep <- median(ebcf[time_indices], na.rm=TRUE)
      
      
      #Calculate median and save in method 1 data vectors
      qle_corrected[t] <- qle_all[t] * ebcf_tstep
      qh_corrected[t]  <- qh_all[t] * ebcf_tstep
      
      #Debugging
      if (!is.na(ebcf_tstep)) {
        method[t] <- 2
      }
      
      
    }
    
  }
  
  # 
  # et <- Sys.time()
  # et-st
  # 
  # 
  # plot(data$LE_CORR, type='l', col="red")
  # lines(qle_corrected)
  # 
  # lines(qle_all, col='blue')
  # 
  # plot(data$H_CORR, type='l', col="red")
  # lines(qh_corrected)
  # 
  # 
  # plot(ebcf_m3, type='l')
  # 
  # 
  #If using all available years for method 3....
  
  
  # #convert time to months/days/hours
  # date_mdh <- format(time, format="%m-%d-%H:%M:%S")
  # 
  # 
  # 
  # if (length(ind_missing_m3) > 0) {
  #   
  #   
  #   #Loop through incides
  #   for (t in ind_missing_m3) {
  #     
  #     
  #     #Start and end index for moving window around time step
  #     #Use a moving window of +/- 5 days and one hour
  #     #(need to remove one time step at each end to match Fluxnet method)
  #     start_time <- format(time[t] - hrs(24*5+1), format="%m-%d-%H:%M:%S")
  #     end_time   <- format(time[t] + hrs(24*5+1), format="%m-%d-%H:%M:%S")
  #     
  #     
  #     #Current year (do min and max to account for time periods outside data range)
  #     #    start_time_cur <- max(1, which(time == start_time))
  #     #   end_time_cur   <- min(length(time), which(time == end_time))
  #     
  #     #Find same time stamps for other years
  #     all_start_times <- which(date_mdh == start_time)
  #     all_end_times   <- which(date_mdh == end_time)
  #     
  #     
  #     #If didn't find other years, skip
  #     if (any(c(length(all_start_times), length(all_end_times)) == 0)) {
  #       next
  #     }
  #     
  #     
  #     #Adjust for first/last year
  #     if (all_start_times[1] > all_end_times[1]) { #missing first start time
  #       all_start_times <- append(1, all_start_times)
  #     }
  #     if (tail(all_start_times, 1) > tail(all_end_times, 1)) { #missing last end time
  #       all_end_times <- append(all_end_times, length(date_mdh))
  #     }
  #     
  #     
  #     #Combine current year's moving window and same time window 
  #     time_indices <- unlist(sapply(1:length(all_start_times), function(x) all_start_times[x]:all_end_times[x]))
  #     
  #     
  #     #Extract EBCF for this time step moving window
  #     #and calculate average
  #     ebcf_tstep <- mean(ebcf[time_indices], na.rm=TRUE)
  #     
  #     
  #     #Calculate median and save in method 1 data vectors
  #     qle_corrected[t] <- qle_all[t] * ebcf_tstep
  #     qh_corrected[t]  <- qh_all[t] * ebcf_tstep
  #     
  #     #Debugging
  #     if (!is.na(ebcf_tstep)) method[t] <- 3
  #     
  #     
  #     
  #   }
  #   
  #   
  # }
  # 

  
  return(list(qle=qle_corrected, qh=qh_corrected))
  
}


###--- extra functions ---#

#Can be used to add hours to a time vector
hrs <- function(u) {
  x <- u * 3600
  return(x)
}


#To add or subtract a year from time stamp
#Can't understand why I can't find a ready R function for this...
#Using this month function from https://stackoverflow.com/questions/14169620/add-a-month-to-a-date
add_months <- function(date, n) {
  seq(date, by = paste (n, "months"), length = 2)[2]
}



