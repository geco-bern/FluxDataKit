#All functions are from FluxnetLSM but reproduced here to allow post-processing
#of PLUMBER2 inputs

#' Synthesises downward longwave radiation based on Tair and rel humidity
SynthesizeLWdown <- function(TairK, RH, technique='Abramowitz_2012') {
  
  #Three techniques available, see Abramowitz et al. (2012),
  #Geophysical Research Letters, 39, L04808 for details
  
  zeroC <- 273.15
  
  #Inputs missing, set lwdown missing
  if (all(is.na(TairK)) | all(is.na(RH))) {
    lwdown <- rep(NA, length(TairK))
    
    #Else synthesise value
  } else {
    
    if (technique=='Swinbank_1963') {
      # Synthesise LW down from air temperature only:
      lwdown <- 0.0000094*0.0000000567*TairK^6
      
    } else if(technique=='Brutsaert_1975') {
      satvapres <- 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
      vapres    <- pmax(5,RH)/100*satvapres
      emiss     <- 0.642*(vapres/TairK)^(1/7)
      lwdown    <- emiss*0.0000000567*TairK^4
      
    } else if(technique=='Abramowitz_2012') {
      satvapres <- 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
      vapres    <- pmax(5,RH)/100*satvapres
      lwdown    <- 2.648*TairK + 0.0346*vapres - 474
      
    } else {
      CheckError('S4: Unknown requested LWdown synthesis technique.')
    }
  }
  
  
  return(lwdown)
}

#-----------------------------------------------------------------------------
#' Converts VPD (hPa) to relative humidity (percentage)
VPD2RelHum <- function(VPD, airtemp, vpd_units, tair_units){
  
  
  #Check that VPD in Pascals
  if(vpd_units != "hPa"){
    error <- paste("Cannot convert VPD to relative humidity. VPD units not recognised,",
                   "expecting VPD in hectopascals")
    stop(error)
  }
  
  #Check that temperature in Celcius. Convert if not
  if(tair_units=="K"){
    airtemp <- airtemp - 273.15
  }
  
  #Hectopascal to Pascal
  hPa_2_Pa <- 100
  
  #Saturation vapour pressure (Pa).
  esat <- calc_esat(airtemp) 
  
  #Relative humidity (%)
  RelHum <- 100 * (1 - ((VPD * hPa_2_Pa) / esat))
  
  #Make sure RH is within [0,100]
  RelHum[RelHum < 0]   <- 0.01
  RelHum[RelHum > 100] <- 100
  
  return(RelHum)
}

#-------------------

#' Calculates saturation vapour pressure
calc_esat <- function(airtemp){
  #Tair in degrees C
  
  #From Jones (1992), Plants and microclimate: A quantitative approach 
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * airtemp / (240.97+airtemp))
  
  return(esat)
}

#-----------------------------------------------------------------------------

linear_pred_co2 <- function(co2, start_ind, end_ind){
  
  #All time steps
  x_all <- 1:length(co2)
  
  co2 <- co2[start_ind:end_ind]
  
  #Linear model
  x  <- 1:length(co2)
  lm <- lm(co2 ~ x)
  
  #Linear prediction
  co2_pred <- lm$coefficients[2] * x_all + lm$coefficients[1]
  
  return(co2_pred)
  
}

#-----------------------------------------------------------------------------
#Converts Qair to relative humidity (%) (Rel2SpecHumidity function from FluxnetLSM
#but final equation re-ordered to get RH from Qair)
SpecHumidity2Rel <- function(specHum, airtemp, tair_units, 
                             pressure, psurf_units){
  
  # required units: airtemp - temp in C; pressure in Pa; relHum as %
  
  #Check that temperature in Celcius. Convert if not
  if(tair_units=="K"){
    airtemp <- airtemp - 273.15
    
  } else if(tair_units != "C"){
    error <- paste("Unknown air temperature units, cannot convert", 
                   "relative to specific humidity. Accepts air temperature in K or C")
    stop(error)
  }
  
  #Check that PSurf is in Pa. Convert if not
  if(psurf_units=="kPa"){
    pressure <- pressure * 1000
    
  } else if(psurf_units != "Pa"){
    error <- paste("Unknown air pressure units, cannot convert", 
                   "relative to specific humidity. Accepts air pressure",
                   "in kPa or Pa")
    stop(error)
  }
  
  
  # Sat vapour pressure in Pa (reference as above)
  esat <- calc_esat(airtemp)
  
  # Then specific humidity at saturation:
  ws <- 0.622*esat / (pressure - esat)
  
  # Re-ordering this equation to get RH:
  #specHum <- (relHum/100) * ws
  
  relHum <- 100 * (specHum / ws)
  
  return(relHum)
  
}

