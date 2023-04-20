# List of small assorted functions included in the
# PLUMBER-2 workflow

# Can be used to add hours to a time vector
hrs <- function(u) {
  x <- u * 3600
  return(x)
}

# To add or subtract a year from time stamp
# Can't understand why I can't find a ready R function for this...
# Using this month function from
# https://stackoverflow.com/questions/14169620/add-a-month-to-a-date

add_months <- function(date, n) {
  seq(date, by = paste (n, "months"), length = 2)[2]
}

#' Calculates saturation vapour pressure
calc_esat <- function(airtemp){
  #Tair in degrees C

  #From Jones (1992), Plants and microclimate: A quantitative approach
  #to environmental plant physiology, p110
  esat <- 613.75 * exp(17.502 * airtemp / (240.97+airtemp))

  return(esat)
}

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
