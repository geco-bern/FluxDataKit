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


#' Gap fill net radiation measurements
#'
#' Gap fill net radiation measurements using a
#' KNN approach, using temperature and ppfd
#'
#' @param df a data frame containing a netrad data column, and temperature
#' and light variables
#'
#' @return a gap filled data frame
#' @export

fill_netrad <- function(df){

    if (sum(is.na(df$netrad)) > 0.0 & sum(is.na(df$netrad))/nrow(df) < 0.4){

      message("Imputing net radiation with KNN ....")
      message(
        paste0("Missing net radiation data fraction: ",
               sum(is.na(df$netrad))/nrow(df)
        )
      )

      # impute missing with KNN
      pp <- recipes::recipe(
        netrad ~ temp + ppfd,
        data = df |> drop_na(temp, ppfd)
      ) |>
        recipes::step_center(
          recipes::all_numeric(),
          -recipes::all_outcomes()
        ) |>
        recipes::step_scale(
          recipes::all_numeric(),
          -recipes::all_outcomes()
        ) |>
        recipes::step_impute_knn(
          recipes::all_outcomes(),
          neighbors = 5
        )

      pp_prep <- recipes::prep(
        pp,
        training = df |> drop_na(netrad, temp, ppfd)
      )

      df_baked <- recipes::bake(
        pp_prep,
        new_data = df
      )

      # fill missing with gap-filled
      df <- df |>
        dplyr::bind_cols(
          df_baked |>
            dplyr::select(
              netrad_filled = netrad)
        ) |>
        dplyr::mutate(
          netrad = ifelse(is.na(netrad), netrad_filled, netrad)
          #qc = ifelse(is.na(netrad), TRUE, FALSE)
        ) |>
        dplyr::select(
          -netrad_filled
        )

    } else {
      df$netrad <- NA
    }

  return(df)
}

