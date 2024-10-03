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
#' @param limit_missing Fraction of missing data maximally to be allowed.
#'
#' @return a gap filled data frame
#' @export

fill_netrad <- function(df, limit_missing = 0.4){

    if (sum(is.na(df$netrad)) > 0.0 & sum(is.na(df$netrad))/nrow(df) < limit_missing){

      message("Imputing net radiation with KNN ....")
      message(
        paste0("Missing net radiation data fraction: ",
               sum(is.na(df$netrad))/nrow(df)
        )
      )

      # impute missing with KNN
      pp <- recipes::recipe(
        netrad ~ temp + ppfd,
        data = df |> tidyr::drop_na(temp, ppfd)
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
        training = df |> tidyr::drop_na(netrad, temp, ppfd)
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


#' Impute with KNN
#'
#' Impute missing values with a K-nearest neighbour
#'
#' @param df a data frame containing columns corresponding to the target and
#' predictor variable names
#' @param target a string specifying the target variable (column) name
#' @param pred1 a string specifying the first predictor variable (column) name
#' @param pred2 a string specifying the second predictor variable (column) name
#' @param pred3 a string specifying the third predictor variable (column) name
#' @param k an integer specifying the number of neighbours to consider
#'
#' @return a gap filled data frame
#' @export

fdk_impute_knn <- function(df, target, pred1, pred2 = NULL, pred3 = NULL, k){

  if (is.null(pred2) && is.null(pred3)){
    forml <- as.formula(paste0(target, " ~ ", pred1))
  } else if (is.null(pred3)){
    forml <- as.formula(paste0(target, " ~ ", pred1, "+", pred2))
  } else {
    forml <- as.formula(paste0(target, " ~ ", pred1, "+", pred2, "+", pred3))
  }

  # impute missing with KNN
  pp <- recipes::recipe(
    forml,
    data = df |>
      tidyr::drop_na(target, pred1, pred2, pred3)
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
      neighbors = k
    )

  pp_prep <- recipes::prep(
    pp,
    training = df |>
      tidyr::drop_na(target, pred1, pred2, pred3)
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
          target_filled = !!target)
    )

  df[[target]] <- ifelse(is.na(df[[target]]), df$target_filled, df[[target]])
  df$target_filled <- NULL

  return(df)
}

#' Interpolate missing FPAR and LAI data
#'
#' Interpolate missing FPAR and LAI data
#'
#' @param df a data frame containing missing values for FPAR and LAI
#'
#' @return a gap filled data frame
#' @export

interpolate2daily_fpar <- function(df){

  df <- df |>
    dplyr::mutate(
      FPAR = ifelse(is.nan(FPAR), NA, FPAR),
      LAI = ifelse(is.nan(LAI), NA, LAI)
    )
  frac_missing <- sum(is.na(df$FPAR))/nrow(df)

  if (frac_missing < 0.25){

    df <- df |>
      dplyr::mutate(
        FPAR = zoo::na.approx(FPAR, na.rm = FALSE, maxgap = 30),
        LAI = zoo::na.approx(LAI, na.rm = FALSE, maxgap = 30)
      )

    # fill remaining with mean seasonal cycle
    meandf <- df |>
      dplyr::mutate(doy = lubridate::yday(TIMESTAMP)) |>
      dplyr::group_by(doy) |>
      dplyr::summarise(FPAR_meandoy = mean(FPAR, na.rm = TRUE))

    df <- df |>
      dplyr::mutate(doy = lubridate::yday(TIMESTAMP)) |>
      dplyr::left_join(
        meandf,
        by = "doy"
      ) |>
      dplyr::mutate(FPAR = ifelse(is.na(FPAR), FPAR_meandoy, FPAR)) |>
      dplyr::select(-FPAR_meandoy, -doy)

  } else {

    message("Fraction of missing FPAR data too large (>0.25). Not interpolating.")

  }

  return(df)
}

#' Interpolate missing CO2 data
#'
#' Interpolate missing CO2 data
#'
#' @param df a data frame containing missing values for CO2_F_MDS
#'
#' @return a gap filled data frame
#' @export

interpolate2daily_CO2_F_MDS <- function(df){

  df <- df |>
    dplyr::mutate(
      CO2_F_MDS = ifelse(is.nan(CO2_F_MDS), NA, CO2_F_MDS)
    )
  frac_missing <- sum(is.na(df$CO2_F_MDS))/nrow(df)

  if (frac_missing < 0.25){

    df <- df |>
      dplyr::mutate(
        CO2_F_MDS = zoo::na.approx(CO2_F_MDS, na.rm = FALSE, maxgap = 30)
      )

    # still missing?
    frac_missing <- sum(is.na(df$CO2_F_MDS))/nrow(df)
    if (frac_missing > 0){

      # pad then interpolate
      len <- nrow(df)
      df <- dplyr::bind_rows(
        dplyr::slice(df, 1:365),
        df,
        dplyr::slice(df, 1:365)
      ) |>
        dplyr::mutate(
          CO2_F_MDS = zoo::na.approx(CO2_F_MDS, na.rm = FALSE, maxgap = 30)
        ) |>
        dplyr::slice(366:(365+len))

      # still missing?
      frac_missing <- sum(is.na(df$CO2_F_MDS))/nrow(df)
      if (frac_missing > 0){
        # fill by mean
        df$CO2_F_MDS[which(is.na(df$CO2_F_MDS))] <- mean(df$CO2_F_MDS, na.rm = TRUE)
      }

    }

  } else {

    message("Fraction of missing CO2_F_MDS data too large (>0.25). Not interpolating, but taking from Mauna Loa.")

    df_co2 <- readr::read_csv(
      url("https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_mlo.csv"),
      comment = "#") |>
      dplyr::select(year, mean) |>
      dplyr::rename(co2_maunaloa = mean)

    df <- df |>
      mutate(year = lubridate::year(TIMESTAMP)) |>
      left_join(
        df_co2,
        by = "year"
      ) |>
      dplyr::mutate(CO2_F_MDS = ifelse(is.na(CO2_F_MDS), co2_maunaloa, CO2_F_MDS)) |>
      dplyr::select(-year, -co2_maunaloa)

  }

  return(df)
}

#' Interpolate missing atmospheric pressure data
#'
#' Interpolate missing atmospheric pressure data
#'
#' @param df a data frame containing missing values for CO2_F_MDS
#'
#' @return a gap filled data frame
#' @export

interpolate2daily_PA_F <- function(df){

  df <- df |>
    dplyr::mutate(
      PA_F = ifelse(is.nan(PA_F), NA, PA_F)
    )
  frac_missing <- sum(is.na(df$PA_F))/nrow(df)

  if (frac_missing < 0.25){

    df <- df |>
      dplyr::mutate(
        PA_F = zoo::na.approx(PA_F, na.rm = FALSE, maxgap = 30)
      )

    # still missing?
    frac_missing <- sum(is.na(df$PA_F))/nrow(df)
    if (frac_missing > 0){

      # pad then interpolate
      len <- nrow(df)
      df <- dplyr::bind_rows(
        dplyr::slice(df, 1:365),
        df,
        dplyr::slice(df, 1:365)
      ) |>
        dplyr::mutate(
          PA_F = zoo::na.approx(PA_F, na.rm = FALSE, maxgap = 30)
        ) |>
        dplyr::slice(366:(365+len))

      # still missing?
      frac_missing <- sum(is.na(df$PA_F))/nrow(df)
      if (frac_missing > 0){
        # fill by mean
        df$PA_F[which(is.na(df$PA_F))] <- mean(df$PA_F, na.rm = TRUE)
      }

    }

  } else {

    message("Fraction of missing PA_F data too large (>0.25).")


  }

  return(df)
}



#' Interpolate missing wind speed data
#'
#' Interpolate missing wind speed data
#'
#' @param df a data frame containing missing values for FPAR and LAI
#'
#' @return a gap filled data frame
#' @export

interpolate2daily_WS_F <- function(df){

  df <- df |>
    dplyr::mutate(
      WS_F = ifelse(is.nan(WS_F), NA, WS_F)
    )
  frac_missing <- sum(is.na(df$WS_F))/nrow(df)

  if (frac_missing < 0.25){

    df <- df |>
      dplyr::mutate(
        WS_F = zoo::na.approx(WS_F, na.rm = FALSE, maxgap = 30)
      )

    # fill remaining with mean seasonal cycle
    meandf <- df |>
      dplyr::mutate(doy = lubridate::yday(TIMESTAMP)) |>
      dplyr::group_by(doy) |>
      dplyr::summarise(WS_F_meandoy = mean(WS_F, na.rm = TRUE))

    df <- df |>
      dplyr::mutate(doy = lubridate::yday(TIMESTAMP)) |>
      dplyr::left_join(
        meandf,
        by = "doy"
      ) |>
      dplyr::mutate(WS_F = ifelse(is.na(WS_F), WS_F_meandoy, WS_F)) |>
      dplyr::select(-WS_F_meandoy, -doy)

    # still missing?
    frac_missing <- sum(is.na(df$WS_F))/nrow(df)
    if (frac_missing > 0){

      # pad then interpolate
      len <- nrow(df)
      df <- dplyr::bind_rows(
        dplyr::slice(meandf, 1:365),
        df,
        dplyr::slice(meandf, 1:365)
      ) |>
        dplyr::mutate(
          WS_F = zoo::na.approx(WS_F, na.rm = FALSE, maxgap = 30)
        ) |>
        dplyr::slice(366:(365+len))

      # still missing?
      frac_missing <- sum(is.na(df$WS_F))/nrow(df)
      if (frac_missing > 0){
        # fill by mean
        df$WS_F[which(is.na(df$WS_F))] <- mean(df$WS_F, na.rm = TRUE)
      }

    }

  } else {

    message("Fraction of missing WS_F data too large (>0.25). Not interpolating.")

  }

  return(df)
}


#' Interpolate missing wind speed data
#'
#' Interpolate missing air temperature
#'
#' @param df a data frame containing missing values for TA_F_MDS
#'
#' @return a gap filled data frame
#' @export

interpolate2daily_TA_F_MDS <- function(df){

  # determine fraction of missing values
  df <- df |>
    dplyr::mutate(
      TA_F_MDS = ifelse(is.nan(TA_F_MDS), NA, TA_F_MDS)
    )
  frac_missing <- sum(is.na(df$TA_F_MDS))/nrow(df)

  # if less than a quarter is missing, fill it, with maximum gaps to be filled:
  # 30 days
  if (frac_missing < 0.25){

    df <- df |>
      dplyr::mutate(
        TA_F_MDS = zoo::na.approx(TA_F_MDS, na.rm = FALSE, maxgap = 30)
      )

    # fill remaining with mean seasonal cycle
    meandf <- df |>
      dplyr::mutate(doy = lubridate::yday(TIMESTAMP)) |>
      dplyr::group_by(doy) |>
      dplyr::summarise(TA_F_MDS_meandoy = mean(TA_F_MDS, na.rm = TRUE))

    df <- df |>
      dplyr::mutate(doy = lubridate::yday(TIMESTAMP)) |>
      dplyr::left_join(
        meandf,
        by = "doy"
      ) |>
      dplyr::mutate(TA_F_MDS = ifelse(is.na(TA_F_MDS), TA_F_MDS_meandoy, TA_F_MDS)) |>
      dplyr::select(-TA_F_MDS_meandoy, -doy)

    # still missing?
    frac_missing <- sum(is.na(df$TA_F_MDS))/nrow(df)
    if (frac_missing > 0){

      # pad then interpolate
      len <- nrow(df)
      df <- dplyr::bind_rows(
        dplyr::slice(meandf, 1:365),
        df,
        dplyr::slice(meandf, 1:365)
      ) |>
        dplyr::mutate(
          TA_F_MDS = zoo::na.approx(TA_F_MDS, na.rm = FALSE, maxgap = 30)
        ) |>
        dplyr::slice(366:(365+len))

      # still missing?
      frac_missing <- sum(is.na(df$TA_F_MDS))/nrow(df)
      if (frac_missing > 0){
        # fill by mean
        df$TA_F_MDS[which(is.na(df$TA_F_MDS))] <- mean(df$TA_F_MDS, na.rm = TRUE)
      }

    }

  } else {

    message("Fraction of missing TA_F_MDS data too large (>0.25). Not interpolating.")

  }

  return(df)
}

