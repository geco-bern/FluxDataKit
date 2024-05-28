
#' match MODIS LAI/FPAR to ERA data
#'
#' Downloads and smooths MODIS LAI/FPAR values
#' for merging into the final LSM model data.
#' Smoothing interpolates values to a daily time step
#' which is then assigned to half-hourly data on a
#' day-by-day basis (i.e. remote sensing data remains
#' static from day-to-day but changes daily across the season).
#'
#' Here the code deviates from the standard PLUMBER-2 workflow
#' and provides a more parsimonious solution.
#'
#' @param df data frame with site info
#' @param path path where to store the MODIS data
#' @param nc_file netcdf file containing the LSM processed flux data
#'
#' @return smoothed time series of LAI/FPAR integrated in the meteorological
#' (ERA) data
#' @export

fdk_match_modis <- function(
    df,
    path,
    nc_file
) {

  #----- settings and startup -----

  # Exception for US-ORv, wetland site with no MODIS LAI available
  if (df['sitename'] == "US-ORv") return(NULL)

  df_modis <- try(read.table(
    file.path(path, paste0(df["sitename"],"_MODIS_data.csv")),
    sep = ",",
    header = TRUE
  )
  )

  if (inherits(df_modis, "try-error")) {
    warning("MODIS data not found, please download data for this site.")
    return(NULL)
  }

  #----- QC screening -----

  # apply scaling factors, ignore if not available
  df_modis <- df_modis |>
    dplyr::mutate(
      scale = ifelse(scale == "Not Available", NA, scale),
      value = ifelse(!is.na(scale),
                     value * as.numeric(scale),
                     value),
      calendar_date = as.Date(calendar_date, "%Y-%m-%d")
    )

  df_modis <- df_modis |>
    tidyr::pivot_wider(
      id_cols = c(site, calendar_date, pixel),
      names_from = band,
      values_from = value
    )

  df_modis <- df_modis |>
    dplyr::rename(
      'lai' = 'Lai_500m',
      'sd_lai' = 'LaiStdDev_500m',
      'sd_fpar' = 'FparStdDev_500m',
      'fpar' = 'Fpar_500m',
      'qc' = 'FparLai_QC'
    )

  # Extracting pixels in the center and immediately around it (*)
  # These correspond to a radius of 500 m around site coordinates
  pixel_no <- c(7, 8, 9,
                12, 13, 14,
                17, 18, 19)

  # Use only good quality data
  # Random bit integer format, ask Martin if need to work these out again...
  qc_flags <- c(0, 2, 24, 26, 32, 34, 56, 58)

  df_modis <- df_modis |>
    filter(
      qc %in% qc_flags,
      pixel %in% pixel_no
    ) |>
    mutate(
      lai = ifelse(sd_lai < 0.01, NA, lai),
      lai = ifelse(lai > 10, NA, lai),
      fpar = ifelse(sd_fpar < 0.01, NA, fpar),
      fpar = ifelse(fpar > 1, NA, fpar)
    ) |>
    na.omit()

  #---- Apply weighted mean ----

  df_modis_mean <- df_modis |>
    group_by(site, calendar_date) |>
    mutate(
      weights_lai = (1/sd_lai^2)/sum(1/sd_lai^2),
      weights_fpar = (1/sd_fpar^2)/sum(1/sd_fpar^2),
    ) |>
    dplyr::summarize(
      lai = stats::weighted.mean(lai, w = weights_lai, na.rm = TRUE),
      fpar = stats::weighted.mean(fpar, w = weights_fpar, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(
      "date" = "calendar_date"
    )

  #---- expand dates ----

  # Use all available data, if required
  # expand to match the data range of the
  # flux data

  start_date <- as.Date(paste0(df['year_start'],"-01-01"))
  end_date <-as.Date(paste0(df['year_end'],"-12-31"))

  if (start_date > min(df_modis$calendar_date, na.rm = TRUE)){
    start_date <- min(df_modis$calendar_date, na.rm = TRUE)
  }

  if (end_date < min(df_modis$calendar_date, na.rm = TRUE)){
    end_date <- min(df_modis$calendar_date, na.rm = TRUE)
  }

  dates <- seq.Date(
    start_date - 100,
    end_date + 100,
    by = "day"
  )

  dates <- data.frame(
    date = dates,
    doy = as.numeric(format(dates, "%j"))
  )

  # only retain valid dates
  dates <- dates |>
    filter(
      doy %in% seq(1, 365, 8)
    )

  df_modis_mean <- dplyr::left_join(
    dates,
    df_modis_mean,
    by = "date"
  )

  #---- smoothing / gapfilling ----

  # Smooth gapfill and include LAI
  # into the netcdf file

  # Add FAPAR, also to plumber data
  # as not available in the standard
  # dataset
  fdk_smooth_ts(
    df = df_modis_mean,
    variable = "FPAR",
    nc_file = nc_file
  )

  # if the product is not plumber based
  # also add LAI
  fdk_smooth_ts(
      df = df_modis_mean,
      variable = "LAI",
      nc_file = nc_file
    )
}
