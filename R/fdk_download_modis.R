#' Download MODIS LAI/FPAR
#'
#' Downloads and smooths MODIS LAI/FPAR values
#' for merging into the final LSM model data.
#' Smoothing interpolates values to an half-hourly
#' time step.
#'
#' @param df data frame with site info
#' @param path path where to store the MODIS data
#'
#' @return raw MODIS data saved to a desired path
#' @export

fdk_download_modis <- function(
    df,
    path
) {

  #----- settings and startup -----

  apply(df, 1, function(x){

    # Exception for US-ORv, wetland site with no MODIS LAI available
    if (df['sitename'] == "US-ORv") return(NULL)

    # extract the range of the data to consider
    # and where required extrapolate to missing
    # years
    start_year <- as.numeric(df['year_start'])
    end_year <- as.numeric(df['year_end'])

    # set products and band names for the
    # download
    product <- "MCD15A2H"
    bands <- c(
      "Lai_500m",
      "LaiStdDev_500m",
      "FparStdDev_500m",
      "Fpar_500m",
      "FparLai_QC"
    )

    #----- data download -----

    # Check if data exists, if not download

    # downloading data
    df_modis <- try(
      MODISTools::mt_subset(
        site_name = as.character(df['sitename']),
        lat = df['lat'],
        lon = df['lon'],
        product = product,
        band = bands,
        start = "2000-01-01",
        end = format(Sys.time(), "%Y-%m-%d"),
        km_lr = 1,
        km_ab = 1,
        internal = TRUE
      )
    )

    if(inherits(df_modis, "try-error") ) {
      warning("MODIS data download failed")
      return(NULL)
    }

    # write data to file
    write.table(
      df_modis,
      file.path(path, paste0(x['sitename'], "_MODIS_data.csv")),
      col.names = TRUE,
      row.names = FALSE,
      quote = FALSE,
      sep = ","
    )

  })
}

#' match MODIS LAI/FPAR to ERA data
#'
#' Downloads and smooths MODIS LAI/FPAR values
#' for merging into the final LSM model data.
#' Smoothing interpolates values to an half-hourly
#' time step.
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
      calendar_date = as.Date(calendar_date)
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

  # Extracting pixels in the centre and immediately around it (*)
  # These correspond to a radius of 500m around site coordinates
  pixel_no <- c(7, 8, 9,
                12, 13, 14,
                17, 18, 19)

  # Use only good quality data
  # Random bit integer format, ask Martin if need to work these out again...
  qc_flags <- c(0, 2, 24 ,26, 32, 34, 56, 58)

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
      fpar = stats::weighted.mean(fpar, w = weights_fpar, na.rm = TRUE)
    )

  #---- expand dates ----

  min_year <- min(as.numeric(format(df_modis_mean$calendar_date, "%Y")))
  max_year <- max(as.numeric(format(df_modis_mean$calendar_date, "%Y")))

  dates <- seq.Date(
    as.Date(sprintf("%s-01-01", min_year)),
    as.Date(sprintf("%s-12-31", max_year)),
    by = "day"
  )

  dates <- data.frame(
    calendar_date = dates,
    idx = 1:length(dates)
  )

  df_modis_mean <- dplyr::left_join(
    dates,
    df_modis_mean,
    by = "calendar_date"
  )

  #---- smoothing / gapfilling ----

  # Smooth gapfill and include LAI
  # into the netcdf file

  fdk_smooth_ts(
    dates = df_modis_mean$calendar_date,
    values = df_modis_mean$lai,
    variable = "LAI",
    start_year = min_year,
    end_year = max_year,
    nc_file = nc_file
  )

  fdk_smooth_ts(
    dates = df_modis_mean$calendar_date,
    values = df_modis_mean$fpar,
    variable = "FPAR",
    start_year = min_year,
    end_year = max_year,
    nc_file = nc_file
  )
}
