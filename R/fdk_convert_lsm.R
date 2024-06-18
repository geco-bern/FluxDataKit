#' Converts LSM NetCDF data to FLUXNET
#'
#' Reads LSM data (as a netcdf) in a given directory
#' by (fluxnet) site name, optionally only returns the meta-data of the
#' site data.
#'
#' Default parameters return a CSV file which is compatible with the current
#' OneFlux/FLUXNET naming conventions. Returned data will retain its
#' half-hourly (HH) time step. For aggregation to a daily time step consider
#' the `fdk_downsample_fluxnet()` function.
#'
#' @param site fluxnet site name
#' @param path path with plumber2 data (both flux and meteo data files)
#' @param fluxnet_format convert to fluxnet formatting (TRUE/FALSE)
#' @param meta_data return meta-data TRUE/FALSE
#' @param out_path where to store the converted data if converted to
#'  fluxnet formatting and returns both half-hourly and daily data.
#' @param overwrite overwriting existing files (TRUE/FALSE). Defaults to FALSE.
#'
#' @return data frame with merged meteo and flux data
#' @export

fdk_convert_lsm <- function(
    site,
    path,
    fluxnet_format = FALSE,
    meta_data = FALSE,
    out_path,
    overwrite = FALSE
){

  # CRAN settings
  IGBP_veg_long <- time <- TIMESTAMP_START <-
    TIMESTAMP_END <- P <- TA_F <- PA_F <- CO2_F <- P_F <-
    TA_F_MDS <- CO2_F_MDS <- NULL

  # list all files
  files <- list.files(
    path,
    utils::glob2rx(paste0(site,"*.nc")),
    full.names = TRUE,
    recursive = TRUE
  )

  # check if both files are there
  if (length(files) != 2){
    stop("Missing either flux or meteo data for the requested site")
  }

  # cut back on read times by only reading
  # flux data when extracting meta-data
  if (meta_data) {
    files <- files[grepl(utils::glob2rx("*_Flux.nc"), files)]
  }

  df <- lapply(files, function(file){
    # convert time (needs attribute read)
    nc <- ncdf4::nc_open(file)

    # Get time vector
    time <- ncdf4::ncvar_get(nc, "time")

    # Get time units
    time_units <- strsplit(
      ncdf4::ncatt_get(nc, "time")$units,
      "seconds since ")[[1]][2]

    # # Convert to Y-M-D h-m-s (hack around attribute issues in dates
    # # when using bind_cols() or joins)
    # time_date <- as.character(as.POSIXct(time, origin = time_units, tz="GMT"))
    # time_date <- as.POSIXct(time_date, tz = "GMT")

    # use lubridate to solve join problem
    time_date <- lubridate::ymd_hms(time_units, tz = "GMT") +
      lubridate::seconds(time)

    # Get variable names
    vars <- names(nc$var)

    # Load variable data
    df <- as.data.frame(
      lapply(vars, function(x) ncdf4::ncvar_get(nc, x))
    )

    # close file
    ncdf4::nc_close(nc)

    # Set names
    colnames(df) <- vars

    # add time column
    df$time <- time_date

    # remove trailing / leading white spaces
    # in IGBP classes
    if ("IGBP_veg_short" %in% colnames(df)) {
    df$IGBP_veg_short <- trimws(df$IGBP_veg_short)
    } else {
        warning("Column 'IGBP_veg_short' does not exist in the data frame. Assigning NA.")
        df$IGBP_veg_short <- NA
    }

    # drop long names
    if("IGBP_veg_long" %in% names(df)){
      df <- subset(df, select = -IGBP_veg_long)
    }

    # subset and constrain data
    if (meta_data) {

      meta_columns <- c(
        "latitude",
        "longitude",
        "reference_height",
        "canopy_height",
        "elevation",
        "IGBP_veg_short",
        "year_start",
        "year_end"
        )

      missing_columns <- setdiff(meta_columns, colnames(df))
      if(length(missing_columns) > 0) {
          for(col in missing_columns) {
              warning(paste("Column", col, "does not exist in the data frame. Assigning NA."))
              df[,col] <- NA
          }
      }

      df$year_start <- format(min(df$time),"%Y")
      df$year_end <- format(max(df$time),"%Y")
      df <- df[1,meta_columns]
      df$sitename <- site
    }

    # return data frame
    return(df)
  })

  # only return meta-data if requested
  # don't merge with meteo data
  if (meta_data) {
    # return only the meta-data
    return(df)
  } else {
    # combine meteo and flux data
    all <- suppressMessages(
      dplyr::left_join(df[[1]], df[[2]], by = "time")
    )
  }

  # format data as fluxnet compatible
  if (fluxnet_format) {
    # convert time, and only return
    # FLUXNET formatted columns

    start_year <- format(min(all$time), "%Y")
    end_year <- format(max(all$time), "%Y")

    all <- all |>
      dplyr::mutate(
        TIMESTAMP_START = format(time, "%Y%m%d%H%M"),
        TIMESTAMP_END = format(time + 30 * 60, "%Y%m%d%H%M")
      )

    old <- c(
      # MICROMET
      P_F = "Precip", # in mm -s
      TA_F_MDS = "Tair",
      SW_IN_F_MDS = "SWdown",
      LW_IN_F_MDS = "LWdown",
      VPD_F_MDS = "VPD",
      WS_F = "Wind",
      PA_F = "Psurf",
      CO2_F_MDS = "CO2air",

      # FLUXES
      NETRAD = "Rnet",
      USTAR = "Ustar",
      SW_OUT = "SWup",
      LE_F_MDS = "Qle",
      LE_CORR = "Qle_cor",
      H_F_MDS = "Qh",
      H_CORR = "Qh_cor",
      LE_CORR_JOINTUNC = 'Qle_cor_uc',
      H_CORR_JOINTUNC = 'Qh_cor_uc',
      NEE_VUT_REF = "NEE",
      NEE_VUT_REF_JOINTUNC = "NEE_uc",
      GPP_NT_VUT_REF = "GPP",
      GPP_NT_VUT_SE = "GPP_se",
      GPP_DT_VUT_REF = "GPP_DT",
      GPP_DT_VUT_SE = "GPP_DT_se",
      RECO_NT_VUT_REF = "Resp",
      RECO_NT_VUT_SE = "Resp_se",

      # QC flags to propagate
      # to daily data
      NETRAD_QC = "Rnet_qc",
      USTAR_QC = "Ustar_qc",
      SW_OUT_QC = "SWup_qc",
      LE_F_MDS_QC = "Qle_qc",
      LE_CORR_QC = "Qle_cor_qc",
      H_F_MDS_QC = "Qh_qc",
      H_CORR_QC = "Qh_cor_qc",
      LE_CORR_JOINTUNC_QC = 'Qle_cor_uc_qc',
      H_CORR_JOINTUNC_QC = 'Qh_cor_uc_qc',
      NEE_VUT_REF_QC = "NEE_qc",
      NEE_VUT_REF_JOINTUNC_QC = "NEE_uc_qc",
      RECO_NT_VUT_REF_QC = "Resp_qc",
      RECO_NT_VUT_SE_QC = "Resp_se_qc",

      P_F_QC = "Precip_qc", # in mm -s
      TA_F_MDS_QC = "Tair_qc",
      SW_IN_F_MDS_QC = "SWdown_qc",
      LW_IN_F_MDS_QC = "LWdown_qc",
      VPD_F_MDS_QC = "VPD_qc",
      WS_F_QC = "Wind_qc",
      PA_F_QC = "Psurf_qc",
      CO2_F_MDS_QC = "CO2air_qc",

      # MODIS data
      LAI = "LAI",
      FPAR = "FPAR"
    )

    # convert to data frame
    keys <- data.frame(old)
    keys$new <- names(old)

    # columns to select
    keys <- keys[which(old %in% colnames(all)),]

    # rename all columns using the keys in the
    # data frame
    all <- all |>
      dplyr::rename_with(~ keys$new, all_of(keys$old))

    # subset columns
    all <- all[c("TIMESTAMP_START","TIMESTAMP_END",keys$new)]

    # Unit conversions from ALMA back to FLUXNET
    # Inverse of what is mentioned in the FluxnetLSM Conversion.R script
    # https://github.com/aukkola/FluxnetLSM/blob/a256ffc894ed8182f9399afa1d83dea43ac36a95/R/Conversions.R
    all <- all |>
      dplyr::mutate(
        P_F = P_F * 60 * 30, # mm/s to mm
        TA_F_MDS = TA_F_MDS - 273.15, # K to C
        PA_F = PA_F / 1000, # Pa to kPa
        CO2_F_MDS = CO2_F_MDS, # ppm to umolCO2 mol-1
        )

    # # adding missing data required by ingestr
    # # for conversion to p-model drivers
    # replacements <- data.frame(
    #   variable = c(
    #     'VPD_F_QC',
    #     'VPD_F_MDS_QC',
    #     'VPD_ERA',
    #     'TA_F_QC',
    #     'TA_F_MDS_QC',
    #     'TA_ERA',
    #     'TMIN_F_QC',
    #     'TMIN_F_MDS',
    #     'TMIN_F_MDS_QC',
    #     'TMIN_ERA',
    #     'TMAX_F_QC',
    #     'TMAX_F_MDS',
    #     'TMAX_F_MDS_QC',
    #     'TMAX_ERA',
    #     'NEE_VUT_REF_QC',
    #     'GPP_VUT_REF_QC'
    #   ),
    #   value = c(
    #     0,
    #     NA,
    #     NA,
    #     0,
    #     NA,
    #     NA,
    #     0,
    #     NA,
    #     NA,
    #     NA,
    #     0,
    #     NA,
    #     NA,
    #     NA,
    #     1,
    #     1
    #   )
    # )
    #
    # # loop over all rows (variables)
    # for (i in seq_len(nrow(replacements))){
    #   try(all <- tibble::add_column(all,
    #         !!(replacements[i, 'variable']) := replacements[i, 'value']
    #         ),
    #       silent = TRUE
    #   )
    #
    #   all <- all |>
    #     dplyr::select(
    #       -ends_with(".1")
    #     )
    # }

  }

  # save data to file, using FLUXNET formatting
  if (fluxnet_format && !missing(out_path)) {

    filename <- sprintf("FLX_%s_PLUMBER_FULLSET_HH_%s_%s_2-3.csv",
                        site,
                        start_year,
                        end_year
    )
    filename <- file.path(
      out_path,
      filename
    )

    if(file.exists(filename) & !overwrite){
      return(invisible())
    }

    message("---> writing data to file:")
    message(sprintf("   %s", filename))

    utils::write.table(
        all,
        file = filename,
        quote = FALSE,
        col.names = TRUE,
        row.names = FALSE,
        sep = ","
      )

    # write downsampled data as well
    suppressWarnings(
      try(
        fdk_downsample_fluxnet(
          all,
          site = site,
          out_path = out_path,
          overwrite = overwrite
        )
      )
    )

  }
  return(all)
}

