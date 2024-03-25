#' Down samples half-hourly FLUXNET data to daily values
#'
#' Down sampling of GPP values is done at-hoc using a simple
#' mean or similar per FLUXNET data specifics.
#'
#' Normally the flux data is generated using a dedicated flux processing
#' workflow. Therefore the data are not 100% identical but correspondence
#' should have an R2 > 0.95, with only auto-correlated errors in the NT product.
#'
#' This function and the data generated should be used with caution, knowing
#' the limitations that this routine implies with respect to all variables
#' included.
#'
#' @param df a half-hourly FLUXNET data frame
#' @param site sitename of a site to process
#' @param out_path the path where to store the converted data
#' @param overwrite overwrite existing output file
#'
#' @return data frame with daily (DD) down sampled values or file in the
#'  FLUXNET format
#' @export

fdk_downsample_fluxnet <- function(
    df,
    site,
    out_path,
    overwrite = FALSE
){

  # Using the FLUXNET instructions, however in some cases there will
  # be no equivalence giving missing information. Downsampled data should
  # therefore not be considered equal to the original FLUXNET/ONEFLUX
  # processing chain.
  # https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/

  df <- df |>
    dplyr::mutate(
      TIMESTAMP = as.Date(TIMESTAMP_START, "%Y%m%d%H%M")
      )

  start_year <- format(min(df$TIMESTAMP), "%Y")
  end_year <- format(max(df$TIMESTAMP), "%Y")

  filename <- sprintf("FLX_%s_PLUMBER_FULLSET_DD_%s_%s_2-3.csv",
                      site,
                      start_year,
                      end_year
  )

  if(!missing(out_path)){
    filename <- file.path(
      out_path,
      filename
    )
  }

  if(file.exists(filename) & !overwrite){
    return(invisible())
  }

  # check required columns fill with NA
  # if any are missing
  output_columns <- data.frame(
    # met
    "P_F" = NA,
    "TA_F_MDS" = NA,
    "SW_IN_F_MDS" = NA,
    "LW_IN_F_MDS" = NA,
    "VPD_F_MDS" = NA,
    "WS_F" = NA,
    "PA_F" = NA,
    "CO2_F_MDS" = NA,

    # radiation
    "NETRAD" = NA,
    "USTAR" = NA,
    "LE_F_MDS" = NA,
    "LE_CORR" = NA,
    "H_F_MDS" = NA,
    "H_CORR" = NA,
    "LE_CORR_JOINTUNC" = NA,
    "H_CORR_JOINTUNC" = NA,
    "SW_OUT" = NA,

    # fluxes
    "NEE_VUT_REF" = NA,
    "NEE_VUT_REF_JOINTUNC" = NA,
    "GPP_NT_VUT_REF" = NA,
    "GPP_NT_VUT_SE" = NA,
    "GPP_DT_VUT_REF" = NA,
    "GPP_DT_VUT_SE" = NA,
    "RECO_NT_VUT_REF" = NA,
    "RECO_NT_VUT_SE" = NA,

    # qc
    "NETRAD_QC" = NA,
    "USTAR_QC" = NA,
    "LE_F_MDS_QC" = NA,
    "H_F_MDS_QC" = NA,
    "LE_CORR_QC" = NA,
    "H_CORR_QC" = NA,
    "NEE_VUT_REF_QC" = NA,
    "NEE_VUT_REF_JOINTUNC_QC" = NA,
    "P_F_QC" = NA,
    "TA_F_MDS_QC" = NA,
    "SW_IN_F_MDS_QC" = NA,
    "LW_IN_F_MDS_QC" = NA,
    "VPD_F_MDS_QC" = NA,
    "WS_F_QC" = NA,
    "PA_F_QC" = NA,
    "CO2_F_MDS_QC" = NA,

    # remote sensing
    "LAI" = NA,
    "FPAR" = NA
  )

  # detect missing columns to fill in required data
  missing_columns <- output_columns |>
    dplyr::select(which(!(colnames(output_columns) %in% colnames(df))))

  # if the columns are missing bind them to the current data frame
  if (ncol(missing_columns) > 0){
      df <- dplyr::bind_cols(df, missing_columns)
  }

  # determine daytime threshold based on 1% quantile of solar radiation
  daytime_thresh <- stats::quantile(df$SW_IN_F_MDS, probs = 0.1)

  # get daytime averages separately
  df_day <- df |>
    dplyr::mutate(DAY = ifelse(SW_IN_F_MDS > daytime_thresh, TRUE, FALSE)) |>
    dplyr::filter(DAY) |>
    dplyr::group_by(TIMESTAMP) |>
    dplyr::summarize(

      # daytime temperature
      TA_DAY_F_MDS = mean(TA_F_MDS, na.rm = TRUE),

      # VPD as the mean of the daytime values
      VPD_DAY_F_MDS = mean(VPD_F_MDS, na.rm = TRUE),

    )

  # downsample the data to a daily time step
  # using FLUXNET naming conventions
  # Aggregate QC flags as the daily fraction of measured or good-quality
  # gap-filled half-hourly values.
  # _F_MDS_QC = 0 (measured);
  # _F_MDS_QC = 1 (filled with high confidence);
  # _F_MDS_QC = 2 (filled with medium confidence);
  # _F_MDS_QC = 3 (filled with low confidence)
  df <- df |>
    dplyr::group_by(TIMESTAMP) |>
    dplyr::summarize(

      # METEO
      # add fraction of daily "missing values"

      # precipitation is the sum of HH values
      P_F = sum(P_F, na.rm = FALSE),
      P_F_QC = mean(P_F_QC < 2, na.rm = FALSE),

      # temperature is the mean of the HH values
      TA_F_MDS = mean(TA_F_MDS, na.rm = FALSE),
      TA_F_MDS_QC = mean(TA_F_MDS_QC < 2, na.rm = FALSE),

      TMIN_F_MDS = min(TA_F_MDS, na.rm = FALSE),
      TMAX_F_MDS = max(TA_F_MDS, na.rm = FALSE),

      # shortwave radiation is the mean of the HH values
      SW_IN_F_MDS = mean(SW_IN_F_MDS, na.rm = FALSE),
      SW_IN_F_MDS_QC = mean(SW_IN_F_MDS_QC < 2, na.rm = FALSE),

      # long wave radiation is the mean of the HH values
      LW_IN_F_MDS = mean(LW_IN_F_MDS, na.rm = FALSE),
      LW_IN_F_MDS_QC = mean(LW_IN_F_MDS_QC < 2, na.rm = FALSE),

      # VPD is the mean of the HH values
      VPD_F_MDS = mean(VPD_F_MDS, na.rm = FALSE),
      VPD_F_MDS_QC = mean(VPD_F_MDS_QC < 2, na.rm = FALSE),

      # wind speed is the mean of the HH values
      WS_F = mean(WS_F, na.rm = FALSE),
      WS_F_QC = mean(WS_F_QC < 2, na.rm = FALSE),

      # atmospheric pressure is the mean of the HH values
      PA_F = mean(PA_F, na.rm = FALSE),
      PA_F_QC = mean(PA_F_QC < 2, na.rm = FALSE),

      # CO2 is the mean of the HH values
      CO2_F_MDS = mean(CO2_F_MDS, na.rm = FALSE),
      CO2_F_MDS_QC = mean(CO2_F_MDS_QC < 2, na.rm = FALSE),

      # FLUXES
      # add fraction of daily "missing values"
      # GPP_NT_VUT_SE = ifelse(
      #   length(which(!is.na(GPP_NT_VUT_SE)) >= length(GPP_NT_VUT_SE) * 0.5 ),
      #   sd(GPP_NT_VUT_REF, na.rm = FALSE)/sqrt(length(which(!is.na(GPP_NT_VUT_REF)))),
      #   NA
      # ),
      #
      # GPP_DT_VUT_SE = ifelse(
      #   length(which(!is.na(GPP_DT_VUT_SE)) >= length(GPP_DT_VUT_SE) * 0.5 ),
      #   sd(GPP_DT_VUT_REF, na.rm = FALSE)/sqrt(length(which(!is.na(GPP_DT_VUT_REF)))),
      #   NA
      # ),

      GPP_NT_VUT_REF = mean(GPP_NT_VUT_REF, na.rm = FALSE),
      GPP_DT_VUT_REF = mean(GPP_DT_VUT_REF, na.rm = FALSE),
      NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC < 2, na.rm = FALSE),

      # NETRAD/USTAR/SW_out is average from HH data
      # (only days with more than 50% records available)
      # add fraction of daily "missing values"
      NETRAD = mean(NETRAD, na.rm = FALSE),
      NETRAD_QC = mean(NETRAD_QC < 2, na.rm = FALSE),

      USTAR = mean(USTAR, na.rm = FALSE),
      USTAR_QC = mean(USTAR_QC < 2, na.rm = FALSE),

      SW_OUT = mean(SW_OUT, na.rm = FALSE),
      # SW_OUT_QC = mean(SW_OUT_QC < 2, na.rm = FALSE),

      # Latent heat is the mean of the HH values
      # add fraction of daily "missing values"
      LE_F_MDS = mean(LE_F_MDS, na.rm = FALSE),
      LE_F_MDS_QC = mean(LE_F_MDS_QC < 2, na.rm = FALSE),

      LE_CORR = mean(LE_CORR, na.rm = FALSE),
      LE_CORR_QC = mean(LE_CORR_QC < 2, na.rm = FALSE),

      # sensible heat is the mean of the HH values
      # add fraction of daily "missing values"
      H_F_MDS = mean(H_F_MDS, na.rm = FALSE),
      H_F_MDS_QC = mean(H_F_MDS_QC < 2, na.rm = FALSE),

      H_CORR = mean(H_CORR, na.rm = FALSE),
      H_CORR_QC = mean(H_CORR_QC < 2, na.rm = FALSE),

      # Joint uncertainty (can't be produced from)
      # available data in a similar manner as described
      # in fluxnet docs
      LE_CORR_JOINTUNC = NA,
      H_CORR_JOINTUNC = NA,

      # All carbon fluxes follow complex reprocessing
      # as Model Efficiency is repeated for each time
      # aggregation

      # MODIS RS data
      LAI = mean(LAI, na.rm = TRUE),
      FPAR = mean(FPAR, na.rm = TRUE)
    )

  # combine daytime averages and whole-day averages
  df <- df |>
    dplyr::left_join(df_day, by = "TIMESTAMP")

  # save data to file, using FLUXNET formatting
  if (!missing(out_path)) {

    utils::write.table(
      df,
      file = filename,
      quote = FALSE,
      col.names = TRUE,
      row.names = FALSE,
      sep = ","
    )

    message("---> writing data to file:")
    message(sprintf("   %s", filename))

    return(invisible(filename))

  } else {
    # return the merged file
    return(df)
  }
}
