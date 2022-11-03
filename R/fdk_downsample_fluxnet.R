#' Down sample half-hourly data to daily values
#'
#' Down sampling of GPP values is done at hoc using a simple
#' mean. Normally the data is generated using a dedicated
#' workflow and are not 100% identical but correspondence
#' between HH and DD should have an R2 > 0.95, with only
#' structured errors in the NT product (not used)
#'
#' @param df FLUXNET based HH data frame
#' @param out_path where to store the converted data if converted to
#'  fluxnet formatting
#'
#' @return data frame with daily (DD) down sampled values or file in the
#'  FLUXNET formats
#' @export

fdk_downsample_fluxnet <- function(
    df,
    site,
    out_path
){

  # Using the FLUXNET instructions, however in some cases there will
  # be no equivalence giving missing information. Downsampled data should
  # therefore note be considered equal to the original FLUXNET/ONEFLUX
  # processing chain.
  # https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/

  df <- df |>
    mutate(
      TIMESTAMP = as.Date(TIMESTAMP_START, "%Y%m%d%H%M")
    )

  start_year <- format(min(df$TIMESTAMP), "%Y")
  end_year <- format(max(df$TIMESTAMP), "%Y")

  # check required columns fill with NA
  # if any are missing
  output_columns <- data.frame(
    "P_F" = NA,
    "TA_F_MDS" = NA,
    "SW_IN_F_MDS" = NA,
    "LW_IN_F_MDS" = NA,
    "VPD_F_MDS" = NA,
    "WS_F" = NA,
    "PA_F" = NA,
    "CO2_F_MDS" = NA,
    "GPP_DT_VUT_REF" = NA,
    "GPP_DT_VUT_SE" = NA,
    "NETRAD" = NA,
    "USTAR" = NA,
    "SW_OUT" = NA,
    "LE_F_MDS" = NA,
    "LE_CORR" = NA,
    "H_F_MDS" = NA,
    "H_CORR" = NA,
    "LAI" = NA,
    "FPAR" = NA
  )

  missing_columns <- output_columns[,which(!(colnames(output_columns) %in% colnames(df)))]

  if (ncol(missing_columns) > 0 ) {
    df <- bind_cols(df, missing_columns)
  }

  df <- df |>
    group_by(TIMESTAMP) |>
    summarize(

      # METEO

      # precipitation is the sum of HH values
      P_F = sum(P_F, na.rm = TRUE),

      # temperature is the mean of the HH values
      TA_F_MDS = mean(TA_F_MDS, na.rm = TRUE),

      # temperature is the mean of the HH values
      SW_IN_F_MDS = mean(SW_IN_F_MDS, na.rm = TRUE),

      # long wave radiation is the mean of the HH values
      LW_IN_F_MDS = mean(LW_IN_F_MDS, na.rm = TRUE),

      # VPD is the mean of the HH values
      VPD_F_MDS = mean(VPD_F_MDS, na.rm = TRUE),

      # wind speed is the mean of the HH values
      WS_F = mean(WS_F, na.rm = TRUE),

      # temperature is the mean of the HH values
      PA_F = mean(PA_F, na.rm = TRUE),

      # CO2 is the mean of the HH values
      CO2_F_MDS = mean(CO2_F_MDS, na.rm = TRUE),

      # FLUXES
      GPP_DT_VUT_REF = ifelse(
        length(which(!is.na(GPP_DT_VUT_REF)) > length(GPP_DT_VUT_REF) * 0.5 ),
        mean(GPP_DT_VUT_REF, na.rm = TRUE),
        NA
      ),

      GPP_DT_VUT_SE = ifelse(
        length(which(!is.na(GPP_DT_VUT_SE)) > length(GPP_DT_VUT_SE) * 0.5 ),
        sd(GPP_DT_VUT_REF, na.rm = TRUE),
        NA
      ),

      # NETRAD/USTAR/SW_out is average from HH data
      # (only days with more than 50% records available)
      NETRAD = ifelse(
          length(which(!is.na(NETRAD)) > length(NETRAD) * 0.5 ),
          mean(NETRAD, na.rm = TRUE),
          NA
        ),

      USTAR = ifelse(
        length(which(!is.na(USTAR)) > length(USTAR) * 0.5 ),
        mean(USTAR, na.rm = TRUE),
        NA
      ),

      SW_OUT = ifelse(
        length(which(!is.na(SW_OUT)) > length(SW_OUT) * 0.5 ),
        mean(SW_OUT, na.rm = TRUE),
        NA
      ),

      # Latent heat is the mean of the HH values
      LE_F_MDS = mean(LE_F_MDS, na.rm = TRUE),
      LE_CORR = mean(LE_CORR, na.rm = TRUE),

      # sensible heat is the mean of the HH values
      H_F_MDS = mean(H_F_MDS, na.rm = TRUE),
      H_CORR = mean(H_CORR, na.rm = TRUE),

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

  # save data to file, using FLUXNET formatting
  if (!missing(out_path)) {

    filename <- sprintf("FLX_%s_PLUMBER_FULLSET_DD_%s_%s_2-3.csv",
                        site,
                        start_year,
                        end_year
    )
    filename <- file.path(
      out_path,
      filename
    )

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

    return(filename)

  } else {
    # return the merged file
    return(df)
  }
}
