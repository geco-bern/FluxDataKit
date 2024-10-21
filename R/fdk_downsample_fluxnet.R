#' Down samples half-hourly FLUXNET data to daily values
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
#' @param save_plots Whether plots of downsampled variables before and after gapfilling should be saved to files (in the output_folder OR to default device)
#' @param method What method should be used for averaging (should be one of "24hr", "daytime", "3hrmax")
#' @param midday_criterion How should midday time points be identified? Should be one of "quantile", "time"
#' @param max_gap max length of gap in temperature to be gapfilled by interpolation
#'
#' @return data frame with daily (DD) down sampled values or file in the
#'  FLUXNET format
#' @export

fdk_downsample_fluxnet_phydro <- function(
    df,
    site,
    out_path,
    fig_path = out_path,
    overwrite = FALSE,
    save_plots = TRUE,
    method = c("legacy", "24hr", "daytime", "3hrmax")[1],
    midday_criterion = c("quantile", "time")[2],
    max_gap = 7
){

  # Using the FLUXNET instructions, however in some cases there will
  # be no equivalence giving missing information. Downsampled data should
  # therefore not be considered equal to the original FLUXNET/ONEFLUX
  # processing chain.
  # https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/

  # Check parameters
  if (!(method %in% c("legacy", "24hr", "daytime", "3hrmax"))){
    stop("method should be one of 24hr, daytime, 3hrmax")
  }

  df =
    df |>
    dplyr::mutate(time = lubridate::as_datetime(as.character(TIMESTAMP_START), tz = "GMT", format="%Y%m%d%H%M")) |>
    dplyr::mutate(date = lubridate::as_date(time)) |>
    dplyr::mutate(TIMESTAMP = date)

  start_year <- format(min(df$TIMESTAMP), "%Y")
  end_year <- format(max(df$TIMESTAMP), "%Y")

  filename <- sprintf("FLX_%s_PLUMBER_FULLSET_DD_%s_%s_2-3_%s.csv",
                      site,
                      start_year,
                      end_year,
                      method
  )

  fig_prefix = paste(site, start_year, end_year, method, sep="_")

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
    "TMAX_F_MDS" = NA,
    "TMIN_F_MDS" = NA,
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
    #"NEE_NT_VUT_REF" = NA,
    #"NEE_DT_VUT_REF" = NA,
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
  # daytime_thresh <- stats::quantile(df$SW_IN_F_MDS, probs = 0.1)
  # Jaideep: Absolute value (10 W/m2) seems to work better, perhaps because of noise
  daytime_thresh <- 10

  # add daytime flag
  df <- df |>
    dplyr::mutate(DAY = ifelse(SW_IN_F_MDS > daytime_thresh, TRUE, FALSE))

  # FIXME: Dont use dots
  # Separate QC variables as aggregation works differently for them:
  # using FLUXNET naming conventions
  # Aggregate QC flags as the daily fraction of measured or good-quality (value < 2) gap-filled half-hourly values.
  # _F_MDS_QC = 0 (measured);
  # _F_MDS_QC = 1 (filled with high confidence);
  # _F_MDS_QC = 2 (filled with medium confidence);
  # _F_MDS_QC = 3 (filled with low confidence)
  colnames_df <- names(df)
  is_QC = colnames_df[grepl("_QC", colnames_df)]

  if (method == "legacy"){

    # Aggregation of quality checks is taken as-is from legacy code.
    # JAIDEEP: Why do some variables use na.rm=F ?
    df_day_QC <- df |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        P_F_QC = mean(P_F_QC < 2, na.rm = TRUE),
        TA_F_MDS_QC = mean(TA_F_MDS_QC < 2, na.rm = TRUE),
        SW_IN_F_MDS_QC = mean(SW_IN_F_MDS_QC < 2, na.rm = TRUE),
        LW_IN_F_MDS_QC = mean(LW_IN_F_MDS_QC < 2, na.rm = TRUE),
        VPD_F_MDS_QC = mean(VPD_F_MDS_QC < 2, na.rm = TRUE),
        WS_F_QC = mean(WS_F_QC < 2, na.rm = TRUE),
        PA_F_QC = mean(PA_F_QC < 2, na.rm = TRUE),
        CO2_F_MDS_QC = mean(CO2_F_MDS_QC < 2, na.rm = TRUE),
        NEE_VUT_REF_QC = mean(NEE_VUT_REF_QC < 2, na.rm = FALSE),
        NETRAD_QC = mean(NETRAD_QC < 2, na.rm = TRUE),
        USTAR_QC = mean(USTAR_QC < 2, na.rm = TRUE),
        LE_F_MDS_QC = mean(LE_F_MDS_QC < 2, na.rm = FALSE),
        LE_CORR_QC = mean(LE_CORR_QC < 2, na.rm = FALSE),
        H_F_MDS_QC = mean(H_F_MDS_QC < 2, na.rm = FALSE),
        H_CORR_QC = mean(H_CORR_QC < 2, na.rm = FALSE),
      )

    # VPD and TA are aggregated as daytime averages
    # JAIDEEP: I am simplifying the logic here: variables should be named
    #    uniformly in this driver object, and their aggregation logic is
    #    entirely specified here (rather that across multiple files).
    #    Hence, I am calling these variables as XX_MDS instead of XX_DAY_MDS,
    #    and am removing theie 24-hr mean versions further below. If the 24-hr
    #    means are needed, they should be retrived from the 24 hr forcing.
    df_day_vars_ta_vpd <- df |>
      dplyr::filter(DAY) |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        TA_F_MDS = mean(TA_F_MDS, na.rm = TRUE),
        VPD_F_MDS = mean(VPD_F_MDS, na.rm = TRUE),
      )

    # Rest of the variables are aggregated as daily averages
    # P_F is removed from this list as it is aggregated separately later.
    df_day_vars <- df |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(

        # METEO
        # precipitation is the sum of HH values
        # P_F = sum(P_F, na.rm = TRUE),  # removing, aggregated separately later

        # JAIDEEP: MAJOR BUG ALERT: DO NOT replace TA_F_MDS here, as it is needed for TMIN/TMAX calculations below.
        # temperature is the mean of the HH values
        # TA_F_MDS = mean(TA_F_MDS, na.rm = TRUE), # removing, see comment above

        TMIN_F_MDS = min(TA_F_MDS, na.rm = TRUE),
        TMAX_F_MDS = max(TA_F_MDS, na.rm = TRUE),

        # shortwave radiation is the mean of the HH values
        SW_IN_F_MDS = mean(SW_IN_F_MDS, na.rm = TRUE),

        # long wave radiation is the mean of the HH values
        LW_IN_F_MDS = mean(LW_IN_F_MDS, na.rm = TRUE),

        # VPD is the mean of the HH values
        # VPD_F_MDS = mean(VPD_F_MDS, na.rm = TRUE), #  removing, see comment above

        # wind speed is the mean of the HH values
        WS_F = mean(WS_F, na.rm = TRUE),

        # atmospheric pressure is the mean of the HH values
        PA_F = mean(PA_F, na.rm = TRUE),

        # CO2 is the mean of the HH values
        CO2_F_MDS = mean(CO2_F_MDS, na.rm = TRUE),

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

        # CARBON FLUXES
        NEE_VUT_REF = mean(NEE_VUT_REF, na.rm = FALSE),
        GPP_NT_VUT_REF = mean(GPP_NT_VUT_REF, na.rm = FALSE),
        GPP_DT_VUT_REF = mean(GPP_DT_VUT_REF, na.rm = FALSE),
        RECO_NT_VUT_REF = mean(RECO_NT_VUT_REF, na.rm = FALSE),
        # RECO_DT_VUT_REF = mean(RECO_DT_VUT_REF, na.rm = FALSE),  # not available

        # NETRAD/USTAR/SW_out is average from HH data
        # (only days with more than 50% records available)
        # add fraction of daily "missing values"
        NETRAD = mean(NETRAD, na.rm = TRUE),

        USTAR = mean(USTAR, na.rm = TRUE),

        SW_OUT = mean(SW_OUT, na.rm = TRUE),
        # SW_OUT_QC = mean(SW_OUT_QC < 2, na.rm = TRUE),

        # Latent heat is the mean of the HH values
        # add fraction of daily "missing values"
        LE_F_MDS = mean(LE_F_MDS, na.rm = FALSE),

        LE_CORR = mean(LE_CORR, na.rm = FALSE),

        # sensible heat is the mean of the HH values
        # add fraction of daily "missing values"
        H_F_MDS = mean(H_F_MDS, na.rm = FALSE),

        H_CORR = mean(H_CORR, na.rm = FALSE),

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
      ) |>

      # Add TA and VPD to the aggregated data
      dplyr::left_join(
        df_day_vars_ta_vpd
      )

  }

  if (method == "24hr"){
    df_day_QC <- df |>
      dplyr::select(TIMESTAMP, all_of(is_QC)) |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        dplyr::across(
          tidyr::everything(),
          ~mean(.x < 2, na.rm = TRUE)
        )
      )

    # Aggregate variables as the daily mean over 24 hours (except P_F)
    df_day_vars <- df |>
      dplyr::select(-any_of(is_QC)) |>
      dplyr::select(-P_F) |>
      dplyr::select(-TIMESTAMP_START, -TIMESTAMP_END, -date) |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        dplyr::across(
          tidyr::everything(),
          ~mean(.x, na.rm = TRUE)
        )
      )
  }

  if (method == "daytime"){
    df_day_QC <- df |>
      dplyr::select(TIMESTAMP, DAY, all_of(is_QC)) |>
      dplyr::filter(DAY) |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        dplyr::across(
          tidyr::everything(),
          ~mean(.x < 2, na.rm = TRUE)
        )
      )



    # Aggregate variables as the daily mean over DAY hours (except P_F)
    df_day_vars <- df |>
      dplyr::select(-any_of(is_QC)) |>
      dplyr::select(-P_F) |>
      dplyr::select(-TIMESTAMP_START, -TIMESTAMP_END, -date) |>
      dplyr::filter(DAY) |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        dplyr::across(
          tidyr::everything(),
          ~mean(.x, na.rm = TRUE)
        )
      )
  }

  if (method == "3hrmax"){
    if (midday_criterion == "quantile"){
      # determine acclimation times based on Shortwave quantiles (SW > 90%ile)
      sw_thresh <- df |>
        dplyr::select(TIMESTAMP, SW_IN_F_MDS) |>
        dplyr::group_by(TIMESTAMP) |>
        dplyr::summarize(SW_thresh = stats::quantile(SW_IN_F_MDS, probs = 0.90)) |>
        ungroup()

      df_3hr <- df |>
        dplyr::left_join(sw_thresh) |>
        dplyr::filter(SW_IN_F_MDS > SW_thresh) |>
        dplyr::mutate(SW_acclim = SW_IN_F_MDS)
    }
    if (midday_criterion == "time"){
      # determine acclimation times based on max Shortwave time (maxSW_time +/- 1.5 hrs)
      sw_max <- df |>
        dplyr::select(TIMESTAMP, time, SW_IN_F_MDS) |>
        dplyr::group_by(TIMESTAMP) |>
        dplyr::filter(SW_IN_F_MDS == max(SW_IN_F_MDS)) |>
        dplyr::mutate(time_maxsw = time) |>
        dplyr::select(-time, -SW_IN_F_MDS) |>
        dplyr::summarize(
          dplyr::across(
            tidyr::everything(),
            mean
          )
        )

      df_3hr <- df |>
        dplyr::left_join(sw_max) |>
        dplyr::filter(time >= time_maxsw - 1.5*3600 &
                 time < time_maxsw + 1.5*3600) |>
        dplyr::mutate(SW_acclim = SW_IN_F_MDS)
    }

    if (save_plots){
      data_mid_day = df |> dplyr::pull(TIMESTAMP) |> mean()
      p1 = df |>
        dplyr::filter(date >= data_mid_day & date < data_mid_day+3) |>
        dplyr::select(time, date, SW_IN_F_MDS) |>
        dplyr::left_join(df_3hr |> dplyr::select(time, SW_acclim),
                  by = "time") |>
        ggplot2::ggplot(ggplot2::aes(x=time)) +
        ggplot2::geom_line(ggplot2::aes(y=SW_IN_F_MDS), col="black")+
        ggplot2::geom_point(ggplot2::aes(y=SW_acclim), col="orange")

      if (missing(out_path)){
        print(p1)
      } else {
        pdf(paste0(fig_path, "/", fig_prefix, ".SW_acclim_sample.pdf"), height=2, width=4)
        print(p1)
        dev.off()
      }
    }

    df_day_QC <- df_3hr |>
      dplyr::select(TIMESTAMP, all_of(is_QC)) |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        dplyr::across(
          tidyr::everything(),
          ~mean(.x < 2, na.rm = TRUE)
        )
      )


    # Aggregate variables as the daily mean over DAY hours
    df_day_vars <- df_3hr |>
      dplyr::select(-any_of(is_QC)) |>
      dplyr::select(-P_F) |>
      dplyr::select(-TIMESTAMP_START, -TIMESTAMP_END, -date) |>
      dplyr::group_by(TIMESTAMP) |>
      dplyr::summarize(
        dplyr::across(
          tidyr::everything(),
          ~mean(.x, na.rm = TRUE)
        )
      )
  }

  # Aggregate P_F always as sum over 24 hrs
  df_day_vars_pf <- df |>
    dplyr::select(P_F, TIMESTAMP) |>
    dplyr::group_by(TIMESTAMP) |>
    dplyr::summarize(
      dplyr::across(
        tidyr::everything(),
        ~sum(.x, na.rm = TRUE)
      )
    )

  # Aggregate Tmax and Tmin always as max and min over 24 hrs
  # (used for gapfilling the actual FLUXNET variables)
  df_tmaxmin <- df |>
    dplyr::group_by(TIMESTAMP) |>
    dplyr::summarize(
      tmax = max(TA_F_MDS, na.rm=TRUE),
      tmin = min(TA_F_MDS, na.rm=TRUE)
    )

  # Calculate day length
  df_daylen <- df |>
    # dplyr::filter(SW_IN_F_MDS > 10) |>
    dplyr::filter(DAY) |>
    dplyr::group_by(TIMESTAMP) |>
    dplyr::summarize(DAYLENGTH = difftime(max(time), min(time), units="hours") |> as.numeric()) |>
    # Average daylength is corrected to 12 hr (as thresholding might slightly underestimate it)
    dplyr::mutate(DAYLENGTH = DAYLENGTH - mean(DAYLENGTH)+12)

  # combine variables, P_F, quality flags, and computed tmax/min
  # This step replaces df from half-hourly to aggregated daily
  df <- df_day_vars |>
    dplyr::left_join(df_day_vars_pf) |>
    dplyr::left_join(df_day_QC) |>
    dplyr::left_join(df_tmaxmin) |>
    dplyr::left_join(df_daylen)

  # clean data - remove if less than 80% is good-quality gap-filled
  df <- df |>
    dplyr::mutate(
      P_F           = ifelse(P_F_QC < 0.5, NA, P_F), # no better approach
      TA_F_MDS      = ifelse(TA_F_MDS_QC < 0.5, NA, TA_F_MDS), # no better approach
      # TA_DAY_F_MDS  = ifelse(TA_F_MDS_QC < 0.5, NA, TA_DAY_F_MDS),
      # VPD_DAY_F_MDS = ifelse(VPD_F_MDS_QC < 0.5, NA, VPD_DAY_F_MDS),
      # TMIN_F_MDS    = ifelse(TA_F_MDS_QC < 0.5, NA, TMIN_F_MDS),
      # TMAX_F_MDS    = ifelse(TA_F_MDS_QC < 0.5, NA, TMAX_F_MDS),
      SW_IN_F_MDS   = ifelse(SW_IN_F_MDS_QC < 0.5, NA, SW_IN_F_MDS),
      LW_IN_F_MDS   = ifelse(LW_IN_F_MDS_QC < 0.5, NA, LW_IN_F_MDS),
      VPD_F_MDS     = ifelse(VPD_F_MDS_QC < 0.5, NA, VPD_F_MDS),
      # WS_F          = ifelse(WS_F_QC < 0.5, NA, WS_F), # no better approach
      # PA_F          = ifelse(PA_F_QC < 0.5, NA, PA_F), # no better approach
      # CO2_F_MDS     = ifelse(CO2_F_MDS_QC < 0.5, NA, CO2_F_MDS) # no better approach
      PA_F          = ifelse(PA_F < 0, mean(PA_F), PA_F), # replace negative PA values with overall mean
    )

  # test for missing forcing data and impute
  vars <- c(
    "P_F",
    "TA_F_MDS",
    # "TA_DAY_F_MDS",
    # "VPD_DAY_F_MDS",
    "TMIN_F_MDS",
    "TMAX_F_MDS",
    "SW_IN_F_MDS",
    "LW_IN_F_MDS",
    "NETRAD",
    "VPD_F_MDS",
    "WS_F",
    "PA_F",
    "CO2_F_MDS",
    "LAI",
    "FPAR"
  )

  missing <- df |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        ~sum(is.na(.))
        )) |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(value > 0) |>
    dplyr::pull(name)

  # Plot variables before gapfilling
  if (save_plots){
    p <- df |> dplyr::select(TIMESTAMP, DAYLENGTH, any_of(vars)) |>
      tidyr::pivot_longer(-TIMESTAMP) |>
      ggplot2::ggplot(ggplot2::aes(x=TIMESTAMP, y=value)) +
      ggplot2::geom_line(col="black", linewidth=0.3) +
      ggplot2::facet_wrap(~name, scales="free")
    if (missing(out_path)){
      print(p)
    } else {
      pdf(paste0(fig_path, "/", fig_prefix, ".before_gapfilling.pdf"), height=5, width=6)
      print(p)
      dev.off()
    }
  }


  # # Gapfill TA by interpolation wherever gap is < 1 week
  # frac_missing_ta <- sum(is.na(df$TA_F_MDS))/nrow(df)
  # # if (frac_missing_ta < 0.01){
  #   df <- df |>
  #     dplyr::mutate(
  #       TA_F_MDS = zoo::na.approx(TA_F_MDS, na.rm = FALSE, maxgap = 7)
  #     )
  # # }

  # TA is used for gapfilling others, so ideally, it should itself be gap free
  # Therefore, we gapfill TA first using SW and DOY,
  # and whatever is still missing is gapfilled using DOY alone (seasonal cycle)
  df <- df |>
    dplyr::mutate(DOY = lubridate::yday(TIMESTAMP))

  # Where temperature is missing, all other drivers which are gapfilled with
  # temp are also unreliable, hence mark a FORCING_QC flag that indicates whether
  # temperature has been (badly) gapfilled. These points can be excluded from
  # comparisons of pmodel outputs with data
  df <- df |>
    dplyr::mutate(FORCING_QC = ifelse(is.na(TA_F_MDS), yes=0, no=1))

  if ("TA_F_MDS" %in% missing){
    df <- fdk_impute_knn(
      df,
      target = "TA_F_MDS",
      pred1 = "SW_IN_F_MDS",
      pred2 = "DOY",
      k = 5
    )

    df <- fdk_impute_knn(
      df,
      target = "TA_F_MDS",
      pred1 = "DOY",
      k = 5
    )

  }

  # Gapfill TMAX and TMIN from hh data
  df <- df |>
    dplyr::mutate(TMAX_F_MDS = ifelse(is.na(TMAX_F_MDS), yes=tmax, no=TMAX_F_MDS),
           TMIN_F_MDS = ifelse(is.na(TMIN_F_MDS), yes=tmin, no=TMIN_F_MDS)) |>
    dplyr::select(-tmin, -tmax)

  # Shortwave radiation: impute with KNN
  if ("SW_IN_F_MDS" %in% missing){
    df <- fdk_impute_knn(
      df,
      target = "SW_IN_F_MDS",
      pred1 = "TA_F_MDS",
      k = 5
    )
  }

  # Longwave radiation: impute with KNN
  if ("LW_IN_F_MDS" %in% missing){
    df <- fdk_impute_knn(
      df,
      target = "LW_IN_F_MDS",
      pred1 = "TA_F_MDS",
      k = 5
    )
  }

  # Net radiation: impute with KNN
  if ("NETRAD" %in% missing){
    df <- fdk_impute_knn(
      df,
      target = "NETRAD",
      pred1 = "TA_F_MDS",
      pred2 = "SW_IN_F_MDS",
      k = 5
    )
  }

  # VPD: impute with KNN
  if ("VPD_F_MDS" %in% missing){
    df <- fdk_impute_knn(
      df,
      target = "VPD_F_MDS",
      pred1 = "TA_F_MDS",
      k = 5
    )
  }

  # JAIDEEP FIXME: anything better?
  # P: set missing to zero
  if ("P_F" %in% missing){
    df <- df |>
      mutate(P_F = ifelse(is.na(P_F), yes=0, no=P_F))
  }

  # CO2: interpolate
  if ("CO2_F_MDS" %in% missing){
    df <- interpolate2daily_CO2_F_MDS(df)
  }

  # Atmospheric pressure: interpolate
  if ("PA_F" %in% missing){
    df <- interpolate2daily_PA_F(df)
  }

  # fAPAR: interpolate
  if ("FPAR" %in% missing){
    df <- interpolate2daily_fpar(df)
  }

  # Wind speed: interpolate
  if ("WS_F" %in% missing){
    df <- interpolate2daily_WS_F(df)
  }

  # Plot variables after gapfilling
  if (save_plots){
    p <- df |>
      dplyr::select(TIMESTAMP, DAYLENGTH, any_of(vars)) |>
      tidyr::pivot_longer(-TIMESTAMP) |>
      ggplot2::ggplot(ggplot2::aes(x=TIMESTAMP, y=value)) +
      ggplot2::geom_line(col="brown", linewidth=0.3) +
      ggplot2::facet_wrap(~name, scales="free")
    if (missing(out_path)){
      print(p)
    } else {
      pdf(paste0(fig_path, "/", fig_prefix, ".after_gapfilling.pdf"), height=5, width=6)
      print(p)
      dev.off()
    }
  }

  # still missing?
  missing <- df |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(vars),
        ~sum(is.na(.))
      )) |>
    tidyr::pivot_longer(everything()) |>
    dplyr::filter(value > 0) |>
    dplyr::pull(name)


  if (length(missing) > 0){
    message(paste(site, "!!! still missing values:"))
    message(paste(missing, collapse = ","))
  }

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
