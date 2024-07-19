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
#'
#' @return data frame with daily (DD) down sampled values or file in the
#'  FLUXNET format
#' @export

fdk_downsample_fluxnet_phydro <- function(
    df,
    site,
    out_path,
    overwrite = FALSE,
    save_plots = TRUE,
    method = c("24hr", "daytime", "3hrmax")[1],
    midday_criterion = c("quantile", "time")[2]
){

  # Using the FLUXNET instructions, however in some cases there will
  # be no equivalence giving missing information. Downsampled data should
  # therefore not be considered equal to the original FLUXNET/ONEFLUX
  # processing chain.
  # https://fluxnet.org/data/fluxnet2015-dataset/fullset-data-product/

  # Check parameters
  if (!(method %in% c("24hr", "daytime", "3hrmax"))){
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

  if(!missing(out_path)){
    filename <- file.path(
      out_path,
      filename
    )
  }

  # if(file.exists(filename) & !overwrite){
  #   return(invisible())
  # }

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
  # JAIDEEP FIXME: probs was set to 0.1, which is 10% quantile. Replaced with 0.01
  daytime_thresh <- stats::quantile(df$SW_IN_F_MDS, probs = 0.01)

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
        pdf(paste0(filename, ".SW_acclim_",method,"_sample.pdf"), height=2, width=4)
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
      tmax = max(TA_F_MDS),
      tmin = min(TA_F_MDS)
    )

  # Calculate day length
  df_daylen <- df |>
    dplyr::filter(DAY) |>
    dplyr::group_by(TIMESTAMP) |>
    dplyr::summarize(DAYLENGTH = difftime(max(time), min(time), units="hours") |> as.numeric())

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
      TMIN_F_MDS    = ifelse(TA_F_MDS_QC < 0.5, NA, TMIN_F_MDS),
      TMAX_F_MDS    = ifelse(TA_F_MDS_QC < 0.5, NA, TMAX_F_MDS),
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
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales="free")
    if (missing(out_path)){
      print(p)
    } else {
      pdf(paste0(filename, ".before_gapfilling.pdf"), height=5, width=6)
      print(p)
      dev.off()
    }
  }

  # TA is used for gapfilling others, so ideally, it should itself be gap free
  # Gapfill TA by interpolation if:
  # - only a few values (<1%) are missing
  # - gap is < 1 week
  frac_missing_ta <- sum(is.na(df$TA_F_MDS))/nrow(df)
  if (frac_missing_ta < 0.01){
    df <- df |>
      dplyr::mutate(
        TA_F_MDS = zoo::na.approx(TA_F_MDS, na.rm = FALSE, maxgap = 7)
      )
  }

  # Gapfill TMAX and TMIN from hh data
  df <- df |>
    dplyr::mutate(TMAX_F_MDS = ifelse(is.na(TMAX_F_MDS), yes=tmax, no=TMAX_F_MDS),
           TMIN_F_MDS = ifelse(is.na(TMIN_F_MDS), yes=tmin, no=TMIN_F_MDS))

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
      ggplot2::geom_line(col="brown") +
      ggplot2::facet_wrap(~name, scales="free")
    if (missing(out_path)){
      print(p)
    } else {
      pdf(paste0(filename, ".after_gapfilling.pdf"), height=5, width=6)
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
