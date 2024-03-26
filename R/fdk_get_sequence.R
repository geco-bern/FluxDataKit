#' Get sequence of good-quality data
#'
#' Determines the longest consecutive sequence of good-quality data, separately
#' for GPP and LE. Short "interruptions" of bad-quality data are ignored as long
#' as they don't exceed the given length threshold
#'
#' @param df data frame with daily FLUXNET-based data
#' @param site a FLUXNET site name to process
#' @param qc_threshold Threshold of the the QC information (fraction of good-
#' quality half-hourly data used for daily aggregates).
#' @param leng_threshold Threshold of interruptions of bad-quality data to be
#' ignored. In days.
#' @param do_plot boolean, whether to create plot, defaults to FALSE
#' @param out_path the path where to store the plot
#'
#' @return A data frame containing information about the start date and end date
#' of the longest sequence.
#' @export
#'
fdk_get_sequence <- function(
    df,
    site = "sitename",
    qc_threshold = 0.5,
    leng_threshold = 25,
    do_plot = FALSE,
    out_path = "data/tmp"
){

  df <- df |>
    mutate(good_gpp = ifelse(NEE_VUT_REF_QC > qc_threshold, TRUE, FALSE),
           good_le = ifelse(LE_F_MDS_QC > qc_threshold, TRUE, FALSE))

  # determine sequences of consecutive TRUE and merge if gap between them is short
  instances_merged <- get_consecutive(
    df$good_gpp,
    merge_threshold = leng_threshold,
    do_merge = TRUE
  )

  df_sequences_merged <- tibble(
    start = lubridate::as_date(df$TIMESTAMP[instances_merged$idx_start]),
    end = lubridate::as_date(df$TIMESTAMP[instances_merged$idx_start + instances_merged$len - 1])
  )

  # determine longest sequence of good quality data
  longest_sequence <- instances_merged |>
    filter(len == max(instances_merged$len))

  out <- tibble(
    sitename = site,
    start = lubridate::as_date(df$TIMESTAMP[longest_sequence$idx_start]),
    end = lubridate::as_date(df$TIMESTAMP[longest_sequence$idx_start + longest_sequence$len - 1])) |>

    # truncate to entire years (1. Jan - 31. Dec)
    mutate(
      year_start_fullyearsgpp = ifelse(
        lubridate::yday(start) == 1,
        lubridate::year(start),
        lubridate::year(start) + 1),
      year_end_fullyearsgpp = ifelse(
        lubridate::yday(end) >= 365,
        lubridate::year(end),
        lubridate::year(end) - 1
      )) |>
    mutate(
      nyears = year_end_fullyearsgpp - year_start_fullyearsgpp + 1
    ) |>
    mutate(
      drop = ifelse(nyears < 1, TRUE, FALSE)
    )

  if (do_plot){

    # get un-merged sequences
    instances <- get_consecutive(
      df$good_gpp,
      do_merge = FALSE
    )

    df_sequences <- tibble(
      start = lubridate::as_date(df$TIMESTAMP[instances$idx_start]),
      end = lubridate::as_date(df$TIMESTAMP[instances$idx_start + instances$len - 1])
    )

    filename_dd <- file.path(out_path, sprintf("%s_plot_daily_seq.png", site))

    # plot
    df <- df |>
      mutate(
        time = lubridate::as_date(TIMESTAMP)
      )

    # # un-merged
    # gg1 <- ggplot2::ggplot() +
    #   ggplot2::geom_rect(
    #     data = df_sequences,
    #     ggplot2::aes(
    #       xmin = start,
    #       xmax = end,
    #       ymin = min(df$GPP_NT_VUT_REF, na.rm = TRUE),
    #       ymax = max(df$GPP_NT_VUT_REF, na.rm = TRUE)
    #     ),
    #     fill = "grey80"
    #   ) +
    #   ggplot2::geom_line(
    #     data = df,
    #     ggplot2::aes(
    #       time,
    #       GPP_NT_VUT_REF
    #     ),
    #     color = "grey50") +
    #   ggplot2::geom_point(
    #     data = df,
    #     ggplot2::aes(
    #       time,
    #       GPP_NT_VUT_REF,
    #       color = NEE_VUT_REF_QC
    #     ),
    #     size = 0.5) +
    #   labs(x = "Time",
    #        y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")")),
    #        title = "Initial sequences") +
    #   scale_color_viridis_c(direction = -1) +
    #   ggplot2::theme_bw()

    # merged
    gg2 <- ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = out,
        ggplot2::aes(
          xmin = lubridate::ymd(paste0(year_start_fullyearsgpp, "-01-01")),
          xmax = lubridate::ymd(paste0(year_end_fullyearsgpp,   "-12-31")),
          ymin = min(df$GPP_NT_VUT_REF, na.rm = TRUE),
          ymax = max(df$GPP_NT_VUT_REF, na.rm = TRUE)
        ),
        fill = "grey80"
      ) +
      ggplot2::geom_line(
        data = df,
        ggplot2::aes(
          time,
          GPP_NT_VUT_REF
        ),
        color = "grey50") +
      ggplot2::geom_point(
        data = df,
        ggplot2::aes(
          time,
          GPP_NT_VUT_REF,
          color = NEE_VUT_REF_QC
        ),
        size = 0.5) +
      labs(x = "Time",
           y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
      scale_color_viridis_c(direction = -1) +
      ggplot2::theme_bw()

    # saving image
    ggplot2::ggsave(
      filename_dd,
      plot = gg2,
      height = 6,
      width = 12,
      dpi = 150
    )

  }

  return(out)
}

get_consecutive <- function(
    good,
    merge_threshold = 5,
    leng_threshold = 5,
    do_merge = FALSE
    ){
  ##------------------------------------
  ## Returns a dataframe that contains information about events (starting index and length)
  ## of consecutive conditions (TRUE) in a boolean vector ('good' - naming is a legacy).
  ##------------------------------------

  ## replace NAs with FALSE (no drought). This is needed because of NAs at head or tail
  good[ which(is.na(good)) ] <- FALSE

  ## identifies periods where 'good' true for consecutive days of length>leng_threshold and
  ## creates data frame holding each instance's info: start of drought by index in 'good' and length (number of days thereafter)
  instances <- data.frame( idx_start=c(), len=c() )
  consecutive_good <- rep( NA, length( good ) )
  ngood  <- 0
  ninst <- 0
  for ( idx in 1:length( good ) ){
    if (good[idx]){
      ngood <- ngood + 1
    } else {
      if (ngood >= leng_threshold) {
        ## create instance
        ninst <- ninst + 1
        addrow <- data.frame( idx_start = idx-(ngood), len = ngood )
        instances <- rbind( instances, addrow )
      }
      ngood <- 0
    }
    consecutive_good[idx] <- ngood
  }
  if (ngood > leng_threshold){
    ## create a last instance if the last good period extends to the end of the time series
    ninst <- ninst + 1
    addrow <- data.frame( idx_start=idx-(ngood), len=ngood )
    instances <- rbind( instances, addrow )
  }

  # get info about gap between events
  instances <- instances |>
    mutate(gap_before = idx_start - (lag(idx_start) + lag(len)))

  if (nrow(instances) > 0){
    if (do_merge && nrow(instances) > 1){

      instances_merged <- instances[1,]
      idx_merged <- 1
      for (idx in 2:nrow(instances)){
        if (instances$gap_before[idx] > merge_threshold){

          # create new sequence
          idx_merged <- idx_merged + 1
          instances_merged <- bind_rows(
            instances_merged,
            instances[idx,]
          )

          # edit length of previously recorded instance
          instances_merged$len[idx_merged - 1] <- instances$idx_start[idx - 1] + instances$len[idx - 1] - instances_merged$idx_start[idx_merged - 1]
        }
      }

      # if all is merged until the end
      instances_merged$len[idx_merged] <- instances$idx_start[idx] + instances$len[idx] - instances_merged$idx_start[idx_merged]

      instances <- instances_merged[,c("idx_start", "len")]
    } else {
      instances <- instances[,c("idx_start", "len")]
      if (nrow(instances) == 1){
        if (instances$idx_start == 0)
          instances$idx_start <- 1
      }
    }

  }

  return( instances )
}
