#' Plot FluxDataKit output
#'
#' Simple routine to generate an overview plot of all
#' flux and climate data. The function takes a data frame
#' of FLUXNET formatted columns as input (this can be either
#' original source material or gap filled data as generated
#' by FluxDataKit).
#'
#' Plots are not returned (shown) in the current interface
#' but only written to file to allow for headless batch
#' processing (with limited overhead).
#'
#' @param df data frame with FLUXNET based data
#' @param site a FLUXNET site name to process
#' @param out_path the path where to store the plot
#' @param overwrite overwrite existing files (TRUE or FALSE)
#'  stored
#' @param daily whether daily or hourly data is to be plotted, defaults to FALSE.
#'
#' @return A plot with FLUXNET variables for rapid visual data inspection
#' @export

fdk_plot <- function(
    df,
    site = "sitename",
    out_path = "data/tmp",
    overwrite = FALSE,
    daily = FALSE
){

  # format filename
  filename_hh <- file.path(out_path, sprintf("%s_plot.png", site))
  filename_dd <- file.path(out_path, sprintf("%s_plot_daily.png", site))

  # exclude non necessary values
  df <- df |>
    dplyr::select(
      -ends_with("se"),
      -ends_with("uc"),
      -contains("longitude"),
      -contains("latitude"),
      -contains("elevation"),
      -starts_with("IGBP"),
      -ends_with("height"),
      -ends_with("_END")
    )

  # check if files are already processed
  if(!daily && (!file.exists(filename_hh) || overwrite)){

    # pivot to long format for easy
    # multi variable plotting
    df <- df |>
      tidyr::pivot_longer(
        names_to = "name",
        values_to = "value",
        cols = 2:(ncol(df)) # date is first column
      )

    # create hh plot
    df <- df |>
      mutate(
        time = as.POSIXct(strptime(TIMESTAMP_START, "%Y%m%d%H%M"))
      )

    p <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df,
        ggplot2::aes(
          time,
          value,
          group = name
        ),
        colour = rgb(0,0,0, 0.1)
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(
        ~ name,
        scales = "free_y",
        nrow = 14
      )

    # saving image
    ggplot2::ggsave(
      filename_hh,
      height = 20,
      width = 20,
      dpi = 150
    )

  }

  if (daily && (!file.exists(filename_dd) || overwrite)){

    # create dd plot
    df <- df |>
      mutate(
        time = lubridate::as_date(TIMESTAMP)
      )

    # gpp
    gg1 <- ggplot2::ggplot() +
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
      ggplot2::labs(x = "Time",
           y = expression(paste("GPP (", mu,"mol CO"[2], " m"^-2, "s"^-1, ")"))) +
      ggplot2::scale_color_viridis_c(direction = -1) +
      ggplot2::theme_bw()

    # LE
    gg2 <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = df,
        ggplot2::aes(
          time,
          LE_F_MDS
        ),
        color = "grey50") +
      ggplot2::geom_point(
        data = df,
        ggplot2::aes(
          time,
          LE_F_MDS,
          color = LE_F_MDS_QC
        ),
        size = 0.5) +
      ggplot2::labs(x = "Time",
           y = expression(paste("LE (W m"^-2, ")"))) +
      ggplot2::scale_color_viridis_c(direction = -1) +
      ggplot2::theme_bw()

    # LE - energy-balance corrected
    gg3 <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = df,
        ggplot2::aes(
          time,
          LE_CORR
        ),
        color = "grey50") +
      ggplot2::geom_point(
        data = df,
        ggplot2::aes(
          time,
          LE_CORR,
          color = LE_F_MDS_QC
        ),
        size = 0.5) +
      ggplot2::labs(x = "Time",
           y = expression(paste("EBC-LE (W m"^-2, ")"))) +
      ggplot2::scale_color_viridis_c(direction = -1) +
      ggplot2::theme_bw()

    cowplot::plot_grid(gg1, gg2, gg3, ncol = 1)

    # saving image
    ggplot2::ggsave(
      filename_dd,
      height = 12,
      width = 12,
      dpi = 150
    )

  }
  return(c(filename_hh, filename_dd))
}




