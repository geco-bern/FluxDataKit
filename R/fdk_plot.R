#' Plot FluxDataKit output
#'
#' @param site a sitename
#' @param path the path where LSM compatible data are
#' @param out_path where to store the images
#' stored
#'
#' @return a plot with key variables
#' @export

fdk_plot <- function(
    site,
    path = "data/tmp",
    out_path = "data/tmp"
){

  # Convert to FLUXNET format
  # and easier to read data frame
  # using FLUXNET columns
  df <- fdk_convert_lsm(
    site = site,
    path = path,
    fluxnet_format = TRUE,
    meta_data = FALSE
  )

  df <- df |>
    mutate(
      date = as.Date(TIMESTAMP_START, "%Y%m%d%H%M")
    ) |>
    select(
      -ends_with("QC"),
      -ends_with("SE"),
      -ends_with("UNC"),
      -starts_with("TIME")
    )

  nrow <- ncol(df)-1

  df <- df |>
    pivot_longer(
      names_to = "name",
      values_to = "value",
      cols = 1:(ncol(df)-1) # date is last column
    )

  p <- ggplot() +
    geom_point(
      data = df,
      aes(
        date,
        value,
        group = name
      )
    ) +
    theme_bw() +
    facet_wrap(
      ~ name,
      scales = "free_y",
      nrow = 14
      )

  # saving image
  ggsave(
    file.path(out_path, sprintf("%s_overview_plot.png", site)),
    height = 20,
    width = 20
    )
}




