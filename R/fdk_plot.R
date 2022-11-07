#' Plot FluxDataKit output
#'
#' @param df data frame with FLUXNET based data
#' @param site sitename
#' @param out_path where to store the images
#' @param overwrite overwrite existing files (TRUE or FALSE)
#' stored
#'
#' @return a plot with key variables
#' @export

fdk_plot <- function(
    df,
    site,
    out_path = "data/tmp",
    overwrite = FALSE
){

  # format filename
  filename <- file.path(out_path, sprintf("%s_plot.png", site))

  # check if files are already processed
  if(!overwrite){
    if(file.exists(filename)) {
      message(" files exist, skipping")
      return(invisible(NULL))
    }
  }

  # exclude non necessary values
  df <- df |>
    select(
      -ends_with("qc"),
      -ends_with("se"),
      -ends_with("uc"),
      -contains("longitude"),
      -contains("latitude"),
      -contains("elevation"),
      -starts_with("IGBP"),
      -ends_with("height"),
      -ends_with("_END")
    )

  # pivot to long format for easy
  # multi variable plotting
  df <- df |>
    pivot_longer(
      names_to = "name",
      values_to = "value",
      cols = 2:(ncol(df)) # date is first column
    )

  # convert time to proper format
  df <- df |>
    mutate(
      time = as.POSIXct(strptime(TIMESTAMP_START, "%Y%m%d%H%M"))
    )

  # plot the data
  p <- ggplot() +
    geom_point(
      data = df,
      aes(
        time,
        value,
        group = name
      ),
      colour = rgb(0,0,0, 0.1)
    ) +
    theme_bw() +
    facet_wrap(
      ~ name,
      scales = "free_y",
      nrow = 14
      )

  # saving image
  ggsave(
    filename,
    height = 20,
    width = 20,
    dpi = 150
    )
}




