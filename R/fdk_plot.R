#' Plot FluxDataKit output
#'
#' @param file a fdk file
#' @param out_path where to store the images
#' stored
#'
#' @return a plot with key variables
#' @export

fdk_plot <- function(
    file,
    out_path = "data/tmp"
){

  # Convert to FLUXNET format
  # and easier to read data frame
  # using FLUXNET columns
  df <- fdk_read_plumber(
    file,
    meta_data = FALSE
  )

  df <- df |>
    select(
      -ends_with("qc"),
      -ends_with("se"),
      -ends_with("uc"),
      -longitude,
      -latitude,
      -elevation,
      -starts_with("IGBP"),
      -reference_height,
      -canopy_height
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

  # format filename
  filename <- tools::file_path_sans_ext(basename(file))
  filename <- file.path(out_path, sprintf("%s_plot.png", filename))

  # saving image
  ggsave(
    filename,
    height = 20,
    width = 20,
    dpi = 150
    )
}




