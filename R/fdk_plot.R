#' Plot FluxDataKit output
#'
#' @param file a fdk file
#' @param out_path where to store the images
#' @param overwrite overwrite existing files (TRUE or FALSE)
#' stored
#'
#' @return a plot with key variables
#' @export

fdk_plot <- function(
    file,
    out_path = "data/tmp",
    overwrite = FALSE
){

  # format filename
  filename <- tools::file_path_sans_ext(basename(file))
  filename <- file.path(out_path, sprintf("%s_plot.png", filename))

  # check if files are already processed
  if(!overwrite){
    if(file.exists(filename)) {
      message(" files exist, skipping")
      return(invisible(NULL))
    }
  }

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
      -contains("longitude"),
      -contains("latitude"),
      -contains("elevation"),
      -starts_with("IGBP"),
      -ends_with("height")
    )

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

  # saving image
  ggsave(
    filename,
    height = 20,
    width = 20,
    dpi = 150
    )
}




