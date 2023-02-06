#' Calculates data coverage
#'
#' Calculates data coverage for the half-hourly (HH) data.
#'
#' @param input_path path with half-hourly gap-filled plumber formatted data
#' @param output_path where to store the converted data if converted to csv
#' @param internal returns an internal data frame or a csv written to file
#' named 'data_coverage.csv' (either TRUE or FALSE)
#'
#' @return data frame or csv written to disk with data coverage statistics
#'  per site
#' @export

fdk_data_coverge <- function(
    input_path = "/data/scratch/beta-v3/lsm/",
    output_path,
    internal = TRUE
){

  min_nr <- function(x) {min(which(x != is.na(x)))}
  max_nr <- function(x) {max(which(x != is.na(x)))}

  # list all flux files
  files <- list.files(input_path, "*Flux.nc", full.names = TRUE)

  # grab their corresponding site name
  sites <- unlist(lapply(strsplit(basename(files),"_"),"[[",1))

  # loop over all the sites and read the flux data
  df <- lapply(sites, function(site){
    tmp <- suppressWarnings(try(fdk_convert_lsm(
      site = site,
      path = input_path,
      fluxnet_format = TRUE
        )
      )
    )

    # date vector
    date <- as.Date(strptime(tmp$TIMESTAMP_START, "%Y%m%d%H%M", tz = "GMT"))

    # remove QC/SE/ERA5 columns
    # these should not be indexed
    tmp <- tmp |>
      select(
        -ends_with("QC"),
        -ends_with("SE"),
        -ends_with("ERA"),
        -starts_with("TIME"),
      )

    tmp <- tmp |>
      summarise_all(
        list(
          start = min_nr,
          end = max_nr
        )
      ) |>
      mutate_all(
        function(x){date[x]}
      )

    tmp$sitename <- site
    return(tmp)
  })

  # merge all
  df <- dplyr::bind_rows(df)

  if(!internal){
    write.table(
      df,
      file = file.path(output_path, "data_coverage.csv"),
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      sep = ","
    )
  } else {
    return(df)
  }
}
