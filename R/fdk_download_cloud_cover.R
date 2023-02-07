#' Download ERA5 cloud cover data
#'
#' Download ERA5 cloud cover data from the ECMWF Copernicus Data Service
#'
#' @param df data frame with site info
#' @param path path where to store the ERA5 data
#' @param user ECMWF CDS user name
#'
#' @return raw cloud cover data saved to a desired path
#' @export

fdk_download_cloud_cover <- function(
    df,
    user,
    path = "data-raw/cloud_cover/"
) {

  #----- settings and startup -----

  # download all drivers, use site info
  # to determine locality etc
  requests <- apply(df, 1, function(x){

    increment <- 3
    increment_l <- (increment - 1)

    req <- lapply(seq(as.numeric(x['year_start']), 2022, increment), function(year){

      filename <- paste0(
        x['sitename'],
        "_",
        year,
        ".nc"
        )

      # trap end date exception
      if(as.numeric(year) + increment_l > 2022){
        end_date <- "2022-12-31"
      } else {
        end_date <- paste0(as.numeric(year) + increment_l, "-12-31")
      }

      output <- fdk_era5_request(
        lon = as.numeric(x['lon']),
        lat = as.numeric(x['lat']),
        filename = filename,
        start_date = paste0(as.numeric(year), "-01-01"),
        end_date = end_date
      )
      return(output)
    })
    return(req)
  })

  requests <- unlist(requests, recursive=FALSE)

  # download the data
  files <- lapply(requests, function(request){

    files <- ecmwfr::wf_request(
      user = user,
      request,
      time_out = 3600 * 4,
      path = path
    )

    if(inherits(files, "try-error")) {
      message("Cloud cover downloads failed!")
      return(NULL)
    } else {
      return(files)
    }
  })

  return(files)
}
