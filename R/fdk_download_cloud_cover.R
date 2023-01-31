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
    lapply(seq(as.numeric(x['year_start']), 2022, 5), function(year){

      filename <- paste0(
        x['sitename'],
        "_",
        year,
        ".csv"
        )

      output <- fdk_era5_request(
        lon = as.numeric(x['lon']),
        lat = as.numeric(x['lat']),
        filename = filename,
        start_date = paste0(as.numeric(year), "-01-01"),
        end_date = paste0(as.numeric(year) + 4, "-12-31")
      )
      return(output)
    })
  })

  requests <- do.call("rbind", requests)

  # download the data
  files <- ecmwfr::wf_request_batch(
    user = user,
    requests,
    workers = 2,
    path = path
  )

  if(inherits(files, "try-error")) {
    message("Cloud cover downloads failed!")
  } else {
    return(files)
  }
}
