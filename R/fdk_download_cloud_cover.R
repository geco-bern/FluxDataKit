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

  df$filename <- paste0(df$sitename, "_", df$variable,"_", df$method,".csv")
  df$date_start <- paste0(df$year_start,"-01-01")
  df$date_end <- paste0(df$year_end,"-12-31")

  # download all drivers, use site info
  # to determine locality etc
  requests <- apply(df, 1, function(x){
    output <- fdk_era5_request(
      lon = as.numeric(x['lon']),
      lat = as.numeric(x['lat']),
      filename = x['filename'],
      start_date = x['date_start'],
      end_date = x['date_end']
    )
    return(output)
  })

  # download the data
  files <- ecmwfr::wf_request_batch(
    user = user,
    requests,
    time_out = 3 * 3600,
    workers = 2,
    path = path
  )

  if(inherits(files, "try-error")) {
    message("Cloud cover downloads failed!")
  }
}
