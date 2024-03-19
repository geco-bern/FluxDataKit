#' Download request for drivers from ECMWF
#'
#' Batch request for ERA5 driver data from the ECMWF API (CDS Toolbox)
#'
#' @param lat latitude
#' @param lon longitude
#' @param filename output filename (CSV)
#' @param start_date start date as YYYY-MM-DD
#' @param end_date end date as YYYY-MM-DD
#'
#' @return A data frame with queried data including the date, position and
#' requested variable
#' @export

fdk_era5_request <- function(
    lat,
    lon,
    filename,
    start_date,
    end_date
) {

  request <- list(
    product_type = "reanalysis",
    format = "netcdf",
    variable = "total_cloud_cover",
    date = paste(start_date, end_date, sep = "/"),
    area = c(lat + 0.05, lon - 0.05, lat - 0.05, lon + 0.05),
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00",
             "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00",
             "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00",
             "21:00", "22:00", "23:00"),
    dataset_short_name = "reanalysis-era5-single-levels",
    target = filename
  )

  return(request)
}
