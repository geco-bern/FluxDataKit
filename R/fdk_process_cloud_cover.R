#' Process ERA5 cloud cover data
#'
#' Process cloud cover data to return daily cloud cover values
#' for a given date range and (flux) site.
#'
#' @param path path with the ERA5 cloud cover data, by site
#' @param site site name to process
#' @param start_date start date of a data series
#' @param end_date end date of a data series
#'
#' @return daily mean cloud cover value (0-1), for a given date range
#' @export

fdk_process_cloud_cover <- function(
    path = "data-raw/cloud_cover/",
    site,
    start_date,
    end_date
  ) {



}
