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
    site = "CA-NS4"
  ) {

 # find all files
 era_files <-  list.files(path, glob2rx(paste0(site, "*.nc")), full.names = TRUE)

 if (length(era_files) > 0){

   # load in the data using terra
   r <- suppressWarnings(
     terra::rast(era_files)
   )

   # split out time, convert
   # time to dates
   time <- terra::time(r)

   # take the mean value by day
   r <- terra::tapp(r, "days", fun = "mean")

   # put into data frame
   df <- terra::values(r, dataframe = TRUE)
   date <- as.Date(names(df), "d_%Y.%m.%d")

   df <- data.frame(
     date = date,
     ccov = as.vector(unlist(df)),
     sitename = site
   )

 } else {
   # read CRU cloud cover data
   df <- readRDS(paste0(path, "df_cru.rds")) |>
     dplyr::filter(sitename == site) |>
     tidyr::unnest(data) |>
     dplyr::select(date, ccov, sitename)
 }

 # return
 return(df)
}
