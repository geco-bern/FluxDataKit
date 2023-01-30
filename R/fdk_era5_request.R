#' Download drivers from ECMWF
#'
#' Batch downloads ERA5 driver data from the ECMWF API (CDS Toolbox)
#'
#' @param user ECMWF Copernicus CDS user
#' @param lat latitude
#' @param lon longitude
#' @param var CDS variable of interest
#' @param product CDS product to query
#' @param filename output filename (CSV)
#' @param start_date start date as YYYY-MM-DD
#' @param end_date end date as YYYY-MM-DD
#' @param method aggregation method (mean, max, min, sum)
#'
#' @return A data frame with queried data including the date, position and
#' requested variable
#' @export

fdk_era5_request <- function(
    user,
    lat,
    lon,
    var,
    product,
    filename,
    start_date,
    end_date,
    method = 'mean'
) {

  # Embedded python workflow call
  code <- "
import cdstoolbox as ct

@ct.application()
@ct.output.download()
def daily_data(lon, lat, var, product, date, method):

    data = ct.catalogue.retrieve(
        product,
        {
          'variable': var,
          'area': [lat + 0.05, lon - 0.05, lat - 0.05, lon + 0.05],
          'product_type': 'reanalysis',
          'date' : date,
          'time': [
                '00:00', '01:00', '02:00',
                '03:00', '04:00', '05:00',
                '06:00', '07:00', '08:00',
                '09:00', '10:00', '11:00',
                '12:00', '13:00', '14:00',
                '15:00', '16:00', '17:00',
                '18:00', '19:00', '20:00',
                '21:00', '22:00', '23:00',
            ]
        }
      )

    if var == 'snowfall' or var == 'total_precipitation' :
      # Convert snowfall from flux (m/s) to hourly accumulated
      # column of water/snow (mm)
      data = data * 3600 * 1000

    data_resampled = ct.cube.resample(data, freq='day', dim='time', how=method)
    data_sel = ct.geo.extract_point(data_resampled, lon=lon, lat=lat)

    return ct.cdm.to_csv(data_sel)
"
  # A query for 2m surface temperature
  request <- list(
    code = code,
    kwargs = list(
      lon = lon,
      lat = lat,
      var = var,
      product = product,
      date = paste(start_date, end_date, sep = "/"),
      method = method
    ),
    workflow_name = "daily_data",
    target = filename
  )

  return(request)
}
