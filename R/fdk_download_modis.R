#' Download MODIS LAI/FPAR
#'
#' Downloads and smooths MODIS LAI/FPAR values
#' for merging into the final LSM model data.
#' Smoothing interpolates values to an half-hourly
#' time step.
#'
#' @param df data frame with site info
#' @param path path where to store the MODIS data
#' @param force force a new download even if the data exists in the
#'  output path
#'
#' @return raw MODIS data saved to a desired path
#' @export

fdk_download_modis <- function(
    df,
    path,
    force = FALSE
) {

  #----- settings and startup -----

  apply(df, 1, function(x){

    # Exception for US-ORv, wetland site with no MODIS LAI available
    if (x['sitename'] == "US-ORv") {return(NULL)}

    # extract the range of the data to consider
    # and where required extrapolate to missing
    # years
    start_year <- as.numeric(x['year_start'])
    end_year <- as.numeric(x['year_end'])

    # set products and band names for the
    # download
    product <- "MCD15A2H"
    bands <- c(
      "Lai_500m",
      "LaiStdDev_500m",
      "FparStdDev_500m",
      "Fpar_500m",
      "FparLai_QC"
    )

    #----- data download -----

    filename <- file.path(path, paste0(x['sitename'], "_MODIS_data.csv"))

    # Check if data exists, if not download
    # override with force (to force a new download)
    if(file.exists(filename)){
      if(!force){
        message(paste0("The file: ", filename, " exists, skipping!
                       Use force = TRUE to redownload existing files."))
        return(NULL)
      }
    }

    # downloading data
    df_modis <- try(
      MODISTools::mt_subset(
        site_name = as.character(x['sitename']),
        lat = x['lat'],
        lon = x['lon'],
        product = product,
        band = bands,
        start = "2000-01-01",
        end = format(Sys.time(), "%Y-%m-%d"),
        km_lr = 1,
        km_ab = 1,
        internal = TRUE
      )
    )

    if(inherits(df_modis, "try-error") ) {
      warning("MODIS data download failed")
      return(NULL)
    }

    # write data to file
    write.table(
      df_modis,
      filename,
      col.names = TRUE,
      row.names = FALSE,
      quote = FALSE,
      sep = ","
    )

  })
}
