
#' Rename remote sensing variables
#'
#' Rename remote sensing variables in the ancillary netcdf data
#' as created by FluxnetLSM.
#'
#' @param infile netcdf file (path) holding the ERA and remote
#'  sensing data
#'
#' @return renames netcdf MODIS data variables
#' @export


fdk_correct_rs <- function(
    infile
) {

  # Open file handle
  nc_out <- ncdf4::nc_open(infile, write=TRUE)

  # Get variable names
  vars <- names(nc_out$var)

  # First check that LAI data is available
  modis_vars <- vars[which(grepl("_MODIS", vars))]

  # Should have two available, check that they are there
  # Koen: not applicable anymore, only including LAI and FPAR
  if (length(modis_vars) != 2) {
    warning("LAI variables not available, check site input file ")
  } else {

    # MODIS
    default_lai    <- "LAI_MODIS"
    default_fpar <- "FPAR_MODIS"
    default_source <- "MODIS"

    alt_lai    <- "LAI_Copernicus"
    alt_source <- "Copernicus"

    # Rename LAI
    if("LAI" %in% vars){
      nc_out <- ncdf4::ncvar_rename(nc_out, "LAI", "LAI_plumber")
    }

    nc_out <- ncdf4::ncvar_rename(nc_out, default_lai, "LAI")
    nc_out <- ncdf4::ncvar_rename(nc_out, default_fpar, "FPAR")

    #Add source in attribute data
    ncdf4::ncatt_put(
      nc = nc_out,
      varid = "LAI",
      attname = "source",
      attval = default_source
    )

    ncdf4::ncatt_put(
      nc = nc_out,
      varid = "FPAR",
      attname = "source",
      attval = default_source
    )

    # Close file handle
    ncdf4::nc_close(nc_out)
  }
}
