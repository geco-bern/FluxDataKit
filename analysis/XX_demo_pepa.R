# Shows the conversion and the downsampling
# of data to a daily timestep in a FLUXNET
# format (from land surface model netcdf files
# as currently listed in the Zenodo archive)
#
# This demo uses data internal to the package
# for testing purposes (the smallest dataset
# available so data is limited)

# load library
library(FluxDataKit)

# site included for demo purposes
site <- "US-Wi7"

# read in data from package directory
# this is LSM (netcdf) data which has
# been gap filled already (not raw FLUXNET data)
df <- FluxDataKit::fdk_convert_lsm(
  site = site,
  path = system.file("extdata", package = "FluxDataKit"),
  fluxnet_format = TRUE
)

# Now downsample the data
df <- FluxDataKit::fdk_downsample_fluxnet(
  df,
  site = site
)

# print some values, this is the SE
# on the daily values (not propagating)
# the standard error which was calculated
# on the half-hourly values
print(df$GPP_DT_VUT_SE)
