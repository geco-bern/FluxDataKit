# Updates MODIS subsets on Euler
#
# This is run for Ameriflux data
# but can be appended with other datasets
#
# Assumes that your local (home dir)
# data directory links to the CES Euler
# data repository as mentioned in the notions.co
# setup page for Euler
#

# update ingestr
devtools::install_github("bluegreen-labs/ingestr")

# load required libraries
library(ingestr)

# bail if not on euler
if(!grepl('eu-', Sys.info()['nodename'])){
  stop("You are not on Euler, source data unavailable - abort abort abort!")
}

# grab fluxnet2015 site information
sites <- ingestr::siteinfo_fluxnet2015

# list all products to download
bundles <- c(
  "modis_lst"
  #"modis_fpar",
  #"modis_evi",
  #"modis_ndvi"
)

# feedback
message("processing MODIS data product: ")

# loop over all bundles
modis <- lapply(bundles, function(bundle) {

  # feedback
  message(paste("  - ", bundle))

  # grab settings
  settings_modis <- get_settings_modis(
    bundle            = bundle,
    data_path         = "~/data/modis_subsets/",
    method_interpol   = "loess",
    keep              = TRUE,
    overwrite_raw     = TRUE,  # not too costly when using networks
    overwrite_interpol= TRUE,
    n_focal           = 0,
    network           = "FLUXNET"
  )

  # run the ingest routine
  df_modis <- ingest(
    sites,
    source = "modis",
    settings = settings_modis,
    parallel = FALSE
  )

  # return status stuff
  return(df_modis)
})
