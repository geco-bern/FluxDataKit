# Should be run as a job
# or a script on the command line

# load required libraries (refresh local install)
try(detach("package:ingestr", unload = TRUE))
library(ingestr)

# set sites
sites <- readRDS("data/flux_data_kit_site-info.rds")
sites$year_start <- 2000
sites$year_end <- as.numeric(format(Sys.Date(), "%Y"))

bundles <-
  c(
    "modis_fpar",
    "modis_lai"
  )

lapply(bundles, function(bundle){

  # feedback
  message("Downloading MODIS data: ")

  # feedback
  message(paste("  - ", bundle))

  settings_modis <- get_settings_modis(
    bundle = bundle,
    data_path = "data-raw/modis/test/",
    method_interpol   = "linear",
    keep              = FALSE,
    overwrite_raw     = FALSE,
    overwrite_interpol= TRUE
  )

  # run the ingest routine
  df_modis <-
      try(ingest(
      sites,
      source = "modis",
      settings = settings_modis,
      parallel = FALSE
    ))

  return(invisible())
})
