# Should be run as a job
# or a script on the command line

# load required libraries
library(ingestr)

# set sites
sites <- readRDS("data/flux_data_kit_site-info.rds")
sites$year_start <- 2000
sites$year_end <- as.numeric(format(Sys.Date(),"%Y"))

bundles <-
  c(
    "modis_lst_aqua",
    "modis_lst_terra",
    "modis_fpar",
    "modis_lai",
    "modis_gpp",
    "modis_refl_1",
    "modis_refl_2",
    "modis_refl_3",
    "modis_refl_4",
    "modis_refl_5",
    "modis_refl_6",
    "modis_refl_7",
    "modis_refl_8",
    "modis_refl_9",
    "modis_refl_10",
    "modis_refl_11",
    "modis_refl_12",
    "modis_refl_13",
    "modis_refl_14",
    "modis_refl_15",
    "modis_refl_16"
  )

lapply(bundles, function(bundle){
  # feedback
  message("processing MODIS data product: ")

  # feedback
  message(paste("  - ", bundle))

  # grab settings
  settings_modis <- get_settings_modis(
    bundle            = bundle,
    data_path         = "data-raw/modis/",
    method_interpol   = "loess",
    keep              = TRUE,
    overwrite_raw     = FALSE,  # not too costly when using networks
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

  return(invisible())
})
