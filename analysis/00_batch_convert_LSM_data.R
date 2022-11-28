# Batch conversion of FLUXNET data to LSM formatting
# in line with the PLUMBER2 release
#library(FluxDataKit)

lapply(list.files("R/","*.R", full.names = TRUE), function(file){
  source(file)
})

library(dplyr)

# Check for FluxnetLSM library
# should be installed but just in case
if(!require(devtools)){install.packages("devtools")}
if(!require(FluxnetLSM)){
  devtools::install_github("computationales/FluxnetLSM")
  }

# Renew install (for debugging purposes)
detach("package:FluxnetLSM", unload = TRUE)
library(FluxnetLSM)

# read in the fluxnetlsm meta data
# this is required by some FluxnetLSM functions
# as we are reprocessing data no holds bar we can
# set the Exclude parameter to FALSE
# (remaining plumber data which is not updated
# will be copied over from the original dataset
# so this parameter although required can be silently
# ignored).
fls_meta_data <- read.csv(
  system.file("extdata", "Site_metadata.csv", package = "FluxnetLSM"),
  header = TRUE,
  stringsAsFactors = FALSE
) |>
  mutate(
    Exclude = FALSE
  )

# read in the meta data as listed
# within FluxDataKit and
# rename the columns
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
  dplyr::select(
    sitename,
    lat,
    lon,
    elv,
    igbp_land_use
  ) |>
  rename(
    'SiteCode' = 'sitename',
    'SiteLatitude' = 'lat',
    'SiteLongitude' = 'lon',
    'SiteElevation' = 'elv',
    'IGBP_vegetation_short' = 'igbp_land_use'
  ) |>
  filter(
    !(SiteCode %in% !!fls_meta_data$SiteCode)
  )

# merge the two datasets
# using bind rows
final_data <- bind_rows(fls_meta_data, sites)

# write data to file, this is the amended
# meta-data required for successful processing of the
# flux data (this includes some columns which aren't
# provided in the original meta-data or have the wrong name)
write.csv(final_data, file = file.path(tempdir(), "meta_data.csv"))

# read in all site meta-data, only test on
# SE-Nor to debug FluxnetLSM for now
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
  mutate(
    data_path = "/scratch/FDK_inputs/flux_data/"
  ) |>
  filter(
    sitename == "FR-Fon"
  ) |>
  mutate(
    product = "plumber"
  )

#---- FluxnetLSM reprocessing routine ----

# process all sites, by calling the processing routine
# all data is returned to the specified output path (out_path)
fdk_process_lsm(
  sites,
  out_path = "/data/scratch/PLUMBER_X/lsm/",
  modis_path = "/scratch/FDK_inputs/modis/",
  format = "lsm",
  site_csv_file = file.path(tempdir(), "meta_data.csv"),
  overwrite = TRUE,
  save_tmp_files = FALSE
)


# quick check
orig <- fdk_convert_lsm(
  site = "FR-Fon",
  path = "data-raw/flux_data/plumber/"
  )

df <- fdk_convert_lsm(
  site = "FR-Fon",
  path = "/data/scratch/PLUMBER_X/lsm/"
)

plot(orig$time,orig$LAI)
# points(df$time,df$LAI_plumber, col = "blue")
# points(df$time,df$LAI, col = "red")
