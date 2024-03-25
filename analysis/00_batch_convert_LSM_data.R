# Batch conversion of FLUXNET data to LSM formatting
# in line with the PLUMBER2 release
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)
lapply(list.files("R/","*.R", full.names = TRUE), source)
library(FluxDataKit)
library(FluxnetLSM)
library(dplyr)
library(ingestr)
library(rsofun)

input_path <- "~/data/FluxDataKit/FDK_inputs"  # "/data/scratch/FDK_inputs"
output_path <- "~/data/FluxDataKit/v3"  #  "/data/scratch/beta-v4"

sites <- FluxDataKit::fdk_site_info |>
  mutate(
    data_path = file.path(input_path, "flux_data/")
  )
  # filter(sitename %in% failed_sites)

# sites <- sites |>
#   filter(sitename == "CH-Lae")
# df <- readr::read_csv("~/data/FluxDataKit/FDK_inputs/flux_data/ameriflux/AMF_US-Ha1_FLUXNET_FULLSET_HR_1991-2020_3-5.csv")
# df <- readr::read_csv("~/data/FluxDataKit/FDK_inputs/flux_data/icos_warmwinter2020/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2020_beta-3.csv")
# plot(df$LE_F_MDS_QC)

#---- create a new release ----

fdk_release(
  df = sites,
  input_path = input_path,
  output_path = output_path,
  overwrite_lsm = FALSE,
  overwrite_fluxnet = FALSE
)

#---- create matching plots ----

# loop over all sites and plot all time series
failed_sites <- lapply(sites$sitename, function(site){
  message(sprintf("Processing %s ----", site))

  message("- converting to FLUXNET format")
  df <- suppressWarnings(try(fdk_convert_lsm(
    site = site,
    fluxnet_format = TRUE,
    path = file.path(output_path, "lsm"),
    overwrite = FALSE
  )
  ))

  if(inherits(df, "try-error")){
    message("!!! conversion to FLUXNET failed  !!!")
    return(site)
  }

  message("- plotting HH FLUXNET data")
  filename <- suppressMessages(
    suppressWarnings(
      try(fdk_plot(
        df,
        site = site,
        out_path = file.path(output_path, "plots"),
        overwrite = FALSE
      )
      )
    )
  )

  message("- plotting DD FLUXNET data")
  # get file name path
  filn <- list.files(
    file.path(output_path, "fluxnet"),
    pattern = paste0("FLX_", site, ".*_FULLSET_DD.*.csv"),
    recursive = TRUE
    )

  df <- read.csv(file.path(file.path(output_path, "fluxnet"), filn))

  filename <- suppressMessages(
    suppressWarnings(
      try(fdk_plot(
        df,
        site = site,
        out_path = file.path(output_path, "plots"),
        overwrite = TRUE,
        daily = TRUE
      )
      )
    )
  )

  if(inherits(filename, "try-error")){
    message("!!! plotting failed !!!")
    return(site)
  }

  return(NULL)
})

saveRDS(unlist(failed_sites), here::here("data/failed_sites.rds"))

