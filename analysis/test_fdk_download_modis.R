library(dplyr)
library(tidyr)
library(FluxnetLSM)
source("R/fdk_process_lsm.R")
source("R/fdk_download_modis.R")
source("R/fdk_smooth_ts.R")

# select datasets / sites to process
datasets <- c("fluxnet2015","icos","oneflux")
sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  filter(
    product != "plumber",
    sitename == "AT-Neu"
  )

df_modis <- readRDS("data/modis.rds")

#----- QC screening -----

# apply scaling factors, ignore if not available
df_modis <- df_modis |>
  dplyr::mutate(
    value = ifelse(!is.na(as.numeric(scale)),
                   value * as.numeric(scale),
                   value),
    calendar_date = as.Date(calendar_date)
  )

df_modis <- df_modis |>
  tidyr::pivot_wider(
    id_cols = c(site, calendar_date, pixel),
    names_from = band,
    values_from = value
  )

df_modis <- df_modis |>
  dplyr::rename(
    'lai' = 'Lai_500m',
    'sd_lai' = 'LaiStdDev_500m',
    'sd_fpar' = 'FparStdDev_500m',
    'fpar' = 'Fpar_500m',
    'qc' = 'FparLai_QC'
  )

# Extracting pixels in the centre and immediately around it (*)
# These correspond to a radius of 500m around site coordinates
pixel_no <- c(7, 8, 9,
              12, 13, 14,
              17, 18, 19)

# Use only good quality data
# Random bit integer format, ask Martin if need to work these out again...
qc_flags <- c(0, 2, 24 ,26, 32, 34, 56, 58)

df_modis <- df_modis |>
  filter(
    qc %in% qc_flags,
    pixel %in% pixel_no
  ) |>
  mutate(
    lai = ifelse(sd_lai < 0.01, NA, lai),
    lai = ifelse(lai > 10, NA, lai),
    fpar = ifelse(sd_fpar < 0.01, NA, fpar),
    fpar = ifelse(fpar > 1, NA, fpar)
  ) |>
  na.omit()

#---- Apply weighted mean ----

df_modis_mean <- df_modis |>
  group_by(site, calendar_date) |>
  mutate(
    weights_lai = (1/sd_lai^2)/sum(1/sd_lai^2),
    weights_fpar = (1/sd_fpar^2)/sum(1/sd_fpar^2),
  ) |>
  dplyr::summarize(
    lai = stats::weighted.mean(lai, w = weights_lai, na.rm = TRUE),
    fpar = stats::weighted.mean(fpar, w = weights_fpar, na.rm = TRUE)
  )

#---- expand dates ----

min_year <- min(as.numeric(format(df_modis_mean$calendar_date, "%Y")))
max_year <- max(as.numeric(format(df_modis_mean$calendar_date, "%Y")))

dates <- seq.Date(
  as.Date(sprintf("%s-01-01", min_year)),
  as.Date(sprintf("%s-12-31", max_year)),
  by = "day"
)

dates <- data.frame(
  calendar_date = dates,
  idx = 1:length(dates)
)

df_modis_mean <- dplyr::left_join(
  dates,
  df_modis_mean
  )

fdk_smooth_ts(
  df_modis_mean$calendar_date,
  df_modis_mean$lai
)

