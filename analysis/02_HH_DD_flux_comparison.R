# Compare averaged GPP from HH with
# calculated daily values

library(tidyverse)


# read in the meta data as listed
# within FluxDataKit and
# rename the columns
sites <- readRDS("data/flux_data_kit_site-info.rds") |>
  filter(
    product == "fluxnet2015"
  )


lapply(sites$sitename[50], function(site){

  site_files <- list.files(
    "data-raw/flux_data/fluxnet2015/",
    site,
    full.names = TRUE
    )

  HH <- read.table(
    site_files[grep("FULLSET_HH", site_files)],
    header = TRUE,
    sep = ","
    ) |>
    mutate(
      date_time = strptime(TIMESTAMP_START, "%Y%m%d%H%M"),
      date = as.Date(date_time)
    ) |>
    dplyr::select(
      date,
      date_time,
      GPP_NT_VUT_REF,
      GPP_DT_VUT_REF
    ) |>
    group_by(date) |>
    summarize(
      GPP_NT_VUT_REF_HH = mean(GPP_NT_VUT_REF, na.rm = TRUE),
      GPP_DT_VUT_REF_HH = mean(GPP_DT_VUT_REF, na.rm = TRUE)
    )

  DD <- read.table(
    site_files[grep("FULLSET_DD", site_files)],
    header = TRUE,
    sep = ","
  ) |>
    mutate(
      date = strptime(TIMESTAMP, "%Y%m%d"),
    ) |>
    dplyr::select(
      date,
      GPP_NT_VUT_REF,
      GPP_DT_VUT_REF
    )

  DD <- left_join(DD, HH)

  par(mfrow=c(3,2))

  plot(DD$GPP_NT_VUT_REF, DD$GPP_NT_VUT_REF_HH, main = paste(site, "GPP NT"))
  points(DD$GPP_NT_VUT_REF, DD$GPP_NT_VUT_REF_HH_M, col = "blue")
  abline(0,1)

  plot(DD$GPP_DT_VUT_REF, DD$GPP_DT_VUT_REF_HH, main = paste(site, "GPP DT"))
  points(DD$GPP_DT_VUT_REF, DD$GPP_DT_VUT_REF_HH_M, col = "blue")
  abline(0,1)

  plot(DD$date, DD$GPP_NT_VUT_REF)
  points(DD$date, DD$GPP_NT_VUT_REF_HH, col = "red")

  plot(DD$date, DD$GPP_DT_VUT_REF)
  points(DD$date, DD$GPP_DT_VUT_REF_HH, col = "red")

  plot(DD$date, DD$GPP_NT_VUT_REF  - DD$GPP_NT_VUT_REF_HH, main = "delta GPP")
  plot(DD$date, DD$GPP_DT_VUT_REF  - DD$GPP_DT_VUT_REF_HH, main = "delta GPP")

})
