# Determine longest good-quality sequence by site
library(tidyverse)
library(FluxDataKit)

path <- "/data_2/FluxDataKit/v3.4"

sites <- FluxDataKit::fdk_site_info |>
  filter(!(sitename %in% c("MX-Tes", "US-KS3")))

# # site subset------------------
# # xxx debug
# # chose representative sites for LES book
# use_sites <- c(
#   # "FI-Hyy", # Boreal Forests/Taiga
#   # "US-SRM", # Deserts & Xeric Shrublands
#   # "FR-Pue", # Mediterranean Forests, Woodlands & Scrub
#   # "DE-Hai", # Temperate Broadleaf & Mixed Forests
#   "IT-Ro1"
#   # "US-Tw1", # Temperate Grasslands, Savannas & Shrublands
#   # "AU-How", # Tropical & Subtropical Grasslands, Savannas & Shrubland
#   # "BR-Sa3", # Tropical
#   # "ZM-Mon", # Tropical deciduous forest (xeric woodland)
#   # "US-ICh"  # Tundra
# )
# sites <- sites |>
#   filter(sitename %in% use_sites)
# #----------------------------

# determine longest sequence of good-quality data for each site
list_seq <- lapply(sites$sitename, function(site){
  message(sprintf("Analysing %s ----", site))

  # get file name path
  filn <- list.files(
    file.path(path, "fluxnet"),
    pattern = paste0("FLX_", site, ".*_FULLSET_DD.*.csv"),
    recursive = TRUE
  )

  df <- read.csv(file.path(file.path(path, "fluxnet"), filn))

  df_seq <- suppressMessages(
    suppressWarnings(
      try(fdk_get_sequence(
        df,
        site = site,
        qc_threshold = 0.25,
        leng_threshold = 90,
        do_plot = TRUE,
        out_path = file.path(path, "plots")
      )
      )
    )
  )

  if(inherits(df_seq, "try-error")){
    message("!!! plotting failed !!!")
    return(NULL)
  }

  return(df_seq)
})


fdk_site_fullyearsequence <- bind_rows(list_seq)

# write CSV file
save(fdk_site_fullyearsequence,
     file = here::here("data/fdk_site_fullyearsequence.rda"),
     compress = "xz"
     )

# write CSV file for upload to Zenodo
readr::write_csv(
  fdk_site_fullyearsequence,
  file = "/data_2/FluxDataKit/v3.4/fdk_site_fullyearsequence.csv"
)
