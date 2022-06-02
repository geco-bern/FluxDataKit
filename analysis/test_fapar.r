library(ingestr)

sites <- readRDS("data/flux_data_kit_site-info.rds") %>%
  filter(product == "oneflux")

sites <- sites[1,]

# settings_gee <- get_settings_gee(
#     bundle            = "modis_fpar",
#     python_path       = "/usr/bin/python3", # on linux
#     gee_path          = "./src/gee_subset/src/gee_subset",
#     data_path         = "data-raw/modis/",
#     method_interpol   = "loess",
#     keep              = TRUE,
#     overwrite_raw     = FALSE,
#     overwrite_interpol= TRUE
#   )
#
# # run the ingest routine
# df_modis <-
#       ingest(
#       sites,
#       source = "gee",
#       settings = settings_gee,
#       parallel = FALSE
#     )

path <- "data-raw/flux_data/oneflux/"

settings_fluxnet <- list(
  getswc       = FALSE,
  filter_ntdt  = TRUE,
  threshold_GPP = 1,
  remove_neg   = FALSE
)

ddf_flux <- ingest(
  siteinfo = sites,
  source   = "fluxnet",
  getvars  = list(
    gpp = "GPP_NT_VUT_REF",
    gpp_unc = "GPP_NT_VUT_SE",
    temp = "TA_F_MDS",
    prec = "P_F",
    vpd = "VPD_F_MDS",
    patm = "PA_F",
    ppfd = "SW_IN_F_MDS",
    netrad = "NETRAD",
    wind = "WS",
    co2_air = "CO2_F_MDS"
    #sw_up = "SW_OUT",
    #lw_down = "LW_IN_F_MDS",
    #le = "LE_F_MDS",
    #le_cor = "LE_CORR",
    #h = "H_F_MDS",
    #h_cor = "H_CORR",
    #g  = "G_F_MDS",
    #ustar = "USTAR"
  ),
  dir = path,
  settings = settings_fluxnet,
  timescale= "hh"
)

data <- ddf_flux$data[[1]]
bla <- data %>%
  mutate(
    date = as.Date(date)
  ) %>%
  group_by(date) %>%
  summarize(
    # Daily summary values, all quite
    # normal
    gpp = sum(gpp, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    tmin = min(temp, na.rm = TRUE),
    tmax = max(temp, na.rm = TRUE),
    prec = sum(prec, na.rm = TRUE),
    vpd = mean(vpd, na.rm = TRUE),
    patm = mean(patm, na.rm = TRUE),
    # radiation values are averages for
    # days with more than 50% of values
    # available
    l = length(netrad),
    lna = length(which(is.na(netrad))),
    netradt = mean(netrad, na.rm = TRUE),
    netrad = ifelse(
      length(which(!is.na(netrad)) > (length(netrad) * 0.5 )),
      mean(netrad, na.rm = TRUE),
      NA
    ),
    ppfd = ifelse(
      length(which(!is.na(ppfd)) > (length(ppfd) * 0.5 )),
      mean(ppfd, na.rm = TRUE),
      NA
    )
  )

plot(bla$netrad)
