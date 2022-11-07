#' Format and run p-model drivers on Euler
#'
#' Takes site information for a single
#' or multiple sites and grabs all data
#' required for a p-model run in rsofun.
#'
#' Parameter settings are provided as
#' arguments, but could be altered after
#' the fact if desired.
#'
#' @param site_info data frame using minimum information required
#' being five columns: sitename, lon, lat, year_start, year_end
#' @param params_siml simulation parameters (preset)
#' @param params_modl model parameters (preset)
#' @param df_soiltexture soil data specifics (preset)
#' @param product which flux product to use
#' @param verbose provide verbose output (default = FALSE)
#'
#' @return returns an rsofun compatible driver file for the provided
#'  sites

fdk_format_drivers <- function(
  site_info,
  params_siml = list(
    spinup             = TRUE,  # to bring soil moisture to steady state
    spinupyears        = 10,    # 10 is enough for soil moisture.
    recycle            = 1,     # number of years recycled during spinup
    soilmstress        = FALSE, # soil moisture stress function is included
    tempstress         = FALSE, # temperature stress function is included
    calc_aet_fapar_vpd = FALSE, # set to FALSE - should be dropped again
    in_ppfd            = TRUE,  # if available from forcing files, set to TRUE
    in_netrad          = FALSE, # if available from forcing files, set to TRUE
    outdt              = 1,
    ltre               = FALSE,
    ltne               = FALSE,
    ltrd               = FALSE,
    ltnd               = FALSE,
    lgr3               = TRUE,
    lgn3               = FALSE,
    lgr4               = FALSE
  ),
  params_modl = list(
    kphio           = 0.09423773,
    soilm_par_a     = 0.33349283,
    soilm_par_b     = 1.45602286
  ),
  df_soiltexture = bind_rows(
    top    = tibble(
      layer = "top",
      fsand = 0.4,
      fclay = 0.3,
      forg = 0.1,
      fgravel = 0.1),
    bottom = tibble(
      layer = "bottom",
      fsand = 0.4,
      fclay = 0.3,
      forg = 0.1,
      fgravel = 0.1)
  ),
  freq = "hh",
  path,
  verbose = TRUE
  ){

  #---- start-up checks ----

  # check format of the site_info
  names(site_info) %in% c("sitename","lon","lat","start_year","end_year","elv")

  #---- complement site_info with WHC based on S_CWDX80 ----

  # some feedback on the processing
  if(verbose) {
    message("Processing WHC data ....")
  }

  # don't include water holding capacity
  # via cwdx80 if not on Euler, Balder or Dash
  if(grepl('eu-', Sys.info()['nodename'])) {

    if(verbose) {
      message("Processing cwdx80 data on server ....")
    }

      filn <- "data-raw/ancillary_data/cwdx80/cwdx80.nc"
      site_info <- site_info |>
        left_join(rbeni::extract_nc(
          dplyr::select(site_info,
                        sitename,
                        lon,
                        lat),
          filn) |>
            unnest(data) |>
            rename(whc = V1),
          by = c("sitename", "lon", "lat")
        )

    # median values
    whc_median <- median(site_info$whc, na.rm = TRUE)

    # append info
    site_info <- site_info |>
      mutate(
        whc = ifelse(
          is.na(whc),
          whc_median,
          whc
          )
        )
  } else {
    # dummy variable
    whc_median <- NA
  }

  #---- grabbing data environmental data from FLUX archives ----

  # some feedback on the processing
  if(verbose){
    message("Processing FLUX data ....")
  }

  settings_fluxnet <- list(
    getswc       = FALSE,
    filter_ntdt  = TRUE,
    threshold_GPP = 1,
    remove_neg   = FALSE
  )

  message("processing flux data using {ingestr}")
  ddf_flux <- ingestr::ingest(
    siteinfo = site_info |> slice(1:3),
    source   = "fluxnet",
    getvars  = list(
      gpp = "GPP_DT_VUT_REF",
      gpp_unc = "GPP_DT_VUT_SE",
      temp = "TA_F_MDS",
      prec = "P_F",
      vpd = "VPD_F_MDS",
      patm = "PA_F",
      ppfd = "SW_IN_F_MDS",
      netrad = "NETRAD",
      wind = "WS",
      co2_air = "CO2_F_MDS",
      lai = "LAI",
      fpar = "FPAR"
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
    timescale = freq
  )

  # GPP conversion factor
  # in FLUXNET given in umolCO2 m-2 s-1. converted to gC m-2 d-1
  c_molmass <- 12.0107  # molar mass of C
  gpp_coversion <- 1e-6 * 60 * 60 * 24 * c_molmass

  #----- convert dates ----

  data <- ddf_flux$data[[1]]
  ddf_flux$data[[1]] <- data |>
    mutate(
      date_time = date,
      date = as.Date(date)
    )

  #---- Processing CRU data (for cloud cover CCOV) ----
  if(verbose){
    message("Processing CRU data ....")
  }

  # adjust this with your local path
  # ddf_cru <- ingest(
  #   site_info = site_info,
  #   source    = "cru",
  #   getvars   = "ccov",
  #   dir       = "data-raw/ancillary_data/cru/",
  #   settings = list(correct_bias = NULL)
  # )

  # memory intensive, purge memory
  gc()

  #---- Merging climate data ----
  if(verbose){
    message("Merging climate data ....")
  }

  # merge all climate drivers into
  # one format
  ddf_flux$data[[1]]$ccov <- 1
  ddf_flux$data[[1]]$snow <- 0

  # ddf_meteo <- ddf_flux |>
  #   tidyr::unnest(data) |>
  #   left_join(
  #     ddf_cru |>
  #       tidyr::unnest(data),
  #     by = c("sitename", "date")
  #   ) |>
  #   group_by(sitename) |>
  #   tidyr::nest()


  #---- Append CO2 data ----
  #
  # CHECK SOURCE DATA
  if(verbose){
    message("Append CO2 data ....")
  }

  # grab the CO2 data matching date ranges
  # df_co2 <- ingest(
  #   site_info,
  #   source  = "co2_cmip",
  #   verbose = FALSE,
  #   dir = "data-raw/ancillary_data/co2/"
  # )

  # rename loess column to fapar as required
  # for input below
  df_co2 <- ddf_flux |>
    dplyr::mutate(
      data = purrr::map(data, ~ dplyr::mutate(., co2 = co2_air))
    )

  #---- Append FAPAR data ----

  if(verbose){
    message("Append FAPAR data ....")
  }

  # rename loess column to fapar as required
  # for input below
  df_fapar <- ddf_flux |>
    dplyr::mutate(
      data = purrr::map(data, ~ dplyr::mutate(., fapar = fpar))
    )

  #---- Format p-model driver data ----
  if(verbose){
    message("Combining all driver data ....")
  }

  output <- fdk_collect_drivers(
    site_info      = site_info,
    params_siml    = params_siml,
    meteo          = ddf_flux,
    fapar          = df_fapar,
    co2            = df_co2,
    params_soil    = df_soiltexture
  )

  # return data, either a driver
  # or processed output
  return(output)
}
