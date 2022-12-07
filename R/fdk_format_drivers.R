#' Format p-model drivers
#'
#' Takes site information for a single
#' or multiple sites and grabs all data
#' required for a p-model run in rsofun.
#'
#' Parameter settings are provided as
#' arguments, but could be altered after
#' the fact if desired.
#'
#' NOTE: Processing is selective and will
#' create different files on non-GECO workstations.
#'
#' @param site_info data frame using minimum information required
#' being five columns: sitename, lon, lat, year_start, year_end
#' @param params_siml simulation parameters (preset)
#' @param params_modl model parameters (preset)
#' @param df_soiltexture soil data specifics (preset)
#' @param product which flux product to use
#' @param verbose provide verbose output (default = FALSE)
#' @param path path with daily FLUXNET data
#'
#' @return returns an rsofun compatible driver file for the provided
#'  sites
#' @export

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
  path,
  verbose = TRUE
  ){

  #---- start-up checks ----

  geco_system <- ifelse(
    Sys.info()['nodename'] == "balder" | Sys.info()['nodename'] == "dash",
    TRUE,
    FALSE
    )

  # check format of the site_info
  names(site_info) %in% c("sitename","lon","lat","start_year","end_year","elv")

  lapply(site_info$sitename, function(site){

  #---- complement site_info with WHC based on S_CWDX80 ----

  # TBD


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
  ddf_flux <- suppressWarnings(
    ingestr::ingest(
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
        fpar = "FPAR",
        le = "LE_F_MDS",
        le_qc = "LE_F_MDS_QC",
        le_cor = "LE_CORR",
        le_cor_qc = "LE_CORR_QC"
        # gpp_qc = "GPP_DT_VUT_REF_QC"
        #lw_down = "LW_IN_F_MDS",
        #h = "H_F_MDS",
        #h_cor = "H_CORR",
        #g  = "G_F_MDS",
        #ustar = "USTAR"
      ),
      dir = path,
      settings = settings_fluxnet,
      timescale = "d"
    )
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

  # set snow to 0 (data not available,
  # but tested for in rsofun)
  ddf_flux$data[[1]]$snow <- 0

  #---- Processing CRU data (for cloud cover CCOV) ----
  if(verbose){
    message("Processing CRU data ....")
  }

  if (geco_system){
    ddf_cru <- ingest(
      siteinfo = site_info,
      source    = "cru",
      getvars   = "ccov",
      dir       = "/data/archive/cru_NA_2021/data/", # f-ing trailing /
      settings = list(correct_bias = NULL)
    )
  } else {
    ddf_flux$data[[1]]$ccov <- 0
  }
  # memory intensive, purge memory
  gc()

  #---- Merging climate data ----
  if(verbose){
    message("Merging climate data ....")
  }

  if (geco_system) {
    ddf_flux <- ddf_flux |>
      tidyr::unnest(data) |>
      left_join(
        ddf_cru |>
          tidyr::unnest(data),
        by = c("sitename", "date")
      ) |>
      group_by(sitename) |>
      tidyr::nest()
  }

  #---- Append CO2 data ----

  if(verbose){
    message("Append CO2 data ....")
  }

  # use in situ co2_air measurements rather than
  # global values
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
  })



}
