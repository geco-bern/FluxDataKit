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
#' @param siteinfo data frame using minimum information required
#' being five columns: sitename, lon, lat, year_start, year_end
#' @param params_siml simulation parameters (preset)
#' @param params_modl model parameters (preset)
#' @param df_soiltexture soil data specifics (preset)
#' @param product which flux product to use
#' @param verbose provide verbose output (default = FALSE)
#'
#' @return returns an rsofun compatible driver file for the provided
#'  sites

format_drivers_site <- function(
  siteinfo,
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
  product,
  verbose = TRUE
  ){

  #---- start-up checks ----

  # check format of the siteinfo
  names(siteinfo) %in% c("sitename","lon","lat","start_year","end_year","elv")

  #---- complement siteinfo with WHC based on S_CWDX80 ----

  # some feedback on the processing
  if(verbose){
    message("Processing WHC data ....")
  }

  # bail if not on euler
  if(grepl('eu-', Sys.info()['nodename'])){

    if(verbose){
      message("Processing cwdx80 data on server ....")
    }

      filn <- "data-raw/ancillary_data/cwdx80/cwdx80.nc"
      siteinfo <- siteinfo %>%
        left_join(rbeni::extract_nc(
          dplyr::select(siteinfo,
                        sitename,
                        lon,
                        lat),
          filn) %>%
            unnest(data) %>%
            rename(whc = V1),
          by = c("sitename", "lon", "lat")
        )

    # median values
    whc_median <- median(siteinfo$whc, na.rm = TRUE)

    # append info
    siteinfo <- siteinfo %>%
      mutate(
        whc = ifelse(
          is.na(whc),
          whc_median,
          whc
          ))
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

  if (product == "oneflux"){
    path = "data-raw/flux_data/oneflux/"
  }

  if (product == "icos"){
    path = "data-raw/flux_data/icos/"
  }

  if (product == "plumber"){
    path = "data-raw/flux_data/plumber_fluxnet/"
  }

  if (product == "ameriflux"){
    path = "data-raw/flux_data/fluxnet2015/"
  }

  ddf_flux <- ingest(
    siteinfo = siteinfo %>% slice(1:3),
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

  #----- Calculate daily values from half-hourly measurements ----

  if (freq != "hh"){
    data <- ddf_flux$data[[1]]
    ddf_flux$data[[1]] <- data %>%
      mutate(
        date = as.Date(date)
      ) %>%
      group_by(date) %>%
      summarize(
        gpp = sum(gpp, na.rm = TRUE),
        temp = mean(temp, na.rm = TRUE),
        tmin = min(temp, na.rm = TRUE),
        tmax = max(temp, na.rm = TRUE),
        prec = sum(prec, na.rm = TRUE),
        vpd = mean(vpd, na.rm = TRUE),
        patm = mean(patm, na.rm = TRUE),
        netrad = mean(netrad[netrad > 0], na.rm = TRUE),
        ppfd = mean(ppfd[ppfd > 0], na.rm = TRUE)
      )
  } else {
    data <- ddf_flux$data[[1]]
    ddf_flux$data[[1]] <- data %>%
      mutate(
        date_time = date,
        date = as.Date(date)
      )
  }

  if (freq != "hh"){

    #---- Processing CRU data (for cloud cover CCOV) ----
    if(verbose){
      message("Processing CRU data ....")
    }

    # adjust this with your local path
    ddf_cru <- ingest(
      siteinfo = siteinfo,
      source    = "cru",
      getvars   = "ccov",
      dir       = "data-raw/ancillary_data/cru/",
      settings = list(correct_bias = NULL)
    )

    # memory intensive, purge memory
    gc()

    #---- Merging climate data ----
    if(verbose){
      message("Merging climate data ....")
    }

    # merge all climate drivers into
    # one format
    ddf_meteo <- ddf_flux %>%
      tidyr::unnest(data) %>%
      left_join(
        ddf_cru %>%
          tidyr::unnest(data),
        by = c("sitename", "date")
      ) %>%
      group_by(sitename) %>%
      tidyr::nest()

    #---- Append CO2 data ----
    #
    # CHECK SOURCE DATA
    if(verbose){
      message("Append CO2 data ....")
    }

    # grab the CO2 data matching date ranges
    df_co2 <- ingest(
      siteinfo,
      source  = "co2_cmip",
      verbose = FALSE,
      dir = "data-raw/ancillary_data/co2/"
    )

    #---- Append FAPAR data ----

    if(verbose){
      message("Append FAPAR data ....")
    }

    if (!dir.exists("data-raw/modis/raw/")){
      stop("no FAPAR data found - download first")
    }

    settings_gee <- get_settings_gee(
        bundle            = "modis_fpar",
        python_path       = "/usr/bin/python3", # doesn't matter data should be downloaded
        gee_path          = "./src/gee_subset/src/gee_subset",
        data_path         = "data-raw/modis/",
        method_interpol   = "loess",
        keep              = TRUE,
        overwrite_raw     = FALSE,
        overwrite_interpol= TRUE
      )

      # run the ingest routine
      df_fapar <-
            ingest(
            siteinfo,
            source = "gee",
            settings = settings_gee,
            parallel = FALSE
          )
  }

  #---- Format p-model driver data ----
  if(verbose){
    message("Combining all driver data ....")
  }

  if(freq != "hh"){
    output <- collect_drivers_sofun(
      site_info      = siteinfo,
      params_siml    = params_siml,
      meteo          = ddf_meteo,
      fapar          = df_fapar,
      co2            = df_co2,
      params_soil    = df_soiltexture
    )
  } else {
    output <- ddf_flux
  }

  # return data, either a driver
  # or processed output
  return(output)
}
