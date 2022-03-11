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

  #---- startup checks ----

  # bail if not on euler
  if(!grepl('eu-', Sys.info()['nodename'])){
    stop("You are not on Euler, source data unavailable - abort abort abort!")
  }

  # bail if not on euler
  if(!dir.exists("~/data")){
    stop("Data path is not linked, create a soft link in your home directory
         to setup a link to the (CES) Euler data storage:
         ln -s /cluster/work/climate/bestocke/data data
         ")
  }

  # check format of the siteinfo
  names(siteinfo) %in% c("sitename","lon","lat","start_year","end_year","elv")

  if(!dir.exists("~/data")){
    stop("Data path is not linked, create a soft link in your home directory
         to setup a link to the (CES) Euler data storage:
         ln -s /cluster/work/climate/bestocke/data data
         ")
  }


  # some feedback on the processing
  if(verbose){
    message("Running on Euler, data linkages in place. Proceeding ....")
  }

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

      filn <- "~/data/mct_data/cwdx80.nc"
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

    whc_median <- median(siteinfo$whc, na.rm = TRUE)
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
    threshold_GPP = 0.8,
    remove_neg   = FALSE
  )

  if (product == "oneflux"){
    path = "~/data/flux_data_kit/oneflux/"
  }

  if (product == "icos"){
    path = "~/data/flux_data_kit/ICOS_releaseX/"
  }

  if (product == "plumber"){
    path = "~/data/flux_data_kit/plumber_fluxnet/"
  }

  if (product == "ameriflux"){
    path = "~/data/flux_data_kit/fluxnet2015/"
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
      netrad = "NETRAD"
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
        netrad = mean(netrad, na.rm = TRUE),
        ppfd = mean(ppfd, na.rm = TRUE) # exclude nighttime values?
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
      dir       = "~/data/cru/ts_4.01/",
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

    if(verbose){
      message("Append CO2 data ....")
    }

    # grab the CO2 data matching date ranges
    df_co2 <- ingest(
      siteinfo,
      source  = "co2_cmip",
      verbose = FALSE,
      dir = "~/data/co2/"
    )

    #---- Append FAPAR data ----

    if(verbose){
      message("Append FAPAR data ....")
    }

    # grab the FAPAR data
    ddf_fapar_unity <- ingest(
      siteinfo  = siteinfo,
      source    = "fapar_unity"
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
      fapar          = ddf_fapar_unity,
      co2            = df_co2,
      params_soil    = df_soiltexture
    )
  } else {
    output <- ddf_meteo
    )
  }

  # return data, either a driver
  # or processed output
  return(output)
}
