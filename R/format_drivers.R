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
#' @param bias_correction bolean TRUE/FALSE, when using high resolution
#'  locations set the bias correction to TRUE, for global runs leave FALSE
#'  (default)
#' @param verbose provide verbose output (default = FALSE)
#' @param run_model shoud model evaluation be run, which returns evaluated
#' data instead of driver files?
#'
#' @return returns an rsofun compatible driver file for the provided
#'  sites

format_drivers <- function(
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
  bias_correction = FALSE,
  verbose = TRUE,
  run_model = FALSE
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

    if(bias_correction){
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

    } else {

      if(verbose){
        message("Processing cwdx80 data from repo ....")
      }

      # use local data for global runs
      filn <- "data/mct/whc_0.5deg.tif"

      if(!file.exists(filn)){
        stop("Please generate 0.5 degree WHC values using:
             data-raw/mct/00_downsample_hwc.R in the project
             directory")
      }

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
    }

    ## fill gaps in whc
    whc_median <- median(siteinfo$whc, na.rm = TRUE)
    siteinfo <- siteinfo %>%
      mutate(whc = ifelse(is.na(whc), whc_median, whc))
  }

  #---- grabbing WATCH data ----

  # some feedback on the processing
  if(verbose){
    message("Processing WATCH data ....")
  }

  if(bias_correction){
    ddf_watch <- ingest(
      siteinfo = siteinfo,
      source    = "watch_wfdei",
      getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
      dir       = "~/data/watch_wfdei/",
      settings  = list(
        correct_bias = "worldclim",
        dir_bias = "~/data/worldclim")
    )
  } else {

    if(verbose){
      message("Processing WATCH data coarse levels ....")
    }

    ddf_watch <- ingest(
      siteinfo = siteinfo,
      source    = "watch_wfdei",
      getvars   = c("temp", "prec", "ppfd", "vpd", "patm"),
      dir       = "~/data/watch_wfdei/",
      settings = list(correct_bias = NULL)
    )

    if(verbose){
     print(head(ddf_watch$data[[1]]))
    }
  }

  # memory intensive, purge memory
  gc()

  #---- Processing CRU data ----
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
  ddf_meteo <- ddf_watch %>%
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

  #---- Format p-model driver data ----

  if(verbose){
    message("Combining all driver data ....")
  }

  output <- collect_drivers_sofun(
    siteinfo       = siteinfo,
    params_siml    = params_siml,
    meteo          = ddf_meteo,
    fapar          = ddf_fapar_unity,
    co2            = df_co2,
    df_soiltexture = df_soiltexture
  )

  if (run_model){
    message("Running model on formatted data ....")
    output <- process_pmodel_data(
      file = output
    )
  }

  # return data, either a driver
  # or processed output
  return(output)
}
