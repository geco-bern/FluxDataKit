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

  geco_system <- FALSE

  # check format of the site_info
  names(site_info) %in% c("sitename","lon","lat","start_year","end_year","elv")

  lapply(site_info$sitename, function(site){

  #---- complement site_info with WHC based on S_CWDX80 ----

  # TBD


    #---- merge in other data ----

      file <- list.files(
        path,
        glob2rx(sprintf("*%s*DD*",site)),
        full.names = TRUE
        )

      daily_fluxes <- read.table(
        file,
        header = TRUE,
        sep = ","
      )

      daily_fluxes <- daily_fluxes |>
        rename(
          date = "TIMESTAMP",
          gpp = "GPP_DT_VUT_REF",
          gpp_unc = "GPP_DT_VUT_SE",
          gpp_qc = "GPP_DT_VUT_REF_QC",
          temp = "TA_F_MDS",
          prec = "P_F",
          vpd = "VPD_F_MDS",
          patm = "PA_F",
          ppfd = "SW_IN_F_MDS",
          netrad = "NETRAD",
          wind = "WS_F",
          co2 = "CO2_F_MDS",
          lai = "LAI",
          fapar = "FPAR",
          le = "LE_F_MDS",
          le_qc = "LE_F_MDS_QC",
          le_cor = "LE_CORR",
          le_cor_qc = "LE_CORR_QC",
          gpp_qc = "GPP_DT_VUT_REF_QC",
          lw_down = "LW_IN_F_MDS",
          h = "H_F_MDS",
          h_cor = "H_CORR"
        ) |>
        select(
          date,
          gpp,
          gpp_unc,
          gpp_qc,
          temp,
          prec,
          vpd,
          patm,
          ppfd,
          netrad,
          wind,
          co2,
          lai,
          fapar,
          le,
          le_qc,
          le_cor,
          le_cor_qc,
          lw_down,
          h,
          h_cor
        ) |>
        mutate(
          date = as.Date(date),
          snow = 0
        )

    df_fapar <- daily_fluxes |>
      select(
        date,
        fapar
      )

    df_fapar <- tibble(
      sitename = site,
      fapar = list(df_fapar)
    )

    df_co2 <- daily_fluxes |>
      select(
        date,
        co2
      )

    df_co2 <- tibble(
      sitename = site,
      co2 = list(df_co2)
    )

    df_flux <- daily_fluxes |>
      select(
      -starts_with("fapar"),
      -starts_with("co2")
    )

    df_flux <- tibble(
      sitename = site,
      data = list(df_flux)
    )

  # GPP conversion factor
  # in FLUXNET given in umolCO2 m-2 s-1. converted to gC m-2 d-1
  c_molmass <- 12.0107  # molar mass of C
  gpp_coversion <- 1e-6 * 60 * 60 * 24 * c_molmass

  #---- Processing CRU data (for cloud cover CCOV) ----
  if(verbose){
    message("Processing CRU data ....")
  }

  if (geco_system){
    df_cru <- ingest(
      siteinfo = site_info,
      source    = "cru",
      getvars   = "ccov",
      dir       = "/data/archive/cru_NA_2021/data/", # f-ing trailing /
      settings = list(correct_bias = NULL)
    )
  } else {
    df_flux$data[[1]]$ccov <- 0
  }
  # memory intensive, purge memory
  gc()

  #---- Merging climate data ----
  if(verbose){
    message("Merging climate data ....")
  }

  if (geco_system) {
    df_flux <- df_flux |>
      tidyr::unnest(data) |>
      left_join(
        df_cru |>
          tidyr::unnest(data),
        by = c("sitename", "date")
      ) |>
      group_by(sitename) |>
      tidyr::nest()
  }

  #---- Format p-model driver data ----
  if(verbose){
    message("Combining all driver data ....")
  }

  output <- fdk_collect_drivers(
    site_info      = site_info,
    params_siml    = params_siml,
    meteo          = df_flux,
    fapar          = df_fapar,
    co2            = df_co2,
    params_soil    = df_soiltexture
  )

  # return data, either a driver
  # or processed output
  return(output)
  })



}
