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
#' being five columns: sitename, lon, lat, elv, whc
#' @param params_siml simulation parameters (preset)
#' @param product which flux product to use
#' @param verbose provide verbose output (default = FALSE)
#' @param path path with daily FLUXNET data
#'
#' @return returns an rsofun compatible driver file for the provided
#'  sites
#' @export

fdk_format_drivers <- function(
    site_info,
    params_siml = dplyr::tibble(
      spinup                    = TRUE,  # to bring soil moisture to steady state
      spinupyears               = 10,    # 10 is enough for soil moisture.
      recycle                   = 1,     # number of years recycled during spinup
      outdt                     = 1,     # periodicity of output. Chose integer greater than 1 to aggregate outputs.
      ltre                      = FALSE,
      ltne                      = FALSE,
      ltrd                      = FALSE,
      ltnd                      = FALSE,
      lgr3                      = TRUE,
      lgn3                      = FALSE,
      lgr4                      = FALSE
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

  #geco_system <- FALSE

  # check format of the site_info
  names(site_info) %in% c("sitename", "lon", "lat", "start_year", "end_year", "elv")

  list_flux <- lapply(site_info$sitename, function(site){

    # get file name path
    filn <- list.files(path,
                       pattern = paste0("FLX_", site, ".*_FULLSET_DD.*.csv"),
                       recursive = TRUE
    )

    # conversion factor from SPLASH: flux to energy conversion,
    # umol/J (Meek et al., 1984)
    kfFEC <- 2.04

    # read from FLUXNET-standard file with daily variables
    df_flux <-  read.csv(file.path(path, filn)) |>

       dplyr::mutate(

         sitename = site,
         date = as.Date(TIMESTAMP),

         # temp is daytime temperature (deg C)
         temp = TA_DAY_F_MDS,

         # vapour pressure deficit is averaged over daytime hours, given in hPa, required in Pa
         vpd = VPD_DAY_F_MDS * 1.0e2,

         # photosynthetic photon flux density based on shortwave radiation
         # convert from J/m2/s to mol/m2/s;
         # kfFEC = 2.04 is the flux-to-energy conversion, micro-mol/J
         # (Meek et al., 1984)
         ppfd = SW_IN_F_MDS * kfFEC * 1.0e-6,

         # net radiation (J m-2 s-1 = W m-2)
         netrad = NETRAD,

         # atmospheric pressure, given in kPa, required in Pa
         patm = PA_F * 1e3,

         # precipitation as snow, in mm (is not provided explicilty in FLUXNET-type data)
         snow = 0,

         # precipitation as water, mean rate over time step (day, 24 hours)
         rain = P_F / (60 * 60 * 24),

         # minimum daily temperature (deg C)
         tmin = TMIN_F_MDS,

         # maximum daily temperature (deg C)
         tmax = TMAX_F_MDS,

         # fraction of absorbed photosynthetically active radiation
         fapar = FPAR,

         # atmospheric CO2 concentration in ppmv
         co2 = CO2_F_MDS
      ) |>
      dplyr::select(sitename,
                    date,
                    temp,
                    vpd,
                    ppfd,
                    netrad,
                    patm,
                    snow,
                    rain,
                    tmin,
                    tmax,
                    fapar,
                    co2) |>
      dplyr::group_by(sitename) |>
      tidyr::nest()


    #--- merge in missing QC flags which are tossed by ingestr ---
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

    # keep long list of variables for further
    # notice - might include more variables later
    qc_fluxes <- daily_fluxes |>
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
        gpp_qc,
        le_cor,
        le_qc,
        le_cor_qc
      ) |>
      mutate(
        date = as.Date(date)
      )

    qc_fluxes <- tibble(
      sitename = site,
      data = list(qc_fluxes)
    )

    df_flux <- df_flux |>
      tidyr::unnest(data) |>
      left_join(
        qc_fluxes |>
          tidyr::unnest(data),
        by = c("sitename", "date")
      )

    # GPP conversion factor
    # in FLUXNET given in umolCO2 m-2 s-1. converted to gC m-2 d-1
    # c_molmass <- 12.0107  # molar mass of C
    # gpp_coversion <- 1e-6 * 60 * 60 * 24 * c_molmass
    # df_flux$data[[1]]$gpp <- df_flux$data[[1]]$gpp

    #---- Processing CRU data (for cloud cover CCOV) ----
    if (geco_system){

      if (verbose){
        message("Processing ERA5 cloud cover data ....")
      }

      # include cloud cover data if on
      # internal GECO system, will skip
      # when external as the download takes
      # lots of time

      # constrain range of dates to
      # required data

      ccov <- fdk_process_cloud_cover(
        path = "data-raw/cloud_cover/",
        site = site
      )

     df_flux <- df_flux |>
        tidyr::unnest(data) |>
        left_join(
          ccov, by = c("sitename", "date")
        ) |>
        group_by(sitename) |>
        tidyr::nest()

    } else {

      message("Filling cloud cover forcing with 0. Use net radiation for simulations.")
      df_flux$data[[1]]$ccov <- 0

    }
    # # memory intensive, purge memory
    # gc()

    # #---- Merging climate data ----
    # if(verbose){
    #   message("Merging climate data ....")
    # }
    # if (geco_system) {
    #   df_flux <- df_flux |>
    #     tidyr::unnest(data) |>
    #     left_join(
    #       df_cru |>
    #         tidyr::unnest(data),
    #       by = c("sitename", "date")
    #     ) |>
    #     group_by(sitename) |>
    #     tidyr::nest()
    # }

    # return data, either a driver
    # or processed output
    return(df_flux)
  })

  df_flux <- list_flux |>
    dplyr::bind_rows() |>
    dplyr::group_by(sitename) |>
    tidyr::nest()

  #---- Format p-model driver data ----
  if(verbose){
    message("Combining all driver data ....")
  }

  df_drivers <- site_info |>

    # nest site info for each site
    dplyr::group_by(sitename) |>
    tidyr::nest() |>
    dplyr::rename(site_info = data) |>

    # combine forcing time series nested data frame
    dplyr::left_join(df_flux, by = "sitename") |>
    dplyr::rename(forcing = data) |>

    # repeat the same simulation parameters for each site
    dplyr::left_join(
      tibble(
        sitename = site_info$sitename
        ) |>
        bind_cols(
          params_siml |>
            dplyr::slice(rep(1:n(), each = nrow(site_info)))
        ) |>
        dplyr::group_by(sitename) |>
        tidyr::nest() |>
        dplyr::rename(params_siml = data),
      by = "sitename")

  return(df_drivers)
}
