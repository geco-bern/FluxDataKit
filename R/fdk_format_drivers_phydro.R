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
#' @param path path with daily (downsampled) FLUXNET data
#' @param out_path path to save drivers and validation data
#'
#' @return returns an rsofun compatible driver file for the provided
#'  sites
#' @export

fdk_format_drivers_phydro <- function(
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
      lgr4                      = FALSE,
      use_phydro                = FALSE,
      use_pml                   = FALSE
    ),
    path,
    out_path = path,
    verbose = TRUE,
    fdk_inputs_dir = "/data_2/FluxDataKit/FDK_inputs/"
){

  #---- start-up checks ----

  geco_system <- ifelse(
    Sys.info()['nodename'] == "balder" | Sys.info()['nodename'] == "dash",
    TRUE,
    FALSE
  )

  # check format of the site_info
  list_flux <- lapply(site_info$sitename, function(site){

    df_combined <- tidyr::tibble(sitename = site)

    # get file name path
    filn <- list.files(file.path(path, "fluxnet"),
                       pattern = paste0("FLX_", site, ".*_FULLSET_DD.*.csv"),
                       recursive = TRUE,
                       full.names = TRUE
    )

    # conversion factor from SPLASH: flux to energy conversion,
    # umol/J (Meek et al., 1984)
    kfFEC <- 2.04

    #---- Processing CRU data (for cloud cover CCOV) ----
    ccov <- fdk_process_cloud_cover(
      path = file.path(fdk_inputs_dir, "cloud_cover"), # "/Users/benjaminstocker/data/FluxDataKit/FDK_inputs/cloud_cover/",
      site = site
    )
    # # dummy data for testing, in case cloud cover data is unavailable
    # ccov <- df_flux |>
    #   tidyr::unnest(data) |>
    #   dplyr::mutate(date = as.Date(TIMESTAMP)) |>
    #   dplyr::select(sitename, date) |>
    #   dplyr::mutate(ccov = 0)

    agg_types <- c("legacy", "24h", "daytime", "3hrmax")
    for (agg in agg_types){

      if (!any(grepl(agg, x=filn))) stop("Missing downsampled file: ", agg)
      fname <- filn[grepl(agg, x=filn)]

      # read from FLUXNET-standard file with daily variables
      df_flux <-  readr::read_csv(file.path(fname)) |>

        dplyr::mutate(

          sitename = site,
          date = as.Date(TIMESTAMP),

          # temperature (deg C)
          temp = TA_F_MDS,

          # vapour pressure deficit
          # given in hPa, required in Pa
          vpd = VPD_F_MDS * 1.0e2,

          # photosynthetic photon flux density based on shortwave radiation
          # convert from J/m2/s to mol/m2/s;
          # kfFEC = 2.04 is the flux-to-energy conversion, micro-mol/J
          # (Meek et al., 1984)
          ppfd = SW_IN_F_MDS * kfFEC * 1.0e-6,

          # net radiation (J m-2 s-1 = W m-2)
          netrad = NETRAD,

          # atmospheric pressure, given in kPa, required in Pa
          patm = PA_F * 1e3,

          # precipitation as snow, in mm
          # (is not provided explicilty in FLUXNET-type data)
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
          co2 = CO2_F_MDS,

          # used as target data for rsofun, not forcing
          # gross primary production
          gpp_dt = GPP_DT_VUT_REF,
          gpp_nt = GPP_NT_VUT_REF,
          gpp_qc = NEE_VUT_REF_QC,

          # energy balance-corrected latent heat flux (~ evapotranspiration)
          # Jaideep: LE_CORR is not available for all sites. Fall back to LE_F_MDS in that case
          le_corr = LE_CORR,
          le = LE_F_MDS,
          le_qc = LE_F_MDS_QC
        )
      # nest by site
      df_flux <- df_flux |>
        dplyr::group_by(sitename) |>
        tidyr::nest()

      df_flux <- df_flux |>
        tidyr::unnest(data) |>
        dplyr::left_join(
          ccov, by = c("sitename", "date")
        ) |>
        dplyr::group_by(sitename) |>
        tidyr::nest()

      # if (geco_system){
      #
      #   if (verbose){
      #     message("Processing ERA5 cloud cover data ....")
      #   }
      #
      #   # include cloud cover data if on
      #   # internal GECO system, will skip
      #   # when external as the download takes
      #   # lots of time
      #
      #   # constrain range of dates to
      #   # required data
      #
      #   ccov <- fdk_process_cloud_cover(
      #     path = file.path(fdk_inputs_dir, "cloud_cover"), # "/Users/benjaminstocker/data/FluxDataKit/FDK_inputs/cloud_cover/",
      #     site = site
      #   )
      #
      #  df_flux <- df_flux |>
      #     tidyr::unnest(data) |>
      #     left_join(
      #       ccov, by = c("sitename", "date")
      #     ) |>
      #     group_by(sitename) |>
      #     tidyr::nest()
      #
      # } else {
      #
      #   message("Filling cloud cover forcing with 0.
      #           Use net radiation for simulations.")
      #
      #   df_flux <- df_flux |>
      #     tidyr::unnest(data) |>
      #     mutate(
      #      ccov = 0
      #     ) |>
      #     group_by(sitename) |>
      #     tidyr::nest()
      #
      # }

      df_flux <- df_flux |>
        dplyr::group_by(sitename) |>
        tidyr::unnest(data) |>
        dplyr::select(
          sitename,
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
          co2,
          ccov,
          gpp_dt,
          gpp_nt,
          gpp_qc,
          le_corr,
          le,
          le_qc
        ) |>
        dplyr::group_by(sitename) |>
        tidyr::nest()

      # remove 29 Feb of leap years
      df_flux <- df_flux |>
        tidyr::unnest(data) |>
        dplyr::filter(
          !(lubridate::mday(date) == 29 & lubridate::month(date) == 2)) |>
        dplyr::group_by(sitename) |>
        tidyr::nest()

      # memory intensive, purge memory
      gc()

      df_flux <- df_flux |>
        set_names(c("sitename", paste0("forcing_", agg)))

      df_combined <- df_combined |>
        left_join(df_flux)
    }
    # return data, either a driver
    # or processed output
    return(df_combined)
  }) |>
    dplyr::bind_rows()

  #---- Format p-model driver data ----
  if (verbose){
    message("Combining all driver data ....")
  }

  # construct the final driver file
  # first join in the site info data
  df_drivers <- site_info |>
    dplyr::select(sitename, lon, lat, elv, whc) |>
    dplyr::group_by(sitename) |>
    tidyr::nest() |>
    dplyr::rename(
      site_info = data
    )

  # join in the flux driver data
  df_drivers <- df_drivers |>
    dplyr::left_join(list_flux, by = "sitename")

  # join in the parameter settings
  df_drivers <- df_drivers |>
    dplyr::left_join(
      dplyr::tibble(
        sitename = site_info$sitename
      ) |>
        dplyr::bind_cols(
          params_siml |>
            dplyr::slice(rep(1:n(), each = nrow(site_info)))
        ) |>
        dplyr::group_by(sitename) |>
        tidyr::nest() |>
        dplyr::rename(params_siml = data),
      by = "sitename")

  # change order - critical for rsofun
  # but also checked in rsofun
  df_drivers <- df_drivers |>
    dplyr::select(
      sitename,
      params_siml,
      site_info,
      forcing = forcing_legacy,
      forcing_24h,
      forcing_daytime,
      forcing_3hrmax
    ) |>
    dplyr::ungroup()

  # remove target variables from forcing dataset
  df_drivers_only = df_drivers |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::starts_with("forcing"),
        .fns = function(df){
          df |>
            dplyr::select(-tidyselect::starts_with(c("gpp", "le"))) |>
            list()
        }
      )
    )

  # collect target variables into validation dataset
  df_validation = df_drivers |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::starts_with("forcing"),
        .fns = function(df){
          df |>
            dplyr::select(date, tidyselect::starts_with(c("gpp", "le"))) |>
            dplyr::mutate(gpp_dt  = ifelse(gpp_qc > 0.5, yes=gpp_dt,  no=NA),
                          gpp_nt  = ifelse(gpp_qc > 0.5, yes=gpp_nt,  no=NA),
                          le_corr = ifelse(le_qc  > 0.5, yes=le_corr, no=NA),
                          le      = ifelse(le_qc  > 0.5, yes=le,      no=NA),
                          ) |>
            list()
        }
      )
    ) |>
    rename_all(.funs = stringr::str_replace,
               pattern = "forcing",
               replacement = "data")

  saveRDS(df_drivers_only, file.path(out_path, "p_model_drivers.rds"))
  saveRDS(df_validation, file.path(out_path, "p_model_validation.rds"))

  saveRDS(df_drivers, file.path(out_path, "p_model_combined_drivers_validation.rds"))

  return(df_drivers)
}
