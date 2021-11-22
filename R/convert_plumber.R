#' Convert plumber variables to FLUXNET
#'
#' Converts plumber data to the FLUXNET
#' format to retain the standard ingestr
#' workflow
#'
#' @param site
#' @param path
#'
#' @return
#' @export

convert_plumber <- function(
  site = "AT-Neu",
  path = "/scratch/CES/plumber/"
  ){

  # read in plumber data for a site
  # and directory
  df <- merge_plumber(
    site = site,
    path = path
  )

  # convert time, and only return
  # FLUXNET formatted columns
  df <- df %>%
    mutate(
      TIMESTAMP_START = format(time, "%Y%m%d%H%M"),
      TIMESTAMP_END = format(time + 30 * 60, "%Y%m%d%H%M")
    ) %>%
    rename(
      TA_F = Tair,
      SW_IN_F = SWdown,
      LW_IN_F = LWdown,
      VPD_F = VPD,
      WS_F = Wind,
      PA_F = Psurf,
      CO2_F = CO2air,
      NETRAD = Rnet,
      USTAR = Ustar,
      SW_OUT = SWup,
      LE_F_MDS = Qle,
      LE_CORR = Qle_cor,
      H_F_MDS = Qh,
      H_CORR = Qh_cor
    ) %>%
    select(
      TIMESTAMP_START,
      TIMESTAMP_END,
      TA_F,
      SW_IN_F,
      LW_IN_F,
      VPD_F,
      WS_F,
      PA_F,
      CO2_F,
      NETRAD,
      USTAR,
      SW_OUT,
      LE_F_MDS,
      LE_CORR,
      H_F_MDS,
      H_CORR
    )

  return(df)
}
