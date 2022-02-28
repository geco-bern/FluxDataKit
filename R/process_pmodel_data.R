#' Run the p-model and return summary stats
#' 
#' This routine calculates summary statistics
#' from p-model output when provided with 
#' an RDS file containing a single or multiple instance
#' of (combined) p-model driver files (ingestr created)
#'
#' @param file an RDS file containing driver data
#'
#' @return summary statistics for modelling purposes

process_pmodel_data <- function(file){
  
  # if input file is a dataframe assume
  # rsofun driver file / no classes are assigned
  if(is.character(file)){
    # read in file
    df <- try(readRDS(file))
  } else {
    df <- file
  }
    
  # check succesfull read
  if(inherits(df, "try-error")){
    return(NULL)
  }
  
  # set model parameters
  params_modl <- list(
    kphio           = 0.09423773,
    soilm_par_a     = 0.33349283,
    soilm_par_b     = 1.45602286,
    tau_acclim_tempstress = 10,
    par_shape_tempstress  = 0.0
  )
  
  ## run P-model for the read file
  df_output <- try(
    suppressWarnings(
      suppressMessages(
        runread_pmodel_f(
          df,
          par = params_modl,
          makecheck = TRUE,
          parallel = FALSE
        )
      )
    )
  )
  
  # check on success of the run
  if(inherits(df, "try-error")){
    return(NULL)
  }
  
  # get basic meta-data (lat lon)
  # drop data first
  siteinfo <- df_output %>%
    dplyr::select(-data) %>%
    unnest(cols = c(sitename, siteinfo))
  
  # Calculate summary statistics ----

  # get mean annual AET separately
  evapotranspiration <- df_output %>% 
    unnest(data) %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(sitename, year) %>% 
    summarise(transp = sum(transp, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(sitename) %>% 
    summarise(aet = mean(transp, na.rm = TRUE))
  
  # photosynthesis parameters
  photosynthesis_parameters <- df_output %>%
    
    # calculate mean over daily AET/PET
    mutate(data = purrr::map(data, ~mutate(., alpha = transp / pet))) %>% 
    
    # gpp-weighted mean of vcmax25, jmax25, gs_accl, mean daily AET/PET
    mutate(
      data = purrr::map(data, ~mutate(.,
      vcmax25_wgt = vcmax25 * gpp,
      jmax25_wgt = jmax25 * gpp,
      gs_accl_wgt = gs_accl * gpp))) %>% 
    mutate(
      data = purrr::map(data, ~summarise(.,
        gpp_sum = sum(gpp), 
        vcmax25_wgt_sum = sum(vcmax25_wgt),
        jmax25_wgt_sum = sum(jmax25_wgt),
        gs_accl_wgt_sum = sum(gs_accl_wgt),
        alpha = mean(alpha)))) %>% 
    mutate(data = purrr::map(data, ~mutate(.,
        vcmax25 = vcmax25_wgt_sum / gpp_sum,
        jmax25 = jmax25_wgt_sum / gpp_sum,
        gs_accl = gs_accl_wgt_sum / gpp_sum))) %>% 
    unnest(data) %>% 
    dplyr::select(sitename, alpha, vcmax25, jmax25, gs_accl)
  
  # get climate indices from ddf_watch
  climate_indices <- df %>%
    mutate(
      mat = purrr::map_dbl(forcing,
                           ~calc_climate_index_mat(.)),
      matgs = purrr::map_dbl(forcing,
                             ~calc_climate_index_matgs(., temp_base = 5.0)),
      tmonthmin = purrr::map_dbl(forcing,
                                 ~calc_climate_index_tmonthmin(.)),
      tmonthmax = purrr::map_dbl(forcing,
                                 ~calc_climate_index_tmonthmax(.)),
      ndaysgs = purrr::map_dbl(forcing,
                               ~calc_climate_index_ndaysgs(., temp_base = 5.0)),
      mai = purrr::map_dbl(forcing,
                           ~calc_climate_index_mai(.)),
      maigs = purrr::map_dbl(forcing,
                             ~calc_climate_index_maigs(., temp_base = 5.0)),
      map = purrr::map_dbl(forcing,
                           ~calc_climate_index_map(.)),
      pmonthmin = purrr::map_dbl(forcing,
                                 ~calc_climate_index_pmonthmin(.)),
      mapgs = purrr::map_dbl(forcing,
                             ~calc_climate_index_mapgs(., temp_base = 5.0)),
      mavgs = purrr::map_dbl(forcing,
                             ~calc_climate_index_mavgs(., temp_base = 5.0)),
      mav = purrr::map_dbl(forcing,
                           ~calc_climate_index_mav(.))) %>% 
    dplyr::select(-forcing, -siteinfo, -params_siml, -df_soiltexture)
  
  # bind everything for output
  output <- climate_indices %>% 
    right_join(photosynthesis_parameters, by = "sitename") %>% 
    left_join(evapotranspiration, by = "sitename") %>% 
    mutate(ai = map / aet)
  
  # bind meta-data to summary stats
  output <- left_join(siteinfo, output, by = "sitename")
  
  # return output (tibble/dataframe)
  return(output)
}
