library(rsofun)
library(tidyverse)

# read data
driver_data <- readRDS("data/rsofun_driver_data_clean.rds")

# optimized parameters
params_modl <- list(
  kphio              = 0.04998,    # setup ORG in Stocker et al. 2020 GMD
  kphio_par_a        = 0.0,        # set to zero to disable temperature-dependence of kphio
  kphio_par_b        = 1.0,
  soilm_thetastar    = 0.6 * 240,  # to recover old setup with soil moisture stress
  soilm_betao        = 0.0,
  beta_unitcostratio = 146.0,
  rd_to_vcmax        = 0.014,      # value from Atkin et al. 2015 for C3 herbaceous
  tau_acclim         = 30.0,
  kc_jmax            = 0.41
)

for(i in driver_data$sitename){
      df <- driver_data |>
        filter(
          sitename == i
        ) |>
        ungroup()

      # run the model for these parameters
      output <- try(rsofun::runread_pmodel_f(
        df,
        par = params_modl,
        makecheck = TRUE
      ))

      print(output)

      if(inherits(output, "try-error")){
        return(NULL)
      }

      # we only have one site so we'll unnest
      # the main model output
      model_data <- output |>
        tidyr::unnest("data")

      validation_data <- df |>
        tidyr::unnest(forcing)

      p <- ggplot() +
        geom_line(
          data = model_data,
          aes(
            date,
            gpp
          ),
          colour = "red"
        ) +
        geom_line(
          data = validation_data,
          aes(
            date,
            gpp
          )
        ) +
        labs(
          x = "Date",
          y = "GPP"
        )

      ggsave(
        file.path(
          "./manuscript/figures/",
          paste0(df$sitename,".png")
        ),
        width = 12,
        height = 7
      )

}
