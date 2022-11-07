library(rsofun)

# set model parameters
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

# run P-model for rsofun driver file
df_output <- rsofun::runread_pmodel_f(
        driver_data,
        par = params_modl,
        makecheck = TRUE,
        parallel = FALSE
      )$data[[1]]

# plot both the original gpp
# data as well as model results
# (no parameter optimization)
p <- ggplot(df_output) +
  geom_point(
    aes(
      date,
      gpp
    )
  ) +
  geom_point(
    data = driver_data$forcing[[1]],
    aes(
      date,
      gpp
    ),
    colour = "red"
  )

plot(p)

