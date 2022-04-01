library(tidyverse)
library(rsofun)

# load data
df <- readRDS("data/p_model_drivers/site_based_drivers.rds") %>%
  ungroup() # for some reason grouping variables mess up the model run

# set p-model settings
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

# run the model
output <- rsofun::runread_pmodel_f(
  df,
  par = params_modl
)

# model complains about years as reported not corresponding
# to the years in the forcing data (will check but can be
# safely ignored for now)

# save the output
saveRDS(
  output,
  file = "data/p_model_output/site_based_p-model_output.rds",
  compress = "xz"
  )
